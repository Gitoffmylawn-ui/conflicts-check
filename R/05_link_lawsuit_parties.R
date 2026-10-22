# Links every party named in the CourtListener lawsuits to the firm's records,
# so the app's Lawsuits tab can show at a glance which cases touch the firm.
#
# Each distinct party name goes through the same matcher and rules as a
# conflicts check. Doing it once here keeps the app fast.
#
# Output:
#   data/lawsuit_party_links.csv   one row per distinct party name
#
# Requires 01-03. Rerun after 03 changes the firm's records.

source("R/conflict_engine.R")

engine <- prepare_engine()
parties <- read_csv("data/matter_parties.csv", col_types = cols(.default = "c"))
firm_parties <- read_csv("data/firm_matter_parties.csv", col_types = cols(.default = "c"))

# Strongest tie first; a party's headline tie is the first one it has.
tie_labels <- c(
  current_client_adverse = "Current client",
  affiliate_of_current_client = "Affiliate of a current client",
  former_client_substantially_related = "Former client",
  former_client_unrelated = "Former client",
  affiliate_of_former_client = "Affiliate of a former client",
  lateral_hire_prior_client = "Prior client of a lateral hire",
  business_relationship = "Counterparty on a firm matter",
  firm_adverse = "Adverse party on a firm matter"
)
tie_kinds <- c(
  "Current client" = "current", "Affiliate of a current client" = "affiliate", "Former client" = "former",
  "Affiliate of a former client" = "former", "Prior client of a lateral hire" = "lateral",
  "Counterparty on a firm matter" = "pair", "Adverse party on a firm matter" = "open"
)

firm_adverse_keys <- unique(filter(firm_parties, role == "adverse")$party_key)

link_party <- function(name) {
  m <- head(match_name(engine, name), 1)
  if (!nrow(m) || m$score < match_thresholds[["review"]]) {
    return(tibble(party_name = name, matched_key = NA, matched_name = NA, match_score = NA, match_status = "none",
                  tie = NA, tie_kind = NA, ties_detail = NA))
  }
  # Practice area is unknown for a lawsuit, so former clients come out as
  # "unrelated"; both former-client types get the same label here anyway.
  h <- hits_for_key(engine, m$key, "adverse", "")
  ties <- if (nrow(h)) tibble(type = h$hit_type, detail = h$detail) else tibble(type = character(), detail = character())
  if (m$key %in% firm_adverse_keys) ties <- add_row(ties, type = "firm_adverse", detail = "Opposed by the firm on one of its matters")
  ties <- ties |> mutate(label = unname(tie_labels[type])) |> arrange(match(type, names(tie_labels)))
  # A bare surname matching a person in the firm's records may well be someone
  # else, so it needs a person to confirm it, however exact the match.
  surname_only <- str_starts(m$key, "NAME:") && !str_detect(norm_name(name), " ")
  tibble(
    party_name = name, matched_key = m$key, matched_name = m$name, match_score = round(m$score, 3),
    match_status = if (m$score >= match_thresholds[["accept"]] && !surname_only) "match" else "review",
    tie = if (nrow(ties)) ties$label[1] else NA,
    tie_kind = if (nrow(ties)) unname(tie_kinds[ties$label[1]]) else NA,
    ties_detail = if (nrow(ties)) paste(unique(paste0(ties$label, ": ", ties$detail)), collapse = " | ") else NA
  )
}

names_to_link <- sort(unique(parties$party_name))
message(sprintf("Linking %s party names", format(length(names_to_link), big.mark = ",")))
links <- map_dfr(names_to_link, link_party, .progress = TRUE)

write_out(links, "lawsuit_party_links")
message(sprintf("%d names matched a known entity or party; %d have a tie to the firm",
                sum(links$match_status == "match"), sum(!is.na(links$tie) & links$match_status == "match")))
