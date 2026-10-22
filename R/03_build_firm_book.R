# Part 2 of the model, firm side, plus labeled test cases.
#
# Real firms keep their client and matter records confidential, so this
# builds a fictional firm, Hartwell & Pierce LLP, on top of the real data:
# its clients are real GLEIF entities, and many of its litigation matters
# mirror real CourtListener dockets (with the real opposing parties).
#
# It then generates new-matter intake requests and labels each one with the
# conflicts a careful analyst should find. Those labels are the answer key
# for testing and training the matching/scoring layer.
#
# Output:
#   data/firm_attorneys.csv, firm_clients.csv, firm_matters.csv,
#   data/firm_matter_parties.csv, lateral_prior_clients.csv,
#   data/intake_requests.csv, intake_parties.csv, intake_expected_hits.csv
#
# Requires 01 and 02 to have run. Seeded, so reruns give the same firm.

source("R/utils.R")
set.seed(673)

read_data <- function(name) read_csv(file.path("data", paste0(name, ".csv")), col_types = cols(.default = "c"))
families <- read_data("families")
entities <- read_data("entities")
rels <- read_data("entity_relationships")
matters <- read_data("matters")
mparties <- read_data("matter_parties")

as_of <- as.Date("2026-10-21")
former_cutoff <- as.Date("2025-06-30") # former clients' last matter closed by here

# ---- Lookups ---------------------------------------------------------------

active_entities <- filter(entities, entity_status == "ACTIVE", !is.na(legal_name))
name_of <- setNames(entities$legal_name, entities$lei)
family_of <- setNames(entities$family_id, entities$lei)
parent_of <- setNames(rels$parent_lei, rels$child_lei)

ancestors <- function(lei) {
  out <- character()
  p <- unname(parent_of[lei])
  while (!is.na(p) && !(p %in% out)) {
    out <- c(out, p)
    p <- unname(parent_of[p])
  }
  out
}

# Exact normalized-name lookup, used to attach LEIs to docket party names.
name_index <- entities |>
  filter(!is.na(norm_name), nzchar(norm_name)) |>
  arrange(entity_status != "ACTIVE", family_id != lei) |>
  distinct(norm_name, .keep_all = TRUE)
resolve_lei <- function(names) name_index$lei[match(norm_name(names), name_index$norm_name)]

# One identity per real-world party: its LEI when known, else its normalized name.
party_key <- function(name, lei) if_else(is.na(lei), paste0("NAME:", norm_name(name)), lei)

terms_of <- function(x) str_trim(str_split(x, ";")[[1]])
side_mentions <- function(terms, side) {
  map_lgl(side, \(s) !is.na(s) && any(str_detect(norm_name(s), fixed(norm_name(terms)))))
}

rand_date <- function(from, to) as.Date(from) + sample(0:as.integer(as.Date(to) - as.Date(from)), 1)

# ---- Fictional people and companies ----------------------------------------

first_names <- c("Avery", "Jordan", "Morgan", "Riley", "Casey", "Quinn", "Harper", "Rowan", "Elliot", "Sasha",
                 "Dana", "Reese", "Cameron", "Hayden", "Parker", "Drew", "Emerson", "Kendall", "Logan", "Sidney",
                 "Taylor", "Blair", "Jamie", "Robin", "Alex", "Marlowe", "Shawn", "Leslie", "Frankie", "Devon")
last_names <- c("Okafor", "Lindqvist", "Ramirez", "Chen", "Abernathy", "Whitlock", "Nakamura", "Delacroix",
                "Brennan", "Petrov", "Osei", "Haddad", "Kowalski", "Iyer", "Fairbanks", "Moreau", "Sato",
                "Gallagher", "Mbeki", "Novak", "Castellano", "Ferreira", "Holloway", "Varga", "Quist",
                "Adeyemi", "Rourke", "Bjork", "Tanaka", "Solberg")
fake_people <- function(n) {
  expand_grid(f = first_names, l = last_names) |> slice_sample(n = n) |> mutate(name = paste(f, l)) |> pull(name)
}
fake_companies <- function(n) {
  a <- c("Ridgeline", "Bluewater", "Granite", "Northgate", "Cedar", "Ironwood", "Harbor", "Summit", "Prairie",
         "Lakeshore", "Copperleaf", "Silverline", "Oakmont", "Redfern", "Westbrook", "Stonebridge")
  b <- c("Logistics", "Analytics", "Foods", "Manufacturing", "Health Partners", "Capital", "Energy Services",
         "Software", "Building Supply", "Medical Devices", "Freight", "Retail Group")
  s <- c("LLC", "Inc.", "Corp.", "Holdings LLC", "L.P.")
  expand_grid(a, b) |> slice_sample(n = n) |> mutate(name = paste(a, b, sample(s, n, TRUE))) |> pull(name)
}

# Unaffiliated organizations whose names resemble seed companies. A good
# conflicts check should surface them but clear them.
lookalikes <- c("Merck KGaA", "Apple Hospitality REIT, Inc.", "Boeing Employees' Credit Union",
                "Ford Models, Inc.", "Verizon Valley Farms LLC", "Chevron Street Bakery LLC",
                "Comcast Creek Outfitters Inc.", "Pfizer Lane Dental Group PLLC")

# How a name might be typed into an intake form.
swap_typo <- function(s) {
  n <- nchar(s)
  if (n < 6) return(s)
  i <- sample(2:(n - 2), 1)
  ch <- strsplit(s, "")[[1]]
  ch[c(i, i + 1)] <- ch[c(i + 1, i)]
  paste(ch, collapse = "")
}
name_variants <- list(
  as_is = \(s) s,
  upper = toupper,
  title = str_to_title,
  drop_suffix = \(s) str_squish(str_remove(s, regex(",?\\s*(Inc\\.?|Incorporated|Corporation|Corp\\.?|Company|Co\\.?|LLC|L\\.L\\.C\\.|Ltd\\.?|Limited|N\\.A\\.)$", ignore_case = TRUE))),
  abbreviate = \(s) s |> str_replace(regex("\\bCorporation\\b", ignore_case = TRUE), "Corp.") |> str_replace(regex("\\bCompany\\b", ignore_case = TRUE), "Co."),
  no_punct = \(s) str_remove_all(s, "[.,']"),
  and_amp = \(s) str_replace_all(s, "&", "and"),
  typo = swap_typo
)
variant_weights <- c(as_is = 3, upper = 1, title = 2, drop_suffix = 3, abbreviate = 2, no_punct = 2, and_amp = 1, typo = 1)
perturb_name <- function(s) {
  for (k in seq_len(sample(1:2, 1))) s <- name_variants[[sample(names(variant_weights), 1, prob = variant_weights)]](s)
  s
}

# ---- Attorneys --------------------------------------------------------------

practice_groups <- c("Commercial Litigation", "IP Litigation", "Product Liability", "Employment",
                     "Securities Litigation", "Antitrust", "Corporate/M&A", "Finance", "Regulatory")
lateral_firms <- c("Ashby Kline LLP", "Marrow & Vance LLP", "Stroud Pellham LLP")

attorneys <- tibble(
  attorney_id = sprintf("A%03d", 1:27),
  name = fake_people(27),
  practice_group = sample(rep_len(practice_groups, 27)),
  title = sample(c("Partner", "Partner", "Counsel", "Associate"), 27, TRUE),
  is_lateral = rep(c(FALSE, TRUE), c(24, 3)),
  prior_firm = c(rep(NA, 24), lateral_firms)
) |>
  mutate(join_date = if_else(is_lateral,
                             map_chr(1:27, \(i) as.character(rand_date("2026-06-01", "2026-09-30"))),
                             map_chr(1:27, \(i) as.character(rand_date("2005-01-01", "2022-12-31")))))

pick_attorney <- function(practice) {
  pool <- filter(attorneys, practice_group == practice, !is_lateral)$attorney_id
  if (!length(pool)) pool <- filter(attorneys, !is_lateral)$attorney_id
  pool[sample.int(length(pool), 1)]
}

practice_from_nos <- function(nos) {
  nos <- coalesce(nos, "")
  case_when(
    str_detect(nos, regex("patent|trademark|copyright", ignore_case = TRUE)) ~ "IP Litigation",
    str_detect(nos, regex("product liab|personal inj|pharmaceutical", ignore_case = TRUE)) ~ "Product Liability",
    str_detect(nos, regex("securit|commodit", ignore_case = TRUE)) ~ "Securities Litigation",
    str_detect(nos, regex("labor|employ|jobs|erisa", ignore_case = TRUE)) ~ "Employment",
    str_detect(nos, regex("antitrust", ignore_case = TRUE)) ~ "Antitrust",
    TRUE ~ "Commercial Litigation"
  )
}

# ---- Clients and matters ----------------------------------------------------

n_fam <- nrow(families)
n_current <- round(0.45 * n_fam)
n_former <- round(0.20 * n_fam)
families$firm_status <- sample(rep(c("current", "former", "none"), c(n_current, n_former, n_fam - n_current - n_former)))

clients <- list()
fmatters <- list()
fparties <- list()
next_matter <- 0

add_matter <- function(client, practice, matter_name, open_date, status, close_date, source,
                       docket_id = NA, others = tibble(party_name = character(), role = character())) {
  next_matter <<- next_matter + 1
  id <- sprintf("M%04d", next_matter)
  fmatters[[id]] <<- tibble(
    matter_id = id, client_id = client$client_id, client_lei = client$lei, matter_name = matter_name,
    practice_area = practice, status = status, open_date = as.character(open_date),
    close_date = as.character(close_date), responsible_attorney_id = pick_attorney(practice),
    source = source, docket_id = docket_id
  )
  fparties[[id]] <<- bind_rows(
    tibble(party_name = client$client_name, role = "client", party_lei = client$lei),
    mutate(others, party_lei = resolve_lei(party_name))
  ) |> mutate(matter_id = id, .before = 1)
}

# Dates for a matter given whether it should still be open.
matter_dates <- function(open_date, terminated, keep_open, former) {
  open_date <- as.Date(open_date)
  if (former && open_date > former_cutoff - 180) open_date <- rand_date("2016-01-01", "2023-06-30")
  if (keep_open) return(list(open = open_date, status = "open", close = NA))
  close <- if (!is.na(terminated)) as.Date(terminated) else open_date + sample(200:900, 1)
  close <- min(close, if (former) former_cutoff else as_of - 30)
  if (close <= open_date) close <- open_date + 30
  list(open = open_date, status = "closed", close = close)
}

for (f in seq_len(n_fam)) {
  fam <- families[f, ]
  if (fam$firm_status == "none") next
  former <- fam$firm_status == "former"
  terms <- terms_of(fam$court_terms)

  # The ultimate parent is usually the client; sometimes it's a subsidiary.
  members <- filter(active_entities, family_id == fam$family_id, lei != fam$family_id)
  client_leis <- if (runif(1) < 0.6 || !nrow(members)) fam$family_id else sample(members$lei, 1)
  if (!former && nrow(members) && runif(1) < 0.4) client_leis <- unique(c(client_leis, sample(members$lei, 1)))

  for (cl in client_leis) {
    client <- tibble(client_id = sprintf("C%03d", length(clients) + 1), lei = cl, client_name = name_of[[cl]],
                     client_status = fam$firm_status, family_id = fam$family_id)
    first_matter <- next_matter + 1
    keep_open <- !former

    # Litigation matters mirror real dockets, but only for the parent, since
    # captions name the parent company.
    dockets <- if (cl == fam$family_id) {
      matters |>
        mutate(on_p = side_mentions(terms, caption_plaintiff_side), on_d = side_mentions(terms, caption_defendant_side)) |>
        filter(xor(on_p, on_d)) |>
        slice_sample(n = 2)
    } else matters[0, ]

    for (d in seq_len(nrow(dockets))) {
      dk <- dockets[d, ]
      adverse_side <- if (dk$on_p) "defendant" else "plaintiff"
      adverse <- filter(mparties, docket_id == dk$docket_id, side == adverse_side) |> head(4)
      if (!nrow(adverse)) {
        adverse <- tibble(party_name = if (dk$on_p) dk$caption_defendant_side else dk$caption_plaintiff_side)
      }
      dt <- matter_dates(dk$date_filed, dk$date_terminated, keep_open, former)
      add_matter(client, practice_from_nos(dk$nature_of_suit), dk$case_name, dt$open, dt$status, dt$close,
                 "courtlistener_docket", dk$docket_id, tibble(party_name = adverse$party_name, role = "adverse"))
      keep_open <- FALSE
    }

    # Transactional and advisory work, often with a counterparty from another family.
    for (k in seq_len(sample(1:2, 1))) {
      practice <- sample(c("Corporate/M&A", "Finance", "Regulatory", "Commercial Litigation"), 1)
      dt <- matter_dates(rand_date("2016-01-01", "2026-06-30"), NA, keep_open, former)
      if (practice %in% c("Corporate/M&A", "Finance")) {
        cp <- slice_sample(filter(active_entities, family_id != fam$family_id), n = 1)$legal_name
        name <- if (practice == "Finance") paste("Credit facility with", cp) else paste("Joint venture with", cp)
        add_matter(client, practice, name, dt$open, dt$status, dt$close, "synthetic", others = tibble(party_name = cp, role = "counterparty"))
      } else if (practice == "Commercial Litigation") {
        adv <- fake_companies(1)
        add_matter(client, practice, paste(client$client_name, "v.", adv), dt$open, dt$status, dt$close, "synthetic",
                   others = tibble(party_name = adv, role = "adverse"))
      } else {
        add_matter(client, practice, "Regulatory compliance advice", dt$open, dt$status, dt$close, "synthetic")
      }
      keep_open <- FALSE
    }

    client_matters <- bind_rows(fmatters[sprintf("M%04d", first_matter:next_matter)])
    clients[[client$client_id]] <- client |> mutate(
      relationship_partner_id = client_matters$responsible_attorney_id[1],
      client_since = min(client_matters$open_date),
      client_until = if (former) max(client_matters$close_date) else NA_character_
    )
  }
}

firm_clients <- bind_rows(clients)
firm_matters <- bind_rows(fmatters)
firm_matter_parties <- bind_rows(fparties) |> mutate(party_key = party_key(party_name, party_lei))

# ---- Lateral hires ----------------------------------------------------------

none_families <- filter(families, firm_status == "none")$family_id
used_keys <- unique(firm_matter_parties$party_key)
lateral_pool <- filter(active_entities, family_id %in% none_families, !lei %in% used_keys)

laterals <- filter(attorneys, is_lateral)
lateral_prior_clients <- slice_sample(lateral_pool, n = 3 * nrow(laterals)) |>
  mutate(
    attorney_id = rep(laterals$attorney_id, each = 3),
    prior_firm = rep(laterals$prior_firm, each = 3),
    practice_area = rep(laterals$practice_group, each = 3),
    matter_description = paste(practice_area, "representation"),
    representation_ended = map_chr(attorney_id, \(a) as.character(rand_date("2023-01-01", "2026-05-31"))),
    party_key = lei
  ) |>
  select(attorney_id, prior_firm, client_name = legal_name, client_lei = lei, party_key, practice_area,
         matter_description, representation_ended)

# ---- Intake requests --------------------------------------------------------

current <- filter(firm_clients, client_status == "current")
former_cl <- filter(firm_clients, client_status == "former")
client_practices <- firm_matters |> group_by(client_lei) |> summarise(practices = list(unique(practice_area)))

affiliates_of <- function(fams, exclude) filter(active_entities, family_id %in% fams, !lei %in% exclude)
current_affiliates <- affiliates_of(current$family_id, firm_clients$lei)
former_affiliates <- affiliates_of(former_cl$family_id, firm_clients$lei)
business_parties <- filter(firm_matter_parties, role == "counterparty", !is.na(party_lei), !party_lei %in% firm_clients$lei)
open_adverse <- firm_matter_parties |>
  filter(role == "adverse") |>
  semi_join(filter(firm_matters, status == "open"), by = "matter_id")
clean_pool <- filter(active_entities, family_id %in% none_families,
                     !lei %in% c(used_keys, lateral_prior_clients$client_lei))

pick <- function(x) if (length(x)) x[sample.int(length(x), 1)] else NA_character_
entity_party <- function(lei) tibble(truth_name = name_of[[lei]], truth_lei = lei)
named_party <- function(name) tibble(truth_name = name, truth_lei = NA_character_)
clean_party <- function() {
  r <- runif(1)
  if (r < 0.55 && nrow(clean_pool)) entity_party(sample(clean_pool$lei, 1))
  else if (r < 0.8) named_party(fake_companies(1)) else named_party(fake_people(1))
}

scenario_counts <- c(
  direct_adverse_current = 20, affiliate_of_current = 20, former_client_related = 12,
  former_client_unrelated = 12, lateral_prior_client = 10, business_relationship = 10,
  adverse_party_becomes_client = 10, affiliate_of_former = 8, lookalike_name = 8, clean = 25
)
scenarios <- sample(rep(names(scenario_counts), scenario_counts))

intakes <- list()
intake_parties <- list()

for (i in seq_along(scenarios)) {
  sc <- scenarios[i]
  id <- sprintf("I%04d", i)
  practice <- sample(practice_groups, 1)
  client <- clean_party()
  adverse <- clean_party()

  # A scenario whose pool came up empty falls back to a clean adverse party.
  scenario_lei <- switch(sc,
    direct_adverse_current = pick(current$lei),
    affiliate_of_current = pick(current_affiliates$lei),
    affiliate_of_former = pick(former_affiliates$lei),
    lateral_prior_client = pick(lateral_prior_clients$client_lei),
    business_relationship = pick(business_parties$party_lei),
    former_client_related = ,
    former_client_unrelated = pick(former_cl$lei),
    NA_character_
  )
  if (!is.na(scenario_lei)) adverse <- entity_party(scenario_lei)
  if (sc == "lookalike_name") adverse <- named_party(pick(lookalikes))
  if (sc %in% c("former_client_related", "former_client_unrelated") && !is.na(scenario_lei)) {
    done <- client_practices$practices[[match(scenario_lei, client_practices$client_lei)]]
    practice <- pick(if (sc == "former_client_related") done else setdiff(practice_groups, done))
  }
  if (sc == "adverse_party_becomes_client" && nrow(open_adverse)) {
    p <- slice_sample(open_adverse, n = 1)
    client <- tibble(truth_name = p$party_name, truth_lei = p$party_lei)
  }

  parties <- bind_rows(
    mutate(client, role = "prospective_client"),
    mutate(adverse, role = "adverse"),
    if (runif(1) < 0.35) mutate(clean_party(), role = "adverse")
  ) |>
    mutate(
      intake_id = id,
      party_name_submitted = map_chr(truth_name, perturb_name),
      party_key = party_key(truth_name, truth_lei)
    )

  intake_parties[[id]] <- parties
  intakes[[id]] <- tibble(
    intake_id = id,
    received_date = as.character(rand_date("2026-07-01", "2026-10-20")),
    prospective_client = parties$party_name_submitted[1],
    practice_area = practice,
    matter_description = sprintf("%s matter adverse to %s", practice,
                                 paste(parties$party_name_submitted[parties$role == "adverse"], collapse = ", ")),
    scenario = sc
  )
}

intake_requests <- bind_rows(intakes)
intake_parties <- bind_rows(intake_parties) |>
  select(intake_id, party_name_submitted, role, truth_name, truth_lei, party_key)

# ---- Answer key ---------------------------------------------------------------
# Labels come from the firm records by exact identity (LEI or normalized
# name), never from the scenario name, so every hit in the answer key is real.

severity_rank <- c(high = 3, medium = 2, low = 1)
actions <- c(
  current_client_adverse = "Conflict: decline, or get informed written consent from both clients (Rule 1.7)",
  prospective_client_adverse_to_firm_client = "Conflict: the prospective client is adverse to a current client in an open matter (Rule 1.7)",
  former_client_substantially_related = "Likely conflict: same practice area as prior work; needs former client's consent (Rule 1.9)",
  affiliate_of_current_client = "Review: check engagement letter and how closely the affiliate is tied to the client",
  lateral_hire_prior_client = "Review: lateral attorney represented this party; consider screening (Rule 1.10)",
  former_client_unrelated = "Clear with note: former client, unrelated work",
  affiliate_of_former_client = "Clear with note: affiliate of a former client",
  business_relationship = "Notify relationship partner: party is a counterparty on a client matter"
)
severities <- c(
  current_client_adverse = "high", prospective_client_adverse_to_firm_client = "high",
  former_client_substantially_related = "high", affiliate_of_current_client = "medium",
  lateral_hire_prior_client = "medium", former_client_unrelated = "low",
  affiliate_of_former_client = "low", business_relationship = "low"
)

hit <- function(type, detail, client_id = NA, matter_id = NA) {
  tibble(hit_type = type, detail = detail, related_client_id = as.character(client_id),
         related_matter_id = as.character(matter_id))
}
no_hits <- hit(character(), character(), character(), character())
combine_hits <- function(out) if (length(out)) bind_rows(out) else no_hits

label_party <- function(p, practice) {
  out <- list()
  key <- p$party_key
  if (p$role == "prospective_client") {
    m <- filter(open_adverse, party_key == key)
    if (nrow(m)) out <- c(out, list(hit("prospective_client_adverse_to_firm_client",
                                        "Named as adverse party in an open firm matter", NA, m$matter_id[1])))
    return(combine_hits(out))
  }

  cur <- filter(current, lei == key)
  if (nrow(cur)) out <- c(out, list(hit("current_client_adverse", "Party is a current client", cur$client_id[1])))

  fc <- filter(former_cl, lei == key)
  if (nrow(fc)) {
    done <- client_practices$practices[[match(key, client_practices$client_lei)]]
    type <- if (practice %in% done) "former_client_substantially_related" else "former_client_unrelated"
    out <- c(out, list(hit(type, paste("Former client; prior work in", paste(done, collapse = ", ")), fc$client_id[1])))
  }

  if (!is.na(p$truth_lei) && !nrow(cur) && !nrow(fc)) {
    fam <- family_of[[p$truth_lei]]
    for (j in seq_len(nrow(firm_clients))) {
      cl <- firm_clients[j, ]
      if (cl$family_id != fam) next
      rel <- if (cl$lei %in% ancestors(p$truth_lei)) "subsidiary of" else if (p$truth_lei %in% ancestors(cl$lei)) "parent of" else "affiliate of"
      type <- if (cl$client_status == "current") "affiliate_of_current_client" else "affiliate_of_former_client"
      out <- c(out, list(hit(type, paste("Party is", rel, cl$client_name), cl$client_id)))
    }
  }

  lat <- filter(lateral_prior_clients, party_key == key)
  if (nrow(lat)) out <- c(out, list(hit("lateral_hire_prior_client",
                                        paste("Prior client of lateral", lat$attorney_id[1], "at", lat$prior_firm[1]))))

  biz <- filter(firm_matter_parties, party_key == key, role == "counterparty")
  if (nrow(biz)) out <- c(out, list(hit("business_relationship", "Counterparty on a firm client matter", NA, biz$matter_id[1])))

  combine_hits(out)
}

intake_expected_hits <- intake_parties |>
  left_join(select(intake_requests, intake_id, practice_area), by = "intake_id") |>
  mutate(hits = pmap(list(intake_id, role, party_key, truth_lei, practice_area), \(intake_id, role, party_key, truth_lei, practice_area) {
    label_party(list(role = role, party_key = party_key, truth_lei = truth_lei), practice_area)
  })) |>
  select(intake_id, party_name_submitted, role, truth_lei, hits) |>
  unnest(hits) |>
  mutate(severity = unname(severities[hit_type]), recommended_action = unname(actions[hit_type])) |>
  arrange(intake_id, desc(severity_rank[severity]))

intake_requests <- intake_requests |>
  left_join(
    intake_expected_hits |>
      group_by(intake_id) |>
      summarise(expected_outcome = paste0(severity[which.max(severity_rank[severity])], "_risk")),
    by = "intake_id"
  ) |>
  mutate(expected_outcome = coalesce(expected_outcome, "clear"))

message("Writing outputs")
write_out(attorneys, "firm_attorneys")
write_out(firm_clients, "firm_clients")
write_out(firm_matters, "firm_matters")
write_out(firm_matter_parties, "firm_matter_parties")
write_out(lateral_prior_clients, "lateral_prior_clients")
write_out(intake_requests, "intake_requests")
write_out(intake_parties, "intake_parties")
write_out(intake_expected_hits, "intake_expected_hits")
message("Outcome mix:")
print(count(intake_requests, scenario, expected_outcome))
