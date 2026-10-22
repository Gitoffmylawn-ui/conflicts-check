# Part 3 of the model: matching and scoring.
#
# Given a new-matter intake (a prospective client, a practice area and the
# parties on the other side), find every relationship the firm's records have
# with those parties and say how serious each one is.
#
#   1. Match:  each submitted name -> the entity or party it most likely is,
#              with a confidence score (fuzzy, tolerant of typos and suffixes)
#   2. Expand: walk the entity graph to the matched party's corporate family
#   3. Check:  compare against clients, matters, laterals and counterparties
#   4. Score:  severity x match confidence; the worst hit sets the outcome
#
# Usage:
#   source("R/conflict_engine.R")
#   engine <- prepare_engine()
#   check_intake(engine, tibble(party_name = c("Acme LLC", "Pfizer"),
#                               role = c("prospective_client", "adverse")),
#                practice_area = "Product Liability")

source("R/utils.R")
suppressPackageStartupMessages(library(stringdist))

# A match at or above `accept` is treated as the same party. Between `review`
# and `accept` it's shown as a possible match for an analyst to confirm, but
# doesn't count toward the outcome.
match_thresholds <- c(accept = 0.88, review = 0.75)

severity_of <- c(
  current_client_adverse = "high", prospective_client_adverse_to_firm_client = "high",
  former_client_substantially_related = "high", affiliate_of_current_client = "medium",
  lateral_hire_prior_client = "medium", former_client_unrelated = "low",
  affiliate_of_former_client = "low", business_relationship = "low"
)
severity_weight <- c(high = 1, medium = 0.6, low = 0.3)

recommended_action <- c(
  current_client_adverse = "Decline, or get informed written consent from both clients (Rule 1.7)",
  prospective_client_adverse_to_firm_client = "Decline, or get consent: the firm is adverse to this party in an open matter (Rule 1.7)",
  former_client_substantially_related = "Get the former client's consent before accepting (Rule 1.9)",
  affiliate_of_current_client = "Check the engagement letter and how closely the affiliate is tied to the client",
  lateral_hire_prior_client = "Consider screening the lateral attorney from the matter (Rule 1.10)",
  former_client_unrelated = "Clear, noting the former relationship",
  affiliate_of_former_client = "Clear, noting the former client's affiliate",
  business_relationship = "Let the relationship partner know before accepting"
)

# ---- Setup ---------------------------------------------------------------------

prepare_engine <- function(data_dir = "data") {
  rd <- function(name) read_csv(file.path(data_dir, paste0(name, ".csv")), col_types = cols(.default = "c"))
  entities <- rd("entities")
  aliases <- rd("entity_aliases")
  rels <- rd("entity_relationships")
  clients <- rd("firm_clients")
  matters <- rd("firm_matters")
  mparties <- rd("firm_matter_parties")
  laterals <- rd("lateral_prior_clients")
  # Hand-kept names GLEIF lacks: renames it never recorded, nicknames.
  extra <- read_csv("config/extra_aliases.csv", col_types = cols(.default = "c"))

  # Every name we could match a submission to: all names of every entity in
  # the graph, plus firm-matter parties that have no LEI (people, small firms).
  index <- bind_rows(
    bind_rows(aliases, extra) |> transmute(key = lei, name = alias, legal = alias_type == "LEGAL_NAME"),
    mparties |> filter(is.na(party_lei)) |> transmute(key = party_key, name = party_name, legal = TRUE)
  ) |>
    mutate(norm = norm_name(name), full = norm_full(name),
           compact = str_remove_all(full, " "), compact_n = str_remove_all(norm, " ")) |>
    filter(!is.na(norm), nzchar(norm)) |>
    distinct(key, norm, .keep_all = TRUE) |>
    left_join(select(entities, key = lei, entity_status, family_id), by = "key") |>
    mutate(is_parent = key %in% family_id, active = entity_status %in% "ACTIVE")

  list(
    index = index,
    name_of = setNames(entities$legal_name, entities$lei),
    family_of = setNames(entities$family_id, entities$lei),
    parent_of = setNames(rels$parent_lei, rels$child_lei),
    clients = clients,
    client_practices = matters |> group_by(client_lei) |> summarise(practices = list(unique(practice_area))),
    open_adverse = mparties |>
      filter(role == "adverse") |>
      semi_join(filter(matters, status == "open"), by = "matter_id"),
    counterparties = filter(mparties, role == "counterparty"),
    laterals = laterals
  )
}

# ---- 1. Name matching ------------------------------------------------------------

# Share of words two names have in common, counting near-identical words
# ("RESEARHC"/"RESEARCH") as shared. Penalizes extra words, so "MERCK KGAA"
# scores low against "MERCK" even though the strings start the same.
token_similarity <- function(a, b) {
  ta <- unique(str_split(a, " ")[[1]])
  tb <- unique(str_split(b, " ")[[1]])
  sims <- matrix(stringsim(rep(ta, each = length(tb)), rep(tb, length(ta)), method = "jw", p = 0.1),
                 nrow = length(tb))
  shared <- min(sum(apply(sims, 2, max) >= 0.92), sum(apply(sims, 1, max) >= 0.92))
  shared / (length(ta) + length(tb) - shared)
}

# Best candidate identities for one submitted name, best first. Names that
# carry a second name ("Meta Platforms, Inc. fka Facebook, Inc.", "Walmart Inc.
# d/b/a Wal-Mart Stores") are matched part by part, keeping each identity's
# best score.
also_known_as <- regex("\\s+(f/?k/?a|a/?k/?a|d/?b/?a|formerly(\\s+known\\s+as)?|also\\s+known\\s+as|doing\\s+business\\s+as)\\.?\\s+",
                       ignore_case = TRUE)
match_name <- function(engine, name, n_candidates = 25) {
  parts <- str_trim(str_split(name, also_known_as)[[1]])
  parts <- parts[nzchar(parts)]
  if (length(parts) > 1) {
    return(map_dfr(parts, \(p) match_name_one(engine, p, n_candidates)) |>
             arrange(desc(score), desc(suffix_match), desc(is_parent), desc(legal_match), desc(active)) |>
             distinct(key, .keep_all = TRUE))
  }
  match_name_one(engine, name, n_candidates)
}

match_name_one <- function(engine, name, n_candidates = 25) {
  idx <- engine$index
  n <- norm_name(name)
  if (is.na(n) || !nzchar(n)) return(idx[0, ] |> mutate(score = numeric()))

  full <- norm_full(name)
  compact <- str_remove_all(full, " ")

  # Two cheap whole-string comparisons pick a shortlist: one on the name
  # without suffixes, one with every space removed, which catches typos that
  # move a space ("AT& TINC.") and would wreck a word-by-word comparison.
  # The space-free check runs with and without suffixes: with them it catches
  # "AT& TINC.", without them "JP Morgan Chase" vs "JPMorgan Chase & Co.".
  jw <- stringsim(n, idx$norm, method = "jw", p = 0.1)
  cjw <- pmax(stringsim(compact, idx$compact, method = "jw", p = 0.1),
              stringsim(str_remove_all(n, " "), idx$compact_n, method = "jw", p = 0.1))
  short <- unique(c(head(order(jw, decreasing = TRUE), n_candidates), head(order(cjw, decreasing = TRUE), 5)))
  cand <- idx[short, ]
  word_score <- 0.5 * jw[short] + 0.5 * map_dbl(cand$norm, \(b) token_similarity(n, b))
  # The space-free score only counts when it's typo-close. Below that it mostly
  # rewards a shared first few letters ("CHEVRON..." vs "CHEVROLET...").
  typo_score <- if_else(cjw[short] >= 0.93, cjw[short] - 0.03, 0)
  cand$score <- if_else(cand$norm == n, 1, pmax(word_score, typo_score))
  # Short single-word names of people and unregistered parties (mostly
  # surnames) are too easy to confuse: "GRAY" is a one-letter typo from
  # "GARY". They have to match exactly. Registered companies keep typo
  # tolerance, since "TELSA" really does mean Tesla.
  short_word <- str_starts(cand$key, "NAME:") & !str_detect(n, " ") & !str_detect(cand$norm, " ") &
    pmin(nchar(n), nchar(cand$norm)) <= 5
  cand$score <- if_else(short_word & cand$norm != n, pmin(cand$score, 0.7), cand$score)
  # Suffixes are dropped for matching, so several entities can tie on the same
  # name. Tie-breakers, in order:
  #   1. the submitted suffix matches  ("... Company Limited" -> the UK arm)
  #   2. the family parent             ("PFIZER" -> Pfizer Inc., not the
  #                                      subsidiary whose legal name is PFIZER)
  #   3. an exact legal name           (two "Omrix Biopharmaceuticals" entities)
  #   4. an active entity
  cand$suffix_match <- cand$full == full & full != n
  cand$legal_match <- cand$full == full & cand$legal

  cand |>
    arrange(desc(score), desc(suffix_match), desc(is_parent), desc(legal_match), desc(active)) |>
    distinct(key, .keep_all = TRUE)
}

# ---- 2-3. Conflict rules ---------------------------------------------------------

ancestors <- function(engine, lei) {
  out <- character()
  p <- unname(engine$parent_of[lei])
  while (!is.na(p) && !(p %in% out)) {
    out <- c(out, p)
    p <- unname(engine$parent_of[p])
  }
  out
}

hits_for_key <- function(engine, key, role, practice_area) {
  hit <- function(type, detail, client_id = NA, matter_id = NA) {
    tibble(hit_type = type, detail = detail, related_client_id = as.character(client_id),
           related_matter_id = as.character(matter_id))
  }
  out <- list()

  # Someone the firm is suing (or defending against) wants to hire the firm.
  if (role == "prospective_client") {
    m <- filter(engine$open_adverse, party_key == key)
    if (nrow(m)) out <- c(out, list(hit("prospective_client_adverse_to_firm_client",
                                        "Named as adverse party in an open firm matter", NA, m$matter_id[1])))
    return(bind_rows(out))
  }

  clients <- engine$clients
  direct <- filter(clients, lei == key)
  for (j in seq_len(nrow(direct))) {
    cl <- direct[j, ]
    if (cl$client_status == "current") {
      out <- c(out, list(hit("current_client_adverse", "Party is a current client", cl$client_id)))
    } else {
      done <- engine$client_practices$practices[[match(key, engine$client_practices$client_lei)]]
      type <- if (practice_area %in% done) "former_client_substantially_related" else "former_client_unrelated"
      out <- c(out, list(hit(type, paste("Former client; prior work in", paste(done, collapse = ", ")), cl$client_id)))
    }
  }

  # Corporate family: parents, subsidiaries and siblings of a client.
  fam <- unname(engine$family_of[key])
  if (!nrow(direct) && !is.na(fam)) {
    for (j in which(clients$family_id == fam)) {
      cl <- clients[j, ]
      rel <- if (cl$lei %in% ancestors(engine, key)) "subsidiary of"
             else if (key %in% ancestors(engine, cl$lei)) "parent of"
             else "affiliate of"
      type <- if (cl$client_status == "current") "affiliate_of_current_client" else "affiliate_of_former_client"
      out <- c(out, list(hit(type, paste("Party is", rel, cl$client_name), cl$client_id)))
    }
  }

  lat <- filter(engine$laterals, party_key == key)
  if (nrow(lat)) out <- c(out, list(hit("lateral_hire_prior_client",
                                        paste("Prior client of lateral", lat$attorney_id[1], "at", lat$prior_firm[1]))))

  cp <- filter(engine$counterparties, party_key == key)
  if (nrow(cp)) out <- c(out, list(hit("business_relationship", "Counterparty on a firm client matter", NA, cp$matter_id[1])))

  bind_rows(out)
}

# ---- 4. Putting it together ------------------------------------------------------

# One row per hit. Parties with a possible-but-unconfirmed match get their
# hits too, marked match_status = "review".
check_party <- function(engine, party_name, role, practice_area) {
  cands <- match_name(engine, party_name) |> filter(score >= match_thresholds[["review"]])
  if (!nrow(cands)) return(NULL)
  # An exact name match settles it. Otherwise consider the best candidate plus
  # any near-ties, so an ambiguous name surfaces every party it could be.
  cands <- if (cands$score[1] == 1) cands[1, ] else filter(cands, score >= cands$score[1] - 0.02)

  hits <- map_dfr(seq_len(nrow(cands)), \(i) {
    cand <- cands[i, ]
    h <- hits_for_key(engine, cand$key, role, practice_area)
    if (!nrow(h)) return(NULL)
    h |> mutate(
      party_name_submitted = party_name, role = role,
      matched_key = cand$key, matched_name = cand$name, match_score = round(cand$score, 3),
      match_status = if (cand$score >= match_thresholds[["accept"]]) "match" else "review",
      .before = 1
    )
  })
  if (!nrow(hits)) return(NULL)
  hits |>
    mutate(
      severity = unname(severity_of[hit_type]),
      risk_score = round(severity_weight[severity] * match_score, 3),
      recommended_action = unname(recommended_action[hit_type])
    )
}

# parties: data frame with party_name and role. Returns the hits (worst
# first) and an overall outcome for the intake.
no_hits <- tibble(
  party_name_submitted = character(), role = character(), matched_key = character(),
  matched_name = character(), match_score = numeric(), match_status = character(),
  hit_type = character(), detail = character(), related_client_id = character(),
  related_matter_id = character(), severity = character(), risk_score = numeric(),
  recommended_action = character()
)

check_intake <- function(engine, parties, practice_area) {
  hits <- bind_rows(no_hits, map2_dfr(parties$party_name, parties$role, \(n, r) check_party(engine, n, r, practice_area)))
  if (!nrow(hits)) return(list(hits = hits, outcome = "clear", needs_review = FALSE))
  hits <- arrange(hits, desc(risk_score))
  confirmed <- filter(hits, match_status == "match")
  outcome <- if (nrow(confirmed)) {
    paste0(names(severity_weight)[min(match(confirmed$severity, names(severity_weight)))], "_risk")
  } else "clear"
  list(hits = hits, outcome = outcome, needs_review = any(hits$match_status == "review"))
}
