# Part 2 of the model, real-world side: litigation matters and who was on
# each side.
#
# Searches CourtListener's RECAP archive (federal court dockets) in two ways:
#   - family queries: recent dockets naming each seed company in the caption
#   - pair queries: dockets naming two seed companies on opposite sides,
#     the company-v-company disputes that matter most for conflicts
#
# Output:
#   data/matters.csv          one row per docket
#   data/matter_parties.csv   party names with the side of the caption they're on
#   data/matter_counsel.csv   law firms and attorneys listed on the docket
#
# Works without an account. If you set COURTLISTENER_TOKEN (free at
# courtlistener.com) requests go out authenticated, with higher rate limits.

source("R/utils.R")

cl_base <- "https://www.courtlistener.com/api/rest/v4/search/"
filed_after <- "2015-01-01"
family_pages <- 2 # 20 dockets per page

token <- Sys.getenv("COURTLISTENER_TOKEN")
cl_headers <- if (nzchar(token)) list(Authorization = paste("Token", token)) else list()

# The search API allows 5 requests a minute, even with a token. Anonymous
# access also has an hourly and a daily cap, and once that's hit CourtListener
# can ask us to wait nearly a day. So: stay under 5 a minute, retry once after
# a minute, and if that still fails, skip uncached searches for the rest of
# the run and write out what we have. Rerun later to fill in.
blocked <- FALSE
cl_fetch <- function(url) {
  cached_get_json(url, "courtlistener", rate_per_min = 4, headers = cl_headers, retry = FALSE)
}
cl_get <- function(url) {
  cache <- file.path("data", "raw", "courtlistener", paste0(as.character(openssl::md5(url)), ".json"))
  if (blocked && !file.exists(cache)) return(list(results = list()))
  tryCatch(
    cl_fetch(url),
    error = function(e) tryCatch(
      {
        Sys.sleep(65)
        cl_fetch(url)
      },
      error = function(e) {
        message("  CourtListener request failed (", conditionMessage(e), "); skipping searches that aren't cached yet")
        blocked <<- TRUE
        list(results = list())
      }
    )
  )
}

terms_of <- function(court_terms) str_split(court_terms, ";")[[1]] |> str_trim()

caption_clause <- function(terms) {
  sprintf("caseName:(%s)", paste0('"', terms, '"', collapse = " OR "))
}

# Split "A v. B" into its two sides. Captions like "In re X" have no sides.
split_caption <- function(caption) {
  parts <- str_split(caption, regex("\\s+vs?\\.?\\s+", ignore_case = TRUE), n = 2)[[1]]
  if (length(parts) < 2) return(c(NA_character_, NA_character_))
  str_remove(parts, regex(",?\\s+et al\\.?$", ignore_case = TRUE))
}

term_on_side <- function(terms, side) {
  if (is.na(side)) return(FALSE)
  any(str_detect(norm_name(side), fixed(norm_name(terms))))
}

parse_result <- function(r, query_type, query_seeds) {
  caption <- nz(r$caseName)
  sides <- split_caption(caption)

  matter <- tibble(
    docket_id = nz(r$docket_id),
    case_name = caption,
    docket_number = nz(r$docketNumber),
    court_id = nz(r$court_id),
    court = nz(r$court),
    date_filed = nz(r$dateFiled),
    date_terminated = nz(r$dateTerminated),
    nature_of_suit = nz(r$suitNature),
    cause = nz(r$cause),
    jurisdiction_type = nz(r$jurisdictionType),
    caption_plaintiff_side = sides[1],
    caption_defendant_side = sides[2],
    url = paste0("https://www.courtlistener.com", nz(r$docket_absolute_url)),
    query_type = query_type,
    query_seeds = query_seeds
  )

  # Docket party lists don't say which side a party is on, so place each one
  # by finding its name in the caption. When RECAP has no party list, fall
  # back to the two caption sides themselves.
  party_names <- unlist(r$party)
  side_of <- function(p) {
    n <- norm_name(p)
    if (!nzchar(n)) return("unknown")
    in_p <- !is.na(sides[1]) && str_detect(norm_name(sides[1]), fixed(n))
    in_d <- !is.na(sides[2]) && str_detect(norm_name(sides[2]), fixed(n))
    if (in_p && !in_d) "plaintiff" else if (in_d && !in_p) "defendant" else "unknown"
  }
  parties <- if (length(party_names)) {
    tibble(party_name = party_names, side = map_chr(party_names, side_of), source = "docket_party_list")
  } else if (!is.na(sides[1])) {
    tibble(party_name = sides, side = c("plaintiff", "defendant"), source = "caption")
  } else {
    tibble(party_name = caption, side = "unknown", source = "caption")
  }

  counsel <- bind_rows(
    tibble(name = unlist(r$firm), counsel_type = "firm"),
    tibble(name = unlist(r$attorney), counsel_type = "attorney")
  )

  list(
    matter = matter,
    parties = mutate(parties, docket_id = matter$docket_id, .before = 1),
    counsel = if (nrow(counsel)) mutate(counsel, docket_id = matter$docket_id, .before = 1) else NULL
  )
}

run_query <- function(q, pages, query_type, query_seeds, include = \(r) TRUE) {
  url <- build_url(cl_base, list(type = "r", q = q, filed_after = filed_after))
  out <- list()
  for (p in seq_len(pages)) {
    res <- cl_get(url)
    hits <- keep(res$results, include)
    out <- c(out, map(hits, parse_result, query_type = query_type, query_seeds = query_seeds))
    if (is.null(res[["next"]])) break
    url <- res[["next"]]
  }
  out
}

seeds <- read_csv("config/seed_companies.csv", col_types = cols(.default = "c"))
results <- list()

message("Family queries")
for (i in seq_len(nrow(seeds))) {
  s <- seeds[i, ]
  message(sprintf("  [%d/%d] %s", i, nrow(seeds), s$seed_name))
  results <- c(results, run_query(caption_clause(terms_of(s$court_terms)), family_pages, "family", s$seed_name))
}

# One query per company against all the companies after it in the list, so
# each pair is covered once. Searching pair by pair would take 253 requests,
# and anonymous access is cut off after roughly 40 an hour.
message("Pair queries")
for (i in seq_len(nrow(seeds) - 1)) {
  a <- seeds[i, ]
  others <- seeds[(i + 1):nrow(seeds), ]
  ta <- terms_of(a$court_terms)
  other_terms <- map(others$court_terms, terms_of)
  message(sprintf("  [%d/%d] %s vs. the rest", i, nrow(seeds) - 1, a$seed_name))
  # Keep only dockets where company A sits on one side and another seed
  # company on the other; this drops search-warrant captions that merely
  # mention both.
  opponent <- function(r) {
    sides <- split_caption(nz(r$caseName))
    a_side <- c(term_on_side(ta, sides[1]), term_on_side(ta, sides[2]))
    if (sum(a_side) != 1) return(NA_character_)
    other_side <- sides[!a_side]
    hit <- map_lgl(other_terms, term_on_side, side = other_side)
    if (any(hit)) others$seed_name[which(hit)[1]] else NA_character_
  }
  q <- paste(caption_clause(ta), "AND", caption_clause(unlist(other_terms)))
  url <- build_url(cl_base, list(type = "r", q = q, filed_after = filed_after))
  res <- cl_get(url)
  for (r in res$results) {
    b <- opponent(r)
    if (!is.na(b)) results <- c(results, list(parse_result(r, "pair", paste(a$seed_name, b, sep = " | "))))
  }
}

if (blocked) message("Some searches were skipped because of the rate limit. Rerun later (or set COURTLISTENER_TOKEN) to fill them in.")
message("Writing outputs")
matters <- map_dfr(results, "matter")
# A docket found by several queries keeps one row; pair hits take priority.
matters <- matters |>
  arrange(docket_id, desc(query_type == "pair")) |>
  group_by(docket_id) |>
  mutate(query_seeds = paste(unique(query_seeds), collapse = "; ")) |>
  slice(1) |>
  ungroup()

write_out(matters, "matters")
write_out(map_dfr(results, "parties") |> distinct(), "matter_parties")
write_out(map_dfr(results, "counsel") |> distinct(), "matter_counsel")
