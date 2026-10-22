# Shared helpers: cached HTTP, JSON field access, name normalization.
# Every script expects the working directory to be the conflicts/ folder.

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(dplyr)
  library(purrr)
  library(readr)
  library(stringr)
  library(tidyr)
  library(tibble)
})

if (!file.exists("R/utils.R")) {
  stop("Set the working directory to the conflicts/ folder before running these scripts.")
}

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

# Turn a JSON null into NA so tibble columns stay atomic.
nz <- function(x) if (is.null(x) || length(x) == 0) NA_character_ else as.character(x[[1]])

# GET a URL and cache the body on disk, keyed by URL. Reruns hit the cache,
# so a long collection can be stopped and restarted without re-downloading.
# A 404 is cached as {"data": null} because "no parent" is a real answer.
# retry = FALSE fails straight away on a rate limit instead of waiting it out;
# httr2 honors a server's Retry-After even when that's most of a day.
cached_get_json <- function(url, source, rate_per_min, headers = list(), retry = TRUE) {
  dir <- file.path("data", "raw", source)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(dir, paste0(as.character(openssl::md5(url)), ".json"))
  if (file.exists(path)) return(fromJSON(path, simplifyVector = FALSE))

  req <- request(url) |>
    req_user_agent("conflicts-analysis-prototype (personal research project)") |>
    req_throttle(capacity = rate_per_min, fill_time_s = 60, realm = source) |>
    req_error(is_error = \(resp) FALSE)
  if (retry) {
    req <- req_retry(
      req,
      max_tries = 6,
      is_transient = \(resp) resp_status(resp) %in% c(429, 500, 502, 503, 504),
      backoff = \(i) 15 * i
    )
  }
  if (length(headers)) req <- req_headers(req, !!!headers)

  resp <- req_perform(req)
  status <- resp_status(resp)
  if (status == 404) {
    writeLines('{"data": null}', path)
    return(list(data = NULL))
  }
  if (status >= 400) stop(sprintf("HTTP %d for %s", status, url))

  body <- resp_body_string(resp)
  writeLines(body, path, useBytes = TRUE)
  fromJSON(body, simplifyVector = FALSE)
}

build_url <- function(base, params) {
  if (!length(params)) return(base)
  query <- paste0(names(params), "=", map_chr(params, \(v) URLencode(as.character(v), reserved = TRUE)))
  paste0(base, "?", paste(query, collapse = "&"))
}

# Corporate suffixes stripped before comparing names. Order matters only for
# readability; matching is on whole words.
corporate_suffixes <- c(
  "INCORPORATED", "INC", "CORPORATION", "CORP", "COMPANY", "CO", "LIMITED", "LTD",
  "LLC", "LLP", "LP", "PLC", "NA", "SA", "AG", "GMBH", "BV", "NV", "SE", "AB",
  "SRL", "SPA", "SAS", "PTY", "UA", "THE"
)

# Uppercase, strip accents, unify "&"/"and", drop punctuation, keep suffixes.
# Periods and apostrophes are deleted rather than spaced so "J.P." and "JP"
# agree. Letters in any script survive, so Chinese or Thai names still match.
norm_full <- function(x) {
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- toupper(x)
  x <- str_replace_all(x, "&", " AND ")
  x <- str_remove_all(x, "[.'’]")
  x <- str_replace_all(x, "[^\\p{L}\\p{N} ]", " ")
  x <- str_replace_all(x, "\\bNATIONAL ASSOCIATION\\b", "NA") # how banks' legal names spell "N.A."
  str_squish(x)
}

# norm_full minus corporate suffixes: "The Boeing Company" and "BOEING CO."
# both become "BOEING".
norm_name <- function(x) {
  x <- norm_full(x)
  pattern <- paste0("\\b(", paste(corporate_suffixes, collapse = "|"), ")\\b")
  x <- str_squish(str_replace_all(x, pattern, " "))
  str_remove(x, "\\s+AND$") # "MERCK AND CO" leaves a dangling AND

}

write_out <- function(df, name) {
  path <- file.path("data", paste0(name, ".csv"))
  write_csv(df, path, na = "")
  message(sprintf("  wrote %s (%s rows)", path, format(nrow(df), big.mark = ",")))
}
