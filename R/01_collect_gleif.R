# Part 1 of the model: the entity graph.
#
# For each seed company, finds its ultimate parent in GLEIF (the Global LEI
# Foundation's public register), pulls every entity in that corporate family,
# and rebuilds the parent/subsidiary tree.
#
# Output:
#   data/families.csv              one row per corporate family
#   data/entities.csv              one row per legal entity (LEI)
#   data/entity_aliases.csv        legal name plus former and trading names
#   data/entity_relationships.csv  child -> parent edges
#
# GLEIF allows about 60 requests a minute. Rebuilding a tree costs one request
# per entity, so a full run takes a while. Responses are cached in
# data/raw/gleif, so rerunning resumes where it stopped.

source("R/utils.R")

gleif_base <- "https://api.gleif.org/api/v1"
max_walk_per_family <- 400 # entities whose children we look up, per family

gleif_get <- function(path, params = list()) {
  cached_get_json(build_url(paste0(gleif_base, path), params), "gleif", rate_per_min = 50)
}

# Follow "next" links until the last page.
gleif_get_all <- function(path) {
  out <- list()
  page <- 1
  repeat {
    res <- gleif_get(path, list(`page[size]` = 200, `page[number]` = page))
    out <- c(out, res$data)
    last <- res$meta$pagination$lastPage
    if (is.null(last) || page >= last) break
    page <- page + 1
  }
  out
}

parse_entity <- function(rec) {
  a <- rec$attributes
  e <- a$entity
  tibble(
    lei = a$lei,
    legal_name = nz(e$legalName$name),
    entity_status = nz(e$status),
    registration_status = nz(a$registration$status),
    category = nz(e$category),
    legal_form_id = nz(e$legalForm$id),
    jurisdiction = nz(e$jurisdiction),
    legal_country = nz(e$legalAddress$country),
    legal_region = nz(e$legalAddress$region),
    hq_city = nz(e$headquartersAddress$city),
    hq_country = nz(e$headquartersAddress$country)
  )
}

parse_aliases <- function(rec) {
  a <- rec$attributes
  e <- a$entity
  other <- c(e$otherNames, e$transliteratedOtherNames)
  bind_rows(
    tibble(lei = a$lei, alias = nz(e$legalName$name), alias_type = "LEGAL_NAME"),
    map_dfr(other, \(o) tibble(lei = a$lei, alias = nz(o$name), alias_type = nz(o$type)))
  )
}

# Pick the GLEIF record for a seed: an exact (normalized) legal-name match,
# preferring active US entities. A LEI in the config overrides the search.
resolve_seed <- function(gleif_name, lei) {
  if (!is.na(lei) && nzchar(lei)) return(lei)
  res <- gleif_get("/lei-records", list(`filter[entity.legalName]` = gleif_name, `page[size]` = 50))
  if (!length(res$data)) stop("No GLEIF match for ", gleif_name)
  cands <- map_dfr(res$data, parse_entity) |>
    mutate(
      exact = norm_name(legal_name) == norm_name(gleif_name),
      score = 4 * exact + 2 * (entity_status == "ACTIVE") + (legal_country %in% "US")
    ) |>
    arrange(desc(score))
  cands$lei[1]
}

# Breadth-first walk down from the ultimate parent using direct-child links.
walk_family <- function(root, member_leis) {
  queue <- root
  seen <- character()
  edges <- list()
  while (length(queue) && length(seen) < max_walk_per_family) {
    node <- queue[1]
    queue <- queue[-1]
    if (node %in% seen) next
    seen <- c(seen, node)
    rels <- gleif_get_all(paste0("/lei-records/", node, "/direct-child-relationships"))
    for (r in rels) {
      rel <- r$attributes$relationship
      if (!identical(nz(rel$status), "ACTIVE")) next
      child <- nz(rel$startNode$id)
      start <- keep(rel$periods, \(p) identical(nz(p$type), "RELATIONSHIP_PERIOD"))
      edges[[length(edges) + 1]] <- tibble(
        child_lei = child,
        parent_lei = node,
        relationship_type = "DIRECT_PARENT",
        relationship_start = if (length(start)) substr(nz(start[[1]]$startDate), 1, 10) else NA_character_
      )
      if (child %in% member_leis) queue <- c(queue, child)
    }
  }
  no_edges <- tibble(child_lei = character(), parent_lei = character(),
                     relationship_type = character(), relationship_start = character())
  list(edges = bind_rows(no_edges, edges), walked = length(seen), complete = !length(queue))
}

seeds <- read_csv("config/seed_companies.csv", col_types = cols(.default = "c"))

families <- list()
entities <- list()
aliases <- list()
relationships <- list()

for (i in seq_len(nrow(seeds))) {
  s <- seeds[i, ]
  message(sprintf("[%d/%d] %s", i, nrow(seeds), s$seed_name))

  seed_lei <- resolve_seed(s$gleif_name, s$lei)
  up <- gleif_get(paste0("/lei-records/", seed_lei, "/ultimate-parent"))
  root_rec <- if (!is.null(up$data)) up$data else gleif_get(paste0("/lei-records/", seed_lei))$data
  root <- root_rec$attributes$lei
  if (root %in% names(families)) next

  members <- c(list(root_rec), gleif_get_all(paste0("/lei-records/", root, "/ultimate-children")))
  member_leis <- map_chr(members, \(m) m$attributes$lei)
  message(sprintf("  family of %s: %d entities", nz(root_rec$attributes$entity$legalName$name), length(members)))

  walk <- walk_family(root, member_leis)

  # Members the walk did not reach (cap hit, or a parent with no LEI) still
  # belong to the family, so link them straight to the ultimate parent.
  reached <- c(root, walk$edges$child_lei)
  unreached <- setdiff(member_leis, reached)
  ultimate_edges <- tibble(
    child_lei = unreached,
    parent_lei = root,
    relationship_type = "ULTIMATE_PARENT_ONLY",
    relationship_start = NA_character_
  )

  families[[root]] <- tibble(
    family_id = root,
    family_name = nz(root_rec$attributes$entity$legalName$name),
    seed_name = s$seed_name,
    sector = s$sector,
    court_terms = s$court_terms,
    member_count = length(members),
    entities_walked = walk$walked,
    tree_complete = walk$complete
  )
  entities[[root]] <- map_dfr(members, parse_entity) |> mutate(family_id = root)
  aliases[[root]] <- map_dfr(members, parse_aliases)
  relationships[[root]] <- bind_rows(walk$edges, ultimate_edges) |> mutate(family_id = root)
}

message("Writing outputs")
entities_df <- bind_rows(entities) |>
  distinct(lei, .keep_all = TRUE) |>
  mutate(norm_name = norm_name(legal_name))

write_out(bind_rows(families), "families")
write_out(entities_df, "entities")
write_out(bind_rows(aliases) |> filter(!is.na(alias)) |> distinct() |> mutate(norm_alias = norm_name(alias)), "entity_aliases")
write_out(bind_rows(relationships) |> filter(child_lei %in% entities_df$lei) |> distinct(child_lei, parent_lei, .keep_all = TRUE), "entity_relationships")
