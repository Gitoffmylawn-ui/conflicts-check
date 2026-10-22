# Conflicts check interface for the fictional firm Hartwell & Pierce LLP.
#
#   Run a check          enter a new matter, see every conflict hit and why
#   Explore an entity    look up any company: its corporate family, the firm's
#                        relationships with it, and its federal lawsuits
#   Firm records         the firm's clients, matters, attorneys and laterals
#   Engine performance   how the engine scores against the answer key
#
# Launch from R:  shiny::runApp("C:/Users/evans/OneDrive/Desktop/R1/conflicts")

source("R/conflict_engine.R")
suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(DT)
  library(visNetwork)
})

# ---- Data -----------------------------------------------------------------------

engine <- prepare_engine()
rd <- function(name, dir = "data") read_csv(file.path(dir, paste0(name, ".csv")), col_types = cols(.default = "c"))
entities <- rd("entities")
families <- rd("families")
aliases <- bind_rows(rd("entity_aliases"), read_csv("config/extra_aliases.csv", col_types = cols(.default = "c")))
rels <- rd("entity_relationships")
firm_matters <- rd("firm_matters")
firm_parties <- rd("firm_matter_parties")
attorneys <- rd("firm_attorneys")
lawsuits <- rd("matters")
lawsuit_parties <- rd("matter_parties")
lawsuit_counsel <- rd("matter_counsel") |> filter(!is.na(name), nzchar(name))
party_links <- rd("lawsuit_party_links")
intakes <- rd("intake_requests")
intake_parties <- rd("intake_parties")
expected_hits <- rd("intake_expected_hits")
clients <- engine$clients

children_of <- split(rels$child_lei, rels$parent_lei)
entity_by_lei <- split(entities, entities$lei)
display_name <- function(key) {
  n <- unname(engine$name_of[key])
  if_else(is.na(n), str_remove(key, "^NAME:"), n)
}
path_to_root <- function(lei) c(lei, ancestors(engine, lei))

practice_areas <- c("Commercial Litigation", "IP Litigation", "Product Liability", "Employment",
                    "Securities Litigation", "Antitrust", "Corporate/M&A", "Finance", "Regulatory")

hit_labels <- c(
  current_client_adverse = "Adverse party is a current client",
  prospective_client_adverse_to_firm_client = "Prospective client is adverse to the firm's client",
  former_client_substantially_related = "Former client, substantially related work",
  affiliate_of_current_client = "Affiliate of a current client",
  lateral_hire_prior_client = "Prior client of a lateral hire",
  former_client_unrelated = "Former client, unrelated work",
  affiliate_of_former_client = "Affiliate of a former client",
  business_relationship = "Business relationship"
)
outcome_style <- list(
  high_risk = list(label = "High risk", bg = "#8e2b22", note = "At least one serious conflict. Don't accept without resolving it."),
  medium_risk = list(label = "Medium risk", bg = "#94680c", note = "A relationship that needs review before accepting."),
  low_risk = list(label = "Low risk", bg = "#22527d", note = "Relationships worth noting, but unlikely to block the matter."),
  clear = list(label = "Clear", bg = "#1d6a47", note = "No relationships found with any party.")
)
severity_bg <- c(high = "rgba(192, 57, 43, 0.30)", medium = "rgba(201, 162, 39, 0.22)", low = "rgba(59, 130, 196, 0.22)")

# Node colors, shared by the maps and their legends.
node_colors <- list(
  submitted = c(bg = "#e9d8a6", border = "#c9a227"),
  focus = c(bg = "#c9a227", border = "#f3e2a8"),
  client_current = c(bg = "#3b82c4", border = "#a9d1f5"),
  client_former = c(bg = "#a9d1f5", border = "#3b82c4"),
  matter = c(bg = "#1f5f56", border = "#4fb3a2"),
  attorney = c(bg = "#4b3a6b", border = "#a58bd0"),
  party = c(bg = "#8c8c8c", border = "#cfcfcf"),
  root = c(bg = "#d8cfb8", border = "#ffffff"),
  entity = c(bg = "#4a5568", border = "#8a94a6")
)

# Test intakes, grouped by scenario for the "load an example" menu.
example_choices <- c(
  list("None" = c("No test intake" = "")),
  split(setNames(intakes$intake_id, paste0(intakes$intake_id, ": ", intakes$prospective_client)), intakes$scenario)
)

# ---- Network diagrams -------------------------------------------------------------

group_rank <- c(submitted = 6, client_current = 5, client_former = 4, focus = 4, matter = 3,
                attorney = 3, party = 2, root = 1, entity = 0)

new_graph <- function() {
  env <- new.env()
  env$nodes <- tibble(id = character(), label = character(), group = character(), title = character())
  env$edges <- tibble(from = character(), to = character(), label = character())
  env
}
add_node <- function(g, id, label, group, title = label) {
  i <- match(id, g$nodes$id)
  if (is.na(i)) {
    g$nodes <- add_row(g$nodes, id = id, label = str_trunc(label, 34), group = group, title = title)
  } else if (group_rank[[group]] > group_rank[[g$nodes$group[i]]]) {
    g$nodes$group[i] <- group
  }
}
add_edge <- function(g, from, to, label = "") {
  if (!any(g$edges$from == from & g$edges$to == to)) g$edges <- add_row(g$edges, from = from, to = to, label = label)
}
add_entity <- function(g, lei, group = "entity") {
  e <- entity_by_lei[[lei]]
  title <- if (is.null(e)) display_name(lei) else
    sprintf("<b>%s</b><br>%s · %s<br>LEI %s", e$legal_name, coalesce(e$jurisdiction, "?"), coalesce(e$entity_status, "?"), lei)
  add_node(g, lei, display_name(lei), if (lei %in% families$family_id && group == "entity") "root" else group, title)
}
# Parent -> child edges up the tree from lei to its ultimate parent.
add_chain <- function(g, lei) {
  chain <- path_to_root(lei)
  for (k in seq_along(chain)) {
    add_entity(g, chain[k])
    if (k > 1) add_edge(g, chain[k], chain[k - 1], "owns")
  }
}
client_group <- function(client_id) {
  if (clients$client_status[match(client_id, clients$client_id)] == "current") "client_current" else "client_former"
}

render_graph <- function(g, hierarchical = FALSE, direction = "UD") {
  nodes <- g$nodes
  nodes$shape <- if_else(nodes$group %in% c("submitted", "matter", "attorney"), "box", "dot")
  net <- visNetwork(nodes, g$edges, width = "100%")
  # Boxes carry their label inside, so they need dark text; dots label below.
  dark_text <- c("submitted", "matter", "attorney")
  for (grp in names(node_colors)) {
    col <- node_colors[[grp]]
    font <- if (grp == "submitted") list(color = "#11161f", size = 15) else if (grp %in% dark_text) list(color = "#f3ead3") else list()
    net <- visGroups(net, groupname = grp, size = if (grp == "focus") 26 else 18, font = font,
                     color = list(background = col[["bg"]], border = col[["border"]],
                                  highlight = list(background = col[["bg"]], border = "#f3e2a8")))
  }
  net <- net |>
    visEdges(arrows = "to", font = list(size = 11, align = "horizontal", color = "#cfc8b8", strokeWidth = 0, background = "#0d131d"),
             color = list(color = "#6b7a90", highlight = "#c9a227"), smooth = list(type = "cubicBezier")) |>
    visNodes(font = list(size = 13, color = "#e9e4d8"), borderWidth = 2) |>
    visOptions(highlightNearest = list(enabled = TRUE, degree = 1)) |>
    visInteraction(navigationButtons = FALSE, zoomView = TRUE, dragView = TRUE)
  if (hierarchical) {
    # Top-down needs room for labels side by side; left-to-right stacks them,
    # which suits a parent with many subsidiaries.
    spacing <- if (direction == "UD") c(level = 120, node = 260) else c(level = 330, node = 85)
    net <- visHierarchicalLayout(net, direction = direction, sortMethod = "directed",
                                 levelSeparation = spacing[["level"]], nodeSpacing = spacing[["node"]]) |>
      visPhysics(enabled = FALSE)
  } else {
    net <- visLayout(net, randomSeed = 7)
  }
  net
}

# How each hit connects the submitted name to the firm's records.
hits_graph <- function(hits) {
  g <- new_graph()
  for (i in seq_len(nrow(hits))) {
    h <- hits[i, ]
    sub_id <- paste0("submitted:", h$party_name_submitted)
    add_node(g, sub_id, h$party_name_submitted, "submitted", paste("Submitted as:", h$party_name_submitted))
    key <- h$matched_key
    if (key %in% entities$lei) add_entity(g, key) else add_node(g, key, h$matched_name, "party")
    add_edge(g, sub_id, key, sprintf("matched %.0f%%", 100 * h$match_score))

    if (h$hit_type %in% c("current_client_adverse", "former_client_substantially_related", "former_client_unrelated")) {
      add_entity(g, key, client_group(h$related_client_id))
    } else if (h$hit_type %in% c("affiliate_of_current_client", "affiliate_of_former_client")) {
      client_lei <- clients$lei[match(h$related_client_id, clients$client_id)]
      add_chain(g, key)
      add_chain(g, client_lei)
      add_entity(g, client_lei, client_group(h$related_client_id))
    } else if (h$hit_type == "lateral_hire_prior_client") {
      lat <- filter(engine$laterals, party_key == key) |> slice(1)
      att <- attorneys[match(lat$attorney_id, attorneys$attorney_id), ]
      att_id <- paste0("attorney:", lat$attorney_id)
      add_node(g, att_id, paste("Lateral:", att$name), "attorney",
               sprintf("<b>%s</b><br>Joined %s from %s", att$name, att$join_date, lat$prior_firm))
      add_edge(g, key, att_id, "prior client of")
    } else if (!is.na(h$related_matter_id)) {
      m <- firm_matters[match(h$related_matter_id, firm_matters$matter_id), ]
      m_id <- paste0("matter:", m$matter_id)
      add_node(g, m_id, paste(m$matter_id, m$matter_name), "matter",
               sprintf("<b>%s</b><br>%s · %s since %s", m$matter_name, m$practice_area, m$status, m$open_date))
      add_edge(g, key, m_id, if (h$hit_type == "business_relationship") "counterparty on" else "adverse in")
      add_entity(g, m$client_lei, client_group(m$client_id))
      add_edge(g, m_id, m$client_lei, "for client")
    }
  }
  render_graph(g, hierarchical = TRUE)
}

# An entity's place in its corporate family, with the firm's clients marked.
family_graph <- function(focus, max_children = 25) {
  g <- new_graph()
  add_chain(g, focus)
  kids <- children_of[[focus]] %||% character()
  for (k in head(kids, max_children)) {
    add_entity(g, k)
    add_edge(g, focus, k, "owns")
  }
  if (length(kids) > max_children) {
    more <- paste0("more:", focus)
    add_node(g, more, sprintf("+ %d more subsidiaries", length(kids) - max_children), "entity")
    add_edge(g, focus, more)
  }
  fam <- unname(engine$family_of[focus])
  for (j in which(clients$family_id == fam)) {
    add_chain(g, clients$lei[j])
    add_entity(g, clients$lei[j], client_group(clients$client_id[j]))
  }
  add_entity(g, focus, "focus")
  g$edges$label <- "" # every edge here means "owns"; the footer says so
  render_graph(g, hierarchical = TRUE, direction = "LR")
}

# ---- Helpers ----------------------------------------------------------------------

parse_parties <- function(client, adverse) {
  adverse <- str_trim(str_split(adverse %||% "", "\n")[[1]])
  bind_rows(
    if (nzchar(str_trim(client))) tibble(party_name = str_trim(client), role = "prospective_client"),
    tibble(party_name = adverse[nzchar(adverse)], role = "adverse")
  )
}

matching_table <- function(parties) {
  map_dfr(seq_len(nrow(parties)), \(i) {
    # Show near misses too (down to 0.6), so an analyst can see a lookalike
    # like Merck KGaA was considered and ruled out, not overlooked.
    cands <- match_name(engine, parties$party_name[i]) |> filter(score >= 0.6) |> head(3)
    if (!nrow(cands)) {
      return(tibble(Submitted = parties$party_name[i], Role = parties$role[i], `Best candidate` = "No similar name in firm records",
                    Score = NA_real_, Verdict = "No match"))
    }
    tibble(
      Submitted = parties$party_name[i], Role = parties$role[i], `Best candidate` = cands$name,
      Score = round(cands$score, 3),
      Verdict = case_when(
        seq_along(cands$score) > 1 ~ "Runner-up",
        cands$score >= match_thresholds[["accept"]] ~ "Same party",
        cands$score >= match_thresholds[["review"]] ~ "Possible match, review",
        TRUE ~ "Similar name, not a match"
      )
    )
  }) |>
    mutate(Role = recode(Role, prospective_client = "Prospective client", adverse = "Adverse"))
}

dt <- function(df, page = 10, selection = "none", scrollX = TRUE, ...) {
  datatable(df, rownames = FALSE, escape = FALSE, selection = selection, fillContainer = FALSE,
            options = list(pageLength = page, dom = "ftip", scrollX = scrollX, ...), class = "compact hover")
}

map_legend <- function(groups) {
  labels <- c(submitted = "Name as submitted", client_current = "Current client", client_former = "Former client",
              root = "Ultimate parent", entity = "Other entity", focus = "This entity", matter = "Firm matter",
              attorney = "Lateral attorney")
  div(class = "legend", lapply(groups, \(g) span(
    span(class = paste("swatch", if (g %in% c("submitted", "matter", "attorney")) "box"),
         style = sprintf("background:%s;border:1px solid %s", node_colors[[g]][["bg"]], node_colors[[g]][["border"]])),
    labels[[g]]
  )))
}

# Banner at the top of each tab. The photo is muted in CSS so bright daytime
# shots sit comfortably in the dark palette.
page_hero <- function(eyebrow, title, text, image, position = "center", small = FALSE, aside = NULL) {
  div(
    class = paste("hero", if (small) "hero-sm"),
    style = sprintf("--hero-img: url('%s'); --hero-pos: %s;", image, position),
    div(class = "hero-text", div(class = "eyebrow", eyebrow), h1(title), div(class = "rule"), p(text)),
    if (!is.null(aside)) div(class = "hero-aside", aside)
  )
}

stat_tile <- function(value, label, sub = NULL) {
  div(class = "stat", div(class = "stat-value", value), div(class = "stat-label", label),
      if (!is.null(sub)) div(class = "stat-sub", sub))
}
stat_row <- function(...) div(class = "stat-row", ...)

# Small colored label, as HTML for use inside tables.
pill <- function(text, kind) sprintf("<span class='pill pill-%s'>%s</span>", kind, text)

hero <- page_hero(
  "Hartwell & Pierce LLP · New business intake", "Conflicts check",
  paste("Screen a prospective client and every adverse party against the firm's clients, matters,",
        "lateral hires and their corporate families before the firm takes on new work."),
  "books.jpg", "center 58%"
)

how_it_works <- card(
  card_header("How it works"),
  card_body(div(
    class = "steps",
    div(class = "step", div(class = "num", "I"), h6("Describe the matter"),
        p("Enter who wants to hire the firm, the practice area, and everyone on the other side, or load a test intake.")),
    div(class = "step", div(class = "num", "II"), h6("Match every name"),
        p("Each name is matched against 3,400 known entities and parties, tolerating typos, missing suffixes and former names.")),
    div(class = "step", div(class = "num", "III"), h6("Review the hits"),
        p("See each relationship found, how serious it is, and what to do next. Click a hit to see how it connects."))
  ))
)

search_suggestions <- c(sg_walmart = "Wal-Mart", sg_janssen = "Janssen", sg_google = "Google Ireland",
                        sg_jpm = "JP Morgan Securities", sg_wells = "Wells Fargo Bank")

firm_stats <- stat_row(
  stat_tile(sum(clients$client_status == "current"), "Current clients"),
  stat_tile(sum(clients$client_status == "former"), "Former clients"),
  stat_tile(sum(firm_matters$status == "open"), "Open matters", sprintf("of %d in total", nrow(firm_matters))),
  stat_tile(sum(!as.logical(attorneys$is_lateral)), "Attorneys", "plus three lateral hires"),
  stat_tile(n_distinct(firm_matters$docket_id, na.rm = TRUE), "Real dockets", "mirrored from CourtListener")
)

# ---- Lawsuits ---------------------------------------------------------------------

# CourtListener records the nature of suit inconsistently ("442 Civil Rights:
# Jobs", "Civil rights jobs"), so group it into plain categories. Order
# matters: employment before civil rights, contract before product liability.
suit_category <- function(nos) {
  nos <- str_to_lower(coalesce(nos, ""))
  case_when(
    nos == "" ~ "Not recorded",
    str_detect(nos, "jobs|labor|e\\.r\\.i\\.s\\.a|erisa|employment|medical leave") ~ "Employment & labor",
    str_detect(nos, "contract|negotiable|recovery|insurance") ~ "Contract",
    str_detect(nos, "p\\.i\\.|personal inj|prod\\. liab|product liab|health care|airplane|motor vehicle|assault|personal liability|marine") ~ "Personal injury & product liability",
    str_detect(nos, "civil rights|disabilities|prisoner|voting|habeas") ~ "Civil rights",
    str_detect(nos, "consumer credit|truth in lending|fair credit|debt|bank|telephone consumer") ~ "Consumer & banking",
    str_detect(nos, "fraud|racketeer") ~ "Fraud & racketeering",
    str_detect(nos, "patent|copyright|trademark") ~ "Intellectual property",
    str_detect(nos, "securit|commodit") ~ "Securities",
    str_detect(nos, "anti-?trust") ~ "Antitrust",
    str_detect(nos, "real property|foreclosure|landlord|rent") ~ "Real property",
    str_detect(nos, "property") ~ "Property damage",
    TRUE ~ "Other statutes"
  )
}

# How serious each tie would be if the firm took the other side against it.
tie_order <- c("Current client", "Affiliate of a current client", "Former client", "Prior client of a lateral hire",
               "Affiliate of a former client", "Counterparty on a firm matter", "Adverse party on a firm matter")
tie_severity <- c("Current client" = "high", "Affiliate of a current client" = "medium", "Former client" = "medium",
                  "Prior client of a lateral hire" = "medium", "Affiliate of a former client" = "low",
                  "Counterparty on a firm matter" = "low")

case_parties <- lawsuit_parties |>
  left_join(party_links, by = "party_name") |>
  mutate(confirmed = match_status %in% "match", match_score = as.numeric(match_score))

case_ties <- case_parties |>
  filter(confirmed, !is.na(tie)) |>
  mutate(rank = match(tie, tie_order)) |>
  arrange(docket_id, rank) |>
  group_by(docket_id) |>
  summarise(top_tie = first(tie), top_kind = first(tie_kind), n_tied = n_distinct(party_name))

cases <- lawsuits |>
  mutate(category = suit_category(nature_of_suit), year = as.integer(substr(date_filed, 1, 4)),
         court_short = str_remove(court, "^District Court, ")) |>
  left_join(case_ties, by = "docket_id") |>
  left_join(firm_matters |> filter(!is.na(docket_id)) |> distinct(docket_id, .keep_all = TRUE) |>
              select(docket_id, firm_matter_id = matter_id, firm_client_id = client_id), by = "docket_id") |>
  left_join(lawsuit_parties |> group_by(docket_id) |> summarise(party_text = paste(party_name, collapse = " ")), by = "docket_id") |>
  mutate(search_text = str_to_lower(paste(case_name, coalesce(party_text, ""), docket_number)))

# Everything about one case, for the pop-up.
case_detail <- function(id) {
  cs <- filter(cases, docket_id == id)
  ps <- filter(case_parties, docket_id == id)
  chip <- \(label, value) div(class = "chip", span(class = "chip-label", label), span(class = "chip-value", value))

  party_item <- function(i) {
    p <- ps[i, ]
    match_note <- if (is.na(p$matched_name)) NULL
      else if (!p$confirmed) sprintf("possible match: %s (%.0f%%), needs confirming", p$matched_name, 100 * p$match_score)
      else if (p$match_score < 1) sprintf("matched %s (%.0f%%)", p$matched_name, 100 * p$match_score)
    div(class = "party",
        div(class = "party-name", p$party_name,
            if (!is.na(p$tie)) HTML(pill(if (p$confirmed) p$tie else paste(p$tie, "?"), if (p$confirmed) p$tie_kind else "none"))),
        if (!is.null(match_note)) div(class = "party-note", match_note),
        if (!is.na(p$ties_detail) && p$confirmed) div(class = "party-note", str_replace_all(p$ties_detail, " \\| ", " · ")))
  }
  side_list <- function(side) {
    idx <- which(ps$side == side)
    if (!length(idx)) return(p(class = "form-text", "No parties recorded on this side."))
    div(class = "party-list", lapply(idx, party_item))
  }

  # If the firm took one side, which confirmed ties sit on the other side?
  verdict <- function(side, other, label) {
    against <- ps |> filter(side == !!other, confirmed, tie %in% names(tie_severity)) |>
      mutate(sev = unname(tie_severity[tie]), rank = match(sev, c("high", "medium", "low"))) |>
      arrange(rank) |> distinct(party_name, .keep_all = TRUE)
    worst <- if (nrow(against)) against$sev[1] else "clear"
    div(class = paste("verdict", paste0("verdict-", worst)),
        div(class = "verdict-title", label),
        if (!nrow(against)) p("No conflicts with the firm's clients.")
        else tags$ul(lapply(seq_len(nrow(against)), \(j) tags$li(
          HTML(sprintf("<span class='sev sev-%s'>%s</span> Against <b>%s</b>, %s", against$sev[j], against$sev[j],
                       htmltools::htmlEscape(against$party_name[j]), str_to_lower(against$tie[j])))))))
  }

  # Dockets list the same firm several ways ("Davis Polk & Wardwell LLP (NYC)",
  # "Davis Polk & Wardwell LLP"), so drop office tags and merge near-duplicates.
  counsel_names <- function(type) {
    lawsuit_counsel |> filter(docket_id == id, counsel_type == type) |>
      mutate(name = str_squish(str_remove(name, "\\s*\\([^)]*\\)\\s*$")), key = norm_name(name)) |>
      distinct(key, .keep_all = TRUE)
  }
  firms <- counsel_names("firm")
  lawyers <- counsel_names("attorney")
  counsel_chips <- function(names, cap = 24) {
    if (!length(names)) return(p(class = "form-text", "None listed."))
    div(class = "counsel", lapply(head(names, cap), \(n) span(class = "counsel-chip", n)),
        if (length(names) > cap) span(class = "form-text", sprintf("+ %d more", length(names) - cap)))
  }
  firm_client <- if (!is.na(cs$firm_client_id)) clients$client_name[match(cs$firm_client_id, clients$client_id)]

  tagList(
    div(class = "chips",
        chip("Court", cs$court_short), chip("Docket", cs$docket_number), chip("Filed", cs$date_filed),
        chip("Status", if (is.na(cs$date_terminated)) "Open or not recorded" else paste("Closed", cs$date_terminated)),
        chip("Type of suit", cs$category), chip("Nature of suit", coalesce(cs$nature_of_suit, "Not recorded"))),
    if (!is.na(cs$firm_matter_id)) div(class = "firm-banner",
      HTML(sprintf("<b>Hartwell &amp; Pierce handles this case</b> as matter %s for %s.", cs$firm_matter_id,
                   htmltools::htmlEscape(firm_client)))),
    h5(class = "section-title", "Parties"),
    layout_columns(
      col_widths = breakpoints(sm = 12, md = c(6, 6)),
      div(class = "side", div(class = "side-title", "Plaintiff side"), side_list("plaintiff")),
      div(class = "side", div(class = "side-title", "Defendant side"), side_list("defendant"))
    ),
    if (any(ps$side == "unknown")) div(class = "side side-unknown",
      div(class = "side-title", "Side not clear from the case name"), side_list("unknown")),
    h5(class = "section-title", "If someone on this case asked the firm to represent them"),
    layout_columns(
      col_widths = breakpoints(sm = 12, md = c(6, 6)),
      verdict("plaintiff", "defendant", "Representing the plaintiff side"),
      verdict("defendant", "plaintiff", "Representing the defendant side")
    ),
    if (any(ps$side == "unknown" & ps$confirmed & !is.na(ps$tie)))
      p(class = "form-text", "Parties whose side isn't clear aren't counted here; check them by hand."),
    h5(class = "section-title", "Counsel on the docket"),
    p(class = "form-text", "CourtListener lists counsel for the whole case, not which side each represents."),
    div(class = "counsel-group", strong("Law firms"), counsel_chips(firms$name)),
    div(class = "counsel-group", strong("Attorneys"), counsel_chips(lawyers$name))
  )
}

court_choices <- cases |> count(court, sort = TRUE) |> with(setNames(court, sprintf("%s (%d)", str_remove(court, "^District Court, "), n)))
category_choices <- sort(unique(cases$category))

# ---- UI ---------------------------------------------------------------------------

ui <- page_navbar(
  title = span(span(class = "brand-mark", "⚖"), "Hartwell & Pierce"),
  window_title = "Hartwell & Pierce · Conflicts",
  theme = bs_theme(
    version = 5, bg = "#0e141e", fg = "#e9e4d8", primary = "#c9a227", secondary = "#2a3547",
    success = "#2e8b57", danger = "#c0392b", info = "#3b82c4", "border-radius" = "10px",
    base_font = font_link("Inter", href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600&display=swap"),
    heading_font = font_link("Playfair Display", href = "https://fonts.googleapis.com/css2?family=Playfair+Display:wght@500;600;700&display=swap")
  ),
  header = tags$head(tags$link(rel = "stylesheet", href = "styles.css")),
  fillable = FALSE,

  nav_panel(
    "Run a check",
    hero,
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        width = 360,
        open = list(desktop = "open", mobile = "always"),
        title = "New matter",
        # The test intake picker is a shortcut that fills in the form, so it
        # sits apart from the form itself.
        div(
          class = "template-box",
          div(class = "template-title", "Start from a test intake"),
          p(class = "template-help", "Pick a ready-made request to fill in the form below, then run the check."),
          selectizeInput("example", NULL, choices = example_choices,
                         options = list(placeholder = "Choose a test intake…"))
        ),
        div(class = "form-divider", span("or enter your own")),
        textInput("client", "Prospective client", placeholder = "Who wants to hire the firm?"),
        selectInput("practice", "Practice area", practice_areas),
        textAreaInput("adverse", "Adverse parties (one per line)", rows = 6,
                      placeholder = "Wal-Mart Stores Inc\nWells Fargo Bank NA"),
        actionButton("run", "Run conflicts check", class = "btn-primary w-100"),
        helpText("Names are matched fuzzily, so typos, missing suffixes and former names still match.")
      ),
      uiOutput("outcome"),
      uiOutput("hits_card"),
      uiOutput("details")
    )
  ),

  nav_panel(
    "Explore an entity",
    page_hero(
      "Research", "Explore an entity",
      paste("Look up any company: where it sits in its corporate family, how the firm is connected to it,",
            "and the federal lawsuits it has been part of."),
      "courts.jpg", "center 78%", small = TRUE
    ),
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        width = 360,
        open = list(desktop = "open", mobile = "always"),
        title = "Find a company",
        textInput("search", NULL, placeholder = "Search by name, e.g. Janssen"),
        div(class = "suggest", span("Try"), lapply(names(search_suggestions), \(id) actionLink(id, search_suggestions[[id]]))),
        DTOutput("search_results")
      ),
      uiOutput("entity_view")
    )
  ),

  nav_panel(
    "Lawsuits",
    page_hero(
      "Federal dockets · CourtListener", "Lawsuits",
      paste("Search 904 real federal cases involving the companies in the firm's records. Open a case to see",
            "who is on each side, who represents them, and where the firm would be conflicted."),
      "session.jpg", "center 62%", small = TRUE
    ),
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        width = 360,
        open = list(desktop = "open", mobile = "always"),
        title = "Find a case",
        textInput("ls_search", NULL, placeholder = "Case name or party, e.g. Pfizer"),
        selectizeInput("ls_category", "Type of suit", choices = category_choices, multiple = TRUE,
                       options = list(placeholder = "All types")),
        selectizeInput("ls_court", "Court", choices = court_choices, multiple = TRUE,
                       options = list(placeholder = "All courts")),
        sliderInput("ls_years", "Filed", min = min(cases$year), max = max(cases$year),
                    value = range(cases$year), step = 1, sep = ""),
        checkboxInput("ls_pair", "Company v. company only"),
        checkboxInput("ls_tied", "Only cases touching the firm's clients"),
        actionLink("ls_reset", "Clear filters")
      ),
      uiOutput("ls_stats"),
      card(card_header("Cases", span(class = "sub", "Click a case to open it")),
           card_body(DTOutput("ls_table"), fillable = FALSE), fill = FALSE)
    )
  ),

  nav_panel(
    "Firm records",
    page_hero(
      "Hartwell & Pierce LLP", "Firm records",
      paste("The firm's book of business: clients, matters, attorneys and lateral hires. The firm is",
            "fictional; its clients are real companies, and many matters mirror real federal cases."),
      "columns.jpg", "center 42%", small = TRUE
    ),
    firm_stats,
    navset_card_underline(
      nav_panel("Clients", DTOutput("t_clients")),
      nav_panel("Matters", DTOutput("t_matters")),
      nav_panel("Attorneys", DTOutput("t_attorneys")),
      nav_panel("Lateral hires' prior clients", DTOutput("t_laterals"))
    )
  ),

  nav_panel(
    "Engine performance",
    page_hero(
      "Quality control", "Engine performance",
      paste("How the conflict engine scores against the answer key: 135 test intakes with names typed the way",
            "people really type them, each labeled with the conflicts a careful analyst should find."),
      "courtroom.jpg", "center 80%", small = TRUE,
      aside = actionButton("rerun_eval", "Re-run evaluation", class = "btn-outline-gold")
    ),
    uiOutput("perf_boxes"),
    layout_columns(
      col_widths = breakpoints(sm = 12, lg = c(5, 7)),
      card(card_header("Intake outcomes", span(class = "sub", "expected ↓ vs. engine →")),
           uiOutput("perf_confusion")),
      card(card_header("Hits by conflict type", span(class = "sub", "precision and recall")), uiOutput("perf_types"))
    ),
    card(card_header("Errors to review"), uiOutput("perf_errors"))
  )
)

# ---- Server -----------------------------------------------------------------------

server <- function(input, output, session) {

  # Run a check ------------------------------------------------------------------

  observeEvent(input$example, {
    if (!nzchar(input$example)) return()
    it <- filter(intakes, intake_id == input$example)
    p <- filter(intake_parties, intake_id == input$example)
    updateTextInput(session, "client", value = coalesce(p$party_name_submitted[p$role == "prospective_client"][1], ""))
    updateSelectInput(session, "practice", selected = it$practice_area)
    updateTextAreaInput(session, "adverse", value = paste(p$party_name_submitted[p$role == "adverse"], collapse = "\n"))
  })

  check <- eventReactive(input$run, {
    parties <- parse_parties(input$client, input$adverse)
    shiny::validate(need(nrow(parties) > 0, "Enter at least one party."))
    r <- check_intake(engine, parties, input$practice)
    r$parties <- parties
    r$example <- input$example
    r
  })

  output$outcome <- renderUI({
    if (input$run == 0) return(how_it_works)
    r <- check()
    st <- outcome_style[[r$outcome]]
    confirmed <- filter(r$hits, match_status == "match")
    layout_columns(
      col_widths = breakpoints(sm = 12, md = c(6, 6, 12), xxl = c(5, 4, 3)),
      value_box(title = "Outcome", value = st$label, p(st$note),
                theme = value_box_theme(bg = st$bg, fg = "#ffffff")),
      value_box(title = "Parties checked", value = nrow(r$parties),
                p(sprintf("%d confirmed hit%s", nrow(confirmed), if (nrow(confirmed) == 1) "" else "s"),
                  if (r$needs_review) strong(" · some names need an analyst to confirm the match")),
                theme = value_box_theme(bg = "#152033", fg = "#e9e4d8")),
      div(class = "map-launch",
          div(class = "title", "Relationship map"),
          div(class = "desc", if (nrow(r$hits)) "See how every hit connects to the firm's clients and matters."
                              else "No relationships to map for this matter."),
          if (nrow(r$hits)) actionButton("open_map", "Open map", class = "btn-outline-gold btn-sm"))
    )
  })

  output$hits_card <- renderUI({
    req(input$run > 0)
    card(card_header("Conflict hits", span(class = "sub", "Click a hit to see how it connects")),
         card_body(DTOutput("hits"), fillable = FALSE), fill = FALSE)
  })

  output$details <- renderUI({
    r <- check()
    panels <- list(accordion_panel("How each name was matched", value = "matching", DTOutput("matching")))
    if (nzchar(r$example)) {
      panels <- c(panels, list(accordion_panel(sprintf("Answer key · test intake %s", r$example),
                                               value = "answer", DTOutput("answer_table"))))
    }
    do.call(accordion, c(panels, list(id = "details_acc", open = FALSE, multiple = TRUE)))
  })

  # The map opens in a pop-up: from the button for every hit, or from a row
  # click for just that hit.
  map_hits <- reactiveVal(NULL)
  show_map <- function(hits, title) {
    map_hits(hits)
    showModal(modalDialog(
      title = title, size = "xl", easyClose = TRUE,
      # The legend goes above the map, and the map takes whatever height the
      # window has left, so the whole pop-up fits without scrolling.
      map_legend(c("submitted", "client_current", "client_former", "root", "entity", "matter", "attorney")),
      div(class = "map-frame mt-2", visNetworkOutput("hit_map", height = "calc(100vh - 290px)")),
      footer = tagList(span(class = "form-text", "Scroll to zoom, drag to pan, hover over a node for details."),
                       modalButton("Close"))
    ))
  }
  observeEvent(input$open_map, show_map(check()$hits, "How every hit connects"))
  observeEvent(input$hits_rows_selected, {
    hits <- check()$hits
    i <- input$hits_rows_selected
    if (nrow(hits) && i <= nrow(hits)) {
      show_map(hits[i, ], sprintf("How “%s” connects", hits$party_name_submitted[i]))
    }
    selectRows(dataTableProxy("hits"), NULL) # so clicking the same row again reopens it
  })

  output$hits <- renderDT({
    r <- check()
    if (!nrow(r$hits)) return(dt(tibble(Result = "No conflict hits for any party.")))
    df <- r$hits |>
      left_join(select(clients, related_client_id = client_id, client_name), by = "related_client_id") |>
      left_join(select(firm_matters, related_matter_id = matter_id, matter_name), by = "related_matter_id") |>
      transmute(
        Severity = sprintf("<span class='sev sev-%s'>%s</span>", severity, severity),
        # Spell out what the name matched only when it wasn't an exact match.
        Party = paste0(
          htmltools::htmlEscape(party_name_submitted),
          if_else(match_score < 1, sprintf("<br><small class='text-muted'>matched %s (%.0f%%)%s</small>",
                                           htmltools::htmlEscape(matched_name), 100 * match_score,
                                           if_else(match_status == "review", ", <b>needs confirming</b>", "")), "")
        ),
        Conflict = sprintf("<b>%s</b><br><small class='text-muted'>%s</small>", unname(hit_labels[hit_type]), detail),
        `Client / matter` = coalesce(client_name, if_else(is.na(related_matter_id), "", paste(related_matter_id, matter_name))),
        `Next step` = recommended_action,
        sev = severity # hidden; drives the row tint
      )
    # Bootstrap 5 paints each cell's background over the row's, so tint cells.
    dt(df, selection = "single", scrollX = FALSE, autoWidth = FALSE,
       columnDefs = list(list(visible = FALSE, targets = ncol(df) - 1))) |>
      formatStyle(setdiff(names(df), "sev"), valueColumns = "sev",
                  backgroundColor = styleEqual(c("high", "medium", "low"), unname(severity_bg)))
  })

  output$hit_map <- renderVisNetwork({
    req(map_hits())
    hits_graph(map_hits())
  })

  output$matching <- renderDT({
    dt(matching_table(check()$parties), page = 8)
  })

  output$answer_table <- renderDT({
    r <- check()
    exp <- filter(expected_hits, intake_id == r$example)
    sc <- intakes$scenario[intakes$intake_id == r$example]
    if (!nrow(exp)) return(dt(tibble(Expected = sprintf("No hits expected (scenario: %s). The engine said: %s.",
                                                       sc, outcome_style[[r$outcome]]$label))))
    found <- filter(r$hits, match_status == "match")
    dt(exp |> transmute(
      Party = party_name_submitted,
      `True identity` = display_name(coalesce(truth_lei, "")),
      `Expected conflict` = unname(hit_labels[hit_type]),
      Severity = str_to_title(severity),
      `Engine found it?` = if_else(paste(party_name_submitted, hit_type) %in% paste(found$party_name_submitted, found$hit_type),
                                   "Yes", "<b style='color:#e57368'>No</b>")
    ))
  })

  # Explore an entity --------------------------------------------------------------

  search_hits <- reactive({
    req(nchar(str_trim(input$search)) >= 2)
    match_name(engine, input$search) |>
      filter(key %in% entities$lei, score >= 0.5) |>
      head(12) |>
      mutate(family = families$seed_name[match(family_id, families$family_id)])
  })

  for (id in names(search_suggestions)) local({
    id <- id
    observeEvent(input[[id]], updateTextInput(session, "search", value = search_suggestions[[id]]))
  })

  output$search_results <- renderDT({
    s <- search_hits()
    datatable(transmute(s, Name = name, Family = family, Match = sprintf("%.0f%%", 100 * score)), rownames = FALSE,
              selection = list(mode = "single", selected = 1), options = list(dom = "t", pageLength = 12, ordering = FALSE),
              class = "compact hover")
  })

  focus <- reactive({
    s <- search_hits()
    i <- input$search_results_rows_selected %||% 1
    req(length(i) == 1, i <= nrow(s))
    s$key[i]
  })

  family_suits <- reactive({
    seed <- families$seed_name[match(unname(engine$family_of[focus()]), families$family_id)]
    filter(lawsuits, str_detect(query_seeds, fixed(seed))) |> arrange(desc(date_filed))
  })

  # The whole right-hand side: a starting screen until there's something to
  # show, then the entity's profile.
  output$entity_view <- renderUI({
    q <- str_trim(input$search %||% "")
    if (nchar(q) < 2) {
      return(card(card_body(class = "empty-state",
        div(class = "eyebrow", "Start here"),
        h3("Search for any company"),
        p(sprintf("The firm's records cover %s legal entities across %d corporate families, from Pfizer's",
                  format(nrow(entities), big.mark = ","), nrow(families)),
          "subsidiaries in Belgium to Walmart's in Chile. Type a name on the left, or try a suggestion."),
        stat_row(stat_tile(format(nrow(entities), big.mark = ","), "Entities"),
                 stat_tile(nrow(families), "Corporate families"),
                 stat_tile(format(nrow(lawsuits), big.mark = ","), "Federal lawsuits"))
      )))
    }
    if (!nrow(search_hits())) {
      return(card(card_body(class = "empty-state", h3("No close match"),
                            p("Nothing in the firm's records resembles that name. Try a shorter or different spelling."))))
    }
    lei <- focus()
    e <- entity_by_lei[[lei]]
    fam <- families[match(e$family_id, families$family_id), ]
    own <- filter(clients, lei == !!lei)
    in_family <- filter(clients, family_id == e$family_id)
    status <- if (nrow(own)) pill(paste(str_to_title(own$client_status[1]), "client"), own$client_status[1])
              else if (nrow(in_family)) pill("Affiliate of a client", "affiliate")
              else pill("No relationship", "none")
    chip <- \(label, value) div(class = "chip", span(class = "chip-label", label), span(class = "chip-value", value))
    n_aliases <- sum(aliases$lei == lei)

    tagList(
      card(class = "profile", card_body(
        div(class = "profile-top",
            div(div(class = "eyebrow", paste(fam$seed_name, "family")), h2(class = "profile-name", e$legal_name)),
            HTML(status)),
        div(class = "chips",
            chip("LEI", e$lei),
            chip("Jurisdiction", coalesce(e$jurisdiction, "Unknown")),
            chip("Status", str_to_sentence(coalesce(e$entity_status, "Unknown"))),
            chip("Headquarters", paste(na.omit(c(str_to_title(e$hq_city), e$hq_country)), collapse = ", ")),
            chip("Ultimate parent", fam$family_name),
            chip("Family size", paste(fam$member_count, "entities")))
      )),
      layout_columns(
        col_widths = breakpoints(sm = 12, lg = c(8, 4)),
        card(card_header("Corporate family", span(class = "sub", "drag, zoom, hover for details")),
             div(class = "map-frame", visNetworkOutput("family_map", height = "500px")), full_screen = TRUE,
             card_footer(map_legend(c("focus", "client_current", "client_former", "root", "entity")))),
        card(card_header("Firm relationships"), uiOutput("entity_relationships"))
      ),
      navset_card_underline(
        nav_panel(sprintf("Federal lawsuits (%d)", nrow(family_suits())), DTOutput("entity_lawsuits")),
        nav_panel(sprintf("Other names (%d)", n_aliases), DTOutput("entity_aliases"))
      )
    )
  })

  output$family_map <- renderVisNetwork(family_graph(focus()))

  output$entity_relationships <- renderUI({
    lei <- focus()
    fam <- unname(engine$family_of[lei])
    own <- filter(clients, lei == !!lei)
    fam_clients <- filter(clients, family_id == fam, lei != !!lei)
    on_matters <- filter(firm_parties, party_key == lei, role != "client") |>
      left_join(select(firm_matters, matter_id, matter_name, status), by = "matter_id")
    lat <- filter(engine$laterals, party_key == lei)
    item <- \(kind, title, text) div(class = paste("rel", paste0("rel-", kind)), div(class = "rel-title", title), div(class = "rel-text", HTML(text)))
    items <- c(
      if (nrow(own)) list(item(own$client_status, paste(str_to_title(own$client_status), "client"),
                               sprintf("%s, client since %s", own$client_id, own$client_since))),
      map(seq_len(nrow(fam_clients)), \(j) item(fam_clients$client_status[j],
                                                paste("Same family as a", fam_clients$client_status[j], "client"),
                                                fam_clients$client_name[j])),
      map(seq_len(nrow(on_matters)), \(j) item("matter", paste("Named as", on_matters$role[j], "on a firm matter"),
                                               sprintf("%s <i>%s</i> (%s)", on_matters$matter_id[j], on_matters$matter_name[j], on_matters$status[j]))),
      map(seq_len(nrow(lat)), \(j) item("lateral", "Prior client of a lateral hire",
                                        sprintf("%s, at %s", lat$attorney_id[j], lat$prior_firm[j])))
    )
    if (!length(items)) {
      return(div(class = "empty-rel", p("No relationship with the firm or its clients."),
                 p(class = "form-text", "Being adverse to this company wouldn't raise a conflict.")))
    }
    div(class = "rel-list", items)
  })

  output$entity_aliases <- renderDT({
    dt(filter(aliases, lei == focus()) |> transmute(Name = alias, Type = str_to_sentence(str_replace_all(alias_type, "_", " "))), page = 5)
  })

  output$entity_lawsuits <- renderDT({
    suits <- family_suits() |>
      transmute(Case = sprintf("<a href='%s' target='_blank'>%s</a>", url, case_name), Court = court,
                Filed = date_filed, `Nature of suit` = coalesce(nature_of_suit, ""),
                Type = if_else(query_type == "pair", pill("Company v. company", "pair"), ""))
    dt(suits, page = 8)
  })

  # Lawsuits -----------------------------------------------------------------------

  cases_filtered <- reactive({
    out <- cases
    q <- str_to_lower(str_trim(input$ls_search %||% ""))
    if (nzchar(q)) out <- filter(out, str_detect(search_text, fixed(q)))
    if (length(input$ls_category)) out <- filter(out, category %in% input$ls_category)
    if (length(input$ls_court)) out <- filter(out, court %in% input$ls_court)
    out <- filter(out, between(year, input$ls_years[1], input$ls_years[2]))
    if (isTRUE(input$ls_pair)) out <- filter(out, query_type == "pair")
    if (isTRUE(input$ls_tied)) out <- filter(out, !is.na(top_tie) | !is.na(firm_matter_id))
    arrange(out, desc(date_filed))
  })

  observeEvent(input$ls_reset, {
    updateTextInput(session, "ls_search", value = "")
    updateSelectizeInput(session, "ls_category", selected = character())
    updateSelectizeInput(session, "ls_court", selected = character())
    updateSliderInput(session, "ls_years", value = range(cases$year))
    updateCheckboxInput(session, "ls_pair", value = FALSE)
    updateCheckboxInput(session, "ls_tied", value = FALSE)
  })

  output$ls_stats <- renderUI({
    f <- cases_filtered()
    stat_row(
      stat_tile(format(nrow(f), big.mark = ","), "Cases", if (nrow(f) < nrow(cases)) sprintf("of %s", format(nrow(cases), big.mark = ","))),
      stat_tile(sum(f$query_type == "pair"), "Company v. company"),
      stat_tile(sum(!is.na(f$top_tie)), "Touch the firm's clients"),
      stat_tile(sum(!is.na(f$firm_matter_id)), "Handled by the firm")
    )
  })

  output$ls_table <- renderDT({
    f <- cases_filtered()
    if (!nrow(f)) return(dt(tibble(Result = "No cases match these filters.")))
    df <- f |> transmute(
      Case = sprintf("<b>%s</b><br><small class='text-muted'>%s · %s</small>",
                     htmltools::htmlEscape(str_trunc(case_name, 130)), court_short, docket_number),
      Filed = date_filed,
      `Type of suit` = category,
      `Firm tie` = case_when(
        !is.na(firm_matter_id) ~ pill("Firm matter", "open"),
        !is.na(top_tie) ~ pill(top_tie, top_kind),
        TRUE ~ ""
      ),
      Kind = if_else(query_type == "pair", pill("Company v. company", "pair"), "")
    )
    dt(df, page = 15, selection = "single", scrollX = FALSE, autoWidth = FALSE)
  })

  observeEvent(input$ls_table_rows_selected, {
    f <- cases_filtered()
    i <- input$ls_table_rows_selected
    if (nrow(f) && i <= nrow(f)) {
      showModal(modalDialog(
        title = str_trunc(f$case_name[i], 160), size = "xl", easyClose = TRUE,
        case_detail(f$docket_id[i]),
        footer = tagList(tags$a(class = "btn btn-outline-gold btn-sm", href = f$url[i], target = "_blank",
                                "View docket on CourtListener ↗"),
                         modalButton("Close"))
      ))
    }
    selectRows(dataTableProxy("ls_table"), NULL)
  })

  # Firm records -------------------------------------------------------------------

  output$t_clients <- renderDT({
    dt(clients |>
         left_join(select(attorneys, relationship_partner_id = attorney_id, partner = name), by = "relationship_partner_id") |>
         left_join(select(families, family_id, Family = seed_name), by = "family_id") |>
         arrange(client_status != "current", client_id) |>
         transmute(ID = client_id, Client = client_name, Status = pill(str_to_title(client_status), client_status), Family,
                   `Relationship partner` = partner, Since = client_since, Until = coalesce(client_until, "")), page = 20)
  })

  output$t_matters <- renderDT({
    others <- firm_parties |> filter(role != "client") |> group_by(matter_id) |>
      summarise(Parties = paste(sprintf("<span class='text-muted'>%s:</span> %s", role, party_name), collapse = "<br>"))
    dt(firm_matters |>
         left_join(select(clients, client_id, client_name), by = "client_id") |>
         left_join(others, by = "matter_id") |>
         left_join(select(attorneys, responsible_attorney_id = attorney_id, attorney = name), by = "responsible_attorney_id") |>
         left_join(select(lawsuits, docket_id, url), by = "docket_id") |>
         transmute(ID = matter_id, Client = client_name, Matter = matter_name, `Practice area` = practice_area,
                   Status = pill(str_to_title(status), status), Opened = open_date, Closed = coalesce(close_date, ""),
                   `Other parties` = coalesce(Parties, ""), Attorney = attorney,
                   Source = if_else(is.na(url), pill("Synthetic", "none"),
                                    sprintf("<a href='%s' target='_blank'>%s</a>", url, pill("Real docket", "pair")))), page = 15)
  })

  output$t_attorneys <- renderDT({
    dt(attorneys |> transmute(ID = attorney_id, Name = name, `Practice group` = practice_group, Title = title,
                              Joined = join_date, Lateral = if_else(is_lateral == "TRUE", pill("Lateral hire", "lateral"), ""),
                              `Prior firm` = coalesce(prior_firm, "")),
       page = 30)
  })

  output$t_laterals <- renderDT({
    dt(engine$laterals |> left_join(select(attorneys, attorney_id, attorney = name), by = "attorney_id") |>
         transmute(Attorney = attorney, `Prior firm` = prior_firm, `Prior client` = client_name,
                   `Practice area` = practice_area, `Representation ended` = representation_ended))
  })

  # Engine performance -------------------------------------------------------------

  eval_version <- reactiveVal(0)
  observeEvent(input$rerun_eval, {
    withProgress(message = "Re-running the evaluation", {
      source("R/04_evaluate.R", local = new.env())
    })
    eval_version(eval_version() + 1)
  })
  eval_data <- reactive({
    eval_version()
    shiny::validate(need(file.exists("data/eval/summary.csv"), "No evaluation yet. Click “Re-run evaluation” above to run one."))
    list(summary = rd("summary", "data/eval"), types = rd("hits_by_type", "data/eval"),
         outcomes = rd("intake_outcomes", "data/eval"), errors = rd("errors", "data/eval"))
  })

  output$perf_boxes <- renderUI({
    s <- eval_data()$summary
    v <- setNames(s$value, s$metric)
    n <- \(k) as.numeric(v[[k]])
    pct <- \(a, b) sprintf("%.0f%%", 100 * n(a) / n(b))
    stat_row(
      stat_tile(pct("outcomes_correct", "intakes_total"), "Outcome correct",
                sprintf("%s of %s test intakes", v[["outcomes_correct"]], v[["intakes_total"]])),
      stat_tile(pct("hits_correct", "hits_raised"), "Hit precision",
                sprintf("%s of %s raised hits were right", v[["hits_correct"]], v[["hits_raised"]])),
      stat_tile(pct("hits_correct", "hits_expected"), "Hit recall",
                sprintf("%s of %s expected hits found", v[["hits_correct"]], v[["hits_expected"]])),
      stat_tile(pct("parties_correct", "parties_total"), "Names identified",
                sprintf("last evaluated %s", v[["evaluated_at"]]))
    )
  })

  # Expected outcome down the side, the engine's answer across the top. The
  # diagonal is where they agree, so anything off it is a mistake.
  output$perf_confusion <- renderUI({
    o <- eval_data()$outcomes
    lv <- c("high_risk", "medium_risk", "low_risk", "clear")
    labels <- c("High", "Medium", "Low", "Clear")
    tab <- table(factor(o$expected_outcome, lv), factor(o$predicted_outcome, lv))
    top <- max(tab)
    cell <- function(i, j) {
      x <- tab[i, j]
      cls <- if (i == j) "cm-agree" else if (x > 0) "cm-miss" else "cm-zero"
      alpha <- if (i == j) 0.12 + 0.55 * x / top else 0
      tags$td(class = cls, style = if (i == j) sprintf("background: rgba(201,162,39,%.2f)", alpha), x)
    }
    tags$table(class = "cm",
      tags$thead(tags$tr(tags$th(""), lapply(labels, tags$th))),
      tags$tbody(lapply(seq_along(lv), \(i) tags$tr(tags$th(labels[i]), lapply(seq_along(lv), \(j) cell(i, j)))))
    )
  })

  output$perf_types <- renderUI({
    t <- eval_data()$types |> mutate(across(c(expected, found, correct, precision, recall), as.numeric))
    bar <- \(x) div(class = "bar", div(class = "bar-fill", style = sprintf("width:%.0f%%", 100 * coalesce(x, 0))),
                    span(class = "bar-label", if (is.na(x)) "\u2013" else sprintf("%.0f%%", 100 * x)))
    tags$table(class = "types",
      tags$thead(tags$tr(tags$th("Conflict"), tags$th("Expected"), tags$th("Precision"), tags$th("Recall"))),
      tags$tbody(lapply(seq_len(nrow(t)), \(i) tags$tr(
        tags$td(unname(hit_labels[t$hit_type[i]])), tags$td(class = "num", t$expected[i]),
        tags$td(bar(t$precision[i])), tags$td(bar(t$recall[i]))
      )))
    )
  })

  output$perf_errors <- renderUI({
    e <- eval_data()$errors
    if (!nrow(e)) {
      return(div(class = "all-clear", span(class = "check", "\u2713"),
                 div(strong("No errors"), p("Every expected hit was found, and nothing extra was raised."))))
    }
    dt(e |> transmute(Error = error, Intake = intake_id, Party = party_name_submitted,
                      Conflict = unname(hit_labels[hit_type]), Detail = detail))
  })
}

shinyApp(ui, server)
