# Conflicts Check

A prototype conflicts-of-interest checker for a law firm, built in R and Shiny.

When a firm is asked to take on a new matter, a conflicts analyst checks everyone involved against the firm's records: current and former clients, their parent companies and subsidiaries, open matters, and the past clients of newly hired lawyers. This tool does that check. It matches messy, misspelled names to the right company, walks real corporate ownership trees, and rates each relationship it finds by how serious it is.

The firm, **Hartwell & Pierce LLP**, is fictional, because real client lists are confidential. Everything around it is real:
- **Corporate families:** 2,656 legal entities in 23 families (Pfizer, JPMorgan, Walmart and others), from GLEIF, the global register of company identifiers.
- **Lawsuits:** 904 federal cases involving those companies, from CourtListener.

## Quick start

The generated data is included, so the app runs straight after cloning. In R 4.4 or later:

```r
install.packages(c("shiny", "bslib", "DT", "visNetwork", "httr2", "jsonlite", "dplyr", "purrr",
                   "readr", "stringr", "tidyr", "tibble", "stringdist"))
shiny::runApp("path/to/conflicts-check")
```

## How it's built

The model has three parts:

1. **Entity graph:** companies, aliases and parent/subsidiary links (real, from GLEIF).
2. **Matter and party tables:** who was on which side of which matter (real dockets from CourtListener, plus the fictional firm's book).
3. **Matching and scoring layer:** `R/conflict_engine.R`, graded against the answer key in `intake_expected_hits.csv`.

## Rebuilding the data

Only needed to refresh or extend the data. Run from this folder:

```r
setwd("path/to/conflicts-check")
source("R/01_collect_gleif.R")          # about an hour the first time; GLEIF allows ~60 requests/min
source("R/02_collect_courtlistener.R")  # ~10 min with a token; search is capped at 5 requests/min
source("R/03_build_firm_book.R")        # seconds
source("R/04_evaluate.R")               # ~20 seconds; grades the engine against the answer key
source("R/05_link_lawsuit_parties.R")   # ~45 seconds; links every lawsuit party to the firm, for the app
```

Rerun 05 whenever 03 or `config/extra_aliases.csv` changes, so the Lawsuits tab stays in step with the firm's records.

To check a single intake yourself:

```r
source("R/conflict_engine.R")
engine <- prepare_engine()
result <- check_intake(
  engine,
  tibble(party_name = c("Ridgeline Foods LLC", "Wal-Mart Stores Inc", "Wells Fargo Bank NA"),
         role = c("prospective_client", "adverse", "adverse")),
  practice_area = "Commercial Litigation"
)
result$outcome   # "high_risk", "medium_risk", "low_risk" or "clear"
result$hits      # one row per hit, worst first
```

Every API response is cached in `data/raw/`, so a stopped run picks up where it left off and reruns are instant. Delete `data/raw/` to refetch fresh data.

To track more companies, add rows to `config/seed_companies.csv`. `court_terms` holds the words to search for in case captions, separated by semicolons. The scripts read only this file, so nothing else needs to change.

For higher CourtListener rate limits, create a free account and set `COURTLISTENER_TOKEN` in your `.Renviron`.

## Tables

### Entity graph (real, GLEIF)

| File | Grain | Key columns |
|---|---|---|
| `families.csv` | One row per corporate family | `family_id` (ultimate parent's LEI), `seed_name`, `sector`, `member_count`, `tree_complete` |
| `entities.csv` | One row per legal entity | `lei`, `legal_name`, `norm_name`, `entity_status`, `jurisdiction`, `hq_city`, `family_id` |
| `entity_aliases.csv` | One row per name an entity has used | `lei`, `alias`, `alias_type` (legal, previous legal, trading name), `norm_alias` |
| `entity_relationships.csv` | One row per child-to-parent edge | `child_lei`, `parent_lei`, `relationship_type` |

`relationship_type` is `DIRECT_PARENT` when GLEIF reports the direct parent. It's `ULTIMATE_PARENT_ONLY` when we only know the top of the family, either because the direct parent has no LEI or because the family hit the 400-entity walk cap (`tree_complete = FALSE`).

### Litigation (real, CourtListener RECAP)

| File | Grain | Key columns |
|---|---|---|
| `matters.csv` | One row per federal docket | `docket_id`, `case_name`, `court_id`, `date_filed`, `nature_of_suit`, `caption_plaintiff_side`, `caption_defendant_side`, `query_type` (`family` or `pair`) |
| `matter_parties.csv` | One row per party on a docket | `docket_id`, `party_name`, `side`, `source` |
| `matter_counsel.csv` | One row per firm or attorney on a docket | `docket_id`, `name`, `counsel_type` |

`side` is inferred by finding the party's name in the caption ("A v. B"), so it's `unknown` when the caption doesn't show it. CourtListener doesn't say which firm represented which party, so counsel is listed per docket only.

### Fictional firm: Hartwell & Pierce LLP (synthetic, built on the real data)

| File | Grain | Key columns |
|---|---|---|
| `firm_attorneys.csv` | One row per attorney | `attorney_id`, `practice_group`, `is_lateral`, `prior_firm` |
| `firm_clients.csv` | One row per client | `client_id`, `lei`, `client_status` (`current` or `former`), `family_id` |
| `firm_matters.csv` | One row per firm matter | `matter_id`, `client_id`, `practice_area`, `status`, `source` (`courtlistener_docket` or `synthetic`) |
| `firm_matter_parties.csv` | One row per party on a firm matter | `matter_id`, `party_name`, `role` (`client`, `adverse`, `counterparty`), `party_lei`, `party_key` |
| `lateral_prior_clients.csv` | One row per client a lateral hire brought conflicts from | `attorney_id`, `client_lei`, `practice_area`, `representation_ended` |

About 45% of families have a current client and 20% a former one. Matters for a parent-company client mirror real dockets, with the real opposing parties as adverse parties.

### Test intakes and answer key

| File | Grain | Key columns |
|---|---|---|
| `intake_requests.csv` | One row per new-matter request | `intake_id`, `prospective_client`, `practice_area`, `scenario`, `expected_outcome` |
| `intake_parties.csv` | One row per party named on a request | `party_name_submitted` (as typed, with realistic variations and typos), `role`, `truth_lei`, `party_key` |
| `intake_expected_hits.csv` | One row per conflict hit an analyst should find | `hit_type`, `severity`, `detail`, `related_client_id`, `related_matter_id`, `recommended_action` |

The matching layer only gets `party_name_submitted`. `truth_lei` and `party_key` are the hidden answers.

`party_key` ties records about the same party together across tables: the LEI when known, otherwise `NAME:` plus the normalized name.

Hit types, from most to least serious:

| `hit_type` | Severity | Meaning |
|---|---|---|
| `current_client_adverse` | high | The adverse party is a current client (Rule 1.7) |
| `prospective_client_adverse_to_firm_client` | high | The prospective client is on the other side of an open firm matter |
| `former_client_substantially_related` | high | Former client, same practice area as the prior work (Rule 1.9) |
| `affiliate_of_current_client` | medium | Parent, subsidiary or sibling of a current client |
| `lateral_hire_prior_client` | medium | A newly hired attorney represented this party (Rule 1.10) |
| `former_client_unrelated` | low | Former client, unrelated work |
| `affiliate_of_former_client` | low | Affiliate of a former client |
| `business_relationship` | low | Counterparty on a client's deal |

Two scenario types should come back clear: `clean`, and `lookalike_name`, where the party only resembles a client (Merck KGaA versus Merck & Co.). They test whether the matcher avoids false alarms.

These labels simplify real ethics analysis. "Substantially related" is approximated by practice area, and affiliate conflicts depend on engagement terms the data doesn't have. They're a reasonable answer key for a prototype, not legal advice.

## The interface

A Shiny app (`app.R`) on top of the engine. Start it from R:

```r
shiny::runApp("C:/Users/evans/OneDrive/Desktop/R1/conflicts")
```

It needs `shiny`, `bslib`, `DT` and `visNetwork` on top of the packages above. It has four tabs:

- **Run a check:** enter a prospective client, a practice area and the adverse parties, one per line, or load one of the 135 test intakes. You get:
  - the outcome
  - every hit with its severity and recommended action
  - a map of how each hit connects to the firm's records
  - how each name was matched, including near misses, so you can see a lookalike was considered and ruled out
  - the answer key, when you loaded a test intake
- **Explore an entity:** search any company to see its corporate family (with the firm's clients marked), its relationships with the firm, its other names, and its federal lawsuits, with links to CourtListener.
- **Lawsuits:** search and filter the 904 federal cases by name or party, type of suit, court, year, company-v-company, or whether they touch the firm's clients. Open a case to see its parties by side, each party's tie to the firm, which side the firm could take without a conflict, and the counsel on the docket.
  - A party named by surname alone ("Stewart") is shown as a possible match to confirm, never a confirmed one.
  - Parties whose side the case name doesn't make clear are listed separately and left out of the conflict verdict.
  - CourtListener doesn't record which side each law firm represented, so counsel is listed for the whole case.
- **Firm records:** Hartwell & Pierce's clients, matters, attorneys and lateral hires.
- **Engine performance:** the latest evaluation results, with a button to re-run them.

The look (dark navy, gold, serif headings) lives in `www/styles.css`. The photos in `www/` are all CC0 (public domain) from Wikimedia Commons:
- `courtroom.jpg`: Beverley Guildhall's courtroom, used as the app background and the Engine performance banner
- `books.jpg`: "Leather bound books", the Run a check banner
- `courts.jpg`: the Royal Courts of Justice, London, the Explore an entity banner
- `columns.jpg`: the Library of Congress, the Firm records banner
- `session.jpg`: the U.S. Supreme Court in session, 1937, the Lawsuits banner (public domain)

No credit is required, but it's noted here anyway.

Shiny automatically runs every file in an app's `R/` folder at startup, which would rerun the data pipeline. `R/_disable_autoload.R` turns that off; don't delete it.

## The conflict engine

`R/conflict_engine.R` checks an intake in four steps:

1. **Match.** Each submitted name is normalized: accents stripped, "&" and "and" unified, "National Association" shortened to "NA", punctuation dropped, "J.P." collapsed to "JP", corporate suffixes set aside. It's then compared against about 3,400 known names: every GLEIF legal, former and trading name, the hand-kept names in `config/extra_aliases.csv`, and firm-matter parties that have no LEI. Three similarities feed one score from 0 to 1:
   - whole-name similarity (Jaro-Winkler)
   - shared words, counting near-identical words as shared, so "MERCK KGAA" scores low against "MERCK"
   - a space-free comparison that catches typos which move a space ("AT& TINC."), counted only when it's typo-close
2. **Decide.** 0.88 or above is treated as the same party. Between 0.75 and 0.88 is shown as a possible match for an analyst to confirm, and doesn't count toward the outcome. When several entities tie on the same name, the tie goes first to the one whose suffix also matches ("Ford Motor Company Limited" is the UK arm), then to the family parent ("PFIZER" means Pfizer Inc.), then to an exact legal name, then to an active entity.
3. **Check.** The matched party is compared against current and former clients, their corporate families (via the GLEIF tree), lateral hires' prior clients, deal counterparties and, for the prospective client, open matters where it's the adverse party.
4. **Score.** Each hit gets `risk_score = severity weight (high 1.0, medium 0.6, low 0.3) × match confidence`. The intake's outcome is the worst confirmed hit's severity.

### How it scores

`R/04_evaluate.R` runs every test intake through the engine and writes `data/eval/` (predicted hits, per-party matching, and an errors file to review). On the current test set:

| | Result |
|---|---|
| Parties identified correctly | 99% (322 of 325) |
| Hit precision / recall | 100% / 100% (108 hits) |
| Intake outcome correct | 100% (135 of 135) |
| False alarms on clean and lookalike intakes | 0 of 33 |

The matcher was tuned against this test set and a second one. A third set it never saw (a different random firm, clients and intakes) also scored 100% on hits and outcomes. To make a new set, change `set.seed(673)` in `03_build_firm_book.R`, then rerun 03 and 04.

Treat these numbers as a sanity check, not proof. The test names are generated variations of real names: dropped suffixes, case changes, one or two typos. Real intake forms are messier, with nicknames, abbreviations, names in the wrong order, and people who share names. The answer key also comes from the same simplified rules the engine applies, so it tests matching and lookup, not legal judgment.

### Names GLEIF doesn't know

No fuzzy matcher can link "BofA" to Bank of America or "Wal-Mart Stores" to Walmart Inc.; GLEIF never recorded Walmart's 2018 rename. Real conflicts teams keep their own alias lists for this, and `config/extra_aliases.csv` is that list here: one row per name, with the LEI it belongs to. It's read every time the engine starts, so a new row takes effect straight away. It currently covers former names (Wal-Mart Stores, Facebook, Google Inc., Tesla Motors) and brand names (BofA, Chase Bank, Verizon, JPMorgan Chase, ExxonMobil, UnitedHealthcare, Google). The brand names came from checking which names fail to link in real court filings: "Verizon" alone appears in 27 cases.

Names that carry a second name, like "Meta Platforms, Inc. fka Facebook, Inc." or "Walmart Inc. d/b/a Wal-Mart Stores", are split at "fka", "aka", "dba" or "formerly", and each part is matched. Short one-word names of people (5 letters or fewer, usually surnames) must match exactly, so "Gray" never matches "Gary"; registered companies keep their typo tolerance, so "Telsa" still finds Tesla.

## Known gaps in the real data

- **Not every company has an LEI.** Chevron Corporation has none: GLEIF records its subsidiaries' parent as `NO_LEI`, so the family can't be built, and ConocoPhillips stands in as the second energy company. A real conflicts system would fill gaps like this from SEC Exhibit 21 subsidiary lists.
- **Large banks are capped.** Bank of America and Citigroup have 750+ entities each. The walk stops at 400, and the rest link straight to the parent.
- **The family name search can pick the wrong company.** It once chose "Chevron Transport Corporation Ltd." for Chevron. Check `families.csv` after adding a seed, and pin the right LEI in the config's `lei` column if needed.

## Sources

- **GLEIF** (api.gleif.org): the global Legal Entity Identifier register, with Level 2 data on who owns whom. Free, open license, no account needed.
- **CourtListener** (courtlistener.com, Free Law Project): the RECAP archive of federal court dockets from PACER.
