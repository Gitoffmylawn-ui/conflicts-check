# Runs the conflict engine on every test intake and grades it against the
# answer key from 03_build_firm_book.R.
#
# The engine only sees what an analyst would: the names as submitted, their
# roles and the practice area. truth_lei and party_key stay hidden.
#
# Output (data/eval/):
#   predicted_hits.csv    every hit the engine raised
#   party_matching.csv    per party: who the engine thought it was vs. the truth
#   errors.csv            missed hits and false alarms, for review

source("R/conflict_engine.R")
dir.create("data/eval", showWarnings = FALSE)

rd <- function(name) read_csv(file.path("data", paste0(name, ".csv")), col_types = cols(.default = "c"))
intakes <- rd("intake_requests")
parties <- rd("intake_parties")
expected <- rd("intake_expected_hits")

engine <- prepare_engine()
message(sprintf("Checking %d intakes against %s indexed names", nrow(intakes), format(nrow(engine$index), big.mark = ",")))

results <- map(seq_len(nrow(intakes)), \(i) {
  it <- intakes[i, ]
  p <- filter(parties, intake_id == it$intake_id) |> transmute(party_name = party_name_submitted, role)
  r <- check_intake(engine, p, it$practice_area)
  list(hits = mutate(r$hits, intake_id = it$intake_id, .before = 1),
       outcome = tibble(intake_id = it$intake_id, predicted_outcome = r$outcome, needs_review = r$needs_review))
})
predicted <- map_dfr(results, "hits")
outcomes <- map_dfr(results, "outcome") |> left_join(select(intakes, intake_id, scenario, expected_outcome), by = "intake_id")

# ---- Name matching ----------------------------------------------------------------
# Correct if the top match is the true party, or, for a party the firm has no
# record of, if nothing reached the accept threshold.

top_match <- parties |>
  mutate(top = map(party_name_submitted, \(n) head(match_name(engine, n), 1))) |>
  mutate(
    matched_key = map_chr(top, \(t) if (nrow(t)) t$key else NA_character_),
    matched_name = map_chr(top, \(t) if (nrow(t)) t$name else NA_character_),
    match_score = map_dbl(top, \(t) if (nrow(t)) t$score else 0),
    known = party_key %in% engine$index$key,
    accepted = match_score >= match_thresholds[["accept"]],
    correct = if_else(known, accepted & matched_key == party_key, !accepted)
  ) |>
  select(-top)

# ---- Hits -----------------------------------------------------------------------
key_cols <- c("intake_id", "party_name_submitted", "hit_type")
confirmed <- filter(predicted, match_status == "match") |> distinct(across(all_of(key_cols)), .keep_all = TRUE)
exp_keys <- distinct(expected, across(all_of(key_cols)), .keep_all = TRUE)

tp <- semi_join(confirmed, exp_keys, by = key_cols)
fp <- anti_join(confirmed, exp_keys, by = key_cols)
fn <- anti_join(exp_keys, confirmed, by = key_cols)
in_review <- semi_join(fn, filter(predicted, match_status == "review"), by = key_cols)

by_type <- full_join(
  count(exp_keys, hit_type, name = "expected"),
  count(confirmed, hit_type, name = "found"), by = "hit_type"
) |>
  left_join(count(tp, hit_type, name = "correct"), by = "hit_type") |>
  mutate(across(c(expected, found, correct), \(x) coalesce(x, 0L)),
         precision = round(correct / found, 2), recall = round(correct / expected, 2)) |>
  arrange(desc(expected))

# ---- Report -----------------------------------------------------------------------
pct <- function(x) sprintf("%.0f%%", 100 * x)
cat("\n== Name matching ==\n")
cat(sprintf("Parties identified correctly: %d of %d (%s)\n", sum(top_match$correct), nrow(top_match), pct(mean(top_match$correct))))
cat(sprintf("  known to the firm's records: %s   unknown (should not match): %s\n",
            pct(mean(top_match$correct[top_match$known])), pct(mean(top_match$correct[!top_match$known]))))

cat("\n== Conflict hits ==\n")
cat(sprintf("Precision %s (%d of %d raised hits were right)   Recall %s (%d of %d expected hits found)\n",
            pct(nrow(tp) / nrow(confirmed)), nrow(tp), nrow(confirmed),
            pct(nrow(tp) / nrow(exp_keys)), nrow(tp), nrow(exp_keys)))
cat(sprintf("%d of the %d missed hits were raised as possible matches for review\n", nrow(in_review), nrow(fn)))
print(as.data.frame(by_type), row.names = FALSE)

cat("\n== Intake outcomes (rows: expected, columns: predicted) ==\n")
levels <- c("high_risk", "medium_risk", "low_risk", "clear")
print(table(expected = factor(outcomes$expected_outcome, levels), predicted = factor(outcomes$predicted_outcome, levels)))
cat(sprintf("Outcome correct for %d of %d intakes (%s); %d flagged for review\n",
            sum(outcomes$expected_outcome == outcomes$predicted_outcome), nrow(outcomes),
            pct(mean(outcomes$expected_outcome == outcomes$predicted_outcome)), sum(outcomes$needs_review)))

write_out_eval <- function(df, name) write_csv(df, file.path("data", "eval", paste0(name, ".csv")), na = "")
write_out_eval(predicted, "predicted_hits")
write_out_eval(top_match, "party_matching")
write_out_eval(outcomes, "intake_outcomes")
write_out_eval(by_type, "hits_by_type")
write_out_eval(tibble(
  metric = c("parties_correct", "parties_total", "hits_correct", "hits_raised", "hits_expected",
             "outcomes_correct", "intakes_total", "flagged_for_review", "evaluated_at"),
  value = c(sum(top_match$correct), nrow(top_match), nrow(tp), nrow(confirmed), nrow(exp_keys),
            sum(outcomes$expected_outcome == outcomes$predicted_outcome), nrow(outcomes),
            sum(outcomes$needs_review), format(Sys.time(), "%Y-%m-%d %H:%M"))
), "summary")
write_out_eval(bind_rows(
  mutate(fn, error = if_else(paste(intake_id, party_name_submitted, hit_type) %in%
                               paste(in_review$intake_id, in_review$party_name_submitted, in_review$hit_type),
                             "missed (raised for review)", "missed")),
  mutate(fp, error = "false alarm")
) |> relocate(error), "errors")
message("Details in data/eval/")
