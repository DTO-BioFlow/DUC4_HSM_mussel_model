#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# Regression test: evalfis_cpp2() (fuzzyfis package) vs. FuzzyR::evalfis()
# (ground truth) and the old evalfis_cpp() (trusted production baseline).
#
# Dev-only script, not S3-synced, not baked into the Docker image. Run with:
#   Rscript tests/test_evalfis_cpp2.R
#
# Exits with status 0 if all cases pass, 1 otherwise (CI-usable).
# ---------------------------------------------------------------------------

# Resolve repo root robustly whether invoked via `Rscript tests/x.R` (relative
# to cwd) or from elsewhere.
this_file <- tryCatch({
  args <- commandArgs(trailingOnly = FALSE)
  fpath <- sub("^--file=", "", args[grepl("^--file=", args)])
  if (length(fpath) == 1) normalizePath(fpath) else NA_character_
}, error = function(e) NA_character_)
if (!is.na(this_file)) {
  repo_root <- normalizePath(file.path(dirname(this_file), ".."))
} else {
  repo_root <- normalizePath(".")
}

# ---------------------------------------------------------------------------
# Load the code under test: prefer the installed fuzzyfis package (production
# path); fall back to Rcpp::sourceCpp() against the raw .cpp for fast local
# iteration before the package/Docker image is (re)built.
# ---------------------------------------------------------------------------
if (requireNamespace("fuzzyfis", quietly = TRUE)) {
  library(fuzzyfis)
  cat(">>> Using installed fuzzyfis package\n")
} else {
  message(">>> fuzzyfis not installed - falling back to Rcpp::sourceCpp() for local iteration")
  library(Rcpp)
  sourceCpp(file.path(repo_root, "pkg", "fuzzyfis", "src", "evalfis2.cpp"))
}

library(FuzzyR)
source(file.path(repo_root, "functions_WS.R"))   # brings in the OLD evalfis_cpp (cppFunction) as trusted baseline

# ---------------------------------------------------------------------------
# Harness
# ---------------------------------------------------------------------------
results <- data.frame(case = character(), passed = logical(), detail = character(), stringsAsFactors = FALSE)

record <- function(case, passed, detail = "") {
  results[nrow(results) + 1, ] <<- list(case = case, passed = passed, detail = detail)
  cat(sprintf("[%s] %s%s\n", if (passed) "PASS" else "FAIL", case,
              if (nzchar(detail)) paste0(" - ", detail) else ""))
}

close_enough <- function(a, b, tol = 1e-6) {
  if (length(a) != length(b)) return(FALSE)
  na_a <- is.na(a); na_b <- is.na(b)
  if (!identical(na_a, na_b)) return(FALSE)
  ok <- na_a | (abs(a - b) <= tol)
  all(ok)
}

expect_error <- function(expr) {
  ok <- tryCatch({ force(expr); FALSE }, error = function(e) TRUE)
  ok
}

# ---------------------------------------------------------------------------
# Synthetic FIS builder: 2 inputs (x1, x2), 1 output (y), 3 MFs each.
# Deliberately small so FuzzyR::evalfis() ground truth runs fast row-by-row.
# addvar/addmf argument shapes mirror build_fuzzy_logic_model_yearrc's proven
# usage in functions_WS.R exactly (positional args, explicit method/params/
# firing.method on addvar).
# ---------------------------------------------------------------------------
make_synthetic_fis <- function(andMethod = "prod", orMethod = "max",
                                impMethod = "min", aggMethod = "max",
                                defuzzMethod = "centroid", include_or_rule = FALSE) {
  fis <- newfis(
    'synth',
    fisType = "mamdani",
    mfType = "t1",
    andMethod = andMethod,
    orMethod = orMethod,
    impMethod = impMethod,
    aggMethod = aggMethod,
    defuzzMethod = defuzzMethod
  )

  fis <- addvar(fis, 'input', "x1", c(0:10), method = NULL, params = NULL, firing.method = "tnorm.min.max")
  fis <- addmf(fis, 'input', 1, 'low',  'trapmf', c(0, 0, 2, 4))
  fis <- addmf(fis, 'input', 1, 'med',  'trimf',  c(2, 5, 8))
  fis <- addmf(fis, 'input', 1, 'high', 'trapmf', c(6, 8, 10, 10))

  fis <- addvar(fis, 'input', "x2", c(0:10), method = NULL, params = NULL, firing.method = "tnorm.min.max")
  fis <- addmf(fis, 'input', 2, 'low',  'trapmf', c(0, 0, 2, 4))
  fis <- addmf(fis, 'input', 2, 'med',  'trimf',  c(2, 5, 8))
  fis <- addmf(fis, 'input', 2, 'high', 'trapmf', c(6, 8, 10, 10))

  fis <- addvar(fis, 'output', "y", c(0:100), method = NULL, params = NULL, firing.method = "tnorm.min.max")
  fis <- addmf(fis, 'output', 1, 'low',  'trapmf', c(0, 0, 20, 40))
  fis <- addmf(fis, 'output', 1, 'med',  'trimf',  c(20, 50, 80))
  fis <- addmf(fis, 'output', 1, 'high', 'trapmf', c(60, 80, 100, 100))

  rules <- rbind(
    c(1, 1, 1, 1, 1),   # x1=low  AND x2=low  -> y=low,  weight 1, AND
    c(3, 3, 3, 1, 1),   # x1=high AND x2=high -> y=high, weight 1, AND
    c(2, 0, 2, 1, 1),   # x1=med  AND x2=don't care -> y=med, weight 1, AND
    c(1, 3, 2, 0.5, 1)  # x1=low  AND x2=high -> y=med, weight 0.5, AND
  )
  if (include_or_rule) {
    rules <- rbind(rules, c(1, 3, 2, 1, 2))  # x1=low OR x2=high -> y=med, weight 1, OR
  }
  fis <- addrule(fis, rules)
  fis
}

# A second FIS with two rules sharing the same consequent MF, to actually
# exercise per-rule aggregation (aggMethod). addrule() sets the FIS's whole
# rule matrix rather than appending, so this is built as its own FIS (with
# the same variables/MFs as make_synthetic_fis) with the full desired rule
# set passed in one addrule() call.
make_agg_fis <- function(aggMethod) {
  fis <- newfis('synth_agg', fisType = "mamdani", mfType = "t1",
                andMethod = "prod", orMethod = "max", impMethod = "min",
                aggMethod = aggMethod, defuzzMethod = "centroid")

  fis <- addvar(fis, 'input', "x1", c(0:10), method = NULL, params = NULL, firing.method = "tnorm.min.max")
  fis <- addmf(fis, 'input', 1, 'low',  'trapmf', c(0, 0, 2, 4))
  fis <- addmf(fis, 'input', 1, 'med',  'trimf',  c(2, 5, 8))
  fis <- addmf(fis, 'input', 1, 'high', 'trapmf', c(6, 8, 10, 10))

  fis <- addvar(fis, 'input', "x2", c(0:10), method = NULL, params = NULL, firing.method = "tnorm.min.max")
  fis <- addmf(fis, 'input', 2, 'low',  'trapmf', c(0, 0, 2, 4))
  fis <- addmf(fis, 'input', 2, 'med',  'trimf',  c(2, 5, 8))
  fis <- addmf(fis, 'input', 2, 'high', 'trapmf', c(6, 8, 10, 10))

  fis <- addvar(fis, 'output', "y", c(0:100), method = NULL, params = NULL, firing.method = "tnorm.min.max")
  fis <- addmf(fis, 'output', 1, 'low',  'trapmf', c(0, 0, 20, 40))
  fis <- addmf(fis, 'output', 1, 'med',  'trimf',  c(20, 50, 80))
  fis <- addmf(fis, 'output', 1, 'high', 'trapmf', c(60, 80, 100, 100))

  rules <- rbind(
    c(1, 1, 1, 1, 1),   # x1=low  AND x2=low  -> y=low
    c(3, 3, 3, 1, 1),   # x1=high AND x2=high -> y=high
    c(2, 0, 2, 1, 1),   # x1=med  AND x2=don't care -> y=med
    c(3, 1, 2, 0.7, 1)  # x1=high AND x2=low  -> y=med (shares consequent with rule 3)
  )
  addrule(fis, rules)
}

# ---------------------------------------------------------------------------
# Test input battery
# ---------------------------------------------------------------------------
default_inputs <- rbind(
  c(5, 5),    # normal mid-range
  c(3, 7),    # normal mid-range
  c(2, 2),    # exact trapmf shoulder
  c(4, 4),    # exact trapmf shoulder
  c(8, 8),    # exact trapmf shoulder
  c(-5, -5),  # out-of-range (below), MFs naturally return 0
  c(15, 15),  # out-of-range (above), MFs naturally return 0
  c(5, 1),    # exercises "don't care" rule (x1=med) with a range of x2
  c(5, 9)     # exercises "don't care" rule (x1=med) with a range of x2
)

or_inputs <- rbind(
  c(1, 5),    # x1 low only
  c(5, 9),    # x2 high only
  c(1, 9),    # both
  c(5, 5)     # neither
)

# ---------------------------------------------------------------------------
# Case 1: default-method parity (today's production config) vs. FuzzyR and
# the old evalfis_cpp(), across the full input battery.
# ---------------------------------------------------------------------------
{
  fis <- make_synthetic_fis()
  ref <- apply(default_inputs, 1, function(x) evalfis(x, fis))
  new_out <- evalfis_cpp2(default_inputs, fis, out_disc = 301)
  old_out <- evalfis_cpp(default_inputs, fis, out_disc = 301)

  ok_vs_ref <- close_enough(new_out, ref)
  ok_vs_old <- close_enough(new_out, old_out)
  record("default-method parity vs FuzzyR::evalfis()", ok_vs_ref,
         if (!ok_vs_ref) sprintf("new=%s ref=%s", paste(round(new_out, 3), collapse = ","), paste(round(ref, 3), collapse = ",")) else "")
  record("default-method parity vs old evalfis_cpp()", ok_vs_old,
         if (!ok_vs_old) sprintf("new=%s old=%s", paste(round(new_out, 3), collapse = ","), paste(round(old_out, 3), collapse = ",")) else "")
}

# ---------------------------------------------------------------------------
# Case 2: andMethod = "min"
# ---------------------------------------------------------------------------
{
  fis <- make_synthetic_fis(andMethod = "min")
  ref <- apply(default_inputs, 1, function(x) evalfis(x, fis))
  new_out <- evalfis_cpp2(default_inputs, fis, out_disc = 301)
  ok <- close_enough(new_out, ref)
  record("andMethod='min' vs FuzzyR::evalfis()", ok,
         if (!ok) sprintf("new=%s ref=%s", paste(round(new_out, 3), collapse = ","), paste(round(ref, 3), collapse = ",")) else "")
}

# ---------------------------------------------------------------------------
# Case 3: impMethod = "prod"
# ---------------------------------------------------------------------------
{
  fis <- make_synthetic_fis(impMethod = "prod")
  ref <- apply(default_inputs, 1, function(x) evalfis(x, fis))
  new_out <- evalfis_cpp2(default_inputs, fis, out_disc = 301)
  ok <- close_enough(new_out, ref)
  record("impMethod='prod' vs FuzzyR::evalfis()", ok,
         if (!ok) sprintf("new=%s ref=%s", paste(round(new_out, 3), collapse = ","), paste(round(ref, 3), collapse = ",")) else "")
}

# ---------------------------------------------------------------------------
# Case 4: aggMethod = "sum", with two rules sharing a consequent MF
# ---------------------------------------------------------------------------
{
  fis <- make_agg_fis(aggMethod = "sum")
  ref <- apply(default_inputs, 1, function(x) evalfis(x, fis))
  new_out <- evalfis_cpp2(default_inputs, fis, out_disc = 301)
  ok <- close_enough(new_out, ref)
  record("aggMethod='sum' (shared consequent) vs FuzzyR::evalfis()", ok,
         if (!ok) sprintf("new=%s ref=%s", paste(round(new_out, 3), collapse = ","), paste(round(ref, 3), collapse = ",")) else "")
}

# ---------------------------------------------------------------------------
# Case 5: orMethod = "max" and "probor", exercising an OR-connective rule
# ---------------------------------------------------------------------------
for (om in c("max", "probor")) {
  fis <- make_synthetic_fis(orMethod = om, include_or_rule = TRUE)
  ref <- apply(or_inputs, 1, function(x) evalfis(x, fis))
  new_out <- evalfis_cpp2(or_inputs, fis, out_disc = 301)
  ok <- close_enough(new_out, ref)
  record(sprintf("orMethod='%s' vs FuzzyR::evalfis()", om), ok,
         if (!ok) sprintf("new=%s ref=%s", paste(round(new_out, 3), collapse = ","), paste(round(ref, 3), collapse = ",")) else "")
}

# ---------------------------------------------------------------------------
# Case 6: unsupported-method validation (each must raise an error)
# ---------------------------------------------------------------------------
{
  bad_and <- make_synthetic_fis(); bad_and$andMethod <- "bogus"
  record("stop() on unsupported andMethod",
         expect_error(evalfis_cpp2(default_inputs[1, , drop = FALSE], bad_and)))

  bad_or <- make_synthetic_fis(); bad_or$orMethod <- "bogus"
  record("stop() on unsupported orMethod",
         expect_error(evalfis_cpp2(default_inputs[1, , drop = FALSE], bad_or)))

  bad_imp <- make_synthetic_fis(); bad_imp$impMethod <- "bogus"
  record("stop() on unsupported impMethod",
         expect_error(evalfis_cpp2(default_inputs[1, , drop = FALSE], bad_imp)))

  bad_agg <- make_synthetic_fis(); bad_agg$aggMethod <- "bogus"
  record("stop() on unsupported aggMethod",
         expect_error(evalfis_cpp2(default_inputs[1, , drop = FALSE], bad_agg)))

  bad_defuzz <- make_synthetic_fis(); bad_defuzz$defuzzMethod <- "bisector"
  record("stop() on unsupported defuzzMethod ('bisector')",
         expect_error(evalfis_cpp2(default_inputs[1, , drop = FALSE], bad_defuzz)))

  bad_mf <- make_synthetic_fis()
  bad_mf$input[[1]]$mf[[1]]$type <- "sigmf"
  record("stop() on unsupported MF type",
         expect_error(evalfis_cpp2(default_inputs[1, , drop = FALSE], bad_mf)))
}

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------
cat("\n=== SUMMARY ===\n")
print(results)

all_passed <- all(results$passed)
cat(sprintf("\n%d/%d cases passed.\n", sum(results$passed), nrow(results)))
quit(status = if (all_passed) 0L else 1L, save = "no")
