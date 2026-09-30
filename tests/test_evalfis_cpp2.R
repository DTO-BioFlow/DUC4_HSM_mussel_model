#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# Regression test: evalfis_cpp2() (fuzzyfis package)
#   - against FuzzyR::evalfis()  = independent ground truth
#   - against the old evalfis_cpp() = regression baseline (same MF code, so it
#     is NOT an independent reference; see the "known issues" section)
#
# Dev-only script, not S3-synced, not baked into the Docker image. Hermetic:
# no S3, no network, no real data. Run with:
#   Rscript tests/test_evalfis_cpp2.R
# or inside the project image (R is not needed on the host):
#   docker run --rm --entrypoint Rscript -v "<repo>:/repo:ro" <image> /repo/tests/test_evalfis_cpp2.R
#
# Exit status 0 if every case is PASS or XFAIL, 1 otherwise (CI-usable).
#
# XFAIL = "expected failure": a documented, known defect that the suite
# asserts is still present (review R2-03). When the defect is fixed the XFAIL
# turns into a FAIL on purpose, so it must then be converted into a normal
# PASS case - a fix cannot go unnoticed and a known bug cannot be hidden.
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

suppressMessages(library(FuzzyR))
source(file.path(repo_root, "functions_WS.R"))

# The old evalfis_cpp is compiled lazily by install_legacy_evalfis_cpp()
# (functions_WS.R). That function calls cppFunction() whose `env` defaults to
# parent.frame(), so when called normally the compiled function stays in the
# installer's own frame and is discarded (review R2-01). Evaluating the
# installer's body in the global environment makes it land in the global
# environment, where this test needs it. The installer itself is left as is
# (it is dead code in production).
eval(body(install_legacy_evalfis_cpp), envir = globalenv())
stopifnot(exists("evalfis_cpp"))

# FuzzyR resolves orMethod/andMethod/... names with match.fun(); it ships no
# 'probor', so define the probabilistic-or t-conorm that evalfis_cpp2() calls
# 'probor' (bounded sum: 1 - prod(1 - x)).
probor <- function(x) 1 - prod(1 - x)

# ---------------------------------------------------------------------------
# Harness
# ---------------------------------------------------------------------------
results <- data.frame(case = character(), status = character(), detail = character(),
                      stringsAsFactors = FALSE)

record <- function(case, passed, detail = "") {
  status <- if (isTRUE(passed)) "PASS" else "FAIL"
  results[nrow(results) + 1, ] <<- list(case = case, status = status, detail = detail)
  cat(sprintf("[%s] %s%s\n", status, case, if (nzchar(detail)) paste0(" - ", detail) else ""))
}

# `bug_present` must be TRUE while the documented defect exists.
record_xfail <- function(case, bug_present, detail = "") {
  if (isTRUE(bug_present)) {
    status <- "XFAIL"
  } else {
    status <- "FAIL"
    detail <- paste0("expected failure no longer fails (defect fixed?) - convert to a normal PASS case. ", detail)
  }
  results[nrow(results) + 1, ] <<- list(case = case, status = status, detail = detail)
  cat(sprintf("[%s] %s%s\n", status, case, if (nzchar(detail)) paste0(" - ", detail) else ""))
}

close_enough <- function(a, b, tol = 1e-9) {
  if (length(a) != length(b)) return(FALSE)
  if (!identical(is.na(a), is.na(b))) return(FALSE)
  all(is.na(a) | abs(a - b) <= tol)
}

expect_error <- function(expr) {
  tryCatch({ force(expr); FALSE }, error = function(e) TRUE)
}

fmt <- function(v) paste(round(v, 4), collapse = ",")

# FuzzyR's evalfis() keeps state in .GlobalEnv and caches by FIS identity (also
# the output grid, i.e. point_n), so a stale cache could silently compare
# against the wrong grid. Clear it before every ground-truth evaluation. A
# FuzzyR failure is reported as a FAIL by the caller, not allowed to abort the
# whole script (this is the test's real boundary to an external package).
fuzzyr_ref <- function(inp, fis, point_n = 101) {
  if (exists("GLOBAL_FIS", envir = .GlobalEnv)) rm("GLOBAL_FIS", envir = .GlobalEnv)
  tryCatch(
    list(value = apply(inp, 1, function(x) evalfis(x, fis, point_n = point_n))),
    error = function(e) list(error = conditionMessage(e))
  )
}

# Parity of evalfis_cpp2(out_disc = N) with FuzzyR::evalfis(point_n = N): both
# discretize the output universe with the same N points, so results must agree
# to floating-point precision.
parity_vs_fuzzyr <- function(label, fis, inp, out_disc = 101, tol = 1e-9) {
  ref <- fuzzyr_ref(inp, fis, out_disc)
  if (!is.null(ref$error)) {
    record(label, FALSE, paste("FuzzyR ground truth failed:", ref$error))
    return(invisible(NULL))
  }
  new <- evalfis_cpp2(inp, fis, out_disc)
  ok <- close_enough(new, ref$value, tol)
  record(label, ok, if (!ok) sprintf("new=%s ref=%s", fmt(new), fmt(ref$value)) else "")
}

# ---------------------------------------------------------------------------
# Synthetic FIS builders (2 inputs, 1 output, 3 MFs each). Small on purpose so
# FuzzyR's row-by-row ground truth is fast. addvar/addmf argument shapes
# mirror build_fuzzy_logic_model_yearrc2's usage in functions_WS.R.
#
#   shoulders = FALSE ("interior"): every MF has a < b and c < d, and no MF
#     starts/ends exactly at a range end, so none of the known edge defects
#     (review R2-03) can influence the result -> exact parity with FuzzyR.
#   shoulders = TRUE  ("edge"): the shape the production model uses -
#     low = c(min,min,q1,q2) and high = c(q3,q4,max,max), plus output sets
#     touching 0 and 100. Exposes R2-03.
# ---------------------------------------------------------------------------
make_fis <- function(andMethod = "prod", orMethod = "max", impMethod = "min",
                     aggMethod = "max", defuzzMethod = "centroid",
                     include_or_rule = FALSE, shoulders = FALSE) {
  fis <- newfis('synth', fisType = "mamdani", mfType = "t1",
                andMethod = andMethod, orMethod = orMethod, impMethod = impMethod,
                aggMethod = aggMethod, defuzzMethod = defuzzMethod)

  in_low  <- if (shoulders) c(0, 0, 2, 4)   else c(-1, 0, 2, 4)
  in_high <- if (shoulders) c(6, 8, 10, 10) else c(6, 8, 10, 11)
  for (k in 1:2) {
    fis <- addvar(fis, 'input', paste0("x", k), c(0:10), method = NULL, params = NULL, firing.method = "tnorm.min.max")
    fis <- addmf(fis, 'input', k, 'low',  'trapmf', in_low)
    fis <- addmf(fis, 'input', k, 'med',  'trimf',  c(2, 5, 8))
    fis <- addmf(fis, 'input', k, 'high', 'trapmf', in_high)
  }

  out_low  <- if (shoulders) c(0, 0, 20, 40)    else c(-5, 5, 20, 40)
  out_high <- if (shoulders) c(60, 80, 100, 100) else c(60, 80, 95, 105)
  fis <- addvar(fis, 'output', "y", c(0:100), method = NULL, params = NULL, firing.method = "tnorm.min.max")
  fis <- addmf(fis, 'output', 1, 'low',  'trapmf', out_low)
  # 'med' is asymmetric on purpose: a symmetric set has centroid 50 at any
  # firing height, which would hide any change of and/or/imp/agg method.
  fis <- addmf(fis, 'output', 1, 'med',  'trimf',  c(20, 35, 80))
  fis <- addmf(fis, 'output', 1, 'high', 'trapmf', out_high)

  # rule columns: x1 MF, x2 MF, consequent MF, weight, connective (1=AND, 2=OR).
  # Rules 3, 4, 5 share consequent 'med' so aggMethod is actually exercised;
  # rule 3 has a "don't care" (0) antecedent.
  rules <- rbind(
    c(1, 1, 1, 1,   1),
    c(3, 3, 3, 1,   1),
    c(2, 0, 2, 1,   1),
    c(1, 3, 2, 0.5, 1),
    c(3, 1, 2, 0.7, 1)
  )
  if (include_or_rule) rules <- rbind(rules, c(1, 3, 2, 1, 2)) # x1=low OR x2=high -> med
  addrule(fis, rules)
}

# ---------------------------------------------------------------------------
# Input batteries
# ---------------------------------------------------------------------------
inp <- rbind(
  c(5, 5),      # mid-range, symmetric
  c(3, 7),      # partial memberships in two sets per input (sensitive to and/or/imp)
  c(2, 2),      # exact MF vertices
  c(4, 4),
  c(8, 8),
  c(-5, -5),    # below range: every MF is 0 -> NA
  c(15, 15),    # above range: every MF is 0 -> NA
  c(5, 1),      # "don't care" rule with a range of x2
  c(5, 9),
  c(1.5, 6.3),  # generic non-vertex values
  c(9.2, 0.7),
  c(-0.5, 10.5), # only the AND rule 'low & high' fires, both terms partial (and/imp sensitive)
  c(7, 1)        # two rules share consequent 'med' and neither dominates (agg sensitive)
)
edge_inp <- rbind(c(0, 0), c(10, 10), c(0, 10))  # exactly at the range ends

# ---------------------------------------------------------------------------
# Section A: parity with FuzzyR::evalfis() (ground truth), same 101-point grid
# ---------------------------------------------------------------------------
cat("\n--- A. Parity with FuzzyR::evalfis() (interior FIS, out_disc = point_n = 101) ---\n")
parity_vs_fuzzyr("default methods (prod/max/min/max)",     make_fis(), inp)
parity_vs_fuzzyr("andMethod='min'",                        make_fis(andMethod = "min"), inp)
parity_vs_fuzzyr("impMethod='prod'",                       make_fis(impMethod = "prod"), inp)
# (aggMethod='sum' is NOT here: it differs from FuzzyR, see section D, R2-18.)
parity_vs_fuzzyr("orMethod='max' with an OR rule",         make_fis(include_or_rule = TRUE), inp)
parity_vs_fuzzyr("orMethod='probor' with an OR rule",      make_fis(orMethod = "probor", include_or_rule = TRUE), inp)
parity_vs_fuzzyr("and='min' + imp='prod' + or='probor' combined (aggMethod stays 'max')",
                 make_fis(andMethod = "min", impMethod = "prod",
                          orMethod = "probor", include_or_rule = TRUE), inp)

# ---------------------------------------------------------------------------
# Section B: sensitivity guards. Parity alone would pass if a method were
# silently ignored by BOTH implementations; require that each non-default
# method really changes the output on this battery.
# ---------------------------------------------------------------------------
cat("\n--- B. Method sensitivity (a non-default method must change the output) ---\n")
base_out    <- evalfis_cpp2(inp, make_fis(), 101)
base_or_out <- evalfis_cpp2(inp, make_fis(include_or_rule = TRUE), 101)
differs <- function(a, b) any(abs(a - b) > 1e-6, na.rm = TRUE)
record("andMethod='min' changes the output",
       differs(evalfis_cpp2(inp, make_fis(andMethod = "min"), 101), base_out))
record("impMethod='prod' changes the output",
       differs(evalfis_cpp2(inp, make_fis(impMethod = "prod"), 101), base_out))
record("aggMethod='sum' changes the output",
       differs(evalfis_cpp2(inp, make_fis(aggMethod = "sum"), 101), base_out))
record("orMethod='probor' changes the output",
       differs(evalfis_cpp2(inp, make_fis(orMethod = "probor", include_or_rule = TRUE), 101), base_or_out))

# ---------------------------------------------------------------------------
# Section C: regression baseline against the old evalfis_cpp() on the
# production-style ("edge") FIS. Both share the same MF code, so this proves
# "no unintended change", not correctness. It is EXPECTED TO CHANGE in the
# planned edge/out-of-range fix (F2) - update it deliberately then.
# ---------------------------------------------------------------------------
cat("\n--- C. Baseline vs old evalfis_cpp() (edge FIS; expected to change in F2) ---\n")
edge_fis <- make_fis(shoulders = TRUE)
for (case in list(list("default battery", inp), list("inputs exactly at the range ends", edge_inp))) {
  new <- evalfis_cpp2(case[[2]], edge_fis, 301)
  old <- evalfis_cpp(case[[2]], edge_fis, 301)
  ok <- close_enough(new, old)
  record(sprintf("baseline vs old evalfis_cpp(): %s", case[[1]]), ok,
         if (!ok) sprintf("new=%s old=%s", fmt(new), fmt(old)) else "")
}

# ---------------------------------------------------------------------------
# Section D: known defects (review R2-03) - asserted as XFAIL until fixed (F2)
# ---------------------------------------------------------------------------
cat("\n--- D. Known defects, expected failures until fixed (review R2-03) ---\n")
ref_edge <- fuzzyr_ref(edge_inp, edge_fis, 101)
new_edge <- evalfis_cpp2(edge_inp, edge_fis, 101)
if (!is.null(ref_edge$error)) {
  record("XFAIL setup: FuzzyR on range-end inputs", FALSE, ref_edge$error)
} else {
  record_xfail("R2-03a: input exactly at a range end gets zero membership -> NA (FuzzyR: valid value)",
               all(is.na(new_edge)) && all(!is.na(ref_edge$value)),
               sprintf("new=%s FuzzyR=%s", fmt(new_edge), fmt(ref_edge$value)))
}
ref_bias <- fuzzyr_ref(inp, edge_fis, 101)
new_bias <- evalfis_cpp2(inp, edge_fis, 101)
if (!is.null(ref_bias$error)) {
  record("XFAIL setup: FuzzyR on edge FIS", FALSE, ref_bias$error)
} else {
  d <- max(abs(new_bias - ref_bias$value), na.rm = TRUE)
  record_xfail("R2-03b: output sets starting/ending at 0/100 are dropped at the universe ends -> centroid differs from FuzzyR",
               d > 1e-6, sprintf("max|diff| = %.4g", d))
}

# R2-18: aggMethod='sum' in evalfis_cpp2() is not equivalent to FuzzyR's.
#  (a) rules sharing a consequent set: evalfis_cpp2 combines the firing
#      degrees with a bounded sum (a + f - a*f) and THEN applies implication;
#      FuzzyR applies implication per rule and adds the resulting curves.
#  (b) different consequent sets: evalfis_cpp2 always unites them with max
#      (evalfis2.cpp, "Union across different consequent MFs"); FuzzyR applies
#      aggMethod across all rules. Hand check at (2,2): centroid with union=max
#      is 33.4879 (evalfis_cpp2), with union=sum 33.2966 (FuzzyR).
# Production uses aggMethod='max', where both agree exactly (section A).
{
  sum_fis <- make_fis(aggMethod = "sum")
  ref_sum <- fuzzyr_ref(inp, sum_fis, 101)
  if (!is.null(ref_sum$error)) {
    record("XFAIL setup: FuzzyR aggMethod='sum' (a)", FALSE, ref_sum$error)
  } else {
    new_sum <- evalfis_cpp2(inp, sum_fis, 101)
    d <- max(abs(new_sum - ref_sum$value), na.rm = TRUE)
    record_xfail("R2-18a: aggMethod='sum' with rules sharing a consequent differs from FuzzyR",
                 d > 1e-6, sprintf("max|diff| = %.4g", d))
  }
}
{
  agg_fis <- make_fis(aggMethod = "sum", include_or_rule = TRUE)
  agg_in <- rbind(c(2, 2)) # rule 1 fires 'low', the OR rule fires 'med'
  ref_agg <- fuzzyr_ref(agg_in, agg_fis, 101)
  if (!is.null(ref_agg$error)) {
    record("XFAIL setup: FuzzyR aggMethod='sum'", FALSE, ref_agg$error)
  } else {
    new_agg <- evalfis_cpp2(agg_in, agg_fis, 101)
    record_xfail("R2-18b: aggMethod='sum' with two different overlapping consequents differs from FuzzyR",
                 abs(new_agg - ref_agg$value) > 1e-6,
                 sprintf("evalfis_cpp2=%s FuzzyR=%s", fmt(new_agg), fmt(ref_agg$value)))
  }
}

# ---------------------------------------------------------------------------
# Section E: input validation and missing-data handling
# ---------------------------------------------------------------------------
cat("\n--- E. Validation and missing data ---\n")
row1 <- inp[1, , drop = FALSE]

bad_and <- make_fis(); bad_and$andMethod <- "bogus"
record("stop() on unsupported andMethod", expect_error(evalfis_cpp2(row1, bad_and)))

bad_or <- make_fis(); bad_or$orMethod <- "bogus"
record("stop() on unsupported orMethod", expect_error(evalfis_cpp2(row1, bad_or)))

bad_imp <- make_fis(); bad_imp$impMethod <- "bogus"
record("stop() on unsupported impMethod", expect_error(evalfis_cpp2(row1, bad_imp)))

bad_agg <- make_fis(); bad_agg$aggMethod <- "bogus"
record("stop() on unsupported aggMethod", expect_error(evalfis_cpp2(row1, bad_agg)))

bad_defuzz <- make_fis(); bad_defuzz$defuzzMethod <- "bisector"
record("stop() on unsupported defuzzMethod ('bisector')", expect_error(evalfis_cpp2(row1, bad_defuzz)))

bad_mf <- make_fis(); bad_mf$input[[1]]$mf[[1]]$type <- "sigmf"
record("stop() on unsupported MF type", expect_error(evalfis_cpp2(row1, bad_mf)))

# x1 has 3 MFs; a rule referencing index 4 is malformed and must be rejected up
# front, not cause an out-of-bounds access in the per-cell loop.
bad_rule_index <- make_fis(); bad_rule_index$rule[1, 1] <- 4
record("stop() on rule referencing out-of-bounds antecedent MF index",
       expect_error(evalfis_cpp2(row1, bad_rule_index)))

# A model with fewer/more inputs than raster columns must fail loudly (this is
# also what happens if PARAMS 'parameters' selects a subset, review R2-05).
record("stop() when the number of input columns does not match the FIS",
       expect_error(evalfis_cpp2(cbind(row1, 5), make_fis())))

# na_sentinel (-9999) must be a non-match by explicit value, not only because
# it happens to fall outside a variable's MF domain: widen x1's 'low' shoulder
# far below the sentinel (what a PARAMS range_<code> override does) and
# confirm the sentinel is still excluded.
{
  fis <- make_fis()
  fis$input[[1]]$mf[[1]]$params <- c(-20000, -20000, 2, 4)
  out <- evalfis_cpp2(rbind(c(-9999, 1)), fis, 301)
  record("na_sentinel treated as non-match even inside a widened MF domain", is.na(out[1]),
         sprintf("expected NA (no rule may fire), got %s", fmt(out)))
}

# NaN / NA_real_ in an input column (the pipeline converts NA to -9999 first,
# but evalfis_cpp2 is also an exported API). In a variable that EVERY rule
# constrains (x1 here, and all 9 variables in the production FIS) the result
# must be NA:
{
  out <- evalfis_cpp2(rbind(c(NaN, 5), c(NA_real_, 5)), make_fis(), 101)
  record("NaN/NA in a variable every rule constrains gives NA", all(is.na(out)), sprintf("got %s", fmt(out)))
}
# ...but in a variable only some rules constrain (rule 3 has a "don't care" on
# x2) the rules that do constrain it are silently dropped and the remaining
# rules still produce a value (review R2-16). Not reachable with the
# production FIS (it has no "don't care" entries).
{
  out <- evalfis_cpp2(rbind(c(5, NaN)), make_fis(), 101)
  record_xfail("R2-16: NaN in a partly-constrained variable should give NA but yields a value from the other rules",
               !is.na(out[1]), sprintf("got %s", fmt(out)))
}

# ---------------------------------------------------------------------------
# Section F: the production model builder (build_fuzzy_logic_model_yearrc2)
# with a synthetic response-curve list (the real rc_list_year.rds is not in
# the repo). Breakpoints are copied from the project's rc_list_year.rds as of
# 2026-09-24 so the FIS has the production shape. NOTE: the builder reads the
# GLOBALS `parameters` and `rc_list` (review R2-04), so they are defined here
# in the global environment on purpose.
# ---------------------------------------------------------------------------
cat("\n--- F. Production builder build_fuzzy_logic_model_yearrc2() ---\n")
rc_list <- list(
  sst           = list(q = c(9.61, 9.97, 12.46, 13.22)),
  sss           = list(q = c(7.49, 12.24, 34.4, 35.33)),
  oxy           = list(q = c(8, 8, 11.07, 11.5)),
  substrate     = list(q = c(0.104, 0.166, 0.377)),
  sedimentation = list(q = c(-0.2, 0.2, 0.6)),
  current_speed = list(q = c(0.001, 0.019, 0.129, 0.999)),
  orb_vel       = list(q = c(0.097, 0.256, 0.417)),
  PP            = list(q = c(0.91, 1.5, 27.3, 28)),
  shear         = list(q = c(0.123, 0.355, 0.794))
)
parameters <- c("temp", "sal", "oxy", "sub", "sed", "cur", "orb", "chl", "shear")
prod_fis <- build_fuzzy_logic_model_yearrc2(parameters, NULL)

record("builder: 9 input variables, 3 MFs each",
       length(prod_fis$input) == 9 && all(vapply(prod_fis$input, function(v) length(v$mf), 1L) == 3L))
record("builder: 4 output MFs", length(prod_fis$output[[1]]$mf) == 4L)
record("builder: full rule grid (3^9 = 19683 rules)", nrow(prod_fis$rule) == 3^9,
       sprintf("got %d", nrow(prod_fis$rule)))
resp <- table(prod_fis$rule[, 10])
record("builder: rule response classes optimal/good/okay/bad = 1/162/2688/16832 (default cutoffs)",
       identical(as.integer(resp), c(1L, 162L, 2688L, 16832L)) && identical(names(resp), c("1", "2", "3", "4")),
       paste(names(resp), as.integer(resp), sep = ":", collapse = " "))

# Cells (one column per input, in FIS order temp,sal,oxy,sub,sed,cur,orb,chl,shear):
# all inputs at the optimal plateau/peak; a mixed cell in the ramps; a cell
# missing one layer (-9999 = NA sentinel) -> NA because every rule constrains
# every variable.
prod_cells <- rbind(
  optimal = c(11,   20,   9.5,  0.166, 0.2,  0.05, 0.256, 10,  0.355),
  mixed   = c(9.8,  10,   8.5,  0.13,  0.0,  0.01, 0.20,  1.2, 0.20),
  missing = c(11,   20,   9.5,  0.166, 0.2,  0.05, 0.256, 10,  -9999)
)
new_prod <- evalfis_cpp2(prod_cells, prod_fis, 301)
old_prod <- evalfis_cpp(prod_cells, prod_fis, 301)
record("builder FIS: baseline vs old evalfis_cpp() (expected to change in F2)", close_enough(new_prod, old_prod),
       sprintf("new=%s old=%s", fmt(new_prod), fmt(old_prod)))
record("builder FIS: cell missing one layer gives NA, complete cells give a value",
       is.na(new_prod[3]) && !any(is.na(new_prod[1:2])), sprintf("got %s", fmt(new_prod)))
nan_cell <- prod_cells[1, , drop = FALSE]; nan_cell[1, 9] <- NaN
record("builder FIS: NaN in one layer gives NA (every rule constrains every variable)",
       is.na(evalfis_cpp2(nan_cell, prod_fis, 301)[1]))
record("builder FIS: all-optimal cell scores higher than the mixed cell",
       new_prod[1] > new_prod[2], sprintf("optimal=%s mixed=%s", fmt(new_prod[1]), fmt(new_prod[2])))

# Ground truth for the production FIS at its own grid (interior cells only).
# The production output sets touch 0 and 100, so this also shows R2-03b.
ref_prod <- fuzzyr_ref(prod_cells[1:2, , drop = FALSE], prod_fis, 101)
if (!is.null(ref_prod$error)) {
  record("XFAIL setup: FuzzyR on the production FIS", FALSE, ref_prod$error)
} else {
  new101 <- evalfis_cpp2(prod_cells[1:2, , drop = FALSE], prod_fis, 101)
  d <- max(abs(new101 - ref_prod$value))
  record_xfail("R2-03b on the production FIS: centroid differs from FuzzyR (output sets touch 0/100)",
               d > 1e-6, sprintf("max|diff| = %.4g", d))
}

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------
cat("\n=== SUMMARY ===\n")
print(table(factor(results$status, levels = c("PASS", "XFAIL", "FAIL"))))
if (any(results$status == "FAIL")) {
  cat("\nFAILED cases:\n")
  print(results[results$status == "FAIL", c("case", "detail")], row.names = FALSE)
}
cat(sprintf("\n%d PASS, %d XFAIL (known defects), %d FAIL, %d cases total.\n",
            sum(results$status == "PASS"), sum(results$status == "XFAIL"),
            sum(results$status == "FAIL"), nrow(results)))
quit(status = if (any(results$status == "FAIL")) 1L else 0L, save = "no")
