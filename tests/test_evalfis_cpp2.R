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
  # unname() both sides: evalfis_cpp2() always returns an unnamed vector,
  # but a FuzzyR reference computed via apply() over a row-named input
  # matrix (e.g. prod_cells, which is named for readability) carries those
  # rownames into its result - identical() on is.na(a) vs is.na(b) then
  # compares names too and spuriously reports "not equal" even when every
  # value matches exactly. Only the values, not the names, define parity.
  a <- unname(a); b <- unname(b)
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
  # evalfis_cpp2() clamps a real input value to the variable's declared
  # range before evaluating it (review R2-03). The "interior" (shoulders =
  # FALSE) MFs deliberately extend slightly past [0,10] (in_low/in_high
  # above) so a value can sit inside an MF's own ramp without being exactly
  # at a shoulder; its declared range must therefore be wide enough to
  # bracket that ramp (and this file's whole input battery, out to +-5) or
  # the clamp would silently reshape those ramps. The "edge" (shoulders =
  # TRUE) FIS keeps range = [0,10] deliberately: that is what makes its
  # low/high MFs degenerate AT the range bound, matching how
  # build_fuzzy_logic_model_yearrc2 always builds them in production.
  in_bounds <- if (shoulders) c(0, 10) else c(-20, 20)
  for (k in 1:2) {
    fis <- addvar(fis, 'input', paste0("x", k), in_bounds, method = NULL, params = NULL, firing.method = "tnorm.min.max")
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
# production-style ("edge") FIS, restricted to inputs that do NOT touch a
# degenerate MF shoulder or fall outside a variable's range. Both
# implementations share the same interior MF math, so equality here proves
# "no unintended change" away from the F2 fix's actual target (correctness
# AT the edges is checked directly against FuzzyR in section D below).
# ---------------------------------------------------------------------------
cat("\n--- C. Baseline vs old evalfis_cpp() (edge FIS, interior inputs only) ---\n")
edge_fis <- make_fis(shoulders = TRUE)
interior_inp <- rbind(c(5, 5), c(3, 7), c(2, 2), c(4, 4), c(8, 8), c(5, 1), c(5, 9), c(1.5, 6.3), c(9.2, 0.7))
{
  # edge_fis's OUTPUT sets ('low'/'high') are always degenerate at 0/100 (the
  # same production convention as its input shoulders), so R2-03b's fix
  # changes the discretized centroid a little for basically any row through
  # it - "interior" input (no edge, in range) does not exempt a row from
  # that, it only rules out the INPUT-side half of the fix (R2-03a) and the
  # clamp (D3). So a difference from the old evaluator here is expected;
  # what must hold is that both sides still produce a valid, plausible
  # value (no NA introduced, nothing wildly off) - correctness itself is
  # checked against FuzzyR in section D (R2-03b fixed / interior_inp).
  new <- evalfis_cpp2(interior_inp, edge_fis, 301)
  old <- evalfis_cpp(interior_inp, edge_fis, 301)
  record("interior battery through edge_fis: both valid, differ slightly from old (R2-03b's output-side fix, not edge/out-of-range-specific)",
         !any(is.na(new)) && !any(is.na(old)) && !close_enough(new, old),
         sprintf("new=%s old=%s", fmt(new), fmt(old)))
}
{
  # At range-end/out-of-range inputs the F2 fix DELIBERATELY makes
  # evalfis_cpp2 differ from the old, unfixed evalfis_cpp (which is left as
  # is - review triage - and still returns NA there). This is not a
  # regression: it is the fix. Correctness of the new value is checked
  # against FuzzyR in section D.
  new <- evalfis_cpp2(edge_inp, edge_fis, 301)
  old <- evalfis_cpp(edge_inp, edge_fis, 301)
  record("range-end inputs now differ from old evalfis_cpp() (fix is active) and old is still all-NA there",
         !close_enough(new, old) && all(is.na(old)),
         sprintf("new=%s old=%s", fmt(new), fmt(old)))
}

# ---------------------------------------------------------------------------
# Section D: R2-03 fix (F2) - degenerate MF shoulders and out-of-range clamp
# ---------------------------------------------------------------------------
cat("\n--- D. R2-03 fix: degenerate MF shoulders and out-of-range clamping ---\n")

# D1: an input landing exactly on a variable's range bound (a degenerate
# input-MF shoulder) must now get a real value, matching FuzzyR exactly (its
# trapmf/trimf already gave 1 there - see FuzzyR:::trapmf/FuzzyR:::trimf).
parity_vs_fuzzyr("R2-03a fixed: input exactly at a range end matches FuzzyR", edge_fis, edge_inp)

# D2: ordinary (non-edge, in-range - interior_inp, not inp: inp deliberately
# contains out-of-range rows, which would pull in D3's clamp behavior too and
# no longer isolate this check) inputs through an edge_fis (output sets
# touching 0/100, a degenerate OUTPUT-MF shoulder) must also match FuzzyR now
# - this isolates the output-side half of the fix from the input-side half
# above. Note this output-side fix is NOT edge-case-only: it shifts the
# discretized centroid a little for ANY cell whose result has some 'low' or
# 'high' membership (i.e. most cells, in production) - see section C.
parity_vs_fuzzyr("R2-03b fixed: output sets touching the universe ends (0/100) match FuzzyR", edge_fis, interior_inp)

# D3: out-of-range clamping is a deliberate MODELING POLICY, not something
# FuzzyR itself does (verified: FuzzyR gives 0 membership, i.e. NA output,
# for a value beyond a variable's declared range too - it has no clamp).
# So this is tested for self-consistency (clamped value == value evaluated
# exactly at the boundary), not against FuzzyR.
{
  below_min <- rbind(c(-5, -5), c(-0.5, 10.5))   # x1 below range min (0)
  at_min    <- rbind(c(0, 0),   c(0, 10))        # same rows, x1 clamped to min
  above_max <- rbind(c(15, 15), c(10.5, -0.5))   # x1 above range max (10)
  at_max    <- rbind(c(10, 10), c(10, 0))
  out_below <- evalfis_cpp2(below_min, edge_fis, 301)
  out_atmin <- evalfis_cpp2(at_min, edge_fis, 301)
  out_above <- evalfis_cpp2(above_max, edge_fis, 301)
  out_atmax <- evalfis_cpp2(at_max, edge_fis, 301)
  record("clamp: value below range min gives the same result as the value AT the min",
         close_enough(out_below, out_atmin),
         sprintf("below=%s at_min=%s", fmt(out_below), fmt(out_atmin)))
  record("clamp: value above range max gives the same result as the value AT the max",
         close_enough(out_above, out_atmax),
         sprintf("above=%s at_max=%s", fmt(out_above), fmt(out_atmax)))
}
{
  # The na_sentinel (-9999) must still short-circuit to NA even though it is
  # numerically far below every variable's range - checked already in
  # section E, repeated here explicitly against the clamp logic specifically
  # (clamp must never be reached for a sentinel value).
  out <- evalfis_cpp2(rbind(c(-9999, 5)), edge_fis, 301)
  record("clamp does not apply to the na_sentinel value (still NA, not clamped to range min)",
         is.na(out[1]), sprintf("got %s", fmt(out)))
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
# every variable; a cell with temp exactly at its range min (-10, a
# degenerate input-MF shoulder - review R2-03a); a cell with oxy below its
# range min (-5, i.e. what the real negative-oxygen data found in review R3
# looks like - review R2-03/clamp).
prod_cells <- rbind(
  optimal   = c(11,   20,   9.5,  0.166, 0.2,  0.05, 0.256, 10,  0.355),
  mixed     = c(9.8,  10,   8.5,  0.13,  0.0,  0.01, 0.20,  1.2, 0.20),
  missing   = c(11,   20,   9.5,  0.166, 0.2,  0.05, 0.256, 10,  -9999),
  temp_edge = c(-10,  20,   9.5,  0.166, 0.2,  0.05, 0.256, 10,  0.355),
  oxy_below = c(11,   20,   -5,   0.166, 0.2,  0.05, 0.256, 10,  0.355)
)
# evalfis_cpp2() returns a plain, unnamed NumericVector - Rcpp does not carry
# R rownames through automatically - so name it explicitly to index by row
# label below instead of by a fragile positional index.
new_prod <- setNames(evalfis_cpp2(prod_cells, prod_fis, 301), rownames(prod_cells))
old_prod <- setNames(evalfis_cpp(prod_cells, prod_fis, 301), rownames(prod_cells))
record("builder FIS: missing-layer cell (sentinel) gives NA in both old and new (unaffected by F2)",
       is.na(new_prod["missing"]) && is.na(old_prod["missing"]),
       sprintf("new=%s old=%s", fmt(new_prod["missing"]), fmt(old_prod["missing"])))
record("builder FIS: optimal/mixed cells (no input edge, in range) both valid but differ slightly from old",
       # Production's output MFs ('bad'/'optimal') are always degenerate at
       # 0/100 (R2-03b), so this output-side fix changes EVERY cell's result
       # a little, not just ones with an out-of-range or edge-of-range
       # input - "interior" input alone does not exempt a cell from it.
       # Correctness of the new value (not just "it differs") is checked
       # against FuzzyR just below.
       !close_enough(new_prod[c("optimal", "mixed")], old_prod[c("optimal", "mixed")]) &&
         !any(is.na(new_prod[c("optimal", "mixed")])) && !any(is.na(old_prod[c("optimal", "mixed")])),
       sprintf("new=%s old=%s", fmt(new_prod[c("optimal", "mixed")]), fmt(old_prod[c("optimal", "mixed")])))
record("builder FIS: temp exactly at range min (R2-03a) now gives a value; old evaluator still NA",
       !is.na(new_prod["temp_edge"]) && is.na(old_prod["temp_edge"]),
       sprintf("new=%s old=%s", fmt(new_prod["temp_edge"]), fmt(old_prod["temp_edge"])))
record("builder FIS: oxy below range min (R2-03 clamp) now gives a value; old evaluator still NA",
       !is.na(new_prod["oxy_below"]) && is.na(old_prod["oxy_below"]),
       sprintf("new=%s old=%s", fmt(new_prod["oxy_below"]), fmt(old_prod["oxy_below"])))
nan_cell <- prod_cells["optimal", , drop = FALSE]; nan_cell[1, 9] <- NaN
record("builder FIS: NaN in one layer gives NA (every rule constrains every variable)",
       is.na(evalfis_cpp2(nan_cell, prod_fis, 301)[1]))
record("builder FIS: all-optimal cell scores higher than the mixed cell",
       new_prod["optimal"] > new_prod["mixed"],
       sprintf("optimal=%s mixed=%s", fmt(new_prod["optimal"]), fmt(new_prod["mixed"])))

# Ground truth for the production FIS at its own grid, including the
# temp_edge cell (R2-03a) - the production output sets also touch 0/100
# (R2-03b), exercised by every parity check on this FIS.
parity_vs_fuzzyr("builder FIS (R2-03 fixed): optimal, mixed and temp-edge cells match FuzzyR",
                 prod_fis, prod_cells[c("optimal", "mixed", "temp_edge"), , drop = FALSE], out_disc = 101)

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
