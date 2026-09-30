#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <string>
#include <vector>
using namespace Rcpp;

// ===========================================================================
// evalfis2.cpp - Mamdani fuzzy inference evaluator (fuzzyfis package)
//
// Replaces the ad hoc cppFunction()-based evalfis_cpp in functions_WS.R.
// Differences from that implementation:
//   - andMethod/orMethod/impMethod/aggMethod are actually honored (each with
//     two implemented values) instead of being parsed and then ignored, and
//     unrecognized values raise a clear error instead of being silently
//     treated as some default.
//   - defuzzMethod remains centroid-only (the only method this evaluator
//     implements), but is now validated: anything else raises an error.
//   - Output-membership-function values are precomputed once per (output MF,
//     discretized point) instead of being recomputed for every raster cell -
//     this was the dominant redundant cost in the original implementation.
//   - MF-type dispatch uses an enum instead of repeated string comparisons.
//   - Antecedent MF indices in the rule table are bounds-checked once up
//     front (not per cell) - a malformed rule table raises a clear R error
//     instead of causing an out-of-bounds vector access.
//   - Missing input values (na_sentinel, default -9999, matching fun9999()
//     in functions_WS.R) are recognized by explicit value check rather than
//     relying on them simply falling outside each variable's configured
//     range - so a PARAMS range_<code> override can't accidentally turn a
//     missing cell into a real "extreme" reading.
//   - A degenerate MF shoulder (trapmf/trimf with a==b or c==d - what every
//     "low"/"high" input MF and every output MF this model builds actually
//     is) now evaluates to 1 exactly at that boundary point, matching
//     FuzzyR's own trapmf/trimf (review R2-03). Previously it evaluated to 0
//     there, so a cell landing exactly on a variable's configured range bound
//     (or the output centroid grid's own endpoints) got zero membership from
//     every set and the result was NA.
//   - A real (non-sentinel) input value outside its variable's declared range
//     is clamped to the nearer bound before evaluation, instead of left to
//     miss every MF and produce NA (review R2-03). This is a deliberate
//     modeling policy, not FuzzyR-equivalent behavior: FuzzyR itself does not
//     clamp and would also give 0 membership (NA output) there.
//
// Index conventions (matches FuzzyR):
//   - Rule matrix MF indices are 1-based in R; converted to 0-based via `-1`
//     before indexing into the parsed MF vectors. A value of 0 means
//     "don't care" (no constraint on that input variable for this rule).
//   - Rule matrix columns (0-based, nin = number of input variables):
//       [0, nin)   antecedent MF index per input variable
//        nin       consequent MF index (1-based; single-output FIS assumed)
//        nin+1     optional rule weight (present iff ncols_rule > nin+1)
//        nin+2     optional AND/OR connective: 1=AND, 2=OR (present iff
//                  ncols_rule > nin+2; missing means AND for every rule)
// ===========================================================================

enum class MFType { TRIMF, TRAPMF, GAUSSMF, GBELLMF };
enum class AndMethod { PROD, MIN };
enum class OrMethod { MAX, PROBOR };
enum class ImpMethod { MIN, PROD };
enum class AggMethod { MAX, SUM };
enum class DefuzzMethod { CENTROID };

struct MF {
  MFType type;
  NumericVector params;
};

static MFType parse_mf_type(const std::string& s) {
  if (s == "trimf")   return MFType::TRIMF;
  if (s == "trapmf")  return MFType::TRAPMF;
  if (s == "gaussmf") return MFType::GAUSSMF;
  if (s == "gbellmf") return MFType::GBELLMF;
  stop("Unsupported MF type: " + s);
}

static AndMethod parse_and_method(const std::string& s) {
  if (s == "prod") return AndMethod::PROD;
  if (s == "min")  return AndMethod::MIN;
  stop("Unsupported andMethod (supported: 'prod', 'min'): " + s);
}

static OrMethod parse_or_method(const std::string& s) {
  if (s == "max")    return OrMethod::MAX;
  if (s == "probor") return OrMethod::PROBOR;
  stop("Unsupported orMethod (supported: 'max', 'probor'): " + s);
}

static ImpMethod parse_imp_method(const std::string& s) {
  if (s == "min")  return ImpMethod::MIN;
  if (s == "prod") return ImpMethod::PROD;
  stop("Unsupported impMethod (supported: 'min', 'prod'): " + s);
}

static AggMethod parse_agg_method(const std::string& s) {
  if (s == "max") return AggMethod::MAX;
  if (s == "sum") return AggMethod::SUM;
  stop("Unsupported aggMethod (supported: 'max', 'sum'): " + s);
}

static DefuzzMethod parse_defuzz_method(const std::string& s) {
  if (s == "centroid") return DefuzzMethod::CENTROID;
  stop("Unsupported defuzzMethod (only 'centroid' is implemented): " + s);
}

// Matches FuzzyR's own trapmf/trimf (R, not exported: FuzzyR:::trapmf /
// FuzzyR:::trimf), which compute e.g. y <- pmax(pmin((x-a)/(b-a), h,
// (d-x)/(d-c)), 0) and then replace NaN (from a 0/0 division, i.e. a
// degenerate shoulder with a==b or c==d evaluated exactly at that point) with
// h=1. mf_eval2 below reproduces that in closed form instead of relying on
// IEEE NaN propagation: a degenerate shoulder (a==b, resp. c==d) evaluates to
// 1 exactly AT the boundary and 0 strictly outside it, same as everywhere
// else in a non-degenerate MF. This fixes review finding R2-03: this model
// builds every "low"/"high" input MF as a degenerate shoulder
// (c(min,min,q1,q2) / c(q3,q4,max,max) - see build_fuzzy_logic_model_yearrc2
// in functions_WS.R) and every output MF the same way at the universe ends
// (0/100) - a raster cell landing exactly on a variable's configured range
// bound, or the output centroid grid's own endpoints, used to get zero
// membership from every set. Non-degenerate MFs (a<b, c<d) are unaffected:
// the added a==b / c==d checks only ever change behaviour exactly at x==a or
// x==d when the shoulder is degenerate.
static double mf_eval2(double x, const MF& mf) {
  const NumericVector& p = mf.params;
  switch (mf.type) {
    case MFType::TRIMF: {
      double a = p[0], b = p[1], c = p[2];
      if (x < a || x > c) return 0.0;
      if (x == a) return (a == b) ? 1.0 : 0.0;
      if (x == c) return (b == c) ? 1.0 : 0.0;
      if (x == b) return 1.0;
      if (x < b) return (x - a) / (b - a);
      return (c - x) / (c - b);
    }
    case MFType::TRAPMF: {
      double a = p[0], b = p[1], c = p[2], d = p[3];
      if (x < a || x > d) return 0.0;
      if (x == a) return (a == b) ? 1.0 : 0.0;
      if (x == d) return (c == d) ? 1.0 : 0.0;
      if (x >= b && x <= c) return 1.0;
      if (x < b) return (x - a) / (b - a);
      return (d - x) / (d - c);
    }
    case MFType::GAUSSMF: {
      double sigma = p[0], c0 = p[1];
      if (sigma <= 0) return 0.0;
      double arg = (x - c0) / sigma;
      return std::exp(-0.5 * arg * arg);
    }
    case MFType::GBELLMF: {
      double a = p[0], b = p[1], c0 = p[2];
      if (a == 0) return 0.0;
      double arg = std::abs((x - c0) / a);
      return 1.0 / (1.0 + std::pow(arg, 2.0 * b));
    }
  }
  return 0.0; // unreachable - parse_mf_type() only ever produces the cases above
}

// AND-reduce a set of membership degrees. Short-circuits to 0 as soon as any
// term drives the running value to 0 - valid for both the prod and min
// t-norms used here.
static double reduce_and(const std::vector<double>& mus, AndMethod am) {
  double v = 1.0;
  for (double mu : mus) {
    v = (am == AndMethod::PROD) ? (v * mu) : std::min(v, mu);
    if (v <= 0.0) return 0.0;
  }
  return v;
}

// OR-reduce a set of membership degrees. No short-circuit: a 0 term never
// drives an OR result down.
static double reduce_or(const std::vector<double>& mus, OrMethod om) {
  double v = 0.0;
  for (double mu : mus) {
    if (om == OrMethod::MAX) {
      v = std::max(v, mu);
    } else { // PROBOR: probabilistic (bounded) sum
      v = v + mu - v * mu;
    }
  }
  return v;
}

// [[Rcpp::export]]
NumericVector evalfis_cpp2(NumericMatrix input, List fis, int out_disc = 301,
                            double na_sentinel = -9999.0) {
  int n = input.nrow();
  if (n == 0) return NumericVector(0);
  int nin = input.ncol();

  AndMethod andMethod       = parse_and_method(as<std::string>(fis["andMethod"]));
  OrMethod orMethod         = parse_or_method(as<std::string>(fis["orMethod"]));
  ImpMethod impMethod       = parse_imp_method(as<std::string>(fis["impMethod"]));
  AggMethod aggMethod       = parse_agg_method(as<std::string>(fis["aggMethod"]));
  parse_defuzz_method(as<std::string>(fis["defuzzMethod"])); // validation only: centroid is the sole implemented method

  List inputs = fis["input"];
  List outputs = fis["output"];
  NumericMatrix rules = as<NumericMatrix>(fis["rule"]);
  int nrules = rules.nrow();
  int ncols_rule = rules.ncol();

  int consequent_col = nin;
  int weight_col     = (ncols_rule > nin + 1) ? nin + 1 : -1;
  int connective_col = (ncols_rule > nin + 2) ? nin + 2 : -1;

  int nin_vars = inputs.size();
  if (nin_vars != nin) {
    stop("Number of input columns does not match fis$input length");
  }

  // --- parse input MFs and each input variable's own declared range once
  // (not per row/rule). The range is what addvar()'s varBounds argument sets
  // (FuzzyR stores just [min,max] there, not the full sequence) - the same
  // range a PARAMS range_<code> override configures
  // (build_fuzzy_logic_model_yearrc2 in functions_WS.R). ---
  std::vector<std::vector<MF>> in_mfs(nin);
  std::vector<double> in_range_min(nin), in_range_max(nin);
  for (int i = 0; i < nin; i++) {
    List invar = inputs[i];
    List mf_list = invar["mf"];
    int nmf = mf_list.size();
    in_mfs[i].reserve(nmf);
    for (int m = 0; m < nmf; m++) {
      List mf = mf_list[m];
      MF parsed;
      parsed.type = parse_mf_type(as<std::string>(mf["type"]));
      parsed.params = as<NumericVector>(mf["params"]);
      in_mfs[i].push_back(parsed);
    }
    NumericVector rng = as<NumericVector>(invar["range"]);
    in_range_min[i] = rng[0];
    in_range_max[i] = rng[1];
  }

  // --- validate the rule table once, up front, instead of inside the hot
  // per-cell loop below: every non-"don't care" antecedent MF index must
  // actually exist for that input variable. A malformed rule table (e.g. a
  // hand-edited spec_rules override) would otherwise cause an out-of-bounds
  // vector access deep in the per-row loop - undefined behavior - instead of
  // a clean, early R-level error. ---
  for (int r = 0; r < nrules; r++) {
    for (int j = 0; j < nin; j++) {
      int mf_index = (int) rules(r, j) - 1;
      if (mf_index < 0) continue; // don't care
      if (mf_index >= (int) in_mfs[j].size()) {
        stop("Rule " + std::to_string(r + 1) + " references MF index " + std::to_string(mf_index + 1) +
             " for input variable " + std::to_string(j + 1) + ", which only has " +
             std::to_string(in_mfs[j].size()) + " MF(s) defined");
      }
    }
  }

  // --- parse output MFs (single output variable assumed) ---
  List outvar = outputs[0];
  NumericVector out_range = as<NumericVector>(outvar["range"]);
  double out_min = out_range[0];
  double out_max = out_range[1];
  List out_mf_list = outvar["mf"];
  int noutMF = out_mf_list.size();
  std::vector<MF> out_mfs(noutMF);
  for (int m = 0; m < noutMF; m++) {
    List mf = out_mf_list[m];
    out_mfs[m].type = parse_mf_type(as<std::string>(mf["type"]));
    out_mfs[m].params = as<NumericVector>(mf["params"]);
  }

  // --- discretization grid for centroid defuzzification (computed once) ---
  if (out_disc < 5) out_disc = 5;
  std::vector<double> xs(out_disc);
  double step = (out_max - out_min) / double(out_disc - 1);
  for (int i = 0; i < out_disc; i++) xs[i] = out_min + i * step;

  // --- hoist output-MF evaluation: this depends only on (output MF,
  // discretized point), never on the raster cell, so compute it once here
  // instead of once per row. This is the main performance fix over the
  // original evalfis_cpp. ---
  NumericMatrix out_mf_grid(noutMF, out_disc);
  for (int m = 0; m < noutMF; m++) {
    for (int xi = 0; xi < out_disc; xi++) {
      out_mf_grid(m, xi) = mf_eval2(xs[xi], out_mfs[m]);
    }
  }

  NumericVector result(n, NA_REAL);
  std::vector<double> agg_out_mf(noutMF);
  std::vector<double> mus;
  mus.reserve(nin);

  for (int irow = 0; irow < n; irow++) {
    std::fill(agg_out_mf.begin(), agg_out_mf.end(), 0.0);

    for (int r = 0; r < nrules; r++) {
      int conn = (connective_col >= 0) ? (int) rules(r, connective_col) : 1;

      mus.clear();
      bool and_short_circuit = false;
      for (int j = 0; j < nin; j++) {
        int mf_index = (int) rules(r, j) - 1; // 1-based -> 0-based; -1 = don't care
        if (mf_index < 0) continue;
        double x = input(irow, j);
        // na_sentinel marks a missing raster cell (see fun9999() in
        // functions_WS.R). Treated as a guaranteed non-match against every
        // real MF by explicit value check, regardless of that variable's
        // configured range - unlike relying on the sentinel simply falling
        // outside the range, this can't be broken by a PARAMS range_<code>
        // override that happens to widen the domain to include it. Checked
        // BEFORE clamping below, so a sentinel is never mistaken for a real
        // out-of-range reading even if it numerically falls inside the
        // variable's configured range.
        double mu;
        if (x == na_sentinel) {
          mu = 0.0;
        } else {
          // A real value outside this variable's declared range (data noise,
          // a sensor artifact, or an unclamped upstream value - e.g. review
          // R2-03's negative-oxygen cells) is clamped to the nearer bound
          // before evaluation, rather than left to fall outside every MF and
          // produce NA. This is a deliberate modeling choice, not something
          // FuzzyR::evalfis() itself does (verified: FuzzyR gives 0
          // membership, i.e. NA output, for a value beyond the range too) -
          // review R2-03. Counting/logging how many cells this affects is
          // done by the R caller (hsm_calc_year_cpp2 in functions_WS.R),
          // which has the raw values before this clamp is applied.
          if (x < in_range_min[j]) x = in_range_min[j];
          else if (x > in_range_max[j]) x = in_range_max[j];
          mu = mf_eval2(x, in_mfs[j][mf_index]);
        }
        mus.push_back(mu);
        if (conn == 1 && mu <= 0.0) { and_short_circuit = true; break; }
      }

      double firing;
      if (and_short_circuit) {
        firing = 0.0;
      } else if (mus.empty()) {
        firing = 1.0; // every antecedent was "don't care" - rule fully fires
      } else if (conn == 1) {
        firing = reduce_and(mus, andMethod);
      } else if (conn == 2) {
        firing = reduce_or(mus, orMethod);
      } else {
        stop("Rule connective must be 1 (AND) or 2 (OR), got: " + std::to_string(conn));
      }

      if (firing <= 0.0) continue;

      if (consequent_col >= ncols_rule) continue;
      int out_idx = (int) rules(r, consequent_col) - 1;
      if (out_idx < 0 || out_idx >= noutMF) continue;

      double weight = (weight_col >= 0) ? rules(r, weight_col) : 1.0;
      double fired_val = firing * weight;

      // Per-rule aggregation into agg_out_mf (rules sharing a consequent MF).
      if (aggMethod == AggMethod::MAX) {
        agg_out_mf[out_idx] = std::max(agg_out_mf[out_idx], fired_val);
      } else { // SUM: bounded / probabilistic sum
        double a = agg_out_mf[out_idx];
        agg_out_mf[out_idx] = a + fired_val - a * fired_val;
      }
    }

    // Centroid defuzzification over the discretized output universe.
    double numerator = 0.0;
    double denominator = 0.0;
    for (int xi = 0; xi < out_disc; xi++) {
      double mu_point = 0.0;
      for (int m = 0; m < noutMF; m++) {
        double mfv = out_mf_grid(m, xi);
        // Implication (impMethod) between the rule-aggregated firing degree
        // and this output MF's shape at this point.
        double implied = (impMethod == ImpMethod::MIN)
                          ? std::min(agg_out_mf[m], mfv)
                          : agg_out_mf[m] * mfv;
        // Union across different consequent MFs when building the composite
        // output profile: always max, per standard Mamdani practice. This is
        // a distinct step from the per-rule aggregation above and is not
        // controlled by aggMethod.
        mu_point = std::max(mu_point, implied);
      }
      if (mu_point > 0.0) {
        numerator += xs[xi] * mu_point;
        denominator += mu_point;
      }
    }

    result[irow] = (denominator == 0.0) ? NA_REAL : (numerator / denominator);
  }

  return result;
}
