# packages really needed
library(FuzzyR)
library(raster)
library(fuzzyfis)

if (!requireNamespace("paws", quietly = TRUE)) {
  stop("Package 'paws' is required for S3 access. Install it with install.packages('paws').")
}

# Functions
source("/app/scripts/functions_WS.R")
source("/app/scripts/functions_S3.R")

# ---------------------------------------------------------------------------
# Parameter loader — reads from the PARAMS file supplied via the PARAMS env var
# ---------------------------------------------------------------------------
load_params <- function() {
  params_file <- Sys.getenv("PARAMS", unset = "/app/scripts/PARAMS")
  if (!file.exists(params_file)) {
    stop(sprintf(
      "Parameters file not found: %s  (set PARAMS env var to override)",
      params_file
    ))
  }

  lines <- readLines(params_file, warn = FALSE)
  lines <- trimws(lines)
  lines <- lines[nchar(lines) > 0 & !startsWith(lines, "#")]

  raw <- list()
  for (line in lines) {
    idx <- regexpr("=", line, fixed = TRUE)
    if (idx < 1) {
      warning(sprintf("Skipping malformed line in params file: %s", line))
      next
    }
    key        <- trimws(substr(line, 1, idx - 1))
    value      <- trimws(substr(line, idx + 1, nchar(line)))
    raw[[key]] <- value
  }

  get_str <- function(key, default = NULL) {
    v <- raw[[key]]
    if (is.null(v) || v == "" || v == "NULL") return(default)
    v
  }
  get_vec <- function(key, default = NULL) {
    v <- raw[[key]]
    if (is.null(v) || v == "" || v == "NULL") return(default)
    as.integer(trimws(strsplit(v, ",")[[1]]))
  }
  get_str_vec <- function(key, default = NULL) {
    v <- raw[[key]]
    if (is.null(v) || v == "" || v == "NULL") return(default)
    trimws(strsplit(v, ",")[[1]])
  }

  p_parameters <- get_str_vec("parameters", default = c("temp", "sal", "oxy", "sub", "sed", "cur", "orb", "chl", "shear"))

  # Per-parameter MF ranges: one range_<code> PARAMS key per entry in
  # p_parameters, falling back to DEFAULT_MF_RANGES (functions_WS.R) so
  # behavior is unchanged unless explicitly overridden. Parsed as numbers,
  # not integers: "0,0.5" used to become c(0,0), and since the evaluator
  # clamps into the declared range that silently collapsed the whole layer
  # to one value (review R4-04/R1-01). A value that isn't two finite numbers
  # with min < max stops the run instead.
  ranges <- setNames(
    lapply(p_parameters, function(code) {
      key <- paste0("range_", code)
      v <- raw[[key]]
      if (is.null(v) || v == "" || v == "NULL") return(DEFAULT_MF_RANGES[[code]])
      rng <- suppressWarnings(as.numeric(trimws(strsplit(v, ",")[[1]])))
      if (length(rng) != 2 || any(!is.finite(rng)) || rng[1] >= rng[2]) {
        stop(sprintf("Invalid '%s' in PARAMS file: %s (must be two numbers min,max with min < max)", key, v))
      }
      rng
    }),
    p_parameters
  )

  rule_thresholds <- list(
    cutoff_bad  = as.numeric(get_str("rule_cutoff_bad",  default = "0.50")),
    cutoff_okay = as.numeric(get_str("rule_cutoff_okay", default = "0.70")),
    cutoff_good = as.numeric(get_str("rule_cutoff_good", default = "0.90")),
    weight      = as.numeric(get_str("rule_weight",      default = "0.5"))
  )

  n_cores_str <- get_str("n_cores", default = "")
  n_cores_override <- if (nzchar(n_cores_str)) as.integer(n_cores_str) else NA_integer_

  # Memory one worker needs to preprocess + evaluate one month; caps the
  # worker count (resolve_worker_count in functions_WS.R). Default: see the
  # mem_per_worker_gb row in README "Run Parameters".
  mem_per_worker_str <- get_str("mem_per_worker_gb", default = "1.5")
  mem_per_worker_gb  <- suppressWarnings(as.numeric(mem_per_worker_str))
  if (is.na(mem_per_worker_gb) || mem_per_worker_gb <= 0) {
    stop(sprintf("Invalid 'mem_per_worker_gb' in PARAMS file: %s (must be a positive number of GB)", mem_per_worker_str))
  }

  list(
    months_to_process = get_vec("months_to_process", default = 1:12),
    rc_list_s3_key    = get_str("rc_list_s3_key",    default = ""),
    bpns_s3_prefix    = get_str("bpns_s3_prefix",    default = ""),
    out_disc          = as.integer(get_str("out_disc", default = "301")),
    parameters        = p_parameters,
    ranges            = ranges,
    rule_thresholds   = rule_thresholds,
    n_cores           = n_cores_override,
    mem_per_worker_gb = mem_per_worker_gb
  )
}

p <- load_params()

if (anyNA(p$months_to_process) || any(!(p$months_to_process %in% 1:12))) {
  stop(sprintf(
    "Invalid 'months_to_process' in PARAMS file: %s (must be integers 1-12)",
    paste(p$months_to_process, collapse = ",")
  ))
}

# Simple timing helpers for section-level runtime reporting
timings <- data.frame(section = character(), seconds = numeric(), stringsAsFactors = FALSE)
tic <- function(section) {
  cat(">>> [START]", section, "\n")
  proc.time()["elapsed"]
}
toc <- function(section, t0) {
  elapsed <- unname(proc.time()["elapsed"] - t0)
  timings <<- rbind(timings, data.frame(section = section, seconds = elapsed, stringsAsFactors = FALSE))
  cat(">>> [DONE]", section, "-", sprintf("%.2f", elapsed), "s\n")
}

script_t0 <- tic("Total runtime")

#########################################
##             Main script             ##
#########################################
# Preparations -----------------------------------------------------------------
t0 <- tic("Preparations")

# Fixed container-internal directories
scripts_dir <- "/app/scripts"
input_dir   <- "/app/input"
output_dir  <- "/app/output"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (!dir.exists(scripts_dir)) stop(sprintf("scripts_dir not found: %s", scripts_dir))
if (!dir.exists(input_dir))   stop(sprintf("input_dir not found: %s",   input_dir))

rc_list_path   <- file.path(input_dir, "rc_list_year.rds")
bpns_input_dir <- file.path(input_dir, "BPNS input layers median")

# Parameters from PARAMS file
months_to_process <- p$months_to_process

# S3 settings
s3_input_prefix  <- Sys.getenv("S3_INPUT_PREFIX", "input")
s3_output_prefix <- Sys.getenv("S3_OUTPUT_PREFIX", "output")

s3_client <- build_s3_client()
s3_bucket <- resolve_bucket()
toc("Preparations", t0)

ensure_rc_list_available <- function(path) {
  if (file.exists(path)) {
    return(TRUE)
  }

  override_key <- if (nzchar(p$rc_list_s3_key)) p$rc_list_s3_key else Sys.getenv("RC_LIST_S3_KEY", "")
  keys <- unique(Filter(nzchar, c(
    override_key,
    safe_s3_key(s3_input_prefix, basename(path)),
    basename(path)
  )))

  for (key in keys) {
    if (download_s3_object(s3_client, s3_bucket, key, path)) {
      return(TRUE)
    }
  }
  FALSE
}

ensure_bpns_inputs_available <- function(dir_path, months) {
  expected_layers <- c(1, 2, 3, 4, 5, 7, 8, 9, 10)
  expected_files <- as.vector(outer(months, expected_layers, function(m, l) sprintf("BPNS_%d_%d.tif", m, l)))

  missing <- expected_files[!file.exists(file.path(dir_path, expected_files))]
  if (length(missing) == 0) {
    return(TRUE)
  }

  bpns_prefix_override <- if (nzchar(p$bpns_s3_prefix)) p$bpns_s3_prefix else Sys.getenv("BPNS_S3_PREFIX", "")
  dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)

  for (fname in missing) {
    keys <- unique(Filter(nzchar, c(
      if (nzchar(bpns_prefix_override)) safe_s3_key(bpns_prefix_override, fname) else "",
      safe_s3_key(s3_input_prefix, file.path("BPNS input layers median", fname)),
      safe_s3_key(s3_input_prefix, fname),
      file.path("BPNS input layers median", fname),
      fname
    )))

    downloaded <- FALSE
    for (key in keys) {
      if (download_s3_object(s3_client, s3_bucket, key, file.path(dir_path, fname))) {
        downloaded <- TRUE
        break
      }
    }

    if (!downloaded) {
      cat(">>> Missing BPNS input after S3 attempts:", fname, "\n")
    }
  }

  missing_after <- expected_files[!file.exists(file.path(dir_path, expected_files))]
  length(missing_after) == 0
}

t0 <- tic("Ensure required input files")
if (!ensure_rc_list_available(rc_list_path)) {
  stop(paste("RC_LIST_PATH file not found locally or in S3:", rc_list_path))
}
if (!ensure_bpns_inputs_available(bpns_input_dir, months_to_process)) {
  stop(paste("BPNS_INPUT_DIR is missing required files locally and in S3:", bpns_input_dir))
}
toc("Ensure required input files", t0)

# months_to_process is loaded from PARAMS via load_params() above

# create response curves ----------------------
t0 <- tic("Load response curves")
rc_list <- readRDS(rc_list_path)
toc("Load response curves", t0)


# build fuzzy logic model ----------------------
t0 <- tic("Build fuzzy logic model")
parameters <- p$parameters

specif_rules_year <- NULL

fuzzy_model_year <- build_fuzzy_logic_model_yearrc2(
  parameters, specif_rules_year,
  ranges = p$ranges,
  rule_thresholds = p$rule_thresholds
)
toc("Build fuzzy logic model", t0)

# Load, preprocess and evaluate each month inside its own worker ------------
# Preprocessing (load 9 layers, aggregate 10x10, NA -> -9999) used to run
# serially for all months before the parallel phase and dominated the
# runtime (review R1-06: 758 of 857 s). Each worker now handles one whole
# month, so the worker count is capped by memory as well as CPUs (R2-12).
folder <- if (grepl("[/\\]$", bpns_input_dir)) bpns_input_dir else paste0(bpns_input_dir, "/")

# Layers whose file is identical in every month (sedimentation, substrate)
# are aggregated once here instead of once per month, and shared with the
# forked workers copy-on-write (review R4-11).
t0 <- tic("Aggregate static layers")
static_layers <- prepare_static_layers(folder, months_to_process)
cat(">>> Static layers aggregated once:", if (length(static_layers)) paste(names(static_layers), collapse = ", ") else "none", "\n")
toc("Aggregate static layers", t0)

t0 <- tic("Preprocess + HSM per month (parallel)")
process_month <- function(j) {
  cat("Processing month:", j, "\n")
  hsm_calc_year_cpp2(prepare_bpns_month(folder, j, static_layers), j, fuzzy_model_year, p$out_disc)
}

workers <- resolve_worker_count(p$n_cores, p$mem_per_worker_gb, length(months_to_process))
n_cores <- workers$n
cgroup_limit <- get_container_cpu_limit()
cat(sprintf(
  ">>> Worker count: %d, limited by %s (host cores=%s, cgroup CPU limit=%s, available memory=%s GB, per worker=%.2f GB -> memory cap=%s, months=%d, override=%s)\n",
  n_cores,
  workers$limit,
  parallel::detectCores(),
  if (is.na(cgroup_limit)) "none" else cgroup_limit,
  if (is.na(workers$available_mem)) "unknown" else sprintf("%.1f", workers$available_mem / 1024^3),
  p$mem_per_worker_gb,
  if (is.na(workers$mem_cap)) "none" else workers$mem_cap,
  length(months_to_process),
  if (is.na(p$n_cores)) "none" else p$n_cores
))
# mc.preschedule = FALSE forks one short-lived child per month: each month's
# full-resolution working memory is released as soon as that month is done
# (a prescheduled child would run several months in one long-lived process),
# and months of uneven cost are balanced across workers.
results_HSM_Cpp <- if (.Platform$OS.type == "windows") {
  lapply(months_to_process, process_month)
} else {
  parallel::mclapply(months_to_process, process_month,
                     mc.cores = n_cores, mc.preschedule = FALSE)
}
names(results_HSM_Cpp) <- as.character(months_to_process)

# mclapply returns a try-error object per element on worker failure instead of
# raising (and a worker killed by the OS, e.g. OOM, leaves NULL), so failures
# must be checked explicitly before writing output (review R1-05). Each failed
# month's cause is printed - mclapply never prints it itself, and the EDITO
# container is gone afterwards (review R4-02).
failures <- describe_failed_months(results_HSM_Cpp, months_to_process)
if (length(failures) > 0) {
  cat(sprintf(">>> Month %s failed: %s\n", names(failures), failures), sep = "")
  stop(sprintf("HSM calculation failed for month(s): %s", paste(names(failures), collapse = ", ")))
}
toc("Preprocess + HSM per month (parallel)", t0)

t0 <- tic("Write raster outputs")
for (i in months_to_process) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  writeRaster(results_HSM_Cpp[[as.character(i)]], filename = file.path(output_dir, paste0("BPNS_", i, ".tif")),
              format = "GTiff", overwrite = TRUE)
}
toc("Write raster outputs", t0)

t0 <- tic("Upload outputs to S3")
if (!is.null(s3_client) && nzchar(s3_bucket)) {
  cat(">>> Uploading output files to", paste0("s3://", s3_bucket, "/", s3_output_prefix), "\n")
  failed_uploads <- upload_dir_to_s3(s3_client, s3_bucket, output_dir, s3_output_prefix)
  # A failed upload must fail the job - otherwise the run exits 0 having
  # silently lost results, since /app/output is not persisted anywhere else
  # (review R1-04). upload_to_s3() already logged each failure's cause.
  if (length(failed_uploads) > 0) {
    stop(sprintf("S3 upload failed for %d file(s): %s", length(failed_uploads), paste(failed_uploads, collapse = ", ")))
  }
  cat(">>> Upload complete\n")
} else {
  cat(">>> Skipping S3 upload: no S3 client or bucket configured\n")
}
toc("Upload outputs to S3", t0)

toc("Total runtime", script_t0)
cat("\n>>> Timing summary (seconds):\n")
print(timings)