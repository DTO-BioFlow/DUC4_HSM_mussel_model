# packages really needed
library(FuzzyR)
library(raster)
library(terra)
library(doSNOW)
library(foreach)

# Functions
source("functions_WS.R")
source("functions_S3.R")

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
setwd("/app")

# Local staging directories (ephemeral — data is pulled from S3 and results pushed back)
input_dir      <- "/tmp/mussel-model/input"
output_dir     <- "/tmp/mussel-model/output"
rc_list_path   <- file.path(input_dir, "rc_list_year.rds")
bpns_input_dir <- file.path(input_dir, "BPNS input layers median")

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

  override_key <- Sys.getenv("RC_LIST_S3_KEY", "")
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

ensure_bpns_inputs_available <- function(dir_path) {
  expected_layers <- c(1, 2, 3, 4, 5, 7, 8, 9, 10)
  expected_files <- as.vector(outer(1:12, expected_layers, function(m, l) sprintf("BPNS_%d_%d.tif", m, l)))

  missing <- expected_files[!file.exists(file.path(dir_path, expected_files))]
  if (length(missing) == 0) {
    return(TRUE)
  }

  bpns_prefix_override <- Sys.getenv("BPNS_S3_PREFIX", "")
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
if (!ensure_bpns_inputs_available(bpns_input_dir)) {
  stop(paste("BPNS_INPUT_DIR is missing required files locally and in S3:", bpns_input_dir))
}
toc("Ensure required input files", t0)

# create response curves ----------------------
t0 <- tic("Load response curves")
rc_list <- readRDS(rc_list_path)
toc("Load response curves", t0)


# build fuzzy logic model ----------------------
t0 <- tic("Build fuzzy logic model")
parameters <- c("temp", "sal", "oxy", "sub", "sed", "cur", "orb", "chl", "shear")

specif_rules_year <- NULL

fuzzy_model_year <- build_fuzzy_logic_model_yearrc(parameters, specif_rules_year)
toc("Build fuzzy logic model", t0)

# load HSM input data (raster shape)
t0 <- tic("Load and preprocess BPNS data")
folder <- if (grepl("[/\\]$", bpns_input_dir)) bpns_input_dir else paste0(bpns_input_dir, "/")

BPNS <- NULL
BPNS <- food_for_HSM(folder)

BPNS_aggr <- NULL
for (i in 1:12) {
  BPNS_aggr[[i]] <- aggregate(BPNS[[i]], fact = 10)
}

# changing NA to -9999 to work with fuzzy logic
BPNS_aggr2 <- NULL
for (i in 1:12) {
  BPNS_aggr2[[i]] <- calc(stack(BPNS_aggr[[i]]), fun9999)
}
toc("Load and preprocess BPNS data", t0)

# Apply fuzzy logic model
t0 <- tic("Run monthly HSM calculations")
# results_HSM_Cpp <- list()
# for (j in 1:12) {
# # for (j in 1:1) {
#   print(paste0("Processing month: ", j))
#   results_HSM_Cpp[[j]] <- hsm_calc_year_cpp(BPNS_aggr2, j, 301)
# }
n_cores <- max(1, parallel::detectCores() - 1)
cat(">>> Worker count:", n_cores, "\n")
months_to_process <- 1:12
results_HSM_Cpp <- if (.Platform$OS.type == "windows") {
  lapply(months_to_process, function(j) {
    cat("Processing month:", j, "\n")
    hsm_calc_year_cpp(BPNS_aggr2, j, 301)
  })
} else {
  parallel::mclapply(months_to_process, function(j) {
    cat("Processing month:", j, "\n")
    hsm_calc_year_cpp(BPNS_aggr2, j, 301)
  }, mc.cores = n_cores)
}
names(results_HSM_Cpp) <- as.character(months_to_process)
toc("Run monthly HSM calculations", t0)

t0 <- tic("Write raster outputs")
for (i in 1:12) {
# for (i in 1) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  writeRaster(results_HSM_Cpp[[i]], filename = file.path(output_dir, paste0("BPNS_", i, ".tif")),
              format = "GTiff", overwrite = TRUE)
}
toc("Write raster outputs", t0)

t0 <- tic("Upload outputs to S3")
if (!is.null(s3_client) && nzchar(s3_bucket)) {
  cat(">>> Uploading output files to", paste0("s3://", s3_bucket, "/", s3_output_prefix), "\n")
  upload_dir_to_s3(s3_client, s3_bucket, output_dir, s3_output_prefix)
  cat(">>> Upload complete\n")
} else {
  cat(">>> Skipping S3 upload: no S3 client or bucket configured\n")
}
toc("Upload outputs to S3", t0)

toc("Total runtime", script_t0)
cat("\n>>> Timing summary (seconds):\n")
print(timings)

timing_file <- file.path(output_dir, "timing_summary.txt")
writeLines(c(
  ">>> Timing summary (seconds):",
  capture.output(print(timings))
), con = timing_file)
cat(">>> Timing summary saved to", timing_file, "\n")