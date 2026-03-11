#########################################
##             Main script             ##
#########################################
# Preparations -----------------------------------------------------------------
# Runtime paths (override with env vars when running in Docker)
project_dir <- Sys.getenv("PROJECT_DIR", getwd())
if (!dir.exists(project_dir)) {
  stop(paste("PROJECT_DIR does not exist:", project_dir))
}
setwd(project_dir)

# Folder names can be customized via env vars and default to input/output.
input_folder_name <- Sys.getenv("INPUT_FOLDER_NAME", "input")
output_folder_name <- Sys.getenv("OUTPUT_FOLDER_NAME", "output")

input_dir <- file.path(project_dir, input_folder_name)
default_output_dir <- file.path(project_dir, output_folder_name)

rc_list_path <- Sys.getenv("RC_LIST_PATH", "")
if (!nzchar(rc_list_path)) {
  rc_list_path <- file.path(input_dir, "rc_list_year.rds")
}

bpns_input_dir <- Sys.getenv("BPNS_INPUT_DIR", "")
if (!nzchar(bpns_input_dir)) {
  bpns_input_dir <- file.path(input_dir, "BPNS input layers median")
}

output_dir <- Sys.getenv("OUTPUT_DIR", "")
if (!nzchar(output_dir)) {
  output_dir <- default_output_dir
}

# S3 settings (optional). If configured, missing inputs are downloaded and outputs uploaded.
s3_endpoint_raw <- Sys.getenv("AWS_S3_ENDPOINT", Sys.getenv("S3_ENDPOINT", ""))
s3_input_prefix <- Sys.getenv("S3_INPUT_PREFIX", input_folder_name)
s3_output_prefix <- Sys.getenv("S3_OUTPUT_PREFIX", output_folder_name)

if (nzchar(s3_endpoint_raw) && !requireNamespace("paws", quietly = TRUE)) {
  stop("Package 'paws' is required for S3 access. Install it with install.packages('paws').")
}

build_s3_client <- function() {
  if (!nzchar(s3_endpoint_raw)) {
    return(NULL)
  }

  endpoint <- if (grepl("^https?://", s3_endpoint_raw)) s3_endpoint_raw else paste0("https://", s3_endpoint_raw)
  region <- Sys.getenv("AWS_DEFAULT_REGION", "waw3-1")
  access_key <- Sys.getenv("AWS_ACCESS_KEY_ID", "")
  secret_key <- Sys.getenv("AWS_SECRET_ACCESS_KEY", "")
  session_token <- Sys.getenv("AWS_SESSION_TOKEN", "")

  if (!nzchar(access_key) || !nzchar(secret_key)) {
    return(NULL)
  }

  creds <- list(
    access_key_id = access_key,
    secret_access_key = secret_key
  )
  if (nzchar(session_token)) {
    creds$session_token <- session_token
  }

  paws::s3(config = list(
    credentials = list(creds = creds),
    endpoint = endpoint,
    region = region
  ))
}

resolve_bucket <- function() {
  bucket <- Sys.getenv("S3_BUCKET", "")
  if (nzchar(bucket)) {
    return(bucket)
  }
  edito_user <- Sys.getenv("EDITO_USERNAME", "")
  if (nzchar(edito_user)) {
    return(paste0("oidc-", edito_user))
  }
  ""
}

safe_s3_key <- function(prefix, filename) {
  if (!nzchar(prefix)) {
    return(filename)
  }
  paste0(gsub("/+$", "", prefix), "/", gsub("^/+", "", filename))
}

download_s3_object <- function(s3, bucket, key, dest_path) {
  if (is.null(s3) || !nzchar(bucket) || !nzchar(key)) {
    return(FALSE)
  }

  dir.create(dirname(dest_path), recursive = TRUE, showWarnings = FALSE)

  ok <- tryCatch({
    obj <- s3$get_object(Bucket = bucket, Key = key)
    body <- obj$Body

    if (is.raw(body)) {
      writeBin(body, dest_path)
    } else if (is.character(body)) {
      writeBin(charToRaw(paste(body, collapse = "")), dest_path)
    } else {
      stop("Unsupported response body type from S3")
    }
    TRUE
  }, error = function(e) {
    cat(">>> S3 download failed for", paste0("s3://", bucket, "/", key), "-", conditionMessage(e), "\n")
    FALSE
  })

  if (ok) {
    cat(">>> Downloaded", paste0("s3://", bucket, "/", key), "->", dest_path, "\n")
  }
  ok
}

upload_to_s3 <- function(s3, bucket, local_path, s3_key) {
  if (is.null(s3) || !nzchar(bucket) || !file.exists(local_path)) {
    return(FALSE)
  }

  ok <- tryCatch({
    s3$put_object(
      Bucket = bucket,
      Key = s3_key,
      Body = readBin(local_path, what = "raw", n = file.info(local_path)$size)
    )
    TRUE
  }, error = function(e) {
    cat(">>> S3 upload failed for", paste0("s3://", bucket, "/", s3_key), "-", conditionMessage(e), "\n")
    FALSE
  })

  if (ok) {
    cat(">>> Uploaded", local_path, "->", paste0("s3://", bucket, "/", s3_key), "\n")
  }
  ok
}

upload_dir_to_s3 <- function(s3, bucket, local_dir, s3_prefix) {
  if (is.null(s3) || !nzchar(bucket) || !dir.exists(local_dir)) {
    return(invisible(NULL))
  }

  local_dir_norm <- normalizePath(local_dir, winslash = "/", mustWork = FALSE)
  files <- list.files(local_dir, recursive = TRUE, full.names = TRUE)

  for (f in files) {
    file_norm <- normalizePath(f, winslash = "/", mustWork = FALSE)
    prefix <- paste0(local_dir_norm, "/")
    rel <- if (startsWith(file_norm, prefix)) substring(file_norm, nchar(prefix) + 1) else basename(file_norm)
    key <- if (!nzchar(s3_prefix)) rel else safe_s3_key(s3_prefix, rel)
    upload_to_s3(s3, bucket, f, key)
  }
}

s3_client <- build_s3_client()
s3_bucket <- resolve_bucket()

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

if (!ensure_rc_list_available(rc_list_path)) {
  stop(paste("RC_LIST_PATH file not found locally or in S3:", rc_list_path))
}
if (!ensure_bpns_inputs_available(bpns_input_dir)) {
  stop(paste("BPNS_INPUT_DIR is missing required files locally and in S3:", bpns_input_dir))
}

# packages really needed
library(FuzzyR)
library(raster)
library(terra)
library(doSNOW)
library(foreach)

# Functions
source("functions_WS_cpp.R")

# create response curves ----------------------
rc_list <- readRDS(rc_list_path)


# build fuzzy logic model ----------------------
parameters <- c("temp", "sal", "oxy", "sub", "sed", "cur", "orb", "chl", "shear")

specif_rules_year <- NULL

fuzzy_model_year <- build_fuzzy_logic_model_yearrc(parameters, specif_rules_year)

# load HSM input data (raster shape)
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

# Apply fuzzy logic model
results_HSM_Cpp <- list()
# for (j in 1:12) {
for (j in 1:1) {
  print(paste0("Processing month: ", j))
  results_HSM_Cpp[[j]] <- hsm_calc_year_cpp(BPNS_aggr2, j, 301)
}

# for (j in 1:12) {
for (i in 1) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  writeRaster(results_HSM_Cpp[[i]], filename = file.path(output_dir, paste0("BPNS_", i, ".tif")),
              format = "GTiff", overwrite = TRUE)
}

if (!is.null(s3_client) && nzchar(s3_bucket)) {
  cat(">>> Uploading output files to", paste0("s3://", s3_bucket, "/", s3_output_prefix), "\n")
  upload_dir_to_s3(s3_client, s3_bucket, output_dir, s3_output_prefix)
  cat(">>> Upload complete\n")
} else {
  cat(">>> Skipping S3 upload: no S3 client or bucket configured\n")
}