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

if (!file.exists(rc_list_path)) {
  stop(paste("RC_LIST_PATH file not found:", rc_list_path))
}
if (!dir.exists(bpns_input_dir)) {
  stop(paste("BPNS_INPUT_DIR directory not found:", bpns_input_dir))
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
for (j in 1:1) {
  print(paste0("Processing month: ", j))
  results_HSM_Cpp[[j]] <- hsm_calc_year_cpp(BPNS_aggr2, j, 301)
}


for (i in 1) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  writeRaster(results_HSM_Cpp[[i]], filename = file.path(output_dir, paste0("BPNS_", i, ".tif")),
              format = "GTiff", overwrite = TRUE)
}