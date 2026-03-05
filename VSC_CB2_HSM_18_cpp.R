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

rc_list_path <- Sys.getenv("RC_LIST_PATH", "")
if (!nzchar(rc_list_path)) {
  rc_list_path <- file.path(project_dir, "rc_list_year.rds")
}

bpns_input_dir <- Sys.getenv("BPNS_INPUT_DIR", "")
if (!nzchar(bpns_input_dir)) {
  bpns_input_dir <- file.path(project_dir, "BPNS input layers median")
}

output_dir <- Sys.getenv("OUTPUT_DIR", "")
if (!nzchar(output_dir)) {
  output_dir <- file.path(project_dir, "results_cpp")
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

#library("profiling")

# functions
# functions
source("functions_WS_cpp.R")

#.init_profiling(
#    script_name = "mussel_model_"
#)

# create response curves ----------------------
#rc_list <- readRDS(paste0(wd_data, "/rc_list_year_4m_100m.rds"))
rc_list <- readRDS(rc_list_path)

# unpack the data list to individual objects
#list2env(rc_list, .GlobalEnv)
# have a look at response curve by month (dfs + plots)
#rc_list


# build fuzzy logic model ----------------------
# select parameters -> list("temp", "sal", "oxy", "sub", "sed", "shear", "cur", "orb", "chl")
# please follow the same order as in the list. 
parameters <- c("temp", "sal", "oxy", "sub", "sed", "cur", "orb", "chl", "shear")

# extra set of fuzzy rules in a list
specif_rules_year <- NULL

# profile_code("build_fuzzy_logic_model_yearrc", {
  fuzzy_model_year <- build_fuzzy_logic_model_yearrc(parameters, specif_rules_year)
# })
  
# Checking the fuzzy logic model 
#showfis(fuzzy_model$APR) # for april on rstudio
#showGUI(fuzzy_model$JAN) # for april via Rshiny

# load HSM input data (raster shape)
# folder <- paste0(wd_data, "/results/BPNS input layers median/")
folder <- if (grepl("[/\\]$", bpns_input_dir)) bpns_input_dir else paste0(bpns_input_dir, "/")

# for worst case scenario
#folder_wc <- paste0(wd_data, "/results/")

BPNS <- NULL
# profile_code("food_for_HSM", {
  BPNS <- food_for_HSM(folder)
# })
#BPNS <- food_for_HSM_wc(folder_wc)


# profile_code("aggregate from 40x40 resolution to 120x120 (factor = 3)", {
  # aggregate from 40x40 resolution to 120x120 (factor = 3)
  BPNS_aggr <- NULL
  #Ward: hier stond oorspronkelijk fact = 100
  for (i in 1:12) {
    BPNS_aggr[[i]] <- aggregate(BPNS[[i]], fact = 10)
    # BPNS_aggr[[i]] <- aggregate(BPNS[[i]], fact = 40)
  }
# })
#for (i in 1:12) {
#  t <- BPNS[[i]]
#  BPNS_aggr[[i]] <- resample(rast(t), rast(nrows = 10, ncols = 10, xmin = xmin(t), xmax =xmax(t), ymin = ymin(t), ymax = ymax(t)))
#}

# changing NA to -9999 to work with fuzzy logic
#takes 27 minutes without aggregation
# profile_code("BPNS_aggr2", {
  BPNS_aggr2 <- NULL
  for (i in 1:12) {
    BPNS_aggr2[[i]] <- calc(stack(BPNS_aggr[[i]]), fun9999)
  }
# })

# Apply fuzzy logic model
# making a cluster of (physical) cores

# profile_code("results_HSM fuzzyR", {
#   # Apply fuzzy logic model sequentially
#   results_HSM_fuzzyR <- list()
#   for (j in 1:12) {
#     #for (j in 1) {
#     print(paste0("Processing month: ", j))
#     results_HSM_fuzzyR[[j]] <- hsm_calc_year_fuzzyR(BPNS_aggr2, j)
#   }
# })
#close cluster later otherwise rasters cannot be written -> temp file lost!!

# profile_code("results_HSM Cpp", {
  # Apply fuzzy logic model sequentially
  results_HSM_Cpp <- list()
  #for (j in 1:12) {
  for (j in 1:1) {
    print(paste0("Processing month: ", j))
    results_HSM_Cpp[[j]] <- hsm_calc_year_cpp(BPNS_aggr2, j, 301)
  }
# })


# profile_code("write raster layer Cpp", {
  # write raster layer  !!! adjust directory !!!
  #for (i in 1:12){
  for (i in 1){
    # writeRaster(results_HSM[[i]], filename=paste0(wd_data, "/results/hsm/median/BPNS_",i,".tif"),
    #             format="GTiff", overwrite=TRUE)
    if(!dir.exists(output_dir)){
      dir.create(output_dir, recursive = TRUE)
    }
    writeRaster(results_HSM_Cpp[[i]], filename=file.path(output_dir, paste0("BPNS_",i,".tif")),
                format="GTiff", overwrite=TRUE)
  }
# })

# profile_code("write raster layer fuzzyR", {
#   # write raster layer  !!! adjust directory !!!
#   for (i in 1:12){
#     #for (i in 1){
#     # writeRaster(results_HSM[[i]], filename=paste0(wd_data, "/results/hsm/median/BPNS_",i,".tif"),
#     #             format="GTiff", overwrite=TRUE)
#     writeRaster(results_HSM_fuzzyR[[i]], filename=paste0("results_fuzzyR/BPNS_",i,".tif"),
#                 format="GTiff", overwrite=TRUE)
#   }
# })
# print("test point: 6")
#close cluster
# stopCluster(cl)

# generate_profiling_report(save_report = TRUE,
#                           show_report = TRUE,
#                           report_name = "profiling_report_mussel_model_cpp_1_month")