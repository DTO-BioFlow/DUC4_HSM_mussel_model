#########################################
##              Functions              ##
#########################################

# The unused/legacy helper functions that used to live in functions_WS_legacy.R
# were deleted on 2026-09-30 (review R3-04, dev_plan.md F5): none was called by
# the pipeline or tests. The original is in git history (last present in commit
# df4d947); a local, git-ignored copy is functions_WS_legacy_bckp_20260924.R.

# function to change NA to -9999 to work in fuzzy logic. -9999 must match
# evalfis_cpp2()'s na_sentinel default (pkg/fuzzyfis/src/evalfis2.cpp) - that
# function recognizes this exact value as "missing" independent of each
# variable's configured range, so this sentinel and that default must stay
# in sync if either one is ever changed.
fun9999 <- function(x) { x[is.na(x)] <- -9999; return(x)}

# CRS shared by the BPNS input layers that don't carry it in their own metadata
BPNS_CRS <- "+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs"

#########################################
##      Container-aware CPU count      ##
#########################################
# parallel::detectCores() reports the HOST's CPU count, not the container's
# cgroup quota - on a resource-limited container (e.g. EDITO) this can badly
# oversubscribe mclapply workers versus what's actually available, risking
# thrashing/OOM. These helpers read the cgroup CPU quota when present so the
# worker count reflects what the container actually has.

# Returns an integer core limit from the cgroup CPU quota, or NA_integer_ if
# no limit is set/readable (e.g. not running under cgroups, or unlimited).
get_container_cpu_limit <- function() {
  # cgroup v2: single file "<quota> <period>", quota "max" means unlimited
  path_v2 <- "/sys/fs/cgroup/cpu.max"
  if (file.exists(path_v2)) {
    line <- tryCatch(readLines(path_v2, n = 1, warn = FALSE), error = function(e) NA_character_)
    if (length(line) == 1 && !is.na(line)) {
      parts <- strsplit(trimws(line), "\\s+")[[1]]
      if (length(parts) == 2 && parts[1] != "max") {
        quota <- suppressWarnings(as.numeric(parts[1]))
        period <- suppressWarnings(as.numeric(parts[2]))
        if (!is.na(quota) && !is.na(period) && period > 0) {
          return(max(1L, as.integer(floor(quota / period))))
        }
      }
    }
  }

  # cgroup v1: quota/period in separate files; quota -1 means unlimited
  quota_path  <- "/sys/fs/cgroup/cpu/cpu.cfs_quota_us"
  period_path <- "/sys/fs/cgroup/cpu/cpu.cfs_period_us"
  if (file.exists(quota_path) && file.exists(period_path)) {
    quota  <- suppressWarnings(as.numeric(tryCatch(readLines(quota_path, n = 1, warn = FALSE), error = function(e) NA)))
    period <- suppressWarnings(as.numeric(tryCatch(readLines(period_path, n = 1, warn = FALSE), error = function(e) NA)))
    if (!is.na(quota) && quota > 0 && !is.na(period) && period > 0) {
      return(max(1L, as.integer(floor(quota / period))))
    }
  }

  NA_integer_
}

#########################################
##    Container-aware memory budget    ##
#########################################
# Each worker loads, aggregates and NA-converts one full-resolution month
# (9 layers x ~27 M cells) before running the HSM, so the worker count must
# also respect the memory the container actually has (review R1-06/R2-12) -
# a CPU-only count (19 workers on a 20-core host) could OOM a pod whose
# memory limit is far smaller than its CPU count suggests.

# Reads one line from a cgroup/proc file as a number; NA if the file is
# missing or unreadable (I/O boundary - falls back to "unknown").
read_number_file <- function(path) {
  if (!file.exists(path)) return(NA_real_)
  line <- tryCatch(readLines(path, n = 1, warn = FALSE), error = function(e) NA_character_)
  if (length(line) != 1 || is.na(line)) return(NA_real_)
  suppressWarnings(as.numeric(trimws(line)))
}

# Returns the bytes still available under the container's cgroup memory
# limit (limit - current usage), or NA if no limit is set/readable.
# Paths are arguments so tests can point them at fixture files.
get_container_memory_available <- function(
    v2_max     = "/sys/fs/cgroup/memory.max",
    v2_current = "/sys/fs/cgroup/memory.current",
    v1_limit   = "/sys/fs/cgroup/memory/memory.limit_in_bytes",
    v1_usage   = "/sys/fs/cgroup/memory/memory.usage_in_bytes") {
  # cgroup v2: memory.max is "max" when unlimited (read_number_file -> NA)
  limit <- read_number_file(v2_max)
  usage <- read_number_file(v2_current)
  if (is.na(limit)) {
    # cgroup v1: "unlimited" is a page-aligned INT64_MAX (~9.2e18)
    limit <- read_number_file(v1_limit)
    usage <- read_number_file(v1_usage)
    if (!is.na(limit) && limit >= 2^60) limit <- NA_real_
  }
  if (is.na(limit)) return(NA_real_)
  max(0, limit - if (is.na(usage)) 0 else usage)
}

# Returns MemAvailable from /proc/meminfo in bytes, or NA if unreadable.
# Inside a container this is the host's (VM's) figure, not the cgroup's.
get_host_memory_available <- function(meminfo = "/proc/meminfo") {
  if (!file.exists(meminfo)) return(NA_real_)
  lines <- tryCatch(readLines(meminfo, warn = FALSE), error = function(e) character(0))
  hit <- grep("^MemAvailable:", lines, value = TRUE)
  if (length(hit) != 1) return(NA_real_)
  kb <- suppressWarnings(as.numeric(strsplit(trimws(hit), "\\s+")[[1]][2]))
  if (is.na(kb)) NA_real_ else kb * 1024
}

# Memory the workers may use, in bytes: the tighter of the cgroup headroom
# and the host's MemAvailable (a cgroup limit can exceed what an
# overcommitted node really has free), or NA if neither is known.
get_available_memory <- function(container = get_container_memory_available(),
                                 host = get_host_memory_available()) {
  known <- c(container, host)[!is.na(c(container, host))]
  if (length(known) == 0) NA_real_ else min(known)
}

# Resolves the worker count to use. Returns a list:
#   n        - worker count (integer >= 1)
#   limit    - which cap was binding: "override", "cpu", "memory" or "jobs"
#   mem_cap  - workers that fit in available memory (NA if memory unknown)
#   available_mem - the memory figure used, in bytes (NA if unknown)
# An explicit override always wins (it is the user's deliberate choice), but
# a line is printed when it exceeds the memory cap. Otherwise the count is
# the tightest of: host cores capped by the cgroup CPU limit minus one,
# floor(available memory / mem_per_worker_gb), and the number of jobs;
# floored at 1. The detection inputs are arguments so tests can inject them.
resolve_worker_count <- function(override = NA_integer_,
                                 mem_per_worker_gb,
                                 n_jobs,
                                 host_cores = parallel::detectCores(),
                                 cpu_limit = get_container_cpu_limit(),
                                 available_mem = get_available_memory()) {
  mem_cap <- if (is.na(available_mem)) NA_integer_ else
    max(1L, as.integer(floor(available_mem / (mem_per_worker_gb * 1024^3))))

  if (!is.na(override) && override > 0) {
    if (!is.na(mem_cap) && override > mem_cap) {
      cat(sprintf(
        ">>> WARNING: n_cores=%d exceeds the memory-based cap of %d worker(s) (%.1f GB available / %.2f GB per worker) - workers may be OOM-killed\n",
        as.integer(override), mem_cap, available_mem / 1024^3, mem_per_worker_gb
      ))
    }
    return(list(n = as.integer(override), limit = "override", mem_cap = mem_cap, available_mem = available_mem))
  }

  n_cpu <- if (!is.na(cpu_limit)) min(host_cores, cpu_limit) else host_cores
  caps <- c(cpu = max(1L, as.integer(n_cpu) - 1L),
            memory = mem_cap,
            jobs = max(1L, as.integer(n_jobs)))
  caps <- caps[!is.na(caps)]
  binding <- names(caps)[which.min(caps)]
  list(n = unname(caps[binding]), limit = binding, mem_cap = mem_cap, available_mem = available_mem)
}

# Loads one month's 9 BPNS input layers as a named RasterStack (full
# resolution, file-backed - nothing is read into memory yet).
load_bpns_month <- function(folder, i) {
  BPNS_sst <- raster(file.path(folder, sprintf("BPNS_%d_1.tif", i)))
  BPNS_sss <- raster(file.path(folder, sprintf("BPNS_%d_2.tif", i)))
  BPNS_chl <- raster(file.path(folder, sprintf("BPNS_%d_3.tif", i)))
  BPNS_oxy <- raster(file.path(folder, sprintf("BPNS_%d_4.tif", i)))
  BPNS_orbvel <- raster(file.path(folder, sprintf("BPNS_%d_5.tif", i)))
  #BPNS_depth <- raster(file.path(folder, sprintf("BPNS_%d_6.tif", i)))
  BPNS_sedrate <- raster(file.path(folder, sprintf("BPNS_%d_7.tif", i)))
  crs(BPNS_sedrate) <- BPNS_CRS
  BPNS_substrate <- raster(file.path(folder, sprintf("BPNS_%d_8.tif", i)))
  crs(BPNS_substrate) <- BPNS_CRS
  BPNS_currentvel <- raster(file.path(folder, sprintf("BPNS_%d_9.tif", i)))
  BPNS_shear <- raster(file.path(folder, sprintf("BPNS_%d_10.tif", i)))
  crs(BPNS_shear) <- BPNS_CRS

  BPNS_1 <- addLayer(BPNS_sst,BPNS_sss,BPNS_oxy,BPNS_substrate,BPNS_sedrate,BPNS_currentvel,BPNS_orbvel,BPNS_chl,BPNS_shear)

  names(BPNS_1) <- c('temp',
                     'sal',
                     'oxy',
                     'substrate',
                     'sedrate',
                     'currentvel',
                     'orbvel',
                     'chl',
                     'shear')
  return(BPNS_1)
}

# Full per-month preprocessing: load, aggregate 10x10 (mean), then replace
# NA with the -9999 sentinel (fun9999). Runs inside each parallel worker, so
# only one full-resolution month is in flight per worker (review R1-06).
prepare_bpns_month <- function(folder, i) {
  calc(stack(aggregate(load_bpns_month(folder, i), fact = 10)), fun9999)
}

hsm_calc_year_cpp <- function(df, j, out_disc = 201) {
  # df: list of 12 raster stacks
  # j: month index
  rstack <- df[[j]]                 # the multi-layer raster (stack/brick)
  # Extract all layer values: matrix with n rows (cells) and p columns (parameters)
  vals <- getValues(rstack)         # returns matrix if multiple layers

  # evalfis_cpp already loops over all rows internally, so evaluate the whole
  # cell matrix in a single call instead of re-parsing the FIS per cell.
  out <- evalfis_cpp(vals, fuzzy_model_year, out_disc)

  hsm <- raster(rstack)             # template
  hsm <- setValues(hsm, out)        # assign suitability values
  return(hsm)
}

#########################################
##            Fuzzy logic              ##
#########################################
# Fuzzy R
# Create fuzzy logic model

build_fuzzy_logic_model_yearrc <- function(params, spec_rules) {
  # create list to store monthly fis
  fis_list <- NULL

  # to list by month
  month <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")

  # Create a fis (Fuzzy inference system)
  musselbed <- NULL # start fresh

  musselbed <- newfis(
    'musselbed_',
    fisType = "mamdani", #sugeno uses average weight
    mfType = "t1",
    andMethod = "prod",
    orMethod = "max",
    impMethod = "min",
    aggMethod = "max",
    defuzzMethod = "centroid"
  )

  #######################
  # Add input variables #
  #######################
  # 1.  Temperature -------------------------------------------------------------------------------------------------------------------------
  if ("temp" %in% params){
    musselbed <- addvar(
      musselbed,
      'input', #input or output
      "temperature",
      c(-10:40),
      method = NULL,
      params = NULL,
      firing.method = "tnorm.min.max"
    )
    # Add membership function (mf)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "temp"), 'optimal', 'trapmf', rc_list$sst$q)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "temp"), 'low', 'trapmf', c(-10,-10,rc_list$sst$q[1],rc_list$sst$q[2]))
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "temp"), 'high', 'trapmf', c(rc_list$sst$q[3],rc_list$sst$q[4],40,40))
  }


  # 2.  Salinity -------------------------------------------------------------------------------------------------------------------------
  if ("sal" %in% params){
    musselbed <- addvar(
      musselbed,
      'input', #input or output
      "salinity",
      c(0:45),
      method = NULL,
      params = NULL,
      firing.method = "tnorm.min.max"
    )
    # Add membership function (mf)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "sal"), 'optimal', 'trapmf', rc_list$sss$q)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "sal"), 'low', 'trapmf', c(0,0,rc_list$sss$q[1],rc_list$sss$q[2]))
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "sal"), 'high', 'trapmf', c(rc_list$sss$q[3],rc_list$sss$q[4],45,45))
  }

  # 3.  Dissolved Oxygen concentration ---> NOT ENOUGH DATA for monthly -------------------------------------------------------------------------------------------------------------------------
  if ("oxy" %in% params){
    musselbed <- addvar(
      musselbed,
      'input', #input or output
      "Oxy",
      c(0:50),
      method = NULL,
      params = NULL,
      firing.method = "tnorm.min.max"
    )
    # Add membership function (mf)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "oxy"), 'optimal', 'trapmf', rc_list$oxy$q)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "oxy"), 'low', 'trapmf', c(0,0,rc_list$oxy$q[1],rc_list$oxy$q[2]))
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "oxy"), 'high', 'trapmf', c(rc_list$oxy$q[3],rc_list$oxy$q[4],50,50))
  }

  # 4.  Substrate -------------------------------------------------------------------------------------------------------------------------
  if ("sub" %in% params){
    musselbed <- addvar(
      musselbed,
      'input', #input or output
      "substrate",
      c(0:200), # to adjust
      method = NULL,
      params = NULL,
      firing.method = "tnorm.min.max"
    )
    # Add membership function (mf)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "sub"), 'optimal', 'trimf', rc_list$substrate$q)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "sub"), 'low', 'trapmf', c(0,0,rc_list$substrate$q[1],rc_list$substrate$q[2]))
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "sub"), 'high', 'trapmf', c(rc_list$substrate$q[2],rc_list$substrate$q[3],200,200))
  }

  # 5.  Sedimentation rate -------------------------------------------------------------------------------------------------------------------------
  if ("sed" %in% params){
    musselbed <- addvar(
      musselbed,
      'input', #input or output
      "sedimentation",
      c(-2:2), # to adjust
      method = NULL,
      params = NULL,
      firing.method = "tnorm.min.max"
    )
    # Add membership function (mf)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "sed"), 'optimal', 'trimf', rc_list$sedimentation$q)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "sed"), 'low', 'trapmf', c(-2,-2,rc_list$sedimentation$q[1],rc_list$sedimentation$q[2])) # to adjust
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "sed"), 'high', 'trapmf', c(rc_list$sedimentation$q[2],rc_list$sedimentation$q[3],2,2)) # to adjust
  }

  # 6.  Current speed -------------------------------------------------------------------------------------------------------------------------
  if ("cur" %in% params){
    musselbed <- addvar(
      musselbed,
      'input', #input or output
      "current speed",
      c(0:5), # to adjust
      method = NULL,
      params = NULL,
      firing.method = "tnorm.min.max"
    )
    # Add membership function (mf)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "cur"), 'optimal', 'trapmf', rc_list$current_speed$q)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "cur"), 'low', 'trapmf', c(0,0,rc_list$current_speed$q[1],rc_list$current_speed$q[2])) # to adjust
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "cur"), 'high', 'trapmf', c(rc_list$current_speed$q[3],rc_list$current_speed$q[4],5,5)) # to adjust
  }

  # 7.  Orbital velocity -------------------------------------------------------------------------------------------------------------------------
  if ("orb" %in% params){
    musselbed <- addvar(
      musselbed,
      'input', #input or output
      "orbital velocity",
      c(0:5), # to adjust
      method = NULL,
      params = NULL,
      firing.method = "tnorm.min.max"
    )
    # Add membership function (mf)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "orb"), 'optimal', 'trimf', rc_list$orb_vel$q)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "orb"), 'low', 'trapmf', c(0,0,rc_list$orb_vel$q[1],rc_list$orb_vel$q[2])) # to adjust
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "orb"), 'high', 'trapmf', c(rc_list$orb_vel$q[2],rc_list$orb_vel$q[3],5,5)) # to adjust
  }

  # 8. Primary Production (PP) -------------------------------------------------------------------------------------------------------------------------
  if ("chl" %in% params){
    musselbed <- addvar(
      musselbed,
      'input', #input or output
      "PP",
      c(0:60),
      method = NULL,
      params = NULL,
      firing.method = "tnorm.min.max"
    )
    # Add membership function (mf)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "chl"), 'optimal', 'trapmf', rc_list$PP$q)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "chl"), 'low', 'trapmf', c(0,0,rc_list$PP$q[1],rc_list$PP$q[2]))
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "chl"), 'high', 'trapmf', c(rc_list$PP$q[3],rc_list$PP$q[4],60,60))
  }


  # 9.  Shear stress -------------------------------------------------------------------------------------------------------------------------
  if ("shear" %in% params){
    musselbed <- addvar(
      musselbed,
      'input', #input or output
      "shear stress",
      c(0:5), # to adjust
      method = NULL,
      params = NULL,
      firing.method = "tnorm.min.max"
    )
    # Add membership function (mf)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "shear"), 'optimal', 'trimf', rc_list$shear$q) # N/m² : from Smile Consult in German Bight
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "shear"), 'low', 'trapmf', c(0,0,rc_list$shear$q[1],rc_list$shear$q[2])) # to adjust
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "shear"), 'high', 'trapmf', c(rc_list$shear$q[2],rc_list$shear$q[3],5,5)) # to adjust
  }


  ########################
  # Add output variables #
  ########################
  # 1.  Suitability -------------------------------------------------------------------------------------------------------------------------
  musselbed <- addvar(
    musselbed,
    'output', #input or output
    "Suitability",
    c(0:100),
    method = NULL,
    params = NULL,
    firing.method = "tnorm.min.max"
  )
  # Add membership function (mf)
  musselbed <- addmf(musselbed, 'output', 1, 'optimal', 'trapmf', c(80,85,100,100))
  musselbed <- addmf(musselbed, 'output', 1, 'good', 'trapmf', c(65,70,80,85))
  musselbed <- addmf(musselbed, 'output', 1, 'okay', 'trapmf', c(25,50,65,70))
  musselbed <- addmf(musselbed, 'output', 1, 'bad', 'trapmf', c(0,0,25,50))

  ###################
  # Add fuzzy rules #
  ###################
  # load monthly fuzzy rules -------------------------------------------------------------------------------------------------------------------------
  rulelist <- expand.grid(c(rep(list(c(1:3)), length(params)))) #use combinations to have all possible scenario's
  rulelist <- as.data.frame(sapply(rulelist, function(x) as.numeric(x)))
  rulelist$response <- NA # add response column
  rulelist$weight <- NA # add response column
  rulelist$and_or <- NA # add AND/OR column

  rulelist$response[which(rowSums(rulelist[,c(1:length(params))] == 1) <  ((length(params)/100)*50))] <- 4
  rulelist$response[which(rowSums(rulelist[,c(1:length(params))] == 1) >= ((length(params)/100)*50) & rowSums(rulelist[,c(1:(length(params)-1))] == 1) < ((length(params)/100)*70))] <- 3
  rulelist$response[which(rowSums(rulelist[,c(1:length(params))] == 1) >= ((length(params)/100)*70) & rowSums(rulelist[,c(1:(length(params)-1))] == 1) < ((length(params)/100)*90))] <- 2
  rulelist$response[which(rowSums(rulelist[,c(1:length(params))] == 1) >= ((length(params)/100)*90))] <- 1
  rulelist$weight <- 0.5 # weight for rule
  rulelist$and_or <- 1 # provide AND/OR column

  if (!is.null(spec_rules)){
    # remove unnecessary rules
    for (j in 1:length(spec_rules)){
      p <- which(spec_rules[[j]][1:length(params)] != 0)
      rulelist <- rulelist[-which(rulelist[,p] == spec_rules[[j]][p]),]
    }
    extra_rules <-t(as.data.frame(specif_rules_month))
    colnames(extra_rules) <-  colnames(rulelist)
    rulelist2 <- rbind(rulelist,extra_rules)
  } else {
    rulelist2 <- rulelist
  }
  musselbed <- addrule(musselbed, as.matrix(rulelist2)) # add rules to fis

  fis_list <- musselbed
  return(fis_list)
}

#########################################
##      Fuzzy logic (new, v2)          ##
#########################################
# New, parallel implementation alongside build_fuzzy_logic_model_yearrc /
# hsm_calc_year_cpp / evalfis_cpp above, which are left untouched as a
# fallback/reference until the new path (this section + evalfis_cpp2 in the
# fuzzyfis package) has been validated in production. See
# tests/test_evalfis_cpp2.R and README.md ("Old vs. New FIS Implementation").

# Default input-variable ranges, matching the values hardcoded in
# build_fuzzy_logic_model_yearrc (several were marked "# to adjust" there -
# now PARAMS-overridable via range_<code> keys, see VSC_CB2_HSM_18.R).
DEFAULT_MF_RANGES <- list(
  temp  = c(-10, 40),
  sal   = c(0, 45),
  oxy   = c(0, 50),
  sub   = c(0, 200),
  sed   = c(-2, 2),
  cur   = c(0, 5),
  orb   = c(0, 5),
  chl   = c(0, 60),
  shear = c(0, 5)
)

# Default rule-generation thresholds, matching the values hardcoded in
# build_fuzzy_logic_model_yearrc. cutoff_bad/okay/good are fractions (0-1) of
# parameters in the "optimal" state that determine each rule's response
# class; weight is the flat weight applied to every generated rule.
DEFAULT_RULE_THRESHOLDS <- list(
  cutoff_bad  = 0.50,
  cutoff_okay = 0.70,
  cutoff_good = 0.90,
  weight      = 0.5
)

build_fuzzy_logic_model_yearrc2 <- function(params, spec_rules,
                                             ranges = DEFAULT_MF_RANGES,
                                             rule_thresholds = DEFAULT_RULE_THRESHOLDS) {
  # create list to store monthly fis
  fis_list <- NULL

  # to list by month
  month <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")

  # Create a fis (Fuzzy inference system)
  musselbed <- NULL # start fresh

  musselbed <- newfis(
    'musselbed_',
    fisType = "mamdani", #sugeno uses average weight
    mfType = "t1",
    andMethod = "prod",
    orMethod = "max",
    impMethod = "min",
    aggMethod = "max",
    defuzzMethod = "centroid"
  )

  #######################
  # Add input variables #
  #######################
  # 1.  Temperature -------------------------------------------------------------------------------------------------------------------------
  if ("temp" %in% params){
    musselbed <- addvar(
      musselbed,
      'input', #input or output
      "temperature",
      seq(ranges$temp[1], ranges$temp[2]),
      method = NULL,
      params = NULL,
      firing.method = "tnorm.min.max"
    )
    # Add membership function (mf)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "temp"), 'optimal', 'trapmf', rc_list$sst$q)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "temp"), 'low', 'trapmf', c(ranges$temp[1],ranges$temp[1],rc_list$sst$q[1],rc_list$sst$q[2]))
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "temp"), 'high', 'trapmf', c(rc_list$sst$q[3],rc_list$sst$q[4],ranges$temp[2],ranges$temp[2]))
  }


  # 2.  Salinity -------------------------------------------------------------------------------------------------------------------------
  if ("sal" %in% params){
    musselbed <- addvar(
      musselbed,
      'input', #input or output
      "salinity",
      seq(ranges$sal[1], ranges$sal[2]),
      method = NULL,
      params = NULL,
      firing.method = "tnorm.min.max"
    )
    # Add membership function (mf)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "sal"), 'optimal', 'trapmf', rc_list$sss$q)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "sal"), 'low', 'trapmf', c(ranges$sal[1],ranges$sal[1],rc_list$sss$q[1],rc_list$sss$q[2]))
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "sal"), 'high', 'trapmf', c(rc_list$sss$q[3],rc_list$sss$q[4],ranges$sal[2],ranges$sal[2]))
  }

  # 3.  Dissolved Oxygen concentration ---> NOT ENOUGH DATA for monthly -------------------------------------------------------------------------------------------------------------------------
  if ("oxy" %in% params){
    musselbed <- addvar(
      musselbed,
      'input', #input or output
      "Oxy",
      seq(ranges$oxy[1], ranges$oxy[2]),
      method = NULL,
      params = NULL,
      firing.method = "tnorm.min.max"
    )
    # Add membership function (mf)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "oxy"), 'optimal', 'trapmf', rc_list$oxy$q)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "oxy"), 'low', 'trapmf', c(ranges$oxy[1],ranges$oxy[1],rc_list$oxy$q[1],rc_list$oxy$q[2]))
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "oxy"), 'high', 'trapmf', c(rc_list$oxy$q[3],rc_list$oxy$q[4],ranges$oxy[2],ranges$oxy[2]))
  }

  # 4.  Substrate -------------------------------------------------------------------------------------------------------------------------
  if ("sub" %in% params){
    musselbed <- addvar(
      musselbed,
      'input', #input or output
      "substrate",
      seq(ranges$sub[1], ranges$sub[2]),
      method = NULL,
      params = NULL,
      firing.method = "tnorm.min.max"
    )
    # Add membership function (mf)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "sub"), 'optimal', 'trimf', rc_list$substrate$q)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "sub"), 'low', 'trapmf', c(ranges$sub[1],ranges$sub[1],rc_list$substrate$q[1],rc_list$substrate$q[2]))
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "sub"), 'high', 'trapmf', c(rc_list$substrate$q[2],rc_list$substrate$q[3],ranges$sub[2],ranges$sub[2]))
  }

  # 5.  Sedimentation rate -------------------------------------------------------------------------------------------------------------------------
  if ("sed" %in% params){
    musselbed <- addvar(
      musselbed,
      'input', #input or output
      "sedimentation",
      seq(ranges$sed[1], ranges$sed[2]),
      method = NULL,
      params = NULL,
      firing.method = "tnorm.min.max"
    )
    # Add membership function (mf)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "sed"), 'optimal', 'trimf', rc_list$sedimentation$q)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "sed"), 'low', 'trapmf', c(ranges$sed[1],ranges$sed[1],rc_list$sedimentation$q[1],rc_list$sedimentation$q[2]))
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "sed"), 'high', 'trapmf', c(rc_list$sedimentation$q[2],rc_list$sedimentation$q[3],ranges$sed[2],ranges$sed[2]))
  }

  # 6.  Current speed -------------------------------------------------------------------------------------------------------------------------
  if ("cur" %in% params){
    musselbed <- addvar(
      musselbed,
      'input', #input or output
      "current speed",
      seq(ranges$cur[1], ranges$cur[2]),
      method = NULL,
      params = NULL,
      firing.method = "tnorm.min.max"
    )
    # Add membership function (mf)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "cur"), 'optimal', 'trapmf', rc_list$current_speed$q)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "cur"), 'low', 'trapmf', c(ranges$cur[1],ranges$cur[1],rc_list$current_speed$q[1],rc_list$current_speed$q[2]))
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "cur"), 'high', 'trapmf', c(rc_list$current_speed$q[3],rc_list$current_speed$q[4],ranges$cur[2],ranges$cur[2]))
  }

  # 7.  Orbital velocity -------------------------------------------------------------------------------------------------------------------------
  if ("orb" %in% params){
    musselbed <- addvar(
      musselbed,
      'input', #input or output
      "orbital velocity",
      seq(ranges$orb[1], ranges$orb[2]),
      method = NULL,
      params = NULL,
      firing.method = "tnorm.min.max"
    )
    # Add membership function (mf)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "orb"), 'optimal', 'trimf', rc_list$orb_vel$q)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "orb"), 'low', 'trapmf', c(ranges$orb[1],ranges$orb[1],rc_list$orb_vel$q[1],rc_list$orb_vel$q[2]))
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "orb"), 'high', 'trapmf', c(rc_list$orb_vel$q[2],rc_list$orb_vel$q[3],ranges$orb[2],ranges$orb[2]))
  }

  # 8. Primary Production (PP) -------------------------------------------------------------------------------------------------------------------------
  if ("chl" %in% params){
    musselbed <- addvar(
      musselbed,
      'input', #input or output
      "PP",
      seq(ranges$chl[1], ranges$chl[2]),
      method = NULL,
      params = NULL,
      firing.method = "tnorm.min.max"
    )
    # Add membership function (mf)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "chl"), 'optimal', 'trapmf', rc_list$PP$q)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "chl"), 'low', 'trapmf', c(ranges$chl[1],ranges$chl[1],rc_list$PP$q[1],rc_list$PP$q[2]))
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "chl"), 'high', 'trapmf', c(rc_list$PP$q[3],rc_list$PP$q[4],ranges$chl[2],ranges$chl[2]))
  }


  # 9.  Shear stress -------------------------------------------------------------------------------------------------------------------------
  if ("shear" %in% params){
    musselbed <- addvar(
      musselbed,
      'input', #input or output
      "shear stress",
      seq(ranges$shear[1], ranges$shear[2]),
      method = NULL,
      params = NULL,
      firing.method = "tnorm.min.max"
    )
    # Add membership function (mf)
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "shear"), 'optimal', 'trimf', rc_list$shear$q) # N/m² : from Smile Consult in German Bight
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "shear"), 'low', 'trapmf', c(ranges$shear[1],ranges$shear[1],rc_list$shear$q[1],rc_list$shear$q[2]))
    musselbed <- addmf(musselbed, 'input', which(parameters %in% "shear"), 'high', 'trapmf', c(rc_list$shear$q[2],rc_list$shear$q[3],ranges$shear[2],ranges$shear[2]))
  }


  ########################
  # Add output variables #
  ########################
  # 1.  Suitability -------------------------------------------------------------------------------------------------------------------------
  # Output scale/breakpoints are NOT parameterized - they define the model's
  # output scale itself, not an input calibration knob.
  musselbed <- addvar(
    musselbed,
    'output', #input or output
    "Suitability",
    c(0:100),
    method = NULL,
    params = NULL,
    firing.method = "tnorm.min.max"
  )
  # Add membership function (mf)
  musselbed <- addmf(musselbed, 'output', 1, 'optimal', 'trapmf', c(80,85,100,100))
  musselbed <- addmf(musselbed, 'output', 1, 'good', 'trapmf', c(65,70,80,85))
  musselbed <- addmf(musselbed, 'output', 1, 'okay', 'trapmf', c(25,50,65,70))
  musselbed <- addmf(musselbed, 'output', 1, 'bad', 'trapmf', c(0,0,25,50))

  ###################
  # Add fuzzy rules #
  ###################
  # load monthly fuzzy rules -------------------------------------------------------------------------------------------------------------------------
  rulelist <- expand.grid(c(rep(list(c(1:3)), length(params)))) #use combinations to have all possible scenario's
  rulelist <- as.data.frame(sapply(rulelist, function(x) as.numeric(x)))
  rulelist$response <- NA # add response column
  rulelist$weight <- NA # add response column
  rulelist$and_or <- NA # add AND/OR column

  frac_optimal <- rowSums(rulelist[,c(1:length(params))] == 1) / length(params)
  # NOTE: the original build_fuzzy_logic_model_yearrc computed the upper
  # bound of the middle two bands (okay/good) over columns
  # 1:(length(params)-1) instead of 1:length(params) - one column short of
  # what the lower bound uses. That looks like a copy/paste bug, but it is
  # reproduced here EXACTLY (via frac_optimal_upper below) so this function's
  # default output matches the old function's output bit-for-bit. Fixing it
  # is a separate, deliberate follow-up - not part of this refactor.
  frac_optimal_upper <- rowSums(rulelist[,c(1:(length(params)-1))] == 1) / length(params)
  rulelist$response[which(frac_optimal <  rule_thresholds$cutoff_bad)] <- 4
  rulelist$response[which(frac_optimal >= rule_thresholds$cutoff_bad  & frac_optimal_upper < rule_thresholds$cutoff_okay)] <- 3
  rulelist$response[which(frac_optimal >= rule_thresholds$cutoff_okay & frac_optimal_upper < rule_thresholds$cutoff_good)] <- 2
  rulelist$response[which(frac_optimal >= rule_thresholds$cutoff_good)] <- 1
  rulelist$weight <- rule_thresholds$weight # weight for rule
  rulelist$and_or <- 1 # provide AND/OR column

  if (!is.null(spec_rules)){
    # remove unnecessary rules
    for (j in 1:length(spec_rules)){
      p <- which(spec_rules[[j]][1:length(params)] != 0)
      rulelist <- rulelist[-which(rulelist[,p] == spec_rules[[j]][p]),]
    }
    # NOTE: fixed vs. the old function, which referenced an undefined global
    # `specif_rules_month` here (dead code today since spec_rules/
    # specif_rules_year is always NULL in production) - uses this function's
    # own `spec_rules` parameter instead.
    extra_rules <-t(as.data.frame(spec_rules))
    colnames(extra_rules) <-  colnames(rulelist)
    rulelist2 <- rbind(rulelist,extra_rules)
  } else {
    rulelist2 <- rulelist
  }
  musselbed <- addrule(musselbed, as.matrix(rulelist2)) # add rules to fis

  fis_list <- musselbed
  return(fis_list)
}

log_out_of_range_cells <- function(vals, fis, month, na_sentinel = -9999) {
  # evalfis_cpp2() clamps a real (non-sentinel) value outside its variable's
  # declared range to the nearer bound rather than producing NA (review
  # R2-03). That clamp is silent by design (it runs per rule, in a hot loop,
  # once per raster cell); this is the observability half of that fix - a
  # per-layer count of how many cells it actually affected, logged once per
  # month so unusually noisy input data (e.g. the negative-oxygen cells found
  # during the review) stays visible instead of only showing up as a slightly
  # different suitability value.
  for (k in seq_along(fis$input)) {
    rng  <- fis$input[[k]]$range
    col  <- vals[, k]
    real <- !is.na(col) & col != na_sentinel
    n_low  <- sum(real & col < rng[1])
    n_high <- sum(real & col > rng[2])
    if (n_low > 0 || n_high > 0) {
      cat(sprintf(
        ">>> Month %d: %s out of declared range [%.4g,%.4g] - %d cell(s) below (clamped to min), %d cell(s) above (clamped to max)\n",
        month, fis$input[[k]]$name, rng[1], rng[2], n_low, n_high
      ))
    }
  }
}

hsm_calc_year_cpp2 <- function(rstack, j, fis, out_disc = 301) {
  # rstack: one month's preprocessed multi-layer raster (prepare_bpns_month);
  # j: month index, used only for logging
  # fis: FuzzyR fis object (explicit parameter, unlike hsm_calc_year_cpp
  # which reads a fuzzy_model_year global from the calling scope)
  vals <- getValues(rstack)         # matrix: n rows (cells) x p columns (parameters)

  log_out_of_range_cells(vals, fis, j)

  out <- evalfis_cpp2(vals, fis, out_disc)  # from the fuzzyfis package (library(fuzzyfis))

  hsm <- raster(rstack)             # template
  hsm <- setValues(hsm, out)        # assign suitability values
  return(hsm)
}


# install.packages("Rcpp")  # if needed
#
# Legacy evalfis_cpp is compiled lazily (call install_legacy_evalfis_cpp()),
# not at source() time: it is unused by the production pipeline (superseded
# by evalfis_cpp2 in the fuzzyfis package), and source()-ing this file runs
# at the start of every single container run - compiling ~200 lines of C++
# on every run for a function nobody calls would be pure wasted time. Kept
# available (not deleted) as a manual reference/fallback until the new path
# has been validated in production; tests/test_evalfis_cpp2.R calls this
# explicitly to get the old function as its trusted-baseline comparison.
install_legacy_evalfis_cpp <- function() {
  library(Rcpp)

  cppFunction('
#include <Rcpp.h>
using namespace Rcpp;

// --- membership function evaluator ---
double mf_eval(double x, const std::string & type, const NumericVector & p) {
  if (type == "trimf") {
    double a = p[0], b = p[1], c = p[2];
    if (x <= a || x >= c) return 0.0;
    if (x == b) return 1.0;
    if (x < b) return (x - a) / (b - a);
    return (c - x) / (c - b);
  }
  else if (type == "trapmf") {
    double a = p[0], b = p[1], c = p[2], d = p[3];
    if (x <= a || x >= d) return 0.0;
    if (x >= b && x <= c) return 1.0;
    if (x < b) return (x - a) / (b - a);
    return (d - x) / (d - c);
  }
  else if (type == "gaussmf") {
    double sigma = p[0], c0 = p[1];
    if (sigma <= 0) return 0.0;
    double arg = (x - c0) / sigma;
    return std::exp(-0.5 * arg * arg);
  }
  else if (type == "gbellmf") {
    double a = p[0], b = p[1], c0 = p[2];
    if (a == 0) return 0.0;
    double arg = std::abs((x - c0) / a);
    return 1.0 / (1.0 + std::pow(arg, 2.0 * b));
  }
  else {
    stop("Unsupported MF type: " + type);
  }
}

// [[Rcpp::export]]
NumericVector evalfis_cpp(NumericMatrix input, List fis, int out_disc = 201) {
  int n = input.nrow();
  if (n == 0) return NumericVector(0);
  int nin = input.ncol();

  // parse FIS top-level settings
  std::string andMethod = as<std::string>(fis["andMethod"]);
  std::string impMethod = as<std::string>(fis["impMethod"]);
  std::string aggMethod = as<std::string>(fis["aggMethod"]);
  std::string defuzzMethod = as<std::string>(fis["defuzzMethod"]);
  if (defuzzMethod.size() == 0) defuzzMethod = as<std::string>(fis["defuzzMethod"]); // fallback

  List inputs = fis["input"];
  List outputs = fis["output"];
  NumericMatrix rules = as<NumericMatrix>(fis["rule"]);
  int nrules = rules.nrow();
  int ncols_rule = rules.ncol();

  // Determine rule column layout:
  // assume antecedents in cols 0..nin-1, consequent at col nin (1-based in R),
  // optional weight at col nin+1, optional connection at nin+2
  int consequent_col = nin;      // corresponds to R column nin+1
  int weight_col = (ncols_rule > nin+1) ? nin+1 : -1;
  // (connection column ignored: we use fis$andMethod globally)

  // --- parse input MFs into C++ structures to avoid repeated R lookups ---
  int nin_vars = inputs.size();
  if (nin_vars != nin) {
    // structure mismatch
    stop("Number of input columns does not match fis$input length");
  }

  // For each input variable, store MF types and parameters
  std::vector< std::vector<std::string> > in_mf_types(nin);
  std::vector< std::vector< NumericVector > > in_mf_params(nin);

  for (int i = 0; i < nin; i++) {
    List invar = inputs[i];
    List mf_list = invar["mf"];
    int nmf = mf_list.size();
    in_mf_types[i].reserve(nmf);
    in_mf_params[i].reserve(nmf);
    for (int m = 0; m < nmf; m++) {
      List mf = mf_list[m];
      std::string t = as<std::string>(mf["type"]);
      NumericVector params = as<NumericVector>(mf["params"]);
      in_mf_types[i].push_back(t);
      in_mf_params[i].push_back(params);
    }
  }

  // --- parse output MFs (assume single output variable as your structure shows) ---
  List outvar = outputs[0];
  NumericVector out_range = as<NumericVector>(outvar["range"]);
  double out_min = out_range[0];
  double out_max = out_range[1];
  List out_mf_list = outvar["mf"];
  int noutMF = out_mf_list.size();
  std::vector<std::string> out_mf_types(noutMF);
  std::vector<NumericVector> out_mf_params(noutMF);
  for (int m = 0; m < noutMF; m++) {
    List mf = out_mf_list[m];
    out_mf_types[m] = as<std::string>(mf["type"]);
    out_mf_params[m] = as<NumericVector>(mf["params"]);
  }

  // precompute discretization for centroid
  if (out_disc < 5) out_disc = 5;
  std::vector<double> xs(out_disc);
  double step = (out_max - out_min) / double(out_disc - 1);
  for (int i = 0; i < out_disc; i++) xs[i] = out_min + i * step;

  NumericVector result(n, NA_REAL);

  // For each observation / raster cell
  for (int irow = 0; irow < n; irow++) {
    // aggregated output MF degrees (after agg across rules) -- init 0
    std::vector<double> agg_out_mf(noutMF, 0.0);

    // iterate rules
    for (int r = 0; r < nrules; r++) {
      double firing;
      if (andMethod == "prod") firing = 1.0;
      else firing = 1.0; // will use min reduction below if not "prod"

      bool skip_rule = false;
      // antecedents: columns 0..nin-1 (R->C indexing: column index j)
      for (int j = 0; j < nin; j++) {
        double rule_val = rules(r, j);      // numeric MF index; often 1-based; 0 may mean "dont care"
        int mf_index = int(rule_val) - 1;   // convert to 0-based
        if (mf_index < 0) {
          // 0 in FuzzyR typically means "dont care" (no constraint on this var)
          continue;
        }
        // evaluate MF of input(irow, j) at this MF
        double x = input(irow, j);
        std::string mf_type = in_mf_types[j][mf_index];
        NumericVector mf_params = in_mf_params[j][mf_index];
        double mu = mf_eval(x, mf_type, mf_params);

        if (andMethod == "prod") {
          firing *= mu;
        } else { // default to "min" t-norm semantics
          firing = std::min(firing, mu);
        }
        if (firing <= 0.0) { // short-circuit
          skip_rule = true;
          break;
        }
      } // end antecedent loop

      if (skip_rule || firing <= 0.0) continue;

      // Get consequent (assume single-output FIS)
      if (consequent_col >= ncols_rule) continue;
      int out_idx = int(rules(r, consequent_col)) - 1; // 0-based
      if (out_idx < 0 || out_idx >= noutMF) continue;

      // optional weight
      double weight = 1.0;
      if (weight_col >= 0 && weight_col < ncols_rule) {
        weight = rules(r, weight_col);
      }
      double fired_val = firing * weight;

      // Implication method: for Mamdani with "min", we store the firing to be combined
      // into output MF via aggregation (aggMethod). Here we accumulate per MF degree using max
      if (aggMethod == "max") {
        agg_out_mf[out_idx] = std::max(agg_out_mf[out_idx], fired_val);
      } else {
        // other aggregation methods could be added
        agg_out_mf[out_idx] = std::max(agg_out_mf[out_idx], fired_val);
      }
    } // end rules loop

    // DEFUZZIFICATION: centroid over discretized universe
    double numerator = 0.0;
    double denominator = 0.0;
    for (int xi = 0; xi < out_disc; xi++) {
      double xv = xs[xi];
      // build aggregated MF at xv: for each output MF, compute mf(xv) then apply implication and aggregation
      double mu_point = 0.0;
      for (int m = 0; m < noutMF; m++) {
        double mfv = mf_eval(xv, out_mf_types[m], out_mf_params[m]);
        // implication: Mamdani min between rule firing degree for that MF and mfv
        double implied = std::min(agg_out_mf[m], mfv);
        // aggregation over rules / consequents: max
        mu_point = std::max(mu_point, implied);
      }
      if (mu_point > 0.0) {
        numerator += xv * mu_point;
        denominator += mu_point;
      }
    }

    if (denominator == 0.0) {
      result[irow] = NA_REAL;
    } else {
      result[irow] = numerator / denominator;
    }
  } // end each row

  return result;
}
')
}
