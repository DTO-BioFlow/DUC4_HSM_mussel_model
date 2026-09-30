#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# Unit test: memory-aware worker sizing (functions_WS.R)
#   - get_container_memory_available(): cgroup v2 / v1 parsing
#   - get_host_memory_available():      /proc/meminfo fallback
#   - get_available_memory():           tighter of the two
#   - resolve_worker_count():           cpu / memory / jobs / override caps
#   - describe_failed_months():         per-month failure causes
# (review R1-06 / R2-12, dev_plan.md F4; R4-01 / R4-02 / R4-08, F6)
#
# Dev-only script, not S3-synced, not baked into the Docker image. Hermetic:
# no S3, no network, no real data - the cgroup and meminfo files are fixture
# files written to a temp dir (removed at exit, pass or fail), and host
# cores / CPU limit / available memory are injected as arguments. Run with:
#   Rscript tests/test_worker_sizing.R
# or inside the project image (R is not needed on the host):
#   docker run --rm --entrypoint Rscript -v "<repo>:/repo:ro" <image> /repo/tests/test_worker_sizing.R
#
# Exit status 0 if every case passes, 1 otherwise.
# ---------------------------------------------------------------------------

this_file <- tryCatch({
  args <- commandArgs(trailingOnly = FALSE)
  fpath <- sub("^--file=", "", args[grepl("^--file=", args)])
  if (length(fpath) == 1) normalizePath(fpath) else NA_character_
}, error = function(e) NA_character_)
repo_root <- if (!is.na(this_file)) normalizePath(file.path(dirname(this_file), "..")) else normalizePath(".")

source(file.path(repo_root, "functions_WS.R"))

results <- data.frame(case = character(), status = character(), detail = character(),
                      stringsAsFactors = FALSE)
record <- function(case, passed, detail = "") {
  status <- if (isTRUE(passed)) "PASS" else "FAIL"
  results[nrow(results) + 1, ] <<- list(case = case, status = status, detail = detail)
  cat(sprintf("[%s] %s%s\n", status, case, if (nzchar(detail)) paste0(" - ", detail) else ""))
}
show <- function(x) paste(format(x), collapse = ",")

GB <- 1024^3
fixture_dir <- file.path(tempdir(), "test_worker_sizing")
dir.create(fixture_dir, showWarnings = FALSE)
fx <- function(name, content) {
  path <- file.path(fixture_dir, name)
  writeLines(content, path)
  path
}
missing <- file.path(fixture_dir, "does_not_exist")

tryCatch({
  # --- cgroup memory parsing -------------------------------------------------
  cgmem <- function(v2_max = missing, v2_current = missing, v1_limit = missing, v1_usage = missing,
                    v2_stat = missing, v1_stat = missing)
    get_container_memory_available(v2_max = v2_max, v2_current = v2_current, v2_stat = v2_stat,
                                   v1_limit = v1_limit, v1_usage = v1_usage, v1_stat = v1_stat)

  got <- cgmem(fx("v2_max", "8589934592"), fx("v2_cur", "1073741824"))
  record("cgroup v2: limit - current", identical(got, 7 * GB), show(got / GB))

  got <- cgmem(fx("v2_max_unl", "max"), fx("v2_cur2", "1073741824"))
  record("cgroup v2: 'max' = no limit -> NA", is.na(got), show(got))

  got <- cgmem(fx("v2_max2", "4294967296"), missing)
  record("cgroup v2: unreadable current -> whole limit", identical(got, 4 * GB), show(got / GB))

  got <- cgmem(v1_limit = fx("v1_lim", "6442450944"), v1_usage = fx("v1_use", "2147483648"))
  record("cgroup v1: limit - usage", identical(got, 4 * GB), show(got / GB))

  got <- cgmem(v1_limit = fx("v1_unl", "9223372036854771712"), v1_usage = fx("v1_use2", "1"))
  record("cgroup v1: INT64_MAX sentinel = no limit -> NA", is.na(got), show(got))

  got <- cgmem(fx("v2_garbage", "not-a-number"))
  record("cgroup: garbage content -> NA", is.na(got), show(got))

  got <- cgmem()
  record("cgroup: no files -> NA", is.na(got), show(got))

  got <- cgmem(fx("v2_max3", "1073741824"), fx("v2_cur3", "2147483648"))
  record("cgroup v2: usage above limit floors at 0", identical(got, 0), show(got))

  # --- page cache is not counted as used (review R4-01) ----------------------
  # 8 GB limit, 7 GB usage of which 5 GB is inactive (reclaimable) file cache
  # -> working set 2 GB -> 6 GB available (was 1 GB before the fix).
  got <- cgmem(fx("v2_max4", "8589934592"), fx("v2_cur4", "7516192768"),
               v2_stat = fx("v2_stat4", c("anon 1073741824", "file 6442450944",
                                          "active_file 1073741824", "inactive_file 5368709120")))
  record("cgroup v2: inactive_file excluded from usage (R4-01)", identical(got, 6 * GB), show(got / GB))

  got <- cgmem(v1_limit = fx("v1_lim4", "8589934592"), v1_usage = fx("v1_use4", "7516192768"),
               v1_stat = fx("v1_stat4", c("cache 6442450944", "inactive_file 1",
                                          "total_inactive_file 5368709120")))
  record("cgroup v1: total_inactive_file excluded (not the per-cgroup inactive_file)",
         identical(got, 6 * GB), show(got / GB))

  got <- cgmem(fx("v2_max5", "8589934592"), fx("v2_cur5", "7516192768"), v2_stat = missing)
  record("cgroup v2: memory.stat unreadable -> raw usage (conservative)", identical(got, 1 * GB), show(got / GB))

  got <- cgmem(fx("v2_max6", "8589934592"), fx("v2_cur6", "1073741824"),
               v2_stat = fx("v2_stat6", "inactive_file 2147483648"))
  record("cgroup v2: inactive_file > usage floors working set at 0", identical(got, 8 * GB), show(got / GB))

  # --- /proc/meminfo fallback -----------------------------------------------
  meminfo <- fx("meminfo", c("MemTotal:       20320832 kB",
                             "MemFree:        12065740 kB",
                             "MemAvailable:   17470288 kB"))
  got <- get_host_memory_available(meminfo)
  record("meminfo: MemAvailable in bytes", identical(got, 17470288 * 1024), show(got))
  record("meminfo: missing file -> NA", is.na(get_host_memory_available(missing)))
  record("meminfo: no MemAvailable line -> NA",
         is.na(get_host_memory_available(fx("meminfo_old", "MemTotal: 1 kB"))))

  # --- combined figure -------------------------------------------------------
  record("available: tighter of cgroup and host", identical(get_available_memory(3 * GB, 10 * GB), 3 * GB))
  record("available: host only", identical(get_available_memory(NA_real_, 10 * GB), 10 * GB))
  record("available: neither known -> NA", is.na(get_available_memory(NA_real_, NA_real_)))

  # --- resolve_worker_count --------------------------------------------------
  rw <- function(override = NA_integer_, mem = 1.5, jobs = 12, host = 20, cpu = NA_integer_, avail = 64 * GB)
    resolve_worker_count(override, mem, jobs, host_cores = host, cpu_limit = cpu, available_mem = avail)

  r <- rw(avail = 5 * GB)
  record("memory cap binds: 5 GB / 1.5 GB -> 3", r$n == 3L && r$limit == "memory" && r$mem_cap == 3L,
         sprintf("n=%d limit=%s", r$n, r$limit))

  r <- rw(avail = 64 * GB, cpu = 4L)
  record("CPU cap binds: cgroup 4 CPUs -> 4 (no core held back, R4-08)", r$n == 4L && r$limit == "cpu", sprintf("n=%d limit=%s", r$n, r$limit))

  r <- rw(avail = 64 * GB, jobs = 2)
  record("jobs cap binds: 2 months -> 2", r$n == 2L && r$limit == "jobs", sprintf("n=%d limit=%s", r$n, r$limit))

  r <- rw(avail = NA_real_)
  record("unknown memory -> CPU/jobs caps only (20 cores, 12 jobs -> 12)",
         r$n == 12L && r$limit == "jobs" && is.na(r$mem_cap), sprintf("n=%d limit=%s", r$n, r$limit))

  r <- rw(avail = 0.5 * GB)
  record("less memory than one worker -> floored at 1", r$n == 1L && r$limit == "memory",
         sprintf("n=%d limit=%s", r$n, r$limit))

  r <- rw(host = 1)
  record("single core host -> floored at 1", r$n == 1L, sprintf("n=%d limit=%s", r$n, r$limit))

  out <- capture.output(r <- rw(override = 8L, avail = 5 * GB))
  record("override wins over memory cap", r$n == 8L && r$limit == "override", sprintf("n=%d limit=%s", r$n, r$limit))
  record("override above memory cap prints a warning", any(grepl("WARNING: n_cores=8 exceeds", out)), show(out))

  out <- capture.output(r <- rw(override = 2L, avail = 5 * GB))
  record("override within memory cap: no warning", r$n == 2L && length(out) == 0, show(out))

  r <- rw(cpu = 2L, avail = 64 * GB)
  record("2-CPU pod -> 2 workers, not 1 (R4-08)", r$n == 2L && r$limit == "cpu", sprintf("n=%d limit=%s", r$n, r$limit))

  # --- describe_failed_months (review R4-02) ---------------------------------
  suppressMessages(library(raster))
  ok <- raster(nrows = 2, ncols = 2, vals = 1:4)
  if (.Platform$OS.type != "windows") {
    # A real forked worker error: mclapply keeps the message only in the
    # try-error's condition attribute - it must come back out.
    res <- suppressWarnings(parallel::mclapply(1:3, function(i) if (i == 2) stop("boom-cause") else ok,
                                               mc.cores = 2, mc.preschedule = FALSE))
    got <- describe_failed_months(res, 1:3)
    record("failed months: real mclapply error message surfaced",
           identical(names(got), "2") && identical(unname(got), "boom-cause"), show(got))
  }
  got <- describe_failed_months(list(ok, NULL, 42), c(5, 6, 7))
  record("failed months: NULL (killed worker) and wrong class reported, success skipped",
         identical(names(got), c("6", "7")) && grepl("killed", got[["6"]]) && grepl("class numeric", got[["7"]]),
         show(got))
  got <- describe_failed_months(list(ok, ok), c(1, 2))
  record("failed months: all succeeded -> empty", length(got) == 0, show(got))
}, finally = unlink(fixture_dir, recursive = TRUE))

record("fixture dir removed", !dir.exists(fixture_dir))

cat("\n=== SUMMARY ===\n")
cat(sprintf("%d PASS, %d FAIL, %d cases total.\n",
            sum(results$status == "PASS"), sum(results$status == "FAIL"), nrow(results)))
quit(status = if (any(results$status == "FAIL")) 1L else 0L, save = "no")
