# Mussel Habitat Suitability Model on EDITO: User Guide

This guide explains how to run the mussel habitat suitability model (HSM) for the Belgian Part of the North Sea (BPNS) on the EDITO platform. It is written for researchers who want to run the model and use its results. You do not need to know R or how the model is programmed.

---

## Contents

1. [What the model does](#1-what-the-model-does)
2. [How a run works on EDITO](#2-how-a-run-works-on-edito)
3. [What you need before you start](#3-what-you-need-before-you-start)
4. [Step-by-step: your first run](#4-step-by-step-your-first-run)
5. [Changing the run settings (PARAMS)](#5-changing-the-run-settings-params)
6. [Using your own input data](#6-using-your-own-input-data)
7. [Understanding the results](#7-understanding-the-results)
8. [Troubleshooting](#8-troubleshooting)
9. [Quick reference](#9-quick-reference)

---

## 1. What the model does

The model estimates how suitable each location in the BPNS is for mussel beds, month by month. For each month it produces a map with a **suitability score from 0 (unsuitable) to 100 (optimal)**.

It is a **fuzzy logic** model. In short:

1. **Nine environmental conditions** are read for each map cell: temperature, salinity, dissolved oxygen, substrate, sedimentation rate, current speed, orbital velocity, chlorophyll (primary production) and shear stress.
2. **Response curves** describe, for each condition, which values are *too low*, *optimal* or *too high* for mussels. Each value is scored by how strongly it belongs to each of these three classes. The boundaries between classes are not sharp: a value can be, for example, 70 % optimal and 30 % too high.
3. **Rules** combine the nine conditions. The more conditions are optimal in a cell, the better the suitability. The rules sort each combination into one of four classes: *bad*, *okay*, *good* or *optimal*.
4. The result is turned back into a single number between 0 and 100 for each cell.

Before the calculation, the input maps are coarsened by a factor of 10 (each output cell is the average of 10 × 10 input cells). The months are calculated in parallel, so a full year runs in a few minutes.

---

## 2. How a run works on EDITO

The model runs as a **process** in the EDITO Datalab. Your files live in your **Personal Storage** (an S3 bucket that belongs to your EDITO account). A run goes like this:

```
 Your Personal Storage (S3 bucket)                     EDITO process (container)
 ─────────────────────────────────                     ─────────────────────────
 scripts/   model scripts + PARAMS  ──── 1. copied ──►  reads your settings
 input/     response curves + maps  ──── 2. copied ──►  runs the model for each month
 output/    BPNS_<month>.tif        ◄─── 3. uploaded ─  writes one map per month
```

1. When the process starts, it copies everything in your `scripts/` folder, including the settings file `PARAMS`.
2. It copies the input files it needs from your `input/` folder.
3. It calculates the model and uploads one map per month to your `output/` folder.

Your EDITO login is used for storage access. You do not need to enter keys, passwords or a bucket name.

> **Important:** the process copies the model scripts from **your own** storage every time it runs. You can change settings (and even the scripts) without anyone rebuilding the process.

---

## 3. What you need before you start

| What | Where to get it |
|---|---|
| An EDITO account with access to the **Datalab** and **Personal Storage** | [EDITO Datalab](https://datalab.dive.edito.eu) |
| Access to the mussel model process in the **Process Playground** | [Process Playground catalogue](https://datalab.dive.edito.eu/process-catalog/process-playground). Ask the model maintainers if you cannot find it. |
| The four **script files**: `VSC_CB2_HSM_18.R`, `functions_WS.R`, `functions_S3.R`, `PARAMS` | From the project repository or the model maintainers |
| The **input data**: `rc_list_year.rds` (response curves) and the BPNS monthly maps `BPNS_<month>_<layer>.tif` | From the model maintainers (the full 12-month set is about 7.6 GB) |

---

## 4. Step-by-step: your first run

### Step 1: Log in and open Personal Storage

1. Log in to the [EDITO Datalab](https://datalab.dive.edito.eu).
2. Open **My Files** (Personal Storage). This is your S3 bucket.

> Use a single bucket. The process finds your bucket on its own and stops with an error if your account has more than one.

### Step 2: Upload the model scripts

1. In your bucket, create a folder called **`scripts`**.
2. Upload these four files into it:
   - `VSC_CB2_HSM_18.R`
   - `functions_WS.R`
   - `functions_S3.R`
   - `PARAMS`

Upload only these four files. Other files from the repository (the `pkg/` folder, `Dockerfile`, tests, documentation) are already built into the process and are not needed here.

### Step 3: Upload the input data

1. In your bucket, create a folder called **`input`**.
2. Upload `rc_list_year.rds` directly into `input/`.
3. Inside `input/`, create a folder called exactly **`BPNS input layers median`** (with the spaces).
4. Upload the monthly map files into it.

When you are done, your bucket should look like this:

```
<your bucket>/
├── scripts/
│   ├── VSC_CB2_HSM_18.R
│   ├── functions_WS.R
│   ├── functions_S3.R
│   └── PARAMS
└── input/
    ├── rc_list_year.rds
    └── BPNS input layers median/
        ├── BPNS_1_1.tif
        ├── BPNS_1_2.tif
        ├── ...
        └── BPNS_12_10.tif
```

**Which map files are needed?** For every month you want to calculate, nine layers are required. The file name is `BPNS_<month>_<layer>.tif`:

| Layer number | Condition | Needed? |
|---|---|---|
| 1 | Temperature | Yes |
| 2 | Salinity | Yes |
| 3 | Chlorophyll (primary production) | Yes |
| 4 | Dissolved oxygen | Yes |
| 5 | Orbital velocity | Yes |
| 6 | Depth | **No**, not used by the model |
| 7 | Sedimentation rate | Yes |
| 8 | Substrate | Yes |
| 9 | Current speed | Yes |
| 10 | Shear stress | Yes |

For example, to calculate only June you need `BPNS_6_1.tif` … `BPNS_6_5.tif` and `BPNS_6_7.tif` … `BPNS_6_10.tif`. You only need to upload the months you plan to calculate.

> **Tip:** Upload the input data once. It stays in your storage and every later run reuses it.

### Step 4: Check the settings file

Open `PARAMS` in a text editor before uploading it, or edit it again later and upload it again. The default file calculates all 12 months with the standard model settings, which is a good first run. Section 5 explains each setting.

### Step 5: Launch the process

1. Open the [Process Playground catalogue](https://datalab.dive.edito.eu/process-catalog/process-playground) and select the mussel model process.
2. In the launch form, keep the default values of the environment variables unless you have a reason to change them (see [Quick reference](#9-quick-reference)).
3. Set the **resources** (CPU and memory). They decide how many months are calculated at the same time:

   | You want | CPU | Memory |
   |---|---|---|
   | All 12 months at once (fastest) | 12 | 20 GB or more |
   | A good compromise | 6 | 10 GB |
   | Minimum (one month at a time, slow) | 1 | 3 GB |

   Each month being calculated needs about 1.5 GB of memory and one CPU. The model works out how many months to run in parallel on its own. You do not need to set this.

4. Click **Launch**.

### Step 6: Follow the run

Open the process **logs** in the Datalab. The model writes a line for every step. A normal run looks like this (shortened, times are examples):

```
>>> Discovered S3 bucket: <your bucket>
>>> Syncing scripts folder from S3 ...
>>> [START] Ensure required input files
>>> Downloaded s3://<your bucket>/input/BPNS input layers median/BPNS_1_1.tif -> ...
>>> [START] Build fuzzy logic model
>>> Static layers aggregated once: sedrate, substrate
>>> Worker count: 12, limited by jobs (...)
Processing month: 1
Processing month: 2
...
>>> [DONE] Preprocess + HSM per month (parallel) - 280.51 s
>>> Uploading output files to s3://<your bucket>/output
>>> Upload complete
>>> [DONE] Total runtime - 612.30 s
```

Downloading the full 12-month input set takes a few minutes. The calculation of 12 months then takes about 5 minutes with 10–12 CPUs, and about 25 minutes with a single CPU. These times are approximate and depend on the node.

If the run fails, the log ends with an error message. See [Troubleshooting](#8-troubleshooting).

### Step 7: Download the results

1. Go back to **My Files** (Personal Storage).
2. Open the **`output`** folder. It contains one file per calculated month: `BPNS_1.tif`, `BPNS_2.tif`, …
3. Download the files and open them in QGIS, ArcGIS, R (`terra`, `raster`) or Python (`rasterio`).

> **Warning:** each run writes to the same `output/` folder and **overwrites** files with the same name. To keep the results of several runs, either download or rename them before the next run, or give each run its own output folder (set `S3_OUTPUT_PREFIX`, for example `output/run_june_test`, in the launch form).

---

## 5. Changing the run settings (PARAMS)

`PARAMS` is a plain text file with one setting per line in the form `name=value`. Lines that start with `#` are comments and are ignored. After changing it, upload it again to `scripts/` (replacing the old one) and launch a new run.

If you remove a setting or set it to `NULL`, its default value is used. The defaults reproduce the model's reference results.

### Settings you are most likely to change

| Setting | Default | What it does |
|---|---|---|
| `months_to_process` | `1,2,3,4,5,6,7,8,9,10,11,12` | Which months to calculate, comma-separated. Example: `6,7,8` for summer only. Only these months' input maps are needed. |
| `range_temp` | `-10,40` | Lowest and highest possible temperature the model considers. |
| `range_sal` | `0,45` | Same, for salinity. |
| `range_oxy` | `0,50` | Same, for dissolved oxygen. |
| `range_sub` | `0,200` | Same, for substrate. |
| `range_sed` | `-2,2` | Same, for sedimentation rate. |
| `range_cur` | `0,5` | Same, for current speed. |
| `range_orb` | `0,5` | Same, for orbital velocity. |
| `range_chl` | `0,60` | Same, for chlorophyll. |
| `range_shear` | `0,5` | Same, for shear stress. |

About the `range_…` settings:

- They set the edges of the *too low* and *too high* classes. Values at or beyond the lower edge count as fully *too low*, and the same applies at the upper edge for *too high*.
- A value in the input maps that lies outside its range is **treated as the nearest edge** (for example, a negative oxygen value is treated as 0). The log reports how many cells this affected, for example: `>>> Month 2: Oxy out of declared range [0,50] - 8 cell(s) below (clamped to min), 0 cell(s) above (clamped to max)`.
- Write two numbers, minimum first. Decimals are allowed (for example `range_shear=0,0.5`). The minimum must be smaller than the maximum, otherwise the run stops with an error.
- The range must enclose the response curve of that condition, or the classes will not make sense.

### Advanced model settings

Change these only if you know how they affect the model. They change the rules, and therefore the results.

| Setting | Default | What it does |
|---|---|---|
| `rule_cutoff_bad` | `0.50` | If fewer than this share of the nine conditions is optimal, the rule gives *bad*. |
| `rule_cutoff_okay` | `0.70` | Share of optimal conditions that separates *okay* from *good*. |
| `rule_cutoff_good` | `0.90` | Share of optimal conditions at or above which the rule gives *optimal*. |
| `rule_weight` | `0.5` | Weight given to every rule. |
| `out_disc` | `301` | Precision of the final 0–100 score (number of steps). Higher is slightly more precise and slower. |

### Settings you should not change

| Setting | Why |
|---|---|
| `parameters` | The model always uses all nine conditions in a fixed order. Removing or reordering them makes the run fail. |
| `rc_list_s3_key`, `bpns_s3_prefix` | Only needed if your input files are **not** in the standard folders (see [Section 6](#6-using-your-own-input-data)). |

### Performance settings (usually not needed)

| Setting | Default | What it does |
|---|---|---|
| `n_cores` | automatic | Number of months calculated at the same time. By default it is chosen from the CPUs and memory you requested. Setting it too high can make the run fail with "out of memory". |
| `mem_per_worker_gb` | `1.5` | Memory reserved per month when choosing the automatic `n_cores`. Increase it (for example to `2`) if a run fails because a worker was killed. |

### Example: summer months with a narrower shear range

```
months_to_process=6,7,8
range_shear=0,2
```

All other settings keep their defaults.

---

## 6. Using your own input data

You can run the model on your own environmental maps or response curves, as long as they follow the same format.

### Environmental maps

- GeoTIFF files, one per month and condition, named `BPNS_<month>_<layer>.tif` (see the layer table in [Step 3](#step-3-upload-the-input-data)).
- All layers of a month must cover the same area with the same grid (same extent and cell size).
- Use the same units as the reference data, so that the response curves and `range_…` settings still fit.
- Cells without data (for example, land) should be empty (NoData). The model then leaves them empty in the output.

### Response curves (`rc_list_year.rds`)

This R file contains, for each of the nine conditions, the breakpoints of its *optimal* class. It is an R list with these entries, each holding a vector `q`:

| Entry | Condition | Breakpoints in `q` |
|---|---|---|
| `sst` | Temperature | 4 (trapezoid) |
| `sss` | Salinity | 4 (trapezoid) |
| `oxy` | Dissolved oxygen | 4 (trapezoid) |
| `substrate` | Substrate | 3 (triangle) |
| `sedimentation` | Sedimentation rate | 3 (triangle) |
| `current_speed` | Current speed | 4 (trapezoid) |
| `orb_vel` | Orbital velocity | 3 (triangle) |
| `PP` | Chlorophyll | 4 (trapezoid) |
| `shear` | Shear stress | 3 (triangle) |

A trapezoid `q = (a, b, c, d)` means: suitability rises from `a` to `b`, is optimal between `b` and `c`, and falls from `c` to `d`. A triangle `q = (a, b, c)` is optimal only at `b`. Values must be in increasing order and lie inside the matching `range_…` setting. The easiest way to make your own file is to load the reference `rc_list_year.rds` in R, change the `q` values and save it again with `saveRDS()`.

### Files in a different location

If your files are not in the standard folders, tell the model where they are in `PARAMS`:

```
# full path of the response-curve file inside your bucket
rc_list_s3_key=my_project/curves/rc_list_year.rds
# folder that holds the BPNS_<month>_<layer>.tif files
bpns_s3_prefix=my_project/maps_2050
```

---

## 7. Understanding the results

| Property | Value |
|---|---|
| Files | `output/BPNS_<month>.tif`, one per calculated month (`BPNS_1.tif` = January) |
| Format | GeoTIFF, single band |
| Values | Suitability from 0 (unsuitable) to 100 (optimal) |
| Empty cells (NoData) | Land, or cells where at least one input condition has no data |
| Resolution | 10 × coarser than the input maps (each output cell is the mean of 10 × 10 input cells). With the reference data this is about 0.0018° (roughly 200 m), 435 × 626 cells. |
| Coordinates | Longitude/latitude in degrees (reference area about 2.24–3.37° E, 51.09–51.88° N) |

> **Check the coordinate system when you open a map.** Some reference input layers carry an incorrect UTM label, although their coordinates are in degrees. If the map appears in the wrong place in your GIS, assign WGS 84 (EPSG:4326) to the layer.

How to read the score: the four output classes roughly cover these parts of the scale: *bad* 0–50, *okay* 25–70, *good* 65–85, *optimal* 80–100. The classes overlap on purpose, so treat the score as a continuous measure rather than as hard classes.

Changing any model setting (the `range_…` and `rule_…` settings, `out_disc`, or the input data) changes the results. Record the `PARAMS` file you used with each set of results. Changing only the performance settings (`n_cores`, `mem_per_worker_gb`) or the resources does **not** change the results.

---

## 8. Troubleshooting

When a run fails, look at the **last lines of the log**. The table lists the most common messages.

| Message in the log | Cause | What to do |
|---|---|---|
| `bucket discovery found multiple buckets` | Your account has more than one bucket. | Contact EDITO support or the model maintainers. |
| `SCRIPT_NAME 'VSC_CB2_HSM_18.R' not found ... after sync` | The main script is not in `scripts/`. | Check the folder name (`scripts`, lower case) and the file name. |
| `Parameters file not found` | `PARAMS` is missing from `scripts/`. | Upload `PARAMS` (no file extension). |
| `Missing BPNS input after S3 attempts: BPNS_6_3.tif` followed by `BPNS_INPUT_DIR is missing required files` | A required map for a selected month is missing. | Upload the named file to `input/BPNS input layers median/`, or remove that month from `months_to_process`. |
| `RC_LIST_PATH file not found locally or in S3` | `rc_list_year.rds` is missing. | Upload it to `input/`, or set `rc_list_s3_key`. |
| `Invalid 'months_to_process'` | A month is not a whole number from 1 to 12. | Fix the list, for example `1,2,3`. |
| `Invalid 'range_xxx' in PARAMS file` | A range is not two numbers, or the minimum is not smaller than the maximum. | Fix the range, for example `range_shear=0,5`. |
| `Invalid 'mem_per_worker_gb'` | Not a positive number. | Use a value like `1.5`, or remove the line. |
| `Number of input columns does not match fis$input length` | The `parameters` setting was changed. | Restore `parameters=temp,sal,oxy,sub,sed,cur,orb,chl,shear`. |
| `Month 5 failed: no result - worker was killed (e.g. out of memory)` | The run needed more memory than requested. | Request more memory, or set `mem_per_worker_gb=2` (fewer months at once), or a lower `n_cores`. |
| `S3 upload failed for ... file(s)` | Results could not be saved to your storage. | Check your storage quota and launch again. The job is marked as failed so results are never lost silently. |
| Run is much slower than expected, log says `Worker count: 1, limited by memory` or `limited by cpu` | Too few resources requested. | Request more CPU and memory (see [Step 5](#step-5-launch-the-process)). |

Messages that are **not** errors:

- `>>> Month 2: Oxy out of declared range ... clamped` means some input values were outside the `range_…` setting and were treated as the nearest edge. The run continues. If the count is large, check your input data or the range.
- `Layer sedrate differs between months - aggregated per month instead of once` means the sedimentation or substrate map differs between months. The run is a bit slower, and the results are correct.

If you cannot solve a problem, send the model maintainers the full log and your `PARAMS` file.

---

## 9. Quick reference

### Storage layout

| Folder in your bucket | Contents | Who writes it |
|---|---|---|
| `scripts/` | `VSC_CB2_HSM_18.R`, `functions_WS.R`, `functions_S3.R`, `PARAMS` | You |
| `input/` | `rc_list_year.rds` | You |
| `input/BPNS input layers median/` | `BPNS_<month>_<layer>.tif` | You |
| `output/` | `BPNS_<month>.tif` | The model |

### Launch-form environment variables

Keep the defaults unless you need to change them. Storage login details are filled in by EDITO.

| Variable | Default | When to change it |
|---|---|---|
| `S3_OUTPUT_PREFIX` | `output` | To keep each run's results in a separate folder, for example `output/run_2026_10`. |
| `S3_INPUT_PREFIX` | `input` | If your input data is in a different top-level folder. |
| `S3_SCRIPTS_PREFIX` | `scripts` | To keep several script and settings sets side by side, for example `scripts_test` with its own `PARAMS`. |
| `SCRIPT_NAME` | `VSC_CB2_HSM_18.R` | Do not change. |

### Checklist before launching

- [ ] `scripts/` holds the three `.R` files and `PARAMS`
- [ ] `input/rc_list_year.rds` exists
- [ ] Nine map files (layers 1–5 and 7–10) exist for every month in `months_to_process`
- [ ] Old results in `output/` are saved, or `S3_OUTPUT_PREFIX` points to a new folder
- [ ] Resources: about 1 CPU and 1.5 GB of memory per month you want to run at the same time
