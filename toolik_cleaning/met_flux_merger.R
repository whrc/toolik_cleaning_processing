
rm(list = ls())

library(data.table)
library(lubridate)


source("C:/Users/klynoe/Documents/pp github reps/toolik_cleaning_processing/toolik_cleaning/sensor_mapper.R")
# ============================================================
# 1. PATHS AND SETTINGS
# ============================================================

flux_path <- "C:/Users/klynoe/Documents/toolik/eddypro_output/2026/"

bio_path <- "C:/Users/klynoe/Documents/toolik/raw_data/met/2026/"

slump_path <- "C:/Users/klynoe/Documents/toolik/raw_data/met/slump_biomet/"

output_path <- "C:/Users/klynoe/Documents/toolik/R_outputs/met_and_flux/"

dir.create(
  output_path,
  recursive = TRUE,
  showWarnings = FALSE
)

start_date <- as.POSIXct(
  "2026-08-01 00:00:00",
  tz = "UTC"
)

stop_date <- as.POSIXct(
  "2026-09-30 23:30:00",
  tz = "UTC"
)

na_strings <- c(
  "",
  "-9999",
  "NA",
  "NaN",
  "NAN",
  "-7999",
  "-99999",
  "9999"
)


# ============================================================
# 2. FUNCTION TO LOAD MULTIPLE BIOMET FILES
#
# Read the original headers and assign them to the data.
# Preserve the original variable names.
# Drop completely empty variables.
# ============================================================

load_biomet <- function(files, dataset_name) {
  
  if (length(files) == 0) {
    stop(
      "No ",
      dataset_name,
      " files found."
    )
  }
  
  dat_list <- lapply(files, function(f) {
    
    hdr <- fread(
      f,
      skip = 1,
      nrows = 0
    )
    
    d <- fread(
      f,
      skip = 4,
      header = FALSE,
      na.strings = na_strings,
      fill = TRUE
    )
    
    if (ncol(d) != ncol(hdr)) {
      stop(
        "Column count does not match the header in: ",
        f,
        "\nData columns: ", ncol(d),
        "\nHeader columns: ", ncol(hdr)
      )
    }
    
    setnames(d, names(hdr))
    
    # Remove completely empty variables from each file
    keep <- vapply(
      d,
      function(x) any(!is.na(x)),
      logical(1)
    )
    
    d[, names(d)[!keep] := NULL]
    
    d
  })
  
  # Combine files using their actual variable names
  d <- rbindlist(
    dat_list,
    use.names = TRUE,
    fill = TRUE
  )
  
  if (!"TIMESTAMP" %in% names(d)) {
    stop(
      "TIMESTAMP column not found in ",
      dataset_name,
      "."
    )
  }
  
  # Standardize timestamps
  d[, TIMESTAMP := as.POSIXct(
    TIMESTAMP,
    tz = "UTC"
  )]
  
  d <- d[!is.na(TIMESTAMP)]
  
  setorder(d, TIMESTAMP)
  
  # Keep one row per timestamp
  d <- d[!duplicated(TIMESTAMP)]
  
  # Remove variables that are empty across all files
  keep <- vapply(
    d,
    function(x) any(!is.na(x)),
    logical(1)
  )
  
  keep["TIMESTAMP"] <- TRUE
  
  d[, names(d)[!keep] := NULL]
  
  cat(
    dataset_name,
    ":",
    nrow(d),
    "rows;",
    ncol(d),
    "columns\n"
  )
  
  print(range(d$TIMESTAMP))
  
  d
}


# ============================================================
# 3. LOAD ALLBIOMET
# ============================================================

bio_files <- list.files(
  path = bio_path,
  pattern = "^toolik-gth89-AllBiomet(_.*)?\\.dat$",
  recursive = TRUE,
  full.names = TRUE
)

cat("\nAllBiomet files:\n")
print(bio_files)

df_bio <- load_biomet(
  files = bio_files,
  dataset_name = "AllBiomet"
)


# ============================================================
# 4. LOAD SLUMPBIOMET
# ============================================================

slump_files <- list.files(
  path = slump_path,
  pattern = "^toolik-gth89-SlumpBiomet.*\\.dat$",
  recursive = TRUE,
  full.names = TRUE
)

cat("\nSlumpBiomet files:\n")
print(slump_files)

df_slump <- load_biomet(
  files = slump_files,
  dataset_name = "SlumpBiomet"
)


df_bio <- prepare_soil_sensors(
  df_bio,
  dataset = "bio",
  soil_sensor_map = bio_map
)

df_slump <- prepare_soil_sensors(
  df_slump,
  dataset = "slump",
  soil_sensor_map = slump_map
)

# Resolve any remaining non-soil variable conflicts
overlap <- setdiff(
  intersect(names(df_bio), names(df_slump)),
  "TIMESTAMP"
)

if (length(overlap) > 0) {
  setnames(
    df_slump,
    old = overlap,
    new = paste0(overlap, "_slump")
  )
}

stopifnot(
  !anyDuplicated(names(df_bio)),
  !anyDuplicated(names(df_slump))
)

# Merge by timestamp
met <- merge(
  df_bio,
  df_slump,
  by = "TIMESTAMP",
  all = TRUE,
  sort = TRUE
)

stopifnot(!anyDuplicated(names(met)))


# ============================================================
# SAVE FINAL MET COLUMN NAMES
# Run after sensor mapping, before merging met and flux
# ============================================================

bio_names <- names(df_bio)
slump_names <- names(df_slump)

met_order <- unique(c(
  "TIMESTAMP",
  setdiff(bio_names, "TIMESTAMP"),
  setdiff(slump_names, "TIMESTAMP")
))

setcolorder(
  met,
  met_order[met_order %in% names(met)]
)

stopifnot(!anyDuplicated(names(met)))

setorder(met, TIMESTAMP)



# ============================================================
# 7. LOAD EDDYPRO FLUX FILES
#
# Read the original EddyPro headers and assign them to data.
# Drop all air_p_mean columns.
# Create a common TIMESTAMP from date + time.
# ============================================================

flux_files <- list.files(
  path = flux_path,
  pattern = "full_output.*\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

cat("\nEddyPro files:\n")
print(flux_files)

if (length(flux_files) == 0) {
  stop("No EddyPro full_output CSV files found.")
}

flux_list <- lapply(flux_files, function(f) {
  
  hdr <- fread(
    f,
    skip = 1,
    nrows = 0
  )
  
  d <- fread(
    f,
    skip = 3,
    header = FALSE,
    na.strings = na_strings,
    fill = TRUE
  )
  
  if (ncol(d) != ncol(hdr)) {
    stop(
      "Column count does not match the header in: ", f,
      "\nData columns: ", ncol(d),
      "\nHeader columns: ", ncol(hdr)
    )
  }
  
  setnames(d, names(hdr))
  
  # Drop all air_p_mean columns
  air_p_cols <- which(names(d) == "air_p_mean")
  
  if (length(air_p_cols) > 0) {
    d[, (air_p_cols) := NULL]
  }
  
  d
})

flux <- rbindlist(
  flux_list,
  use.names = TRUE,
  fill = TRUE
)

if (!all(c("date", "time") %in% names(flux))) {
  stop(
    "EddyPro data must contain columns named 'date' and 'time'."
  )
}

# Create a common UTC timestamp while retaining date and time
flux[, TIMESTAMP := as.POSIXct(
  paste(date, time),
  tz = "UTC"
)]

flux <- flux[!is.na(TIMESTAMP)]

setorder(flux, TIMESTAMP)

# Keep one row per timestamp
flux <- flux[!duplicated(TIMESTAMP)]

cat(
  "\nEddyPro:",
  nrow(flux),
  "rows;",
  ncol(flux),
  "columns\n"
)

print(range(flux$TIMESTAMP))

# ============================================================
# 8. LIMIT ALL THREE DATASETS TO THE REQUESTED DATE RANGE
# ============================================================

met <- met[
  TIMESTAMP >= start_date &
    TIMESTAMP <= stop_date
]

flux <- flux[
  TIMESTAMP >= start_date &
    TIMESTAMP <= stop_date
]


# ============================================================
# 9. CHECK FOR VARIABLE NAME CONFLICTS
#
# TIMESTAMP is the only intended shared join variable.
# Stop rather than silently creating .x / .y column names.
# ============================================================

flux_conflicts <- setdiff(
  intersect(names(met), names(flux)),
  "TIMESTAMP"
)

if (length(flux_conflicts) > 0) {
  stop(
    "Variable names overlap between met and flux: ",
    paste(flux_conflicts, collapse = ", "),
    "\nResolve these conflicts before merging."
  )
}


# ============================================================
# 10. CREATE THE COMPLETE 30-MINUTE UTC TIME GRID
#
# Includes every timestamp from August 1 through
# September 30, 2026, including 00:00 and 23:30.
# ============================================================

ts_grid <- data.table(
  TIMESTAMP = seq(
    from = start_date,
    to = stop_date,
    by = "30 min"
  )
)

stopifnot(nrow(ts_grid) == 2928L)


# ============================================================
# 11. MERGE ALLBIOMET + SLUMPBIOMET + FLUX
#
# First merge the two met datasets.
# Then merge flux.
# Finally merge everything onto the full time grid.
# ============================================================

master_data <- merge(
  met,
  flux,
  by = "TIMESTAMP",
  all = TRUE,
  sort = TRUE
)

master <- merge(
  ts_grid,
  master_data,
  by = "TIMESTAMP",
  all.x = TRUE,
  sort = TRUE
)

setorder(master, TIMESTAMP)


# ============================================================
# 12. RESTORE COLUMN ORDER
#
# TIMESTAMP
# Original AllBiomet variables
# SlumpBiomet variables
# EddyPro variables
# ============================================================

master_order <- c(
  "TIMESTAMP",
  setdiff(bio_names, "TIMESTAMP"),
  setdiff(slump_names, "TIMESTAMP"),
  setdiff(names(flux), "TIMESTAMP")
)

master_order <- master_order[
  master_order %in% names(master)
]

setcolorder(
  master,
  unique(master_order)
)


# ============================================================
# 13. FINAL CHECKS
# ============================================================

cat("\nMaster dataset summary:\n")

cat("Rows:", nrow(master), "\n")
cat("Columns:", ncol(master), "\n")

cat(
  "Start:",
  format(min(master$TIMESTAMP), "%Y-%m-%d %H:%M", tz = "UTC"),
  "\n"
)

cat(
  "End:",
  format(max(master$TIMESTAMP), "%Y-%m-%d %H:%M", tz = "UTC"),
  "\n"
)

cat(
  "Duplicate timestamps:",
  sum(duplicated(master$TIMESTAMP)),
  "\n"
)

cat(
  "Missing timestamps:",
  sum(is.na(master$TIMESTAMP)),
  "\n"
)

stopifnot(
  nrow(master) == nrow(ts_grid),
  !anyDuplicated(master$TIMESTAMP),
  !anyNA(master$TIMESTAMP)
)

# Format timestamps only after all merges are complete
master[, TIMESTAMP := format(
  TIMESTAMP,
  "%Y-%m-%d %H:%M",
  tz = "UTC"
)]


# ============================================================
# 14. WRITE THE COMBINED MASTER FILE
# ============================================================

outfile <- file.path(
  output_path,
  "toolik_master_flux_biomet_202608_202609.csv"
)

fwrite(
  master,
  outfile,
  na = "NA"
)

cat("\nFinished writing:\n", outfile, "\n")
