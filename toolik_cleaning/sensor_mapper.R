

library(data.table)

# ============================================================
# sensor_mapper.R
# Three schema versions for each dataset
# ============================================================

period_start <- as.POSIXct(c(
  "1900-01-01 00:00:00",
  "2026-08-05 00:00:00",
  "2026-08-26 00:00:00"
), tz = "UTC")

period_end <- as.POSIXct(c(
  "2026-08-05 00:00:00",
  "2026-08-26 00:00:00",
  "2100-01-01 00:00:00"
), tz = "UTC")

make_map <- function(canonical, sources) {
  stopifnot(length(sources) == 3L)
  
  data.table(
    canonical = canonical,
    period = 1:3,
    source = sources
  )
}

# ============================================================
# ALLBIOMET
# Final schema:
# SWC_12_36_1_1_1 to SWC_12_36_1_3_1
# TS_2_38_1_1_1 to TS_2_38_1_3_1
# TS_2_38_5_1_1 to TS_2_38_5_12_1
# ============================================================

bio_map <- rbindlist(list(
  # Tower soil moisture
  make_map("SWC_12_36_1_1_1", c(
    "SWC_12_36_1_1_1",
    "SWC_12_36_1_1_1",
    "SWC_12_36_1_1_1"
  )),
  make_map("SWC_12_36_1_2_1", c(
    "SWC_12_36_2_1_1",
    "SWC_12_36_1_2_1",
    "SWC_12_36_1_2_1"
  )),
  make_map("SWC_12_36_1_3_1", c(
    "SWC_12_36_2_2_1",
    "SWC_12_36_1_3_1",
    "SWC_12_36_1_3_1"
  )),
  
  # Tower soil temperature
  make_map("TS_2_38_1_1_1", c(
    "TS_2_38_1_1_1",
    "TS_2_38_1_1_1",
    "TS_2_38_1_1_1"
  )),
  make_map("TS_2_38_1_2_1", c(
    "TS_2_38_2_1_1",
    "TS_2_38_1_2_1",
    "TS_2_38_1_2_1"
  )),
  make_map("TS_2_38_1_3_1", c(
    "TS_2_38_2_2_1",
    "TS_2_38_1_3_1",
    "TS_2_38_1_3_1"
  ))
), use.names = TRUE)

# Tower-adjacent temperature profile, 12 positions
for (i in 1:12) {
  bio_map <- rbind(
    bio_map,
    make_map(
      sprintf("TS_2_38_5_%d_1", i),
      c(
        sprintf("TS_2_38_3_%d_1", i),
        sprintf("TS_2_38_2_%d_1", i),
        sprintf("TS_2_38_5_%d_1", i)
      )
    )
  )
}

# ============================================================
# SLUMPBIOMET
# Final schema:
# SWC_12_36_2_1_1 to SWC_12_36_4_1_1
# TS_2_38_2_1_1 to TS_2_38_4_1_1
# TS_2_38_6_1_1 to TS_2_38_6_6_1
# TS_2_38_7_1_1 to TS_2_38_7_6_1
# ============================================================

slump_map <- rbindlist(list(
  # Surface soil moisture
  make_map("SWC_12_36_2_1_1", c(
    "SWC_12_36_1_1_1",
    "SWC_12_36_2_1_1",
    "SWC_12_36_2_1_1"
  )),
  make_map("SWC_12_36_3_1_1", c(
    "SWC_12_36_2_1_1",
    "SWC_12_36_3_1_1",
    "SWC_12_36_3_1_1"
  )),
  make_map("SWC_12_36_4_1_1", c(
    "SWC_12_36_3_1_1",
    "SWC_12_36_4_1_1",
    "SWC_12_36_4_1_1"
  )),
  
  # Surface soil temperature
  make_map("TS_2_38_2_1_1", c(
    "TS_2_38_1_1_1",
    "TS_2_38_5_1_1",
    "TS_2_38_2_1_1"
  )),
  make_map("TS_2_38_3_1_1", c(
    "TS_2_38_2_1_1",
    "TS_2_38_6_1_1",
    "TS_2_38_3_1_1"
  )),
  make_map("TS_2_38_4_1_1", c(
    "TS_2_38_23_1_1",
    "TS_2_38_7_1_1",
    "TS_2_38_4_1_1"
  ))
), use.names = TRUE)

# Slump bare-soil profile: 6 positions
for (i in 1:6) {
  slump_map <- rbind(
    slump_map,
    make_map(
      sprintf("TS_2_38_6_%d_1", i),
      c(
        sprintf("TS_2_38_3_%d_1", i),
        sprintf("TS_2_38_3_%d_1", i),
        sprintf("TS_2_38_6_%d_1", i)
      )
    )
  )
}

# Slump recovered-vegetation profile: 6 positions
for (i in 1:6) {
  slump_map <- rbind(
    slump_map,
    make_map(
      sprintf("TS_2_38_7_%d_1", i),
      c(
        sprintf("TS_2_38_4_%d_1", i),
        sprintf("TS_2_38_4_%d_1", i),
        sprintf("TS_2_38_7_%d_1", i)
      )
    )
  )
}

# ============================================================
# APPLY ONE DATASET'S MAPPING
# ============================================================

prepare_soil_sensors <- function(df, dataset, soil_sensor_map) {
  DT <- as.data.table(copy(df))
  
  if (!"TIMESTAMP" %in% names(DT)) {
    stop(dataset, ": TIMESTAMP column not found.")
  }
  
  timestamp <- as.POSIXct(DT$TIMESTAMP, tz = "UTC")
  
  # Keep original source values untouched during mapping.
  raw <- copy(DT)
  
  targets <- unique(soil_sensor_map$canonical)
  
  # Create/reset final output columns.
  for (nm in targets) {
    set(DT, j = nm, value = rep(NA_real_, nrow(DT)))
  }
  
  missing_sources <- character()
  
  for (j in seq_len(nrow(soil_sensor_map))) {
    target <- soil_sensor_map$canonical[j]
    source <- soil_sensor_map$source[j]
    p <- soil_sensor_map$period[j]
    
    rows <- which(
      !is.na(timestamp) &
        timestamp >= period_start[p] &
        timestamp < period_end[p]
    )
    
    if (!length(rows)) next
    
    if (!source %in% names(raw)) {
      missing_sources <- c(missing_sources, source)
      next
    }
    
    set(
      DT,
      i = rows,
      j = target,
      value = raw[[source]][rows]
    )
  }
  
  # Remove obsolete soil headers; keep final names only.
  old_soil <- grep("^(TS_|SWC_)", names(DT), value = TRUE)
  old_soil <- setdiff(old_soil, targets)
  
  if (length(old_soil)) {
    DT[, (old_soil) := NULL]
  }
  
  # Final output column order from the latest file headers.
  if (dataset == "bio") {
    final_order <- c(
      "TIMESTAMP", "RECORD",
      "TA_2_1_1_1_1", "RH_19_3_1_1_1",
      "WS_16_33_1_1_1", "MWS_16_34_1_1_1",
      "WD_20_35_1_1_1", "PA_4_2_1_1_1",
      "P_RAIN_8_19_1_1_1",
      "SHF_6_37_1_1_1", "SHF_6_37_2_1_1",
      "SD_10_99_1_1_1", "PPFD_7_21_1_1_1",
      "PPFDR_7_23_1_1_1", "SWIN_6_10_1_1_1",
      "SWOUT_6_11_1_1_1", "LWIN_6_14_1_1_1",
      "LWOUT_6_15_1_1_1", "ALB_6_26_1_1_1",
      "RN_6_5_1_1_1",
      "SWC_12_36_1_1_1", "SWC_12_36_1_2_1",
      "SWC_12_36_1_3_1",
      "TS_2_38_1_1_1", "TS_2_38_1_2_1",
      "TS_2_38_1_3_1",
      sprintf("TS_2_38_5_%d_1", 1:12)
    )
  } else if (dataset == "slump") {
    final_order <- c(
      "TIMESTAMP", "RECORD",
      "SHF_6_37_3_1_1", "SHF_6_37_4_1_1",
      "SHF_6_37_5_1_1",
      "SWC_12_36_2_1_1", "SWC_12_36_3_1_1",
      "SWC_12_36_4_1_1",
      "TS_2_38_2_1_1", "TS_2_38_3_1_1",
      "TS_2_38_4_1_1",
      sprintf("TS_2_38_6_%d_1", 6:1),
      sprintf("TS_2_38_7_%d_1", 6:1)
    )
  } else {
    stop("dataset must be 'bio' or 'slump'.")
  }
  
  final_order <- final_order[final_order %in% names(DT)]
  remaining <- setdiff(names(DT), final_order)
  setcolorder(DT, c(final_order, remaining))
  
  if (length(missing_sources)) {
    message(
      dataset, ": source headers not present in loaded data: ",
      paste(unique(missing_sources), collapse = ", ")
    )
  }
  
  message(dataset, ": mapping and final column order complete.")
  DT[]
}
