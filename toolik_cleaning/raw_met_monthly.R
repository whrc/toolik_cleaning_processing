
rm(list = ls())
library(data.table)
library(zoo)
library(stringr)
library(plyr)
library(dplyr)
library(tidyr)
library(purrr)


#LOAD AND SPLIT ALLBIOMET AND ALLLOGGER FOR BUCKET UPLOAD#
#ALL LOGGER 



# ============================================================
# PATHS
# ============================================================

raw_path <- "C:/Users/klynoe/Documents/toolik/raw_data/met/annual_met"

base_path <- "C:/Users/klynoe/Documents/toolik/R_outputs/met/"

dir.create(
  base_path,
  recursive = TRUE,
  showWarnings = FALSE
)

na_strings <- c(
  "-9999",
  "NA",
  "NaN",
  "NAN",
  "-7999"
)


# ============================================================
# FUNCTION TO LOAD VARIABLE-STRUCTURE DAT FILES
# ============================================================
# Each file can have a different number of columns.
# Each file gets its own header.

load_toolik_files <- function(files) {
  
  # -----------------------------
  # Read headers
  # -----------------------------
  
  h_list <- lapply(
    files,
    fread,
    skip = 1,
    nrows = 0,
    na.strings = na_strings
  )
  
  
  # -----------------------------
  # Read data
  # -----------------------------
  
  dat_list <- lapply(
    files,
    fread,
    skip = 4,
    header = FALSE,
    na.strings = na_strings,
    fill = TRUE
  )
  
  
  # -----------------------------
  # Apply individual headers
  # -----------------------------
  
  for (i in seq_along(dat_list)) {
    
    h <- names(h_list[[i]])
    
    # If data has MORE columns than header
    if (length(h) < ncol(dat_list[[i]])) {
      
      h <- c(
        h,
        paste0(
          "extra_",
          seq_len(
            ncol(dat_list[[i]]) - length(h)
          )
        )
      )
    }
    
    # If header has MORE columns than data
    if (length(h) > ncol(dat_list[[i]])) {
      
      h <- h[
        seq_len(
          ncol(dat_list[[i]])
        )
      ]
    }
    
    names(dat_list[[i]]) <- h
  }
  
  
  # -----------------------------
  # Combine all files
  # -----------------------------
  
  df <- rbindlist(
    dat_list,
    use.names = TRUE,
    fill = TRUE
  )
  
  
  # -----------------------------
  # Timestamp
  # -----------------------------
  
  df[, TIMESTAMP := as.POSIXct(
    TIMESTAMP,
    tz = "UTC"
  )]
  
  
  # Remove invalid timestamps
  
  df <- df[
    !is.na(TIMESTAMP)
  ]
  
  
  # -----------------------------
  # Sort and remove duplicates
  # -----------------------------
  
  setorder(
    df,
    TIMESTAMP
  )
  
  df <- unique(
    df,
    by = "TIMESTAMP"
  )
  
  
  return(df)
}


# ============================================================
# ALL LOGGER
# ============================================================

logger_files <- list.files(
  path = raw_path,
  pattern = "^toolik-gth89-AllLogger(_.*)?\\.dat$",
  recursive = TRUE,
  full.names = TRUE
)

basename(logger_files)


logger <- load_toolik_files(
  logger_files
)


# ============================================================
# LOGGER DATE CUTOFF
# ============================================================

timestamp_cutoff <- as.POSIXct(
  "2026-03-01 00:00",
  format = "%Y-%m-%d %H:%M",
  tz = "UTC"
)

logger <- logger[
  TIMESTAMP >= timestamp_cutoff
]


# ============================================================
# COMPLETE 30-MINUTE TIME GRID
# ============================================================

tsdf <- data.table(
  TIMESTAMP = seq(
    from = min(logger$TIMESTAMP),
    to = max(logger$TIMESTAMP),
    by = 30 * 60
  )
)


logger <- merge(
  tsdf,
  logger,
  by = "TIMESTAMP",
  all.x = TRUE
)


# ============================================================
# SPLIT LOGGER BY MONTH
# ============================================================

logger <- logger %>%
  mutate(
    TIMESTAMP = as.POSIXct(
      TIMESTAMP,
      tz = "UTC"
    ),
    year_month = format(
      TIMESTAMP,
      "%Y%m"
    )
  )


logger_nested <- logger %>%
  group_by(year_month) %>%
  nest() %>%
  ungroup() %>%
  mutate(
    file_path = file.path(
      base_path,
      paste0(
        "toolik_gth89_logger_",
        year_month,
        ".csv"
      )
    )
  )


# ============================================================
# WRITE MONTHLY LOGGER FILES
# ============================================================

walk2(
  logger_nested$data,
  logger_nested$file_path,
  ~ {
    
    dat <- .x
    
    
    # -----------------------------
    # Remove column 2 (RECORD)
    # -----------------------------
    
    # dat <- dat %>%
    #  select(-2)
    
    
    # -----------------------------
    # Remove completely empty columns
    # -----------------------------
    
    dat <- dat %>%
      select(
        where(
          ~ !all(is.na(.))
        )
      )
    
    
    # -----------------------------
    # Format timestamp
    # -----------------------------
    
    dat <- dat %>%
      mutate(
        TIMESTAMP = format(
          TIMESTAMP,
          "%Y-%m-%d %H:%M"
        )
      )
    
    
    # -----------------------------
    # Write CSV
    # -----------------------------
    
    write.csv(
      dat,
      .y,
      row.names = FALSE,
      na = "NA"
    )
  }
)

#####################################
                               #ALL BIOMET#
                                ####~~####


# 1. Define folder path and list all AllBiomet files

# ============================================================
# 1. PATHS
# ============================================================

fp <- "C:/Users/klynoe/Documents/toolik/raw_data/met/annual_met/"

base_path <- "C:/Users/klynoe/Documents/toolik/R_outputs/met/"

dir.create(base_path, recursive = TRUE, showWarnings = FALSE)


# ============================================================
# 2. FIND ALL BIOMET FILES
# ============================================================

files <- list.files(
  path = fp,
  pattern = "^toolik-gth89-AllBiomet(_.*)?\\.dat$",
  recursive = TRUE,
  full.names = TRUE
)

files
length(files)


# ============================================================
# 3. READ EACH FILE
# ============================================================
# Each file gets its own header because the column structure
# may change between files.
#
# Header is on line 2 (skip = 1)
# Data begin on line 5 (skip = 4)

h_list <- lapply(
  files,
  fread,
  skip = 1,
  nrows = 0
)

dat_list <- lapply(
  files,
  fread,
  skip = 4,
  header = FALSE,
  na.strings = c(
    "-9999",
    "NA",
    "NaN",
    "NAN",
    "-7999"
  ),
  fill = TRUE
)


# ============================================================
# 4. APPLY EACH FILE'S OWN HEADER
# ============================================================

for (i in seq_along(dat_list)) {
  
  # Get header
  h <- names(h_list[[i]])
  
  # Make sure the number of names matches the data
  if (length(h) < ncol(dat_list[[i]])) {
    
    # Add names for unexpected extra columns
    h <- c(
      h,
      paste0(
        "extra_",
        seq_len(ncol(dat_list[[i]]) - length(h))
      )
    )
    
  } else if (length(h) > ncol(dat_list[[i]])) {
    
    # Trim header if necessary
    h <- h[seq_len(ncol(dat_list[[i]]))]
  }
  
  names(dat_list[[i]]) <- h
}


# ============================================================
# 5. COMBINE ALL FILES
# ============================================================
# use.names = TRUE + fill = TRUE means:
#
# File 1: TIMESTAMP, AirT, RH, Wind
# File 2: TIMESTAMP, AirT, RH, Wind, PAR
#
# becomes:
#
# TIMESTAMP, AirT, RH, Wind, PAR
#
# with NA where PAR did not exist.

df_bio <- rbindlist(
  dat_list,
  use.names = TRUE,
  fill = TRUE
)


# ============================================================
# 6. CLEAN TIMESTAMP
# ============================================================

df_bio[, TIMESTAMP := as.POSIXct(
  TIMESTAMP,
  tz = "UTC"
)]


# Remove rows with invalid/missing timestamps
df_bio <- df_bio[
  !is.na(TIMESTAMP)
]


# ============================================================
# 7. REMOVE DUPLICATE TIMESTAMPS
# ============================================================
# If multiple files overlap, retain the first occurrence.

setorder(df_bio, TIMESTAMP)

df_bio <- unique(
  df_bio,
  by = "TIMESTAMP"
)


# ============================================================
# 8. CREATE COMPLETE 30-MINUTE TIME GRID
# ============================================================

start_date <- as.POSIXct(
  "2026-03-01 00:00:00",
  tz = "UTC"
)

stop_date <- as.POSIXct(
  "2026-07-30 23:30:00",
  tz = "UTC"
)

tsdf <- data.table(
  TIMESTAMP = seq(
    from = start_date,
    to = stop_date,
    by = 30 * 60
  )
)


# ============================================================
# 9. MERGE BIOMET DATA ONTO COMPLETE TIME GRID
# ============================================================

met <- merge(
  tsdf,
  df_bio,
  by = "TIMESTAMP",
  all.x = TRUE
)


# ============================================================
# 10. SPLIT BY MONTH
# ============================================================

met <- met %>%
  mutate(
    TIMESTAMP = as.POSIXct(
      TIMESTAMP,
      tz = "UTC"
    ),
    year_month = format(
      TIMESTAMP,
      "%Y%m"
    )
  )


# ============================================================
# 11. NEST BY MONTH
# ============================================================

data_nested <- met %>%
  group_by(year_month) %>%
  nest() %>%
  ungroup() %>%
  mutate(
    file_path = file.path(
      base_path,
      paste0(
        "toolik_gth89_biomet_",
        year_month,
        ".csv"
      )
    )
  )


# ============================================================
# 12. WRITE MONTHLY FILES
# ============================================================

walk2(
  data_nested$data,
  data_nested$file_path,
  ~ {
    
    dat <- .x 
    
    # Remove column 2 (RECORD)
    dat <- dat %>%
      select(-2)
    
    # Remove columns that are completely NA
    dat <- dat %>%
      select(
        where(~ !all(is.na(.)))
      )
    
    # Format timestamp
    dat <- dat %>%
      mutate(
        TIMESTAMP = format(
          TIMESTAMP,
          "%Y-%m-%d %H:%M"
        )
      )
    
    write.csv(
      dat,
      .y,
      row.names = FALSE,
      na = "NA"
    )
  }
)






   #########PRINT FULL BIOMET WITH UNITS#####
~~~~~~~~~~#NEEDS UPDATE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!<--------------!
           #1# GET RID OF THE RECORD ROW (COL2) set time

met = met [,-2]

met$TIMESTAMP <- format(
  as.POSIXct(met$TIMESTAMP, tz = "UTC"),"%Y-%m-%d %H:%M")

#2# add units

units <- c(
  "yyyy-mm-dd HH:MM",  # TIMESTAMP etc - NEEDS ADJUSTMENT FROM 26/04/2026 
  "degC",
  "%",
  "m/s",
  "W/m^2",
  "W/m^2",
  "m",
  "umol/s/m^2",
  "umol/s/m^2",
  "W/m^2",
  "W/m^2",
  "W/m^2",
  "W/m^2",
  "W/m^2",
  "W/m^2",
  "m^3/m^3",
  "m^3/m^3",
  "m^3/m^3",
  "degC",
  "degC",
  "degC"
)

#3# check alignement

length(units) == ncol(met)

#4# make row and bind to met

units_row <- as.data.frame(t(units), stringsAsFactors = FALSE)
names(units_row) <- names(met)
met_out <- rbind(units_row, met)

#5# save
base_path = "C:/Users/klynoe/Documents/toolik/R_outputs/met/"

write.csv(met_out, "toolik-gth89-AllBiomet_1.csv", row.names = FALSE)



