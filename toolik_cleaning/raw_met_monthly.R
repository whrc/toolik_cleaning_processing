
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


setwd("C:/Users/klynoe/Documents/toolik/raw_data/met/")


na_strings <- c("-9999","NA","NaN","NAN","-7999")

files <- c(
  "toolik-gth89-AllLogger.dat",
  "toolik-gth89-AllLogger_until20251027.dat",
  "toolik-gth89-AllLogger_until20251203.dat",
  "toolik-gth89-AllLogger_until20251209.dat",
  "toolik-gth89-AllLogger_until20251212.dat"
)

# read header once (authoritative column set)
h <- fread(files[1], skip = 1, nrows = 0, na.strings = na_strings)

# read all files, allow different column counts
df <- rbindlist(
  lapply(files, fread,
         skip = 4,
         header = FALSE,
         na.strings = na_strings,
         fill = TRUE),
  fill = TRUE
)

# enforce column names
setnames(df, names(h))

# convert TIMESTAMP safely
df[, TIMESTAMP := as.POSIXct(TIMESTAMP, tz = "UTC")]

# deduplicate
setkey(df, TIMESTAMP)
df <- unique(df)

#df = rbind.fill(df,met)
df = df[!duplicated(df$TIMESTAMP),]


timestamp_cutoff <- as.POSIXct("2025-05-01 14:00", format = "%Y-%m-%d %H:%M", tz = "UTC")
df <- df %>%
  dplyr::filter(TIMESTAMP >= timestamp_cutoff)


# Create a complete sequence of timestamps at 30-minute intervals
tsdf <- data.frame(TIMESTAMP = seq(
  from = min(df$TIMESTAMP),
  to = max(df$TIMESTAMP),
  by = 60*30
))


df = merge(tsdf,df,by = 'TIMESTAMP',all.x = T)

#####SPLIT AND WRITE ALLOGGER####

base_path = "C:/Users/klynoe/Documents/toolik/R_outputs/met/"

met=df

met <- met %>%
  mutate(TIMESTAMP = as.POSIXct(TIMESTAMP, tz = "UTC"))


data_nested = met %>%
  mutate(TIMESTAMP = as.POSIXct(TIMESTAMP, format = "%Y-%m-%d %H:%M:%S")) %>%
  mutate(year_month = format(TIMESTAMP, "%Y%m")) %>%
  group_by(year_month) %>%
  nest() %>%
  ungroup() %>%  # Important to prevent errors with grouped data
  mutate(file_path = paste0(base_path, "toolik_gth89_logger_", year_month, ".csv"))

walk2(
  .x = data_nested$data, 
  .y = data_nested$file_path, 
  ~ {
    .x <- .x %>%
      mutate(
        TIMESTAMP = format(as.POSIXct(TIMESTAMP, tz = "UTC"),
                           "%Y-%m-%d %H:%M")
      )
    
    write.csv(.x, .y, row.names = FALSE)
  }
)



#ALL BIOMET#
rm(list = ls())
setwd("C:/Users/klynoe/Documents/toolik/raw_data/met/")


na_strings <- c("-9999","NA","NaN","NAN","-7999")

files <- c(
  "toolik-gth89-AllBiomet.dat",
  "toolik-gth89-AllBiomet_until20251027.dat",
  "toolik-gth89-AllBiomet_until20251203.dat",
  "toolik-gth89-AllBiomet_until20251209.dat",
  "toolik-gth89-AllBiomet_until20251212.dat"
)

# read header once (authoritative column set)
h <- fread(files[1], skip = 1, nrows = 0, na.strings = na_strings)

# read all files, allow different column counts
df <- rbindlist(
  lapply(files, fread,
         skip = 4,
         header = FALSE,
         na.strings = na_strings,
         fill = TRUE),
  fill = TRUE
)

# enforce column names
setnames(df, names(h))

# convert TIMESTAMP safely
df[, TIMESTAMP := as.POSIXct(TIMESTAMP, tz = "UTC")]

# deduplicate
setkey(df, TIMESTAMP)
df <- unique(df)
df = df[!duplicated(df$TIMESTAMP),]


timestamp_cutoff <- as.POSIXct("2025-05-01 14:00", format = "%Y-%m-%d %H:%M", tz = "UTC")
df <- df %>%
  dplyr::filter(TIMESTAMP >= timestamp_cutoff)


# Create a complete sequence of timestamps at 30-minute intervals
tsdf <- data.frame(TIMESTAMP = seq(
  from = min(df$TIMESTAMP),
  to = max(df$TIMESTAMP),
  by = 60*30
))


df = merge(tsdf,df,by = 'TIMESTAMP',all.x = T)


base_path = "C:/Users/klynoe/Documents/toolik/R_outputs/met/"

met=df

# -----------------------------
# Ensure TIMESTAMP is POSIXct
# -----------------------------
met <- met %>%
  mutate(TIMESTAMP = as.POSIXct(TIMESTAMP, tz = "UTC"))

# -----------------------------
# Nest by year_month and prepare file paths
# -----------------------------
data_nested <- met %>%
  mutate(year_month = format(TIMESTAMP, "%Y%m")) %>%
  group_by(year_month) %>%
  nest() %>%
  ungroup() %>%
  mutate(file_path = paste0(base_path, "toolik_gth89_biomet_", year_month, ".csv"))

# -----------------------------
# Write each nested data frame
# -----------------------------
walk2(
  .x = data_nested$data, 
  .y = data_nested$file_path, 
  ~ {
    # Format TIMESTAMP explicitly before writing
    .x <- .x %>%
      mutate(TIMESTAMP = format(TIMESTAMP, "%Y-%m-%d %H:%M"))
    
    write.csv(.x, .y, row.names = FALSE)
  }
)





