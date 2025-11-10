

library(data.table)
library(zoo)
library(stringr)
library(plyr)
library(dplyr)
library(tidyr)
library(purrr)


#LOAD AND SPLIT ALLBIOMET AND ALLLOGGER FOR BUCKET UPLOAD#
#ALL LOGGER 

df2 = fread(input = "C:/Users/klynoe/Documents/toolik/raw_data/met/toolik-gth89-AllLogger.dat",skip=4, header = F , na.strings = c('-9999','NA','NaN','NAN','-7999'))
df1 = fread(input = "C:/Users/klynoe/Documents/toolik/raw_data/met/toolik-gth89-AllLogger_until20251027.dat",skip=4, header = F , na.strings = c('-9999','NA','NaN','NAN','-7999'))
h = fread("C:/Users/klynoe/Documents/toolik/raw_data/met/toolik-gth89-AllLogger.dat",skip=1, nrows = 0 , na.strings = c('-9999','NA','NaN','NAN','-7999'))

df = merge(df1,df2, all=T)

names(df) = names(h)

#df = rbind.fill(df,met)
df = df[!duplicated(df$TIMESTAMP),]


df$TIMESTAMP <- as.POSIXct(df$TIMESTAMP, tz = "UTC")

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

met <- met %>%
  mutate(
    TIMESTAMP_fixed = sub("(\\d{4}-\\d{2}-\\d{2}) (\\d{2})(\\d{2})", "\\1 \\2:\\3", TIMESTAMP),
    TIMESTAMP = as.POSIXct(TIMESTAMP_fixed, format = "%Y-%m-%d %H:%M", tz = "UTC")
  )

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
    # Ensure the nested data frame has the same column names and types as units_row
    .x[] = lapply(.x, as.character)  # Convert all columns to character
    
    # Write to CSV
    write.csv(.x, .y, row.names = FALSE)
  }
)







#ALL BIOMET#

df1 = fread(input = "C:/Users/klynoe/Documents/toolik/raw_data/met/toolik-gth89-AllBiomet.dat",skip=4, header = F , na.strings = c('-9999','NA','NaN','NAN','-7999'))
df2 = fread(input = "C:/Users/klynoe/Documents/toolik/raw_data/met/toolik-gth89-AllBiomet_until20251027.dat",skip=4, header = F , na.strings = c('-9999','NA','NaN','NAN','-7999'))
h = fread("C:/Users/klynoe/Documents/toolik/raw_data/met/toolik-gth89-AllBiomet.dat",skip=1, nrows = 0 , na.strings = c('-9999','NA','NaN','NAN','-7999'))

df = merge(df1,df2, all=T)
names(df) = names(h)

#df = rbind.fill(df,met)
df = df[!duplicated(df$TIMESTAMP),]


df$TIMESTAMP <- as.POSIXct(df$TIMESTAMP, tz = "UTC")

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

met <- met %>%
  mutate(
    TIMESTAMP_fixed = sub("(\\d{4}-\\d{2}-\\d{2}) (\\d{2})(\\d{2})", "\\1 \\2:\\3", TIMESTAMP),
    TIMESTAMP = as.POSIXct(TIMESTAMP_fixed, format = "%Y-%m-%d %H:%M", tz = "UTC")
  )

data_nested = met %>%
  mutate(TIMESTAMP = as.POSIXct(TIMESTAMP, format = "%Y-%m-%d %H:%M:%S")) %>%
  mutate(year_month = format(TIMESTAMP, "%Y%m")) %>%
  group_by(year_month) %>%
  nest() %>%
  ungroup() %>%  # Important to prevent errors with grouped data
  mutate(file_path = paste0(base_path, "toolik_gth89_biomet_", year_month, ".csv"))

walk2(
  .x = data_nested$data, 
  .y = data_nested$file_path, 
  ~ {
    # Ensure the nested data frame has the same column names and types as units_row
    .x[] = lapply(.x, as.character)  # Convert all columns to character
    
    # Write to CSV
    write.csv(.x, .y, row.names = FALSE)
  }
)






