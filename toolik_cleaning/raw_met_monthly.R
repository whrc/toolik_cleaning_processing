

library(data.table)
library(zoo)
library(stringr)
library(plyr)
library(dplyr)



#LOAD AND SPLIT ALLBIOMET AND ALLLOGGER FOR BUCKET UPLOAD#
#ALL LOGGER 

df = fread(input = "C:/Users/klynoe/Documents/toolik/raw_data/toolik-gth89-AllLogger.dat",skip=4, header = F , na.strings = c('-9999','NA','NaN','NAN','-7999'))

h = fread("C:/Users/klynoe/Documents/toolik/raw_data/toolik-gth89-AllLogger.dat",skip=1, nrows = 0 , na.strings = c('-9999','NA','NaN','NAN','-7999'))

#df = fread('C:/Users/klynoe/Documents/resolute_bay/R_outputs/resolute_bay_met_merged.csv', header = T
#, na.strings = c('-9999','NA','NaN','NAN','-7999'))


names(df) = names(h)

#df = rbind.fill(df,met)
df = df[!duplicated(df$TIMESTAMP),]


df$TIMESTAMP <- as.POSIXct(df$TIMESTAMP, tz = "UTC")

timestamp_cutoff <- as.POSIXct("2025-05-01 14:00", format = "%Y-%m-%d %H:%M", tz = "UTC")


base_path = "C:/Users/klynoe/Documents/toolik/R_outputs/met/"


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

df = fread(input = "C:/Users/klynoe/Documents/toolik/raw_data/toolik-gth89-AllBiomet.dat",skip=4, header = F , na.strings = c('-9999','NA','NaN','NAN','-7999'))

h = fread("C:/Users/klynoe/Documents/toolik/raw_data/toolik-gth89-AllBiomet.dat",skip=1, nrows = 0 , na.strings = c('-9999','NA','NaN','NAN','-7999'))

#df = fread('C:/Users/klynoe/Documents/resolute_bay/R_outputs/resolute_bay_met_merged.csv', header = T
#, na.strings = c('-9999','NA','NaN','NAN','-7999'))


names(df) = names(h)

#df = rbind.fill(df,met)
df = df[!duplicated(df$TIMESTAMP),]


df$TIMESTAMP <- as.POSIXct(df$TIMESTAMP, tz = "UTC")

timestamp_cutoff <- as.POSIXct("2025-05-01 14:00", format = "%Y-%m-%d %H:%M", tz = "UTC")

base_path = "C:/Users/klynoe/Documents/toolik/R_outputs/met/"


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






