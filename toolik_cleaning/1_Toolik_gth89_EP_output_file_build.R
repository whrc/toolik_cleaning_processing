

rm(list = ls())

library(data.table)
library(plyr)
library(ggplot2)
library(cowplot)
library(openair)
library(viridis)
library(dplyr)
library(lubridate)
library(tidyr)

####BELOW LINES IS FOR MERGING MULTIPLE MONTHS/FILES OF DATA####
####FOR SINGLE MONTH FILE PREPARATION SEE 2ND CHAPTHER 
####################################################################

#load in the full output flux data ##################
fp = 'C:/Users/klynoe/Documents/toolik/eddypro_output/first_annual'
#fp = 'C:/Users/klynoe/Documents/pond_inlet/202410_full/eddypro/full_output/2024'
files = list.files(path = fp,pattern = '*full_output.+csv$',recursive = T,full.names = T)

#load the headers and data into their own lists
h   = lapply(files, fread,skip = 1,nrow = 0)
dat = lapply(files, fread,skip = 3,header = F,na.strings=c('-9999'))

ls()

#assign the headers to the data
for (i in 1:length(h)) {
  names(dat[[i]]) = names(h[[i]])
}

#df=as.data.frame(dat)
#make all the lists into one dataframe
df = dat[[1]]
for (i in 2:length(dat)) {
  df = rbind.fill(df,dat[[i]])
}

#create a timestamp from the date and time
df$ts = as.POSIXct(x = paste(df$date,df$time,sep = ' '),tz = 'UTC')
df = df[!duplicated(df$ts),]


#take the min and max date rounding to the nearest full month
mindate = floor_date(min(df$ts),unit = 'month')
maxdate = ceiling_date(max(df$ts),unit = 'month')

#create a timestamp variable every half hour to the full months
ts = seq(from = mindate,to = maxdate,by = 60*30)
ts = as.data.frame(ts)

ts = seq(min(df$ts),max(df$ts),by = 60*30)
ts = as.data.frame(ts)


#merge with the flux data frame to create NAs where data is missing
df = merge(ts,df,by = 'ts',all.x = T)


#save off flux data
write.csv(df,'C:/Users/klynoe/Documents/toolik/R_outputs/flux/toolik_fluxes_202505_202607.csv',row.names = F)


################multiple met



# 1. Define folder path and list all AllBiomet files
fp <- "C:/Users/klynoe/Documents/toolik/raw_data/met/annual_met/"
files <- list.files(path = fp, pattern = "toolik-gth89-AllBiomet_.+\\.dat$", recursive = TRUE, full.names = TRUE)

# 2. Bulk load headers and data matching your specific Toolik format
h_list <- lapply(files, fread, skip = 1, nrows = 0)
dat_list <- lapply(files, fread, skip = 4, header = FALSE, 
                   na.strings = c('-9999','NA','NaN','NAN','-7999'))

# 3. Dynamically map headers to their respective datasets
for (i in seq_along(dat_list)) {
  names(dat_list[[i]]) <- names(h_list[[i]])
}

# 4. Ultra-fast bind matching by column names (fill = TRUE handles new columns)
df_master <- rbindlist(dat_list, use.names = TRUE, fill = TRUE)

# 5. Clean up timestamps and drop duplicate rows
df_master$TIMESTAMP <- as.POSIXct(df_master$TIMESTAMP, tz = "UTC")
df_master <- unique(df_master, by = "TIMESTAMP")

# 6. Generate a continuous 30-minute time grid starting from your specific date
start_date <- as.POSIXct("2025-05-06 09:30:00", tz = "UTC")
stop_date = as.POSIXct("2026-07-30 23:30:00", tz = "UTC")
ts_grid <- data.frame(TIMESTAMP = seq(
  from = start_date,
  to = stop_date,
  by = 60 * 30
))

# 7. Merge onto grid to reveal NAs where records are missing
df_master <- merge(ts_grid, df_master, by = "TIMESTAMP", all.x = TRUE)
###########################

write.csv(df_master,'C:/Users/klynoe/Documents/toolik/R_outputs/met/toolik_met_202505_202607.csv',row.names = F)
###########

###MERGE FLUX AND MET!!!!!!##########

df$TIMESTAMP = df$ts

flux = df
met = df_master

# 1. Ensure both master files are formatted as data.tables
setDT(met)
setDT(flux)

# 2. Force both TIMESTAMP columns to be identical in format and timezone
met$TIMESTAMP <- as.POSIXct(met$TIMESTAMP, tz = "UTC")
flux$TIMESTAMP <- as.POSIXct(flux$TIMESTAMP, tz = "UTC")

# 3. Combine them using a full outer join to prevent any data loss
DF_1 <- merge(met, flux, by = "TIMESTAMP", all = TRUE)


###############
##ADD TOMST - REFORMAT AND MERGE

tomst = read.csv("C:/Users/klynoe/Documents/toolik/R_outputs/TOMST_combined_long_20260728.csv", header = T)


# Create variable names
tomst_wide <- tomst %>%
  mutate(
    logger = recode(Logger,
                    "center" = "C",
                    "east"   = "E",
                    "west"   = "W",
                    "north"  = "N",
                    "south"  = "S"
    ),
    temp_var = case_when(
      Depth == "+15cm" ~ paste0("TMS_", logger, "_T_1"),
      Depth == "0cm"   ~ paste0("TMS_", logger, "_T_2"),
      Depth == "-10cm" ~ paste0("TMS_", logger, "_T_3")
    ),
    vwc_var = if_else(
      Depth == "-10cm",
      paste0("TMS_", logger, "_VWC"),
      NA_character_
    )
  ) %>%
  
  # Put Temperature and VWC into one long column of variable names
  pivot_longer(
    cols = c(Temperature, VWC),
    names_to = "Type",
    values_to = "Value"
  ) %>%
  mutate(
    Variable = case_when(
      Type == "Temperature" ~ temp_var,
      Type == "VWC" ~ vwc_var
    )
  ) %>%
  filter(!is.na(Variable)) %>%
  select(TIMESTAMP, Variable, Value) %>%
  pivot_wider(
    names_from = Variable,
    values_from = Value
  )

tomst_wide <- tomst_wide %>%
  select(
    TIMESTAMP,
    TMS_C_T_1, TMS_C_T_2, TMS_C_T_3, TMS_C_VWC,
    TMS_E_T_1, TMS_E_T_2, TMS_E_T_3, TMS_E_VWC,
    TMS_W_T_1, TMS_W_T_2, TMS_W_T_3, TMS_W_VWC,
    TMS_N_T_1, TMS_N_T_2, TMS_N_T_3, TMS_N_VWC,
    TMS_S_T_1, TMS_S_T_2, TMS_S_T_3, TMS_S_VWC
  )
# 3. Clean timestamps—handles hourly formats (e.g., "2025-08-04 10:00") cleanly
tomst_wide$TIMESTAMP <- as.POSIXct(tomst_wide$TIMESTAMP, format = "%Y-%m-%d %H:%M", tz = "UTC")

setDT(DF_1)
setDT(tomst_wide)
#

flux_biomet_tms <- merge(DF_1, tomst_wide, by = "TIMESTAMP", all = TRUE)


# 1. Ensure it is a data.table
setDT(flux_biomet_tms)

# 2. Get names of columns to process (skipping TIMESTAMP)
clean_cols <- setdiff(names(flux_biomet_tms), "TIMESTAMP")

# 3. Fast clean loop across all columns
for (col in clean_cols) {
  # Fix standard bad values
  set(flux_biomet_tms, i = which(flux_biomet_tms[[col]] == -9999), j = col, value = NA)
  set(flux_biomet_tms, i = which(flux_biomet_tms[[col]] == -7999), j = col, value = NA)
  
  # Catch hidden NaNs or Infinite spikes in numeric columns
  if (is.numeric(flux_biomet_tms[[col]])) {
    set(flux_biomet_tms, i = which(is.nan(flux_biomet_tms[[col]])), j = col, value = NA)
    set(flux_biomet_tms, i = which(is.infinite(flux_biomet_tms[[col]])), j = col, value = NA)
  }
}

flux_biomet_tms$TIMESTAMP = flux_biomet_tms$ts
############
##SAVE!!!###
flux_biomet_tms$ts <- format(as.POSIXct(flux_biomet_tms$ts), format ="%Y-%m-%d %H:%M:%S" )
flux_biomet_tms$TIMESTAMP <- format(as.POSIXct(flux_biomet_tms$TIMESTAMP), format ="%Y-%m-%d %H:%M:%S" )
write.csv(
  flux_biomet_tms,
  "C:/Users/klynoe/Documents/toolik/R_outputs/GTH89_flux_biomet_tms_combined_2025_2026.csv"
  , row.names = F)

################################















################
####SINGLE FILE EDDYPRO PREPARATION####
####################################

fp = fp = 'C:/Users/klynoe/Documents/toolik/eddypro_output/2026'### CHANGE MONTH TO WHICH YOU WANT TO LOAD
file = list.files(path = fp,pattern = '*full_output.+csv$',recursive = T,full.names = T)


#load the headers and data into their own lists
h   = lapply(file, fread,skip = 1,nrow = 0)
dat = lapply(file, fread,skip = 3,header = F,na.strings=c('-9999'))

#assign the headers to the data
for (i in 1:length(h)) {
  names(dat[[i]]) = names(h[[i]])
}
#turn the data into a dataframe
df =as.data.frame(do.call(cbind, dat))

#create a timestamp from the date and time
df$ts = as.POSIXct(x = paste(df$date,df$time,sep = ' '),tz = 'UTC')
df = df[!duplicated(df$ts),]

#save off flux data -----> remember to rename the output file as desired
write.csv(df,'C:/Users/klynoe/Documents/toolik/R_outputs/flux/toolik_fluxes_202603_202604.csv',row.names = F)

############################################
##########################################################
##METBUILD#####
  ########


df = fread(input = "C:/Users/klynoe/Documents/toolik/raw_data/met/toolik-gth89-AllBiomet.dat",skip=4, header = F , na.strings = c('-9999','NA','NaN','NAN','-7999'))
#df2 = fread(input = "C:/Users/klynoe/Documents/toolik/raw_data/met/toolik-gth89-AllBiomet_until20260424.dat",skip=4, header = F , na.strings = c('-9999','NA','NaN','NAN','-7999'))
h = fread("C:/Users/klynoe/Documents/toolik/raw_data/met/toolik-gth89-AllBiomet.dat",skip=1, nrows = 0 , na.strings = c('-9999','NA','NaN','NAN','-7999'))

#df = merge(df1,df2, all = T)

names(df) = names(h)

#df = rbind.fill(df,met)
df = df[!duplicated(df$TIMESTAMP),]


df$TIMESTAMP <- as.POSIXct(df$TIMESTAMP, tz = "UTC")

timestamp_cutoff <- as.POSIXct("2026-01-01 14:00", format = "%Y-%m-%d %H:%M", tz = "UTC")
df <- df %>%
  dplyr::filter(TIMESTAMP >= timestamp_cutoff)


# Create a complete sequence of timestamps at 30-minute intervals
tsdf <- data.frame(TIMESTAMP = seq(
  from = min(df$TIMESTAMP),
  to = max(df$TIMESTAMP),
  by = 60*30
))


df = merge(tsdf,df,by = 'TIMESTAMP',all.x = T)

df_out <- df

df_out$TIMESTAMP <- format(
  df_out$TIMESTAMP,
  "%Y-%m-%d %H:%M:%S",
  tz = "UTC"
)

write.csv(
  df_out,
  "C:/Users/klynoe/Documents/toolik/R_outputs/met/toolik-gth89-AllBiomet2605.csv",
  row.names = FALSE,
  quote = TRUE
)


###############






























#write.csv(df, "C:/Users/klynoe/Documents/toolik/R_outputs/met/toolik-gth89-AllBiomet2606.csv", row.names = F)



#############USE THIS FOR EDDYPRO! BUT GO TO EXCEL AND MAKE ALL NUMERIC AND RESET YYYY-MM-DD HH:MM - EP WONT READ IF NOT

# 1. Read the data, skipping the comment line (original file line 1)
raw_df <- read.table("C:/Users/klynoe/Documents/toolik/raw_data/met/CR6Series_AllBiomet_2026_07_24_15_39_33.dat", 
                     sep = ",", 
                     header = T, 
                     skip = 1, 
                     na.strings = c('-9999','NA','NaN','NAN','-7999'),
                     stringsAsFactors = FALSE)


df <- raw_df[-2, ]
#rownames(df) <- NULL # Reset row numbers so Units are explicitly Row 1

# 3. Explicitly overwrite Row 1, Column 1 with the literal format string
df[1, 1] <- "yyyy-mm-dd HH:MM"

# 4. Convert all data rows (Row 2 and below) in Column 1 to POSIXct
# We store them in a list or character vector to mix text (Row 1) and dates (Rows 2+)
date_pool <- as.POSIXct(df[-1, 1], format = "%Y-%m-%d %H:%M:%S")

# 5. Combine the format string and POSIXct objects back into Column 1
df[, 1] <- c("yyyy-mm-dd HH:MM", as.character(date_pool))


df = merge(tsdf,df,by = 'TIMESTAMP',all.x = T)

write.csv(df, "C:/Users/klynoe/Documents/toolik/R_outputs/met/toolik-gth89-AllBiomet2607.csv", row.names = F)
#####################

####################
###READ ALL RAw MONTHLY BIOMET FILES AND MERGE TO ONE
####ADD TOMST AND FLUX####

fp = 'C:/Users/klynoe/Documents/toolik/R_outputs/met/monthly'

files = list.files(path = fp,pattern = '*biomet.+csv$',recursive = T,full.names = T)

dt_list <- lapply(files, fread)

biomet = rbindlist(dt_list, fill = TRUE)

biomet$TIMESTAMP <- as.POSIXct(biomet$TIMESTAMP, format = "%Y-%m-%d %H:%M", tz = "UTC")

biomet =  merge(ts,biomet,by = 'ts', all.x = T)
biomet$TIMESTAMP = biomet$ts

flux_biomet = merge(biomet, df, by = "ts")
###########################################################




