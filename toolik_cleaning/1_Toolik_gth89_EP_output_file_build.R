

rm(list = ls())

library(data.table)
library(plyr)
library(ggplot2)
library(cowplot)
library(openair)
library(viridis)
library(dplyr)
library(lubridate)

####BELOW LINES IS FOR MERGING MULTIPLE MONTHS/FILES OF DATA####
####FOR SINGLE MONTH FILE PREPARATION SEE 2ND CHAPTHER 
####################################################################

#load in the full output flux data ##################
fp = 'C:/Users/klynoe/Documents/toolik/eddypro_output/2025'
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
write.csv(df,'C:/Users/klynoe/Documents/toolik/R_outputs/flux/toolik_fluxes_202505_202510.csv',row.names = F)


################
####SINGLE FILE EDDYPRO PREPARATION####
####################################

fp = fp = 'C:/Users/klynoe/Documents/toolik/eddypro_output/2025'### CHANGE MONTH TO WHICH YOU WANT TO LOAD
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
write.csv(df,'C:/Users/klynoe/Documents/pond_inlet/R_outputs/pond_inlet_fluxes_merged_2025_05_31.csv',row.names = F)

############################################
##########################################################
##METBUILD#####
  ########


df1 = fread(input = "C:/Users/klynoe/Documents/toolik/raw_data/met/toolik-gth89-AllBiomet.dat",skip=4, header = F , na.strings = c('-9999','NA','NaN','NAN','-7999'))
df2 = fread(input = "C:/Users/klynoe/Documents/toolik/raw_data/met/toolik-gth89-AllBiomet_until20251027.dat",skip=4, header = F , na.strings = c('-9999','NA','NaN','NAN','-7999'))
h = fread("C:/Users/klynoe/Documents/toolik/raw_data/met/toolik-gth89-AllBiomet.dat",skip=1, nrows = 0 , na.strings = c('-9999','NA','NaN','NAN','-7999'))

df = merge(df1,df2, all = T)

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


write.csv(df, "C:/Users/klynoe/Documents/toolik/R_outputs/met/toolik-gth89-AllBiomet.csv", row.names = F)








