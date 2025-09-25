

#CALCULATE DAILY AVERAGES OF FLUXES

rm(list = ls())

library(data.table)
library(ggplot2)
library(cowplot)
library(openair)
library(plotrix)
library(signal)
library(svMisc)
library(zoo)
library(stringr)
library(plyr)
library(dplyr)
library(purrr)
library(tidyr)

### 1 - READ/SUBSET DATA
df = fread('C:/Users/klynoe/Documents/toolik/R_outputs/toolik_Flux_Met_Clean_august_25.csv')

df$TIMESTAMP = paste(df$date, df$time)
df$TIMESTAMP = as.POSIXct(df$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

df$TIMESTAMP = df$ts

# Ensure TIMESTAMP is in POSIXct format with UTC timezone
df$TIMESTAMP <- as.POSIXct(df$TIMESTAMP, tz = "UTC")


#check time stamps to ensure we have a complete series
TIMESTAMP = seq(from = min(df$TIMESTAMP), to = max(df$TIMESTAMP),by = 60*30,tz = "UTC")
tsdf = as.data.frame(TIMESTAMP)

df = merge(tsdf,df,by = "TIMESTAMP",all = T)

df_07 <- df[format(df$TIMESTAMP, "%m") == "07", ]

### 2 - CALCULATE MEAN NEE, ER, ....

df_daily <- aggregate(co2_flux.c ~ date, data = df, FUN = mean, na.rm = TRUE)

df_daily <- aggregate(co2_flux.c ~ date, data = df_07, FUN = mean, na.rm = TRUE)

## PLOT

ggplot(df_daily, aes(x = date, y = co2_flux.c)) +
  geom_line(color = "darkgreen", size = 1) +
  geom_point(color = "black", size = 1) +
  labs(
    title = "Daily Average NEE",
    x = "Date",
    y = expression(NEE~(µmol~m^{-2}~s^{-1}))
  ) +
  theme_minimal()

ggplot(df_daily, aes(x = date, y = co2_flux.c)) +
  geom_line(color = "darkgreen", size = 1) +
  geom_point(color = "black", size = 1) +
  labs(
    title = "Daily Average NEE",
    x = "Date",
    y = expression(NEE~(µmol~m^{-2}~s^{-1}))
  ) +
  theme_minimal()

######### save

write.csv(x = df_,file = 'C:/Users/klynoe/Documents/toolik/R_outputs/daily_NEE_07_25.csv',row.names = F,quote = F)



