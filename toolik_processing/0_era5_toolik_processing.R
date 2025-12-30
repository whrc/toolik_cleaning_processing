rm(list = ls())

library(data.table)
library(ggplot2)
library(dplyr)
library(zoo)

setwd('C:/Users/klynoe/Documents/pond_inlet/')

#read in the full dataset
era = fread('./gapfill/ERA5hourly_2024_pond_inlet.csv')

#make more friendly names
era = era[,c('dewpoint_temperature_2m', 'temperature_2m', 'soil_temperature_level_1','soil_temperature_level_2',
            'volumetric_soil_water_layer_1','volumetric_soil_water_layer_2', 'surface_solar_radiation_downwards_hourly',
            'total_precipitation_hourly','u_component_of_wind_10m','v_component_of_wind_10m','surface_pressure',
            'surface_latent_heat_flux_hourly','surface_sensible_heat_flux_hourly','date')]

#names(era)[c(47,48,59:70)] = c('date','dew','st1','st2','pres','surface_solar_radiation_downwards_hourly','temperature_2m','ppt','u','v','vwc1','vwc2')

#adjust to the timezone of interest (make sure it's listed in UTC)
era$date
tz = -6 #hours from UTC
era$date = era$date+(tz*60*60)

#subset down to time range of interest
#era = subset(era,era$date >= as.POSIXct('2017-1-1',tz='UTC'))

#convert temps from K to deg C
era$temperature_2m           = era$temperature_2m-273.15
era$dewpoint_temperature_2m  = era$dewpoint_temperature_2m-273.15
era$soil_temperature_level_1 = era$soil_temperature_level_1-273.15
era$soil_temperature_level_2 = era$soil_temperature_level_2-273.15

#make negatives NAs so we can fill them using linear interpolation.
era$surface_solar_radiation_downwards_hourly = era$surface_solar_radiation_downwards_hourly/3600 #convert from J m-2 to Wm-2, divide by seconds in an hour
era$surface_latent_heat_flux_hourly = era$surface_latent_heat_flux_hourly/-3600 #convert from J m-2 to Wm-2, divide by seconds in an hour
era$surface_sensible_heat_flux_hourly = era$surface_sensible_heat_flux_hourly/-3600 #convert from J m-2 to Wm-2, divide by seconds in an hour

#caluclate rh from the dewpoint and temperature
era$relative_humidity = 100*(exp((17.625*era$dewpoint_temperature_2m)/(243.04+era$dewpoint_temperature_2m))/exp((17.625*era$temperature_2m)/(243.04+era$temperature_2m)))

#create windspeed from u and v
era$wind_speed = sqrt(era$v_component_of_wind_10m^2 + era$u_component_of_wind_10m^2)

#create a date data frame with every half hour in the time frame of interest
date = seq(from = min(era$date),
           to = max(era$date),
           by = 60*30)
datedf = as.data.frame(date)

#merge with era 5
eram = merge(datedf,era,by = 'date',all = T)

#gapfill middle half hours
eram$dew   = na.approx(object = eram$dewpoint_temperature_2m,maxgap = 6)
eram$rh    = na.approx(object = eram$relative_humidity,maxgap = 6)
eram$st1   = na.approx(object = eram$soil_temperature_level_1,maxgap = 6)
eram$st2   = na.approx(object = eram$soil_temperature_level_2,maxgap = 6)
eram$rad   = na.approx(object = eram$surface_solar_radiation_downwards_hourly,maxgap = 6)
eram$ppt   = na.approx(object = eram$total_precipitation_hourly,maxgap = 6)
eram$pres  = na.approx(object = eram$surface_pressure,maxgap = 6)
eram$airt  = na.approx(object = eram$temperature_2m,maxgap = 6)
eram$vwc1  = na.approx(object = eram$volumetric_soil_water_layer_1,maxgap = 6)
eram$vwc2  = na.approx(object = eram$volumetric_soil_water_layer_2,maxgap = 6)
eram$ws    = na.approx(object = eram$wind_speed,maxgap = 6)
eram$le    = na.approx(object = eram$surface_latent_heat_flux_hourly,maxgap = 6)
eram$h     = na.approx(object = eram$surface_sensible_heat_flux_hourly,maxgap = 6)


#check out the data
ggplot(data = eram)+theme_bw()+geom_hline(yintercept = 0)+
  geom_point(aes(date,airt,col='temperature_2m'))+
  geom_point(aes(date,st1,col='soilT1'))+
  geom_point(aes(date,st2,col='soilT2'))

ggplot(data = eram)+theme_bw()+geom_hline(yintercept = 0)+
  geom_point(aes(date,rad))

ggplot(data = eram)+theme_bw()+geom_hline(yintercept = 0)+
  geom_point(aes(date,vwc1,col='vwc1'))+
  geom_point(aes(date,vwc2,col='vwc2'))
  
ggplot(data = eram)+theme_bw()+geom_hline(yintercept = 0)+
  geom_point(aes(date,rh))

ggplot(data = eram)+theme_bw()+
  geom_point(aes(date,pres))

ggplot(data = eram)+theme_bw()+geom_hline(yintercept = 0)+
  geom_point(aes(date,ws))

#resave off for comparison
write.csv(x = eram,file = './gapfill/era5_pond_inlet.csv',row.names = F)
