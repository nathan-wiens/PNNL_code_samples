
library(data.table)
library(lattice)
library(ncdf4)
library(raster)
library(dplyr)
library(tidyr)
library(data.table)
library(readr)
library(ggplot2)
library(foreign)
library(lubridate)

setwd('C:/Users/wiens/Desktop/Research/Data')

##### seasonal flow calculator ####
.seasonal_flow_calc <- function(flow_data){
  #' Calculates April-July seasonal flow
  #'
  #' input is a time series of streamflow data

  flow_months <- c(4,5,6,7) # season of interest
  colnames(flow_data)[2] <-"flow"
  
  # calculates seasonal flow
  monthlyQ <- flow_data%>%mutate(year = year(date)-1, month = month(date))%>%
    dplyr::select(year,month,flow)
  seasonal_flow <- monthlyQ%>%filter(month %in% flow_months)%>%group_by(year)%>%
    summarise(seasonal_flow = sum(flow))
  seasonal_flow1948_2012 <- seasonal_flow%>%filter(year>=1948 & year <=2012)%>%
    dplyr::select(seasonal_flow)
  
  return(seasonal_flow1948_2012)
}
##### spacial average of region with highest seasonal correlation #####
.seasonal_spacial_avg_max_cor <- function(flow_data, flow_months, var_months, level, flow_overflow_months, var_overflow_months, var.name, nc.name){
  flow_months_length <- length(flow_months)
  var_months_length <- length(var_months)
  
  ##### net cdf processing ####
  ncheight <- nc.name
  dname <- var.name
  var.nc <- nc_open(ncheight)
  var <- ncvar_get(var.nc,dname)
  lon <- var.nc$dim$lon$vals
  lat <- var.nc$dim$lat$vals
  time <- var.nc$dim$time$vals
  lat <- rev(lat)
  lon[lon>180] <- lon[lon>180]-360
  
  # accounts for slight difference in data structure for skin temperature
  if(var.name == "skt"){
    var <- var[,,]
    var500mb_2d <- matrix(var, nrow=length(time), byrow=TRUE)
    var500mb_2d <- var500mb_2d%>%head(792)
  }else{
    var.nc$dim$level$vals -> lev
    var500mb <- var[,,level,]
    var500mb_2d <- matrix(var500mb, nrow=length(time), byrow=TRUE)
    var500mb_2d <- var500mb_2d[1:792,]
  }
  
  x <- 1:12
  months <- data.frame(month = x)
  months <- data.frame(month = months[rep(seq_len(nrow(months)), times = 66), ])
  #####
  year <- data.frame(year = rep(1948:2013, each = 12))
  
  monthly_var500mb_2d <- cbind(year, months, var500mb_2d)
  
  seasonal_var_means <- data.frame(year = 1948:2012)
  for(i in 3:ncol(monthly_var500mb_2d)){
    single <- cbind(year, months, monthly_var500mb_2d[i])
    names(single)[3] <- 'variable'
    single <- single%>%filter(month %in% var_months)
    
    if(flow_months[1] < var_months[1]){
      single <- single%>%head(nrow(single)-(var_months_length - var_overflow_months))
      single <- single%>%tail(nrow(single)-var_overflow_months)
    }else{
      single <- single%>%tail(nrow(single) - var_months_length)
    }
    group <- data.frame(group = rep(1:nrow(single), each = var_months_length))%>%head(nrow(single))
    single <- cbind(single, group)
    single <- single%>%group_by(group)%>%summarise(mean = mean(variable))%>%dplyr::select(mean)
    names(single)[1] <- paste("smean_loc_", i-2, sep="")
    seasonal_var_means <- cbind(seasonal_var_means, single)
  }
  
  # Calculates correlation between seasonal variable means and seasonal flow
  corr_array2 <- cor(.seasonal_flow_calc(flow_data), 
                    seasonal_var_means%>%dplyr::select(-year))
  
  # Finds the coordinates for a 3x3 grid of data points around maximum correlation
  latitude <- ceiling(which.max(corr_array2)/length(lon))
  longitude <- (which.max(corr_array2) - (length(lon)*(latitude-1)))
  lat_range <- c(latitude-1, latitude, latitude +1)
  lon_range <- c(longitude -1, longitude, longitude +1)
  
  # Creates a data frame of 3x3 grid and takes the average
  total <- data.frame(mean = rep(0:0, each = 867))
  for(i in 1:3){
    for(j in 1:3){
      if(var.name == "skt"){
        var.data <- var[lon_range[i], lat_range[j],]
      }else{
        var.data <- var[lon_range[i], lat_range[j], level,]
        }
      total <- total+var.data
    }
  }
  mean <- total/9
  
  mean <- cbind(year, months, mean%>%head(792))
  mean <- mean%>%filter(month %in% var_months)
  
  if(flow_months[1] < var_months[1]){
    mean <- mean%>%head(nrow(mean)-(var_months_length - var_overflow_months))
    mean <- mean%>%tail(nrow(mean)-var_overflow_months)
  }else{
    mean <- mean%>%tail(nrow(mean) - var_months_length)
  }
  group <- data.frame(group = rep(1:nrow(single), each = var_months_length))%>%head(nrow(mean))
  mean <- cbind(mean, group)
  mean <- mean%>%group_by(group)%>%summarise(mean = mean(mean))%>%dplyr::select(mean)
  colnames(mean) <- paste(var.name, "_avg_",paste(var_months, collapse = "_"), sep = "")
  return(mean)
}
##### runs spacial average function for all seasons and variables
.climate_data <- function(variables, nc.names, seasons, natural_flow){
  all_data <- data.frame(year = (1948:2012))
    for(i in 1:length(climate_variables)){
      for(j in 1:length(seasons)){
      data <- .seasonal_spacial_avg_max_cor(natural_flow, c(4,5,6,7), seasons[[j]], 6,0, overflow[j], climate_variables[i], nc.names[i])
      all_data <- cbind(all_data,data)
      }
    }
  return(all_data)
}

# Natural flow data file
natural_flows <- read.csv("naturalFlows1908-2013.csv")%>%dplyr::select(-X, -leesFerry, -flamingGorgeWY, -blueMesa, -sum)
natural_flows$date <- as.Date(natural_flows$date, "%m/%d/%y")
climate_variables <- c("hgt", "uwnd", "vwnd", "skt")
# File names of variables
nc.names <- c("hgt.mon.mean.nc","uwnd.mon.mean.nc", "vwnd.mon.mean.nc", "skt.sfc.mon.mean.nc" )
# Seasons of interest
seasons <- list(c(10), c(10, 11, 12), c(10, 11, 12, 1), c(2, 3))
overflow <- c(0,0,1,2)


div5_variables <- .climate_data(climate_variables, nc.names, seasons, natural_flows%>%dplyr::select(date, ucrb))
crystalMorrow_variables <- .climate_data(climate_variables, nc.names, seasons, natural_flows%>%dplyr::select(date, crystalMorrow))
flamingGorgeUT_variables <- .climate_data(climate_variables, nc.names, seasons, natural_flows%>%dplyr::select(date, flamingGorgeUT))
navajo_variables <- .climate_data(climate_variables, nc.names, seasons, natural_flows%>%dplyr::select(date, navajo))

# Writes data files with seasonal natural flow and data
write.csv(div5_variables, "div5_climate.csv")
write.csv(crystalMorrow_variables, "crystalMorrow_climate.csv")
write.csv(flamingGorgeUT_variables, "flamingGorgeUT_climate.csv")
write.csv(navajo_variables, "navajo_climate.csv")
