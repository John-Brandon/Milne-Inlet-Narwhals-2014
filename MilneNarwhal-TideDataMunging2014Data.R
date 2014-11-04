###==============
# Author: John R. Brandon
# eMail:  jbrandon greeneridge [or jbrandon@gmail.com]
# Date :  Fall 2014
# OS   :  Mac OS 10.9.5 (x86_64-apple-darwin10.8.0 (64-bit))
# Language : R (ver 3.0.2 (2013-09-25) -- "Frisbee Sailing")
# IDE  : Rstudio v 0.98.484
###=== === +++ === === +++ === === +++ === ===
# Purpose : Work on munging tide data for Milne Inlet narwhal shore based survey for LGL / Baffinland 
#  Notes : 
#  1) Your main directory will differ. See 'base.dir' variable below to edit that path to match your system
#  2) Tide height is under "Elevation" column in *.csv data files
#     Tide height is measure in meters
#  3) Time for tides is in UTC. Need to convert to local time. Nunavut is -04:00
#  4) Saves munged tide data into a workspace that can be loaded by other scripts (e.g. to plot tides, etc.)
#====== +++ === === +++ === === +++ === ===
library(lubridate) # package of functions for working with date and time data

# Some initializations
rm(list=ls()) # clear leftovers from previous workspace
if (getOption("stringsAsFactors")) options(stringsAsFactors = FALSE) # set global option, don't want strings read as factors

#====== +++ === === +++ === === +++ === ===
# Function to convert julian dates in tide data to calendar date and time (in local Nunuvat time)
#====== +++ === === +++ === === +++ === ===
StampTime.Tides = function(tide.dat){
# Comments here to document what this function is doing : TODO (jbrandon)
  unique.yrs = unique(tide.dat$Year) # get unique years in this tide data, function expects only one unique year
  yr = ifelse(length(unique.yrs) == 1, yes = unique.yrs, no = print("ERROR: There is not a single unique year in this tide data file"))
  origin.day = paste(yr, "01", "01", sep = "-") # start counting julian dates from Jan 1st
  tmp.datetime = as.Date(tide.dat$Day, origin = origin.day) # get calendar date, given julian day of year
  tmp.datetime = tmp.datetime - 1 # take into account apparent offset 
  hour(tmp.datetime) = dat.tides.2013$Hour # assign hours to datetime
  minute(tmp.datetime) = dat.tides.2013$Min # assign minutes to datetime
  second(tmp.datetime) = dat.tides.2013$Sec # assign seconds to datetime  
  tmp.datetime = with_tz(time = tmp.datetime, tzone = "America/Iqaluit") # change time zone from UTC (should subtract 4 hrs)
  tide.dat$datetime = tmp.datetime # assign new column to data.frame with datetime
  plot(tide.dat$datetime, tide.dat$Elevation, type = 'l', xlab = "Date", ylab = "Tide Height (m)", main = as.character(unique.yrs))  
  StampTime.Tides = tide.dat # return the updated data.frame, with new column that has datetime
}

#====== +++ === === +++ === === +++ === ===
# Assign levels to tide: (1) High or Low (+ / -) and (2) Rising / Falling
#====== +++ === === +++ === === +++ === ===
AssignHighLow.Tides = function(tide.dat){
# Comments here to document what this function is doing : TODO (jbrandon)
  ii = NULL; neg.ii = NULL; pos.ii = NULL
  falling.ii = NULL; rising.ii = NULL
  
  delta = rep(-9999, nrow(tide.dat)); delta[1] = NA
  tide.dat$highlow = rep(NA, nrow(tide.dat))
  tide.dat$delta = rep(NA, nrow(tide.dat))
  tide.dat$risingfalling = rep(NA, nrow(tide.dat))
  tide.dat$tidestate = rep(NA, nrow(tide.dat))
  
  for(ii in 2:nrow(tide.dat)){
    delta[ii] = tide.dat$Elevation[ii] - tide.dat$Elevation[ii-1]
  }
  
  tide.dat$delta = delta # assign rate of change (positive values if tide is rising, negative if falling)

# GIVEN TIDE DELTA, ASSIGN LEVELS FOR "RISING", "FALLING", "HIGH FALLING", "LOW RISING" etc. 

  falling.ii = which(tide.dat$delta < 0) # which tide records for delta are negative (falling tide)
  rising.ii = which(tide.dat$delta >= 0) # which tide records for delta are positive (rising tide) 

  tide.dat$risingfalling[falling.ii] = "falling" # assign rising / falling factor
  tide.dat$risingfalling[rising.ii] = "rising" # assign rising / falling factor
  
  neg.ii = which(tide.dat$Elevation < 0) # which tide heights are negative ("low")
  pos.ii = which(tide.dat$Elevation >= 0) # which tide heights are negative ("high")  
  
  tide.dat$highlow[neg.ii] = "low" # assign high / low factor
  tide.dat$highlow[pos.ii] = "high" # assign high / low factor

  tide.dat$tidestate = paste(tide.dat$highlow, tide.dat$risingfalling, sep = ".") # combined state, e.g. "low.falling" tide
  tide.dat$tidestate[1] = NA
  
  AssignHighLow.Tides = tide.dat
}

#====== +++ === === +++ === === +++ === ===
# Munge 2013 Tide data
#====== +++ === === +++ === === +++ === ===
setwd("~/Documents/2014 Work/Milne Inlet Narwhals/Data/2013") # Set working directory for data
dfile.2013.tides = "2013_BruceHeadTides_5min_Elevation.csv" # 2013 Tide data file, saved as comma delimited
dat.tides.2013 = read.csv(file = dfile.2013.tides, header = TRUE, as.is = TRUE, na.strings = c("NA", "x", "X")) # Read data file 

dat.tides.2013 = StampTime.Tides(dat.tides.2013) # append data.frame with new column that has calendar datetime

dat.tides.2013 = AssignHighLow.Tides(dat.tides.2013) # append data.frame with new columns for "high/low" and "delta" (directional rate of tide change)

# write.csv(x = dat.tides.2013, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check

#====== +++ === === +++ === === +++ === ===
# Munge 2014 Tide data
#====== +++ === === +++ === === +++ === ===
setwd("~/Documents/2014 Work/Milne Inlet Narwhals/Data/2014") # Set working directory for data
dfile.2014.tides = "2014_BruceHeadTides_5min_Elevation.csv" # 2014 Tide data file, saved as comma delimited
dat.tides.2014 = read.csv(file = dfile.2014.tides, header = TRUE, as.is = TRUE, na.strings = c("NA", "x", "X")) # Read data file 

dat.tides.2014 = StampTime.Tides(dat.tides.2014) # append data.frame with new column that has calendar datetime

dat.tides.2014 = AssignHighLow.Tides(dat.tides.2014) # append data.frame with new columns for "high/low" and "delta" (directional rate of tide change)

# write.csv(x = dat.tides.2014, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check

#====== +++ === === +++ === === +++ === ===
# Save workspace image
#====== +++ === === +++ === === +++ === ===
save.image("~/Documents/2014 Work/Milne Inlet Narwhals/2014 Analysis/Code/TideData.MilneNarwhal.2014.RData")

# SCRATCH CODE BELOW
foo = dat.tides.2014
foo = subset(foo, Day %in% c(245, Day = 246)) # select two days
plot(foo$datetime, foo$Elevation, xlab = "Date", ylab = "Height (m)", type = 'l')
plot(foo$Min, foo$Elevation)
