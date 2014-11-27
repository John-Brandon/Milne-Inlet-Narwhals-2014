###=== === +++ === === +++ === === +++ === ===
# Author: John R. Brandon
# eMail:  jbrandon greeneridge [or jbrandon@gmail.com]
# Date :  Fall 2014
# OS   :  Mac OS 10.9.5 (x86_64-apple-darwin10.8.0 (64-bit))
# Language : R (ver 3.0.2 (2013-09-25) -- "Frisbee Sailing")
# IDE  : Rstudio v 0.98.484
###=== === +++ === === +++ === === +++ === ===
# Purpose : Munge 2014 Milne Inlet LGL / Baffinland Environmental data (i.e. wind speed and direction)
#            Merge wind speed and direction data into main dat2014 database
#  Notes : 
#  1) Your main directory will differ. 
#  2) There is a <= sign in the ice % column that probably needs changing before file is read, likewise a ">" in WindSpeed
#  3) Need to manually change format of Date in spreadsheet to "yyyy-mm-dd" to be consistent with POSIXct
#  4) Need to manually change format of Time in spreadsheet to "hh:mm:ss" to be consistent with POSIXct
#====== +++ === === +++ === === +++ === ===
library(plyr) # Hadley Wickham's "Plier" package for splitting data.frames and applying functions to each subset
library(lubridate) # package with handy functions for working with dates and times
library(reshape2)

# Set Options
if (getOption("stringsAsFactors")) options(stringsAsFactors = FALSE) # set global option, don't want strings read as factors

# Read data
setwd("~/Documents/2014 Work/Milne Inlet Narwhals/Data/2014") # Set working directory for data
env.dfile = "Enviro.dat.2014.csv" # 2014 Environmental data, saved as comma delimited
system("open Enviro.dat.2014.csv")
env2014 = read.csv(file = env.dfile, header = TRUE, as.is = TRUE, na.strings = c("NA", "x", "X")) # Read data file 

str(env2014) # check

# subset data of interest : Date, Time, WindSpeed and WindDir
env2014 = subset(env2014, select = c(Date, Time, WindSpeed, WindDir))
env2014$datetime = paste(env2014$Date, env2014$Time)
write.csv(x = env2014, file = "foo4.csv", row.names = FALSE); system("open foo4.csv") # check
env2014$datetime = as.POSIXct(env2014$datetime) # convert to POSIXct -- seemed to take a very long time for some reason

# round datetime to nearest five minutes and nearest half hour

