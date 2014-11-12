###==============
# Author: John R. Brandon
# eMail:  jbrandon greeneridge [or jbrandon@gmail.com]
# Date :  Fall 2014
# OS   :  Mac OS 10.9.5 (x86_64-apple-darwin10.8.0 (64-bit))
# Language : R (ver 3.0.2 (2013-09-25) -- "Frisbee Sailing")
# IDE  : Rstudio v 0.98.484
###=== === +++ === === +++ === === +++ === ===
# Purpose : Munge Milne Inlet narwhal data for LGL / Baffinland 
#  Notes : 
#  1) Your main directory will differ. See 'base.dir' variable below to edit that path to match your system
#  2) POSIXct is a class for representing calendar dates and times (these must be input in a strictly decreasing unambigous order, i.e. "YYYY-MM-DD HH:MM:SS")
#  3) These data are also known as "Relative Abundance and Distribution" (RAD) data 
#
#====== +++ === === +++ === === +++ === ===
library(plyr) # Hadley Wickham's "Plier" package for common tasks (e.g. summarizing) with data.frames
# library(dplyr) # I think this might be an updated version of the plyr package?
library(ggplot2)
library(lubridate) # useful alternative functions for working with standard POSIXct format

# Some initializations
rm(list=ls()) # clear leftovers from previous workspace
if (getOption("stringsAsFactors")) options(stringsAsFactors = FALSE) # set global option, don't want strings read as factors

load("~/Documents/2014 Work/Milne Inlet Narwhals/2014 Analysis/Code/TideData.MilneNarwhal.2014.RData") # Load TIDE workspace

# Read data
setwd("~/Documents/2014 Work/Milne Inlet Narwhals/Data/2014") # Set working directory for data
dfile = "2014.milne.inlet.narwhal.csv" # 2014 RAD data file, saved as comma delimited
dat2014 = read.csv(file = dfile, header = TRUE, as.is = TRUE, na.strings = c("NA", "x", "X")) # Read data file 

#====== +++ === === +++ === === +++ === ===
# Function to convert factors to numeric values
#====== +++ === === +++ === === +++ === ===
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]} 

#====== +++ === === +++ === === +++ === ===
# Clean up some typos
#====== +++ === === +++ === === +++ === ===
substratum.typos.ii = which(dat2014$SubStratum == "13") 
dat2014$SubStratum[substratum.typos.ii] = "I3"

group.size.typos.ii = which(dat2014$GroupSize == "I")
dat2014$GroupSize[group.size.typos.ii] = 1

dat2014$GroupSize = as.numeric(dat2014$GroupSize) # coerce if not already numeric (was read as character initially)

#====== +++ === === +++ === === +++ === ===
# Extract Stratum (A, B, C, etc) and Substratum Number (1, 2, 3, etc) from Substratum (e.g. A1, A2, A3, B1, B2, etc.)
#====== +++ === === +++ === === +++ === ===
dat2014$Stratum = substring(dat2014$SubStratum, first = 1, last = 1) # Create a vector with Stratum from SubStratum vector
dat2014$SubStratum.num = substring(dat2014$SubStratum, first = 2, last = 2) # Create a vector with SubStratum.num from SubStratum vector

#====== +++ === === +++ === === +++ === ===
# Convert Date and Time into POSIXct class format
# Date needs to be in an unambiguous format, i.e. yyyy-mm-dd (easiest to set format in Excel if this is ambiguous)
#  head(dat2014$Date)  # check
#====== +++ === === +++ === === +++ === ===

convert.datetime = function(dat){
# dat is a data.frame
  dat$datetime = with(dat, paste(Date, Time)) 
  dat$datetime = as.POSIXct(dat$datetime) # Convert DateTime to POSIXct class
  dat$datetime = ymd_hms(dat$datetime) # Not sure this is necessary to work with 'lubridate' functions. POSIXct might be sufficient.
  dat$datetime = force_tz(time = dat$datetime, tzone = "America/Iqaluit") # change from default time zone to EDT, but don't change time
  return(dat)  
}

dat2014 = convert.datetime(dat2014)

# dat2014$datetime = with(dat2014, paste(Date, Time)) 
# dat2014$datetime = as.POSIXct(dat2014$datetime) # Convert DateTime to POSIXct class
# 
# dat2014$datetime = force_tz(time = dat2014$datetime, tzone = "America/Iqaluit") # change from default time zone to EDT, but don't change time
# 
# length(unique(dat2014$datetime)) # number of "Relative Abundance and Distribution" (RAD) samples in 2014

#====== +++ === === +++ === === +++ === ===
# Create a sequence of Date/Times, from the start of the season to the end of the season:
#  Incrementing (1) every hour, and (2) every half hour
#====== +++ === === +++ === === +++ === ===
start.season = unique(dat2014$datetime)[1]
increment.timestamp = 60
end.season = unique(dat2014$datetime)[length(unique(dat2014$datetime))]
hour(end.season) = 23 # take it to the end of the last day
hourly.timestamps = seq(from=start.season, by=increment.timestamp*60, to=end.season)
increment.timestamp = 30
half.hourly.timestamps = seq(from=start.season, by=increment.timestamp*60, to=end.season)

#====== +++ === === +++ === === +++ === ===
# Add a column to data.frame, assigning TRUE or FALSE to vessel count
#  If CountType is "PRE", "C" or "POST" vessel count is TRUE, otherwise FALSE
#====== +++ === === +++ === === +++ === ===
dat2014$Vessel.related.count = rep(FALSE, nrow(dat2014)) # create dummy column to be filled below
Vessel.related.count.ii = which(dat2014$CountType %in% c("PRE", "C", "POST")) # which CountType records are "PRE", "C" or "POST"
dat2014$Vessel.related.count[Vessel.related.count.ii] = TRUE  # fill column

# write.csv(x = dat2014, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check

#====== +++ === === +++ === === +++ === ===
# Make a column which assigns an ID number for each count (a single day may have multiple counts)
# TODO (jbrandon): Move this code to 'Munging' script
#====== +++ === === +++ === === +++ === ===
assign.count.ids = function(dat.df){
  count.id = seq(from = 1, to = length(unique(dat.df$datetime)))
  ii = NULL
  dat.df$Count.id = rep(-99, nrow(dat.df))
  for(ii in 1:length(unique(dat.df$datetime))){ # probably a more elegant way to do this, rather than a loop.
    rec.numbers = NULL
    rec.numbers = which(dat.df$datetime == unique(dat.df$datetime)[ii])  
    dat.df$Count.id[rec.numbers] = count.id[ii]
  }
  return(dat.df)
}

dat2014 = assign.count.ids(dat2014)

# count.id = seq(from = 1, to = length(unique(dat2014$datetime))); count.id 
# ii = NULL
# dat2014$Count.id = rep(-99, nrow(dat2014))
# for(ii in 1:length(unique(dat2014$datetime))){ # probably a more elegant way to do this, rather than a loop.
#   rec.numbers = NULL
#   rec.numbers = which(dat2014$datetime == unique(dat2014$datetime)[ii])  
#   dat2014$Count.id[rec.numbers] = count.id[ii]
# }

# write.csv(x = dat2014, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check


#====== +++ === === +++ === === +++ === ===
# Designate survey.counts for Inclusion:
#  A complete survey.count is one that includes only stratum counts meeting the sightability criteria across all stratum during that survey.count.  
#  i.e. counts across each substratum were conducted during good to excellent sightability 
#  Any poor sightability, even if just one substratum, results in exclusion of that entire count under present criteria
#  Note also, that in addition to "P" for poor, if no attempt at a count was made for a sub-stratum (e.g. due to fog)
#    then those were entered as "x" (read into R as "NA"), so those counts with any "NA" or "P" will be excluded.
#====== +++ === === +++ === === +++ === ===
unique(dat2014$Sightability)
length(which(is.na(dat2014$Sightability)))
length(which(dat2014$Sightability == "P")) 

unique(dat2014$Count.id[which(is.na(dat2014$Sightability))]) # check to see which count.id's had NA's for no effort (due to fog, etc.)
unique(dat2014$Count.id[which(dat2014$Sightability == "P")]) # check to see which count.id's had P's for Poor sightability conditions

CountInclude = function(sightability){
  # Does this count meet the criteria of having all sub-stratum observed during Good or Excellent conditions?
  # 'sightability' here is a vector containing a code ("P", "G", "E", or NA) for each sub-stratum in a given count 
  # 'include.count' is returned as TRUE or FALSE
  unique.sight.codes = NULL; include.count = TRUE
  unique.sight.codes = unique(sightability)
  if ("P" %in% unique(sightability)) include.count = FALSE
  if (NA %in% unique(sightability)) include.count = FALSE
  return(include.count)
}

counts.to.include = ddply(dat2014, "Count.id", summarise, Include.count = CountInclude(Sightability)) # ! means NOT in R, so if no "P" then Include = TRUE 

dat2014 = merge(x = dat2014, y = counts.to.include, by.x = "Count.id", by.y = "Count.id") # expand from concise counts.to.include to full length column in data.frame 

# write.csv(x = dat2014, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check

#====== +++ === === +++ === === +++ === ===
# Rounding 'datetime' (class POSIXct) to nearest hour
#  Do this with just the hour (no date) and with datetime
#====== +++ === === +++ === === +++ === ===
rounded.hour = dat2014$datetime # create a new column that will contain datetime rounded to the nearest hour 
rounded.hour = format(round(rounded.hour, units="hours"), format="%H:%M") # seems to work
dat2014$rounded.hour = rounded.hour # append the rounded hours to data.frame

datetime.rounded.to.hr = dat2014$datetime
datetime.rounded.to.hr = round_date(datetime.rounded.to.hr, unit = "hour") # rounds to nearest hour
dat2014$datetime.rounded.to.hr = datetime.rounded.to.hr # append the rounded hours to data.frame

# write.csv(x = dat2014, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check

#====== +++ === === +++ === === +++ === ===
# After rounding the start times for counts to the nearest hour, there are instances where
#  multiple counts are assigned to the same hour. This is undesirable, because we want
#  the counts to be in an incremental sequence. So, we need to find those instances and adjust the hours 
#  to achieve the incremental sequence. 
#====== +++ === === +++ === === +++ === ===
# foo = dat2014$datetime.rounded.to.hr
# length(foo)
# 
# foo.dupes = duplicated(foo)
# which(foo.dupes == TRUE)
# rm(foo); rm(foo.dupes)

#====== +++ === === +++ === === +++ === ===
# Rounding 'datetime' (class POSIXct) to nearest half hour 
#  uses lubridate
#====== +++ === === +++ === === +++ === ===
dat2014$datetime.nearest.half.hr = dat2014$datetime
minute(dat2014$datetime.nearest.half.hr) = round(minute(dat2014$datetime.nearest.half.hr)/30)*30 # round to nearest five minute

#====== +++ === === +++ === === +++ === ===
# Rounding 'datetime' (class POSIXct) to nearest five minute (to align with tide data, which are every 5 minutes)
#  uses lubridate
#====== +++ === === +++ === === +++ === ===
dat2014$datetime.rounded.to.five.min = dat2014$datetime
minute(dat2014$datetime.rounded.to.five.min) = round(minute(dat2014$datetime.rounded.to.five.min)/5)*5 # round to nearest five minute

# write.csv(x = dat2014, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check

#====== +++ === === +++ === === +++ === ===
# Merge tide data into RAD count data.frame
#====== +++ === === +++ === === +++ === ===
dat.tides.2014$datetime = as.POSIXct(dat.tides.2014$datetime) # make sure datetime class is consistent with main data.frame's

# just get the columns of tide data that are desired for merged data.frame
dat.tides.2014.subset = subset(dat.tides.2014, select = c(datetime, Elevation, highlow, delta, risingfalling, tidestate))

dat2014 = merge(x = dat2014, y = dat.tides.2014.subset, by.x = "datetime.rounded.to.five.min", by.y = "datetime")

# write.csv(x = dat2014, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check

#====== +++ === === +++ === === +++ === ===
# Add a column with a factor for GroupSize. Two levels: (1) ZeroCount and (2) PositiveCount
#====== +++ === === +++ === === +++ === ===
dat2014$GroupSizeLevel = rep(NA, nrow(dat2014))
dat2014$GroupSizeLevel[which(dat2014$GroupSize == 0)] = "ZeroCount"
dat2014$GroupSizeLevel[which(dat2014$GroupSize > 0)] = "PositiveCount"

#====== +++ === === +++ === === +++ === ===
# Create another data.frame, with a subset of the counts which meet sightability criteria
#====== +++ === === +++ === === +++ === ===
dat2014.include = subset(dat2014, Include.count == TRUE)

#====== +++ === === +++ === === +++ === ===
# Save workspace image
#====== +++ === === +++ === === +++ === ===
save.image("~/Documents/2014 Work/Milne Inlet Narwhals/2014 Analysis/Code/MilneNarwhal.2014.RData")
