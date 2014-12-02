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
#  4) Manually added a column in spreadsheet with LargeVess.Trans.ID (makes it easier to pull out start and stop times for large vessel transits)
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
# Extract Stratum ("A", "B", "C", etc) and Substratum Number ("1", "2", "3", etc) from Substratum (e.g. A1, A2, A3, B1, B2, etc.)
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
  dat$datetime = ymd_hms(dat$datetime) # perhaps redundant, but this conversion is for 'lubridate'. POSIXct might be sufficient.
  dat$datetime = force_tz(time = dat$datetime, tzone = "America/Iqaluit") # change from default time zone to EDT, but don't change time
  return(dat)  
}

dat2014 = convert.datetime(dat2014)

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
dat2014$include.group = rep(FALSE, nrow(dat2014))
include.ii = which(dat2014$Sightability %in% c("G", "E"))
dat2014$include.group[include.ii] = TRUE
unique(dat2014$include.group) # check

# write.csv(x = dat2014, file = "foo2.csv", row.names = FALSE); system("open foo2.csv") # check

unique(dat2014$Count.id[which(is.na(dat2014$Sightability))]) # check to see which count.id's had NA's for no effort (due to fog, etc.)
unique(dat2014$Count.id[which(dat2014$Sightability == "P")]) # check to see which count.id's had P's for Poor sightability conditions

CountInclude = function(sightability){
  # Does this sub-stratum count meet the criteria of having all sub-stratum observed during Good or Excellent conditions?
  # 'sightability' here is a vector containing a code ("P", "G", "E", or NA) for each sub-stratum in a given count 
  # 'include.count' is returned as TRUE or FALSE
  vector.boolean.include = NULL; include.count = TRUE
  vector.boolean.include = sightability %in% c("P", NA)
  if (any(vector.boolean.include == TRUE )) include.count = FALSE
  return(include.count)
}

# counts.to.include.sub = ddply(dat2014, .(Count.id, SubStratum), summarise, 
#                           Include.count = CountInclude(Sightability),
#                           datetime = unique(datetime)) 

counts.to.include.stratum = ddply(dat2014, .(Count.id, Stratum), summarise, Include.stratum.count = CountInclude(Sightability)) #datetime = unique(datetime)
head(counts.to.include.stratum) # check
nrow(dat2014); nrow(counts.to.include.stratum)

# write.csv(x = counts.to.include.stratum, file = "foo2.csv", row.names = FALSE); system("open foo2.csv") # check

# dat2014 = merge(x = dat2014, y = counts.to.include, by.x = "Count.id", by.y = "Count.id") # expand from concise counts.to.include to full length column in data.frame 

# write.csv(x = dat2014, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check

dat2014 = join(x = dat2014, y = counts.to.include.stratum, by = c("Count.id", "Stratum")) # expand from concise counts.to.include to full length column in data.frame 

# write.csv(x = dat2014, file = "foo2.csv", row.names = FALSE); system("open foo2.csv") # check

#====== +++ === === +++ === === +++ === ===
# Rounding 'datetime' (class POSIXct) to nearest hour
#  Do this with just the hour (no date) and with datetime
#   TODO: Make this into a generic function that can be called on non-2014 data -- possibly add functionality to 'convert.datetime()' function
#====== +++ === === +++ === === +++ === ===
rounded.hour = dat2014$datetime # create a new column that will contain datetime rounded to the nearest hour 
rounded.hour = format(round(rounded.hour, units="hours"), format="%H:%M") # seems to work
dat2014$rounded.hour = rounded.hour # append the rounded hours to data.frame

datetime.rounded.to.hr = dat2014$datetime
datetime.rounded.to.hr = round_date(datetime.rounded.to.hr, unit = "hour") # rounds to nearest hour
dat2014$datetime.rounded.to.hr = datetime.rounded.to.hr # append the rounded hours to data.frame

# write.csv(x = dat2014, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check

#====== +++ === === +++ === === +++ === ===
# Rounding 'datetime' (class POSIXct) to nearest half hour 
#  uses lubridate
#====== +++ === === +++ === === +++ === ===
dat2014$datetime.nearest.half.hr = dat2014$datetime
minute(dat2014$datetime.nearest.half.hr) = round(minute(dat2014$datetime.nearest.half.hr)/30)*30 # round to nearest five minute

#====== +++ === === +++ === === +++ === ===
# Extract start and end times for each vessel count and create a new data.frame with those
#  New data.frame will be used for plotting gray bars during vessel passage on time series plot of counts 
#====== +++ === === +++ === === +++ === ===
names(dat2014)
unique(dat2014$CountType)
unique(dat2014$LargeVess.Trans.ID)
large.vess.count = length(which(!is.na(unique(dat2014$LargeVess.Trans.ID)))) # four large vess transits during counts in 2014

large.vess.transit = NULL; start.time = as.POSIXct(NA, tz = ""); stop.time = as.POSIXct(NA,tz = tz(dat2014$datetime))
for(ii in 1:large.vess.count){
  large.vess.ii = which(dat2014$LargeVess.Trans.ID == ii)
  large.vess.transit[ii] = ii
  start.time[ii] = min(dat2014$datetime.nearest.half.hr[large.vess.ii]) # large.vess.start.stop
  stop.time[ii] = max(dat2014$datetime.nearest.half.hr[large.vess.ii]) 
}
library(lubridate)
start.time = with_tz(start.time, tzone = tz(dat2014$datetime)) # force time zone to equal survey data tz
stop.time = with_tz(stop.time, tzone = tz(dat2014$datetime))

large.vess.times = data.frame(large.vess.transit, start.time, stop.time)

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
dat2014.include = subset(dat2014, include.group == TRUE)

#====== +++ === === +++ === === +++ === ===
# Save workspace image
#====== +++ === === +++ === === +++ === ===
save.image("~/Documents/2014 Work/Milne Inlet Narwhals/2014 Analysis/Code/MilneNarwhal.2014.RData")

load("~/Documents/2014 Work/Milne Inlet Narwhals/2014 Analysis/Code/MilneNarwhal.2014.RData")
#====== +++ === === +++ === === +++ === ===
# %%% SCRATCH CODE BELOW %%%
#====== +++ === === +++ === === +++ === ===

#====== +++ === === +++ === === +++ === ===
# Search comments for keywords related to hunting
#====== +++ === === +++ === === +++ === ===
# hunt.words = c("shot", "gunshot", "gunshots", "shots", "shooting", "hunt", "hunting")
comments.ii = with(dat2014, which(! Comments %in% c(NA, ""))) # subset of comments that aren't blank
comments = dat2014[comments.ii, ]
comments = with(comments, data.frame(Count.id, Date, Time, Comments))
write.csv(x = comments, file = "foo3.csv", row.names = FALSE); system("open foo3.csv")
str(comments)
View(comments)
# comments[,1] = as.integer(comments[,1]) # I don't like "hard-coding" of index (= 1) here TODO: jbrandon
# names(comments) = c("Count.id", "Comments")
# write.csv(x = comments, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check
# head(comments)
# 
# comments.split = strsplit(comments$Comments, " ") # returns a list, with a character vector containing individual words for each comment
# str(comments.split) 
# comments.split[[1]][1] # This is the first 'word' in the first comment
# comments.split = lapply(comments.split, tolower) # coerce comments to lower case to ease matching, i.e. %in%
# matches = lapply(comments.split, function(x) x %in% hunt.words) # list like comments.split, but each word gets true or false if matches hunt.words
# matches = lapply(matches, function(x) any(x))
# match.ii = which(matches == TRUE); length(match.ii) # return index of matches in comments.split
# 
# xx = c("a", "b", "c")
# xx %in% hunt.words
# ? '%in%'
# ?grep