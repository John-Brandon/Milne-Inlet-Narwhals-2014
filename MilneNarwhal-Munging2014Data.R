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
# Clean up some typos
#====== +++ === === +++ === === +++ === ===
typos.ii = which(dat2014$SubStratum == "13") 
dat2014$SubStratum[typos.ii] = "I3"

#====== +++ === === +++ === === +++ === ===
# Start munging
#====== +++ === === +++ === === +++ === ===
#dat2014 = cbind(dat2014, colsplit(dat2014$SubStratum, split="", names = c("Stratum", "SubStratum.num")))
dat2014$Stratum = substring(dat2014$SubStratum, first = 1, last = 1) # Create a vector with Stratum from SubStratum vector
dat2014$SubStratum.num = substring(dat2014$SubStratum, first = 2, last = 2) # Create a vector with SubStratum.num from SubStratum vector

dat2014$datetime = with(dat2014, paste(Date, Time)) 
dat2014$datetime = as.POSIXct(dat2014$datetime) # Convert DateTime to POSIXct class

dat2014$datetime = force_tz(time = dat2014$datetime, tzone = "America/Iqaluit") # change from default time zone to EDT, but don't change time

length(unique(dat2014$datetime)) # number of "Relative Abundance and Distribution" (RAD) samples in 2014

#====== +++ === === +++ === === +++ === ===
# Keep munging:
# Table counts of group sizes by sub-stratum
#====== +++ === === +++ === === +++ === ===
group.size = table(dat2014$SubStratum, dat2014$GroupSize) # ?table
group.size = as.data.frame(group.size) # data.frame with group size frequencies in counts (e.g. 1 group of 34 in substratum F1, 0 groups of 33, etc.)
names(group.size) = c("SubStratum", "GroupSize", "Freq") # make it easier to read
write.csv(file = "foo.csv", x = group.size, row.names = FALSE); system("open foo.csv") # check

#====== +++ === === +++ === === +++ === ===
# Add a column to data.frame, assigning TRUE or FALSE to vessel count
#  If CountType is "PRE", "C" or "POST" vessel count is TRUE, otherwise FALSE
#====== +++ === === +++ === === +++ === ===
dat2014$Vessel.related.count = rep(FALSE, nrow(dat2014)) # create dummy column to be filled below
Vessel.related.count.ii = which(dat2014$CountType %in% c("PRE", "C", "POST")) # which CountType records are "PRE", "C" or "POST"
dat2014$Vessel.related.count[Vessel.related.count.ii] = TRUE  # fill column

write.csv(x = dat2014, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check

#====== +++ === === +++ === === +++ === ===
# Make a column which assigns an ID number for each count (a single day may have multiple counts)
# TODO (jbrandon): Move this code to 'Munging' script
#====== +++ === === +++ === === +++ === ===
count.id = seq(from = 1, to = length(unique(dat2014$datetime))); count.id 
ii = NULL
dat2014$Count.id = rep(-99, nrow(dat2014))
for(ii in 1:length(unique(dat2014$datetime))){ # probably a more elegant way to do this, rather than a loop.
  rec.numbers = NULL
  rec.numbers = which(dat2014$datetime == unique(dat2014$datetime)[ii])  
  dat2014$Count.id[rec.numbers] = count.id[ii]
}

write.csv(x = dat2014, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check

#====== +++ === === +++ === === +++ === ===
# Summarize abundance by SubStratum (integrating over time)
#====== +++ === === +++ === === +++ === ===
tot.counts = group.size
ii = sapply(tot.counts, is.factor) # intermediate step to convert columns that are presently factors to characters
tot.counts[ii] <- lapply(tot.counts[ii], as.character) # convert columns that are factors to character strings
tot.counts$TotalCount = as.numeric(tot.counts$GroupSize) * tot.counts$Freq # TotalCounts are product of group size and numbers of groups
tot.counts$Stratum = substring(tot.counts$SubStratum, first = 1, last = 1) # Create a vector with Stratum from SubStratum vector
tot.counts$SubStratum.num = substring(tot.counts$SubStratum, first = 2, last = 2) # Create a vector with SubStratum.num from SubStratum vector

tot.counts.subs = ddply(tot.counts, "SubStratum", summarise, TotalCount = sum(TotalCount)) # uses 'plyr' package, could also use function aggregate
# tot.counts.subs = aggregate(tot.counts.tmp$TotalCount, by = list(SubStratum = tot.counts.tmp$SubStratum), sum) # same result as line above

write.csv(x = tot.counts.subs, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check

#====== +++ === === +++ === === +++ === ===
# Summarize by Stratum (integrating over time) -- tot.counts.strat is plotted as histogram in plotting script
#====== +++ === === +++ === === +++ === ===
tot.counts.strat = ddply(tot.counts, "Stratum", summarise, TotalCount = sum(TotalCount)) # uses 'plyr' package, could also use function aggregate
head(tot.counts.strat) # check
tot.counts.strat

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
length(which(dat2014$Sightability == "L")) # TODO (hsmith): Data needs QC checking
length(which(dat2014$Sightability == 3))  # TODO (hsmith): Data needs QC checking

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

write.csv(x = dat2014, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check

#====== +++ === === +++ === === +++ === ===
# Rounding 'datetime' (class POSIXct) to nearest hour
#  Do this with just the hour (no date) and with datetime
#====== +++ === === +++ === === +++ === ===
rounded.hour = dat2014$datetime # create a new column that will contain datetime rounded to the nearest hour 
rounded.hour = format(round(datetime.rounded.to.hr, units="hours"), format="%H:%M") # seems to work
dat2014$rounded.hour = rounded.hour # append the rounded hours to data.frame

datetime.rounded.to.hr = dat2014$datetime
datetime.rounded.to.hr = round_date(datetime.rounded.to.hr, unit = "hour") # rounds to nearest hour
dat2014$datetime.rounded.to.hr = datetime.rounded.to.hr # append the rounded hours to data.frame

# write.csv(x = dat2014, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check

#====== +++ === === +++ === === +++ === ===
# Rounding 'datetime' (class POSIXct) to nearest five minute (to align with tide data, which are every 5 minutes)
#  uses lubridate
#====== +++ === === +++ === === +++ === ===
dat2014$datetime.rounded.to.five.min = dat2014$datetime
minute(dat2014$datetime.rounded.to.five.min) = floor(minute(dat2014$datetime.rounded.to.five.min)/5)*5 # round to nearest five minute

write.csv(x = dat2014, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check

#====== +++ === === +++ === === +++ === ===
# Merge tide data into RAD count data.frame
#====== +++ === === +++ === === +++ === ===
dat.tides.2014$datetime = as.POSIXct(dat.tides.2014$datetime) # make sure datetime class is consistent with main data.frame's

# just get the columns of tide data that are desired for merged data.frame
dat.tides.2014.subset = subset(dat.tides.2014, select = c(datetime, Elevation, highlow, delta, risingfalling, tidestate))

dat2014 = merge(x = dat2014, y = dat.tides.2014.subset, by.x = "datetime.rounded.to.five.min", by.y = "datetime")

write.csv(x = dat2014, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check

#====== +++ === === +++ === === +++ === ===
# Save workspace image
#====== +++ === === +++ === === +++ === ===

save.image("~/Documents/2014 Work/Milne Inlet Narwhals/2014 Analysis/Code/MilneNarwhal.2014.RData")

#====== +++ === === +++ === === +++ === ===
# Scratch code below
#====== +++ === === +++ === === +++ === ===
