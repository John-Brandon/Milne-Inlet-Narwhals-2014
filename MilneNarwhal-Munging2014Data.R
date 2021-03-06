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
#  1) Your main directory will differ. See 'base.dir' variable below and edit that path to match your project directory
#  2) POSIXct is a class for representing calendar dates and times (these must be input in a strictly decreasing unambigous order, i.e. "YYYY-MM-DD HH:MM:SS")
#  3) These data are also known as "Relative Abundance and Distribution" (RAD) data 
#  4) Manually added a column in spreadsheet with LargeVess.Trans.ID (makes it easier to pull out start and stop times for large vessel transits)
#====== +++ === === +++ === === +++ === ===
load.packages = function(){
  library(plyr) # Hadley Wickham's "Plier" package for splitting data.frames and applying functions to each subset  
  library(dplyr) # Plyr 2.0 -- more functions, and faster operations
  library(lubridate) # Package with handy functions for working with dates and times
  library(reshape)
  library(reshape2)  # Wickham's package that, with respect to data.frames, melts (long format) and casts (wide format). Google it.
  library(ggplot2)  # Plotting
  library(scales) 
  library(MASS) # glm.nb()
  library(lme4) # glmer()
}

load.packages()

# Some initializations
rm(list=ls()) # clear leftovers from previous workspace
if (getOption("stringsAsFactors")) options(stringsAsFactors = FALSE) # set global option, don't want strings read as factors

# Initialize working directory for this session
base.dir = "~/Documents/2014 Work/Milne Inlet Narwhals/Data/2014" # TODO: Make the directory code more flexible for other machines
# paste("", "", sep = "/") # If you're using R running MS Windows OS, I believe you'll need to change sep = "//" (or maybe try "\\")
load("~/Documents/2014 Work/Milne Inlet Narwhals/2014 Analysis/Code/TideData.MilneNarwhal.2014.RData") # Load TIDE workspace
setwd(base.dir) # Set working directory for data
load("~/Documents/2014 Work/Milne Inlet Narwhals/2014 Analysis/Code/MilneNarwhal.2014.RData") #Debugging

# Read data 
# TODO: Consider moving data input to another more general script. 
#  That seperate script could load and process data (calling the functions in this script)
dfile = "2014.milne.inlet.narwhal.csv" # 2014 RAD data file, saved as comma delimited
dat2014 = read.csv(file = dfile, header = TRUE, as.is = TRUE, na.strings = c("NA", "x", "X")) # Read data file 

#====== +++ === === +++ === === +++ === ===
# Function to convert factors to numeric values
#====== +++ === === +++ === === +++ === ===
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]} 

#====== +++ === === +++ === === +++ === ===
# Clean up some known typos: DEFUNCT CODE (typos should be fixed in data)
#====== +++ === === +++ === === +++ === ===
clean.typos = function(dat){
  substratum.typos.ii = which(dat$SubStratum == "13") 
  dat$SubStratum[substratum.typos.ii] = "I3"
  
  group.size.typos.ii = which(dat$GroupSize == "I")
  dat$GroupSize[group.size.typos.ii] = 1
  
  dat$GroupSize = as.numeric(dat$GroupSize) # coerce if not already numeric (was read as character initially)  
  return(dat)
}

#====== +++ === === +++ === === +++ === ===
# Remove 'extraneous' columns [those not needed for quantitative analyses]
#====== +++ === === +++ === === +++ === ===
remove.extraneous.columns = function(dat){
  columns.to.remove = c("Obs", "Obs_BI", "Obs_DR", "Comments", "Entered", "Checked")
  columns.to.keep = setdiff(names(dat), columns.to.remove)
  dat = dat[ , columns.to.keep] 
  return(dat)
}

#====== +++ === === +++ === === +++ === ===
# Extract Stratum ("A", "B", "C", etc) and Substratum Number ("1", "2", "3", etc) from Substratum (e.g. A1, A2, A3, B1, B2, etc.)
#====== +++ === === +++ === === +++ === ===
extract.stratum = function(dat){
  dat$Stratum = substring(dat$SubStratum, first = 1, last = 1) # Create a vector with Stratum from SubStratum vector
  dat$SubStratum.num = substring(dat$SubStratum, first = 2, last = 2) # Create a vector with SubStratum.num from SubStratum vector
  return(dat)
}

#====== +++ === === +++ === === +++ === ===
# Convert Date and Time into POSIXct class format
# Date needs to be in an unambiguous format, i.e. yyyy-mm-dd (easiest to set format in Excel if this is ambiguous)
#  head(dat2014$Date)  # check
#====== +++ === === +++ === === +++ === ===
convert.datetime = function(dat){
# dat is a data.frame
# if datetime is not already a column in the data.frame, create a concatinated datetime column
  if(! "datetime" %in% names(dat)){ 
    dat$datetime = with(dat, paste(Date, Time)) 
    dat$datetime = as.POSIXct(dat$datetime) # Convert DateTime to POSIXct class
  } 

  dat$datetime = ymd_hms(dat$datetime) # perhaps redundant, but this conversion is for 'lubridate'. POSIXct might be sufficient.
  dat$datetime = force_tz(time = dat$datetime, tzone = "America/Iqaluit") # change from default time zone to EDT, but don't change time
  
  return(dat)  
}

#====== +++ === === +++ === === +++ === ===
# Create a sequence of Date/Times, from the start of the season to the end of the season:
#  - Incrementing (1) every hour, and (2) every half hour
#  - This is used for plotting the x-axis range in plot of counts and large vessel presence 
#====== +++ === === +++ === === +++ === ===
create.long.seq.datetime = function(dat){
  start.season = unique(dat$datetime)[1]
  increment.timestamp = 60
  end.season = unique(dat$datetime)[length(unique(dat$datetime))]
  hour(end.season) = 23 # take it to the end of the last day
  hourly.timestamps = seq(from=start.season, by=increment.timestamp*60, to=end.season)
  increment.timestamp = 30
  half.hourly.timestamps = seq(from=start.season, by=increment.timestamp*60, to=end.season)  
  return(half.hourly.timestamps)
}

#====== +++ === === +++ === === +++ === ===
# Add a column to data.frame, assigning TRUE or FALSE to vessel count
#  If CountType is "PRE", "C" or "POST" vessel count is TRUE, otherwise FALSE
#====== +++ === === +++ === === +++ === ===
assign.vessel.boolean = function(dat){
  dat$Vessel.related.count = rep(FALSE, nrow(dat)) # create dummy column to be filled below
  Vessel.related.count.ii = which(dat$CountType %in% c("PRE", "C", "POST")) # which CountType records are "PRE", "C" or "POST"
  dat$Vessel.related.count[Vessel.related.count.ii] = TRUE  # fill column  
  return(dat)
}

#====== +++ === === +++ === === +++ === ===
# Make a column which assigns an ID number for each count (a single day may have multiple counts)
#====== +++ === === +++ === === +++ === ===
assign.count.ids = function(dat){
  count.id = seq(from = 1, to = length(unique(dat$datetime)))
  ii = NULL
  dat$Count.id = rep(-99, nrow(dat))
  for(ii in 1:length(unique(dat$datetime))){ # probably a more elegant way to do this, rather than a loop.
    rec.numbers = NULL
    rec.numbers = which(dat$datetime == unique(dat$datetime)[ii])  
    dat$Count.id[rec.numbers] = count.id[ii]
  }
  return(dat)
}

#====== +++ === === +++ === === +++ === ===
# Assign a column with results of a group level filter on sightability -- not currently used
#====== +++ === === +++ === === +++ === ===
# dat2014$include.group = rep(FALSE, nrow(dat2014))
# include.ii = which(dat2014$Sightability %in% c("G", "E"))
# dat2014$include.group[include.ii] = TRUE
# unique(dat2014$include.group) # check

#====== +++ === === +++ === === +++ === ===
# Designate survey.counts for Inclusion:
#  A complete survey.count is one that includes only stratum counts meeting the sightability criteria across all stratum during that survey.count.  
#  i.e. counts across each substratum were conducted during good to excellent sightability 
#  Any poor sightability, even if just one substratum, results in exclusion of that entire count under present criteria
#  Note also, that in addition to "P" for poor, if no attempt at a count was made for a sub-stratum (e.g. due to fog)
#    then those were entered as "x" (read into R as "NA"), so those counts with any "NA" or "P" will be excluded.
#====== +++ === === +++ === === +++ === ===
CountInclude = function(sightability){
  # Does this sub-stratum count meet the criteria of having all sub-stratum observed during Good or Excellent conditions?
  # 'sightability' here is a vector containing a code ("P", "G", "E", or NA) for each sub-stratum in a given count 
  # 'include.count' is returned as TRUE or FALSE
  vector.boolean.include = NULL; include.count = TRUE
  vector.boolean.include = sightability %in% c("P", NA)
  if (any(vector.boolean.include == TRUE )) include.count = FALSE
  return(include.count)
}

assign.strat.sight.2014 = function(dat){
  counts.to.include.stratum = ddply(dat, .(Count.id, Stratum), summarise, 
                                    Include.stratum.count = CountInclude(Sightability)) # Passes Sightability for each sub-stratum in Stratum
  
  # Expand from concise counts.to.include to full length column in data.frame 
  dat = join(x = dat, y = counts.to.include.stratum, by = c("Count.id", "Stratum"))   
  
  return(dat)
}

#====== +++ === === +++ === === +++ === ===
# Rounding 'datetime' (class POSIXct) to nearest hour
#  Do this with just the hour (no date) and with datetime
#====== +++ === === +++ === === +++ === ===
round.to.nearest.hr = function(dat){
  rounded.hour = dat$datetime # create a new column that will contain datetime rounded to the nearest hour 
  rounded.hour = format(round(rounded.hour, units="hours"), format="%H:%M") # seems to work
  dat$rounded.hour = rounded.hour # append the rounded hours to data.frame
  
  datetime.rounded.to.hr = dat$datetime
  datetime.rounded.to.hr = round_date(datetime.rounded.to.hr, unit = "hour") # rounds to nearest hour
  dat$datetime.rounded.to.hr = datetime.rounded.to.hr # append the rounded hours to data.frame  
  dat$datetime.rounded.to.hr = force_tz(dat$datetime.rounded.to.hr, tz = tz(dat$datetime))
  return(dat)
}

#====== +++ === === +++ === === +++ === ===
# Rounding 'datetime' (class POSIXct) to nearest half hour 
#====== +++ === === +++ === === +++ === ===
round.to.nearest.half.hr = function(dat){
  #  uses lubridate
  dat$datetime.nearest.half.hr = ymd_hms(dat$datetime)
  dat$datetime.nearest.half.hr = force_tz(dat$datetime.nearest.half.hr, tz = tz(dat$datetime))
  minute(dat$datetime.nearest.half.hr) = round(minute(dat$datetime.nearest.half.hr)/30)*30 # round to nearest half hour
  return(dat)
}

#====== +++ === === +++ === === +++ === ===
# Rounding 'datetime' (class POSIXct) to nearest five minute (to align with tide data, which are every 5 minutes)
#====== +++ === === +++ === === +++ === ===
round.to.nearest.five.min = function(dat){
#  uses lubridate
  dat$datetime.rounded.to.five.min = dat$datetime
  dat$datetime.rounded.to.five.min = ymd_hms(dat$datetime.rounded.to.five.min)
  minute(dat$datetime.rounded.to.five.min) = round(minute(dat$datetime.rounded.to.five.min)/5)*5 # round to nearest five minute
  return(dat)
}

#====== +++ === === +++ === === +++ === ===
# Add column with decimal form of hour:min
#  This works with 'datetime.nearest.half.hr'
#====== +++ === === +++ === === +++ === ===
calc.dec.hour = function(dat){
  dat$dec.hour = ymd_hms(dat$datetime.nearest.half.hr)  
  dat$dec.hour = hour(dat$dec.hour) + minute(dat$dec.hour)/60 + second(dat$dec.hour)/3600
  return(dat)
}

#====== +++ === === +++ === === +++ === ===
# Add column with Julian date
#====== +++ === === +++ === === +++ === ===
calc.julian.date = function(dat){
  dat$julian.date = ymd(dat$Date)  
  dat$julian.date = yday(dat$julian.date)
  return(dat)
}

#====== +++ === === +++ === === +++ === ===
# Add column with decimal Julian date
#====== +++ === === +++ === === +++ === ===
calc.decimal.julian.date = function(dat){
  dat$dec.julian.date = dat$julian.date
  dat$dec.julian.date = dat$dec.julian.date + (dat$dec.hour / 24)
  return(dat)
}

#====== +++ === === +++ === === +++ === ===
# Wrapper function for calls to munge dates and times, and to assign Count.id
#====== +++ === === +++ === === +++ === ===
do.dates.and.ids = function(dat){
# Wrapper function for calls to munge dates and times, and to assign Count.id
  if("StartTime" %in% names(dat)) dat$Time = dat$StartTime # 2013 data labeled differently
  dat = convert.datetime(dat)  # munge datetimes (function is defined the 'MilneNarwhal-Munging2014Data.R' script)
  dat = round.to.nearest.hr(dat) # returns a column with time rounded to nearest hour
  dat = round.to.nearest.half.hr(dat) # returns a column with time rounded to nearest half-hour
  dat = round.to.nearest.five.min(dat) # returns a column with time rounded to nearest five minutes
  dat = calc.dec.hour(dat) # returns a column with decimal hour
  dat = calc.julian.date(dat) # returns a column with julian date
  dat = calc.decimal.julian.date(dat) # returns a column with decimal julian date
  dat = assign.count.ids(dat)  # assign id numbers for each count  
  return(dat)
}

#====== +++ === === +++ === === +++ === ===
# Add a column with a factor for GroupSize. 
#  Two levels: (1) ZeroCount and (2) PositiveCount
#====== +++ === === +++ === === +++ === ===
factor.group.size = function(dat){
  dat$GroupSizeLevel = rep(NA, nrow(dat))
  dat$GroupSizeLevel[which(dat$GroupSize == 0)] = "ZeroCount"
  dat$GroupSizeLevel[which(dat$GroupSize > 0)] = "PositiveCount"
  return(dat)
}

#====== +++ === === +++ === === +++ === ===
# Add a column with a factor for Count.quality. 
#  Two levels: (1) "Good" and (2) "Poor"
#====== +++ === === +++ === === +++ === ===
factor.count.quality = function(dat){
  dat$count.quality = rep(NA, nrow(dat))
  dat = ddply(dat, .(Count.id), transform, count.quality = ifelse(all(Include.stratum.count), "Good", "Poor"))
  return(dat)
}

#====== +++ === === +++ === === +++ === ===
# Merge tide data into RAD count data.frame
#  Not sure if names in 2013 tide data are consistent, so naming this specifically for dat2014
#====== +++ === === +++ === === +++ === ===
merge.2014.dat.tides = function(dat, dat.tides){ 
  dat.tides$datetime = as.POSIXct(dat.tides$datetime) # make sure datetime class is consistent with main data.frame's
  
  # just get the columns of tide data that are desired for merged data.frame
  dat.tides.2014.subset = subset(dat.tides, select = c(datetime, Elevation, highlow, delta, risingfalling, tidestate))
  dat = merge(x = dat, y = dat.tides.2014.subset, by.x = "datetime.rounded.to.five.min", by.y = "datetime")
  return(dat)
  # write.csv(x = dat2014, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check  
}

#====== +++ === === +++ === === +++ === ===
# Filter out large vessel counts
#====== +++ === === +++ === === +++ === ===
filter.out.large.vessels = function(dat){
  dat = filtered.dat2014
  dat = subset(dat, CountType == "H")
  return(dat)
}

#====== +++ === === +++ === === +++ === ===
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Get the data and get it in shape. This is where the magic happens. 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#====== +++ === === +++ === === +++ === ===
base.dir = "~/Documents/2014 Work/Milne Inlet Narwhals/Data/2014" # TODO: Make the directory code more flexible for other machines
setwd(base.dir) # Set working directory for data
dat2014 = read.csv(file = dfile, header = TRUE, as.is = TRUE, na.strings = c("NA", "x", "X")) # Read data file 
dat2014 = clean.typos(dat2014)
dat2014 = remove.extraneous.columns(dat2014)
dat2014 = extract.stratum(dat2014) 
dat2014 = do.dates.and.ids(dat2014) 
dat2014 = assign.vessel.boolean(dat2014)
dat2014 = factor.group.size(dat2014)
dat2014 = merge.2014.dat.tides(dat2014, dat.tides.2014)
dat2014 = assign.strat.sight.2014(dat2014)
dat2014 = factor.count.quality(dat2014)
half.hourly.timestamps.2014 = create.long.seq.datetime(dat2014) # Consider moving this function call to Plotting Script
filtered.dat2014 = filter.sight(dat2014)  # filter dat2014 for modeling (3389 rows)
filtered.dat2014.less.vessels = filter.out.large.vessels(filtered.dat2014)
View(dat2014)
write.csv(x = dat2014, file = "foo4.csv", row.names = FALSE); system("open foo4.csv") # check
write.csv(x = filtered.dat2014, file = "filtered.dat2014.csv", row.names = FALSE); system("open filtered.dat2014.csv") # check

#====== +++ === === +++ === === +++ === ===
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#====== +++ === === +++ === === +++ === ===


#====== +++ === === +++ === === +++ === ===
# Extract start and end times for each vessel count and create a new data.frame with those
#  New data.frame will be used for plotting gray bars during vessel passage on time series plot of counts 
#====== +++ === === +++ === === +++ === ===
extract.vessel.transit.times = function(dat){

  dat$datetime.nearest.half.hr = force_tz(dat$datetime.nearest.half.hr, tz = tz(dat$datetime))
    
  large.vess.count = length(which(!is.na(unique(dat$LargeVess.Trans.ID)))) # four large vess transits during counts in 2014  
  
  large.vess.transit = NULL
  start.time = as.POSIXct(NA, tz = tz(dat$datetime)) # why tz inconsistent with line below??
  stop.time = as.POSIXct(NA, tz = tz(dat$datetime))
  
  for(ii in 1:large.vess.count){ 
    # ii = 1
    large.vess.ii = which(dat$LargeVess.Trans.ID == ii)
    large.vess.transit[ii] = ii
    start.time[ii] = min(dat$datetime.nearest.half.hr[large.vess.ii]) # large.vess.start.stop
    stop.time[ii] = max(dat$datetime.nearest.half.hr[large.vess.ii]) 
  }
  
  start.time = with_tz(start.time, tzone = tz(dat$datetime)) # force time zone to equal survey data tz
  stop.time = with_tz(stop.time, tzone = tz(dat$datetime)) # requires lubridate package
  
  large.vess.times = data.frame(large.vess.transit, start.time, stop.time)
  large.vess.times = data.frame(start.time, stop.time)
  return(large.vess.times)
}

large.vess.times = extract.vessel.transit.times(dat2014)

#====== +++ === === +++ === === +++ === ===
# Create another data.frame, with a subset of the counts which meet sightability criteria
# Not used. Filtering is now done by filter.sight() function in CombineAnnualData script
#====== +++ === === +++ === === +++ === ===
# dat2014.include = subset(dat2014, include.group == TRUE)
# dat2014.include = subset(dat2014, include.stratum.count == TRUE)

#====== +++ === === +++ === === +++ === ===
# Save workspace image
#====== +++ === === +++ === === +++ === ===
save.image("~/Documents/2014 Work/Milne Inlet Narwhals/2014 Analysis/Code/MilneNarwhal.2014.RData")

load("~/Documents/2014 Work/Milne Inlet Narwhals/2014 Analysis/Code/MilneNarwhal.2014.RData")
