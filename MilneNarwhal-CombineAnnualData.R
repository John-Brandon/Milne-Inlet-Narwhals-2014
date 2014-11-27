###=== === +++ === === +++ === === +++ === ===
# Author: John R. Brandon
# eMail:  jbrandon greeneridge [or jbrandon@gmail.com]
# Date :  Fall 2014
# OS   :  Mac OS 10.9.5 (x86_64-apple-darwin10.8.0 (64-bit))
# Language : R (ver 3.0.2 (2013-09-25) -- "Frisbee Sailing")
# IDE  : Rstudio v 0.98.484
###=== === +++ === === +++ === === +++ === ===
# Purpose : Combine 2013 and 2014 RAD (and tide) data into a single data.frame for modeling
#  Notes : These routines work on data that has already been extracted and munged, so the first step is to load that pre-massaged data 
#  1) Your main directory will differ. 
#  2) These data are also known as "Relative Abundance and Distribution" (RAD) data 
#  3) Loads existing workspace, that contains 2014 data (dat2014)
#  4) Loads existing workspace, that contains munged 2013 and 2014 tide data
#====== +++ === === +++ === === +++ === ===
library(plyr) # Hadley Wickham's "Plier" package for splitting data.frames and applying functions to each subset
library(dplyr)
library(lubridate) # package with handy functions for working with dates and times
library(reshape)
library(reshape2)  # Wickham's package that, with respect to data.frames, melts (long format) and casts (wide format). Google it.

# install.packages("dplyr")
# library(dplyr) # 'dplyr' is Wickham's updated version of 'plyr'

rm(list=ls()) # clear leftovers from previous workspace

# Load workspace with munged 2014 data
load("~/Documents/2014 Work/Milne Inlet Narwhals/2014 Analysis/Code/MilneNarwhal.2014.RData") 

# Load workspace with munged 2013 AND 2014 tide data
load("~/Documents/2014 Work/Milne Inlet Narwhals/2014 Analysis/Code/TideData.MilneNarwhal.2014.RData")

#====== +++ === === +++ === === +++ === ===
# Read 2013 RAD data 
#====== +++ === === +++ === === +++ === ===
setwd("~/Documents/2014 Work/Milne Inlet Narwhals/Data/2013") # Set working directory to 2013 data
dfile2013 = "2013.milne.inlet.narwhal.csv" # 2014 RAD data file, saved as comma delimited
dat2013 = read.csv(file = dfile2013, header = TRUE, as.is = TRUE, na.strings = c("NA", "x", "X")) # Read data file 

system(paste("open", dfile2013))
#====== +++ === === +++ === === +++ === ===
# Munge 2013 RAD data -- follow Scott Raborn's "filters" from 2013 report
#  After filters, there were 5 days that met criteria for inclusion in model: 13, 14, 21, 23 and 26 Aug (see Table D-1 of 2013 report)
#  Note: 2013 was the Pilot Study year. So, there was some on the fly learning, and data collection protocol evolved in field.
#   e.g. not all stratum surveyed (missing counts) and environmental data not recorded for each stratum during first few counts.
#====== +++ === === +++ === === +++ === ===

dat2013$Time = dat2013$StartTime 
dat2013 = convert.datetime(dat2013)  # munge datetimes (function is defined the 'MilneNarwhal-Munging2014Data.R' script)
dat2013 = assign.count.ids(dat2013)  # assign id numbers for each count

filtered.dat2013 = subset(dat2013, Sightability %in% c("G", "E")) # filter out any sighting conditions that are not Good - Excellent
str(filtered.dat2013) # check

dates.to.remove = c("2013-08-10", "2013-08-11", "2013-08-12", "2013-08-17", "2013-08-18", "2013-08-19", "2013-08-22", "2013-08-25")
filtered.dat2013 = subset(filtered.dat2013, ! Date %in% dates.to.remove) # filter dates

# filter times -- this is cluggy, but 2013 was a pilot study year and there was some growing pains in the data collection
delete.ii = which(day(filtered.dat2013$datetime) == 13 & hour(filtered.dat2013$datetime) < 14)
filtered.dat2013 = filtered.dat2013[-delete.ii,]
delete.ii = which(day(filtered.dat2013$datetime) == 23 & hour(filtered.dat2013$datetime) == 10)
filtered.dat2013 = filtered.dat2013[-delete.ii,]
delete.ii = which(day(filtered.dat2013$datetime) == 26 & hour(filtered.dat2013$datetime) < 15)
filtered.dat2013 = filtered.dat2013[-delete.ii,]

View(filtered.dat2013)

tot.counts.2013 = ddply(filtered.dat2013, .(datetime, Stratum), summarise, 
                        value = sum(GroupSize, na.rm = TRUE), 
                        VesselPresence = unique(WatchType),
                        SeaState = unique(SeaState)) # uses 'plyr' package, could also use function aggregate

write.csv(x = tot.counts.2013, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check

dat.mat.2013 = cast(tot.counts.2013, datetime + VesselPresence ~ Stratum) # reshape the data.frame into Table D-1 from 2013 report (cast in Wickham's lexicon)

dat.mat.2013$datetime.rounded.to.half.hour = dat.mat.2013$datetime # Round datetime to the nearest half-hour
minute(dat.mat.2013$datetime.rounded.to.half.hour) = round(minute(dat.mat.2013$datetime.rounded.to.half.hour)/30)*30 # round to nearest five minute

View(dat.mat.2013)
# write.csv(x = dat.mat.2013, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check

# dat.mat.2013.tmp = cast(tot.counts.2013, datetime + VesselPresence ~ Stratum, sum, margins = c("grand_row", "grand_col")) # reshape the data.frame into Table D-1 from 2013 report (cast in Wickham's lexicon)
# View(dat.mat.2013.tmp)

# write.csv(x = dat.mat.2013.tmp, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check

## TODO : Merge 2013 count data with environmental data (e.g. tide data)
# Reference how this was carried out for 2014 tide data (using merge / join)
#  think we'll need the dat.mat.2013$date.time.rounded.to.half.hour as an ID
dat.mat.2013.foo = dat.mat.2013 %>% mutate(sum.count.with.na = A + B + C + D + E + F + G + H + I)
# dat.mat.2013.foo = dat.mat.2013.foo %>% # SO WRONG
#   mutate(sum.count.without.na = sum(!is.na(A) + !is.na(B) + 
#                                       !is.na(C) + !is.na(D) + !is.na(E) + 
#                                       !is.na(F) + !is.na(G) + !is.na(H) + !is.na(I)))

View(dat.mat.2013.foo)

View(dat.tides.2013)
## TODO : Merge 2013 and 2014 data with counts and environmentals

# theme_set(theme_bw())
rm(dd)
dd <- data.frame(x = 1:10, y = 1:10)
qplot(x, ymin = 0, ymax = y, data = data.frame(x = 1:10, y = 1:10), geom = "linerange",color = I("red"))
?poisson.test
## TODO : Mutate certain variables to include in regression, e.g. consider making julian day out of datetime for the X's
# theme_set(theme_grey())

save.image("~/Documents/2014 Work/Milne Inlet Narwhals/2014 Analysis/Code/MilneNarwhal.2014.RData") 

dir()
?data.matrix
?model.matrix
?model.frame

?gl
dd <- data.frame(a = gl(3,4), b = gl(4,1,12)) # balanced 2-way
dd
options("contrasts")
model.matrix(~ a + b, dd)

DF <- data.frame(a = 1:3, b = letters[10:12],
                 c = seq(as.Date("2004-01-01"), by = "week", len = 3),
                 stringsAsFactors = TRUE)
data.matrix(DF[1:2])
data.matrix(DF)
