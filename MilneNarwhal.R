###==============
# Author: John R. Brandon
# eMail:  jbrandon@greeneridge or jbrandon@gmail.com
# Date :  Fall 2014
# OS   :  Mac OS 10.9.5 (x86_64-apple-darwin10.8.0 (64-bit))
# Language : R (ver 3.0.2 (2013-09-25) -- "Frisbee Sailing")
# IDE  : Rstudio v 0.98.484
###=== === +++ === === +++ === === +++ === ===
# Purpose : Visualize and analyze Milen Inlet narwhal data for LGL / Baffinland 
#  Notes : 
#  1) Your main directory will differ. See 'base.dir' variable below to edit that path to match your system
#  2) POSIXlt is a class for representing calendar dates and times (in decreasing order to be unambiguous, i.e. "YYYY-MM-DD HH:MM:SS")
#  3) 
#====== +++ === === +++ === === +++ === ===

rm(list=ls()) # clear leftovers from previous workspace
# Load workspace image
#load.image("~/Documents/2014 Work/Milne Inlet Narwhals/2014 Analysis/Code/MilneNarwhal.2014.RData")

setwd("~/Documents/2014 Work/Milne Inlet Narwhals/Data/2014") # Set working directory for data
dfile = "2014.milne.inlet.narwhal.csv"
dat2014 = read.csv(file = dfile, header = TRUE, as.is = TRUE, na.strings = c("NA", "x", "X"))
nrow(dat2014)
names(dat2014)
unique(dat2014$SubStratum) # SubStratum

# Clean up some typos
typos.ii = which(dat2014$SubStratum == "13") 
dat2014$SubStratum[typos.ii] = "I3"

# Start munging
dat2014$Stratum = substring(dat2014$SubStratum, first = 1, last = 1) # Create a vector with Stratum from SubStratum vector
unique(dat2014$Stratum) # check

dat2014$datetime = with(dat2014, paste(Date, Time)) # Convert DateTime to POSIXlt class
dat2014$datetime = as.POSIXlt(dat2014$datetime)
head(dat2014$datetime) # check

# Keep munging
length(unique(dat2014$datetime)) # number of RAD samples
col1 = unique(dat2014$datetime) # make a colum
col.names = c("datetime", unique(dat2014$SubStratum))
col.names

unique(dat2014$GroupSize)
table(dat2014$GroupSize)
gs = dat2014$GroupSize[-which(is.na(dat2014$GroupSize))] # Group size without blank observations due to weather

max(gs)

which(is.na(dat2014$GroupSize))

str(dat2014$GroupSize)
plot(dat2014$datetime, dat2014$GroupSize) 
hist(dat2014$GroupSize, breaks = 100, col = "grey")

# Save workspace image
save.image("~/Documents/2014 Work/Milne Inlet Narwhals/2014 Analysis/Code/MilneNarwhal.2014.RData")
