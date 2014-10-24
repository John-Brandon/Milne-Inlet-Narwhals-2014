###==============
# Author: John R. Brandon
# eMail:  jbrandon greeneridge [or jbrandon@gmail.com]
# Date :  Fall 2014
# OS   :  Mac OS 10.9.5 (x86_64-apple-darwin10.8.0 (64-bit))
# Language : R (ver 3.0.2 (2013-09-25) -- "Frisbee Sailing")
# IDE  : Rstudio v 0.98.484
###=== === +++ === === +++ === === +++ === ===
# Purpose : Visualize and analyze Milen Inlet narwhal data for LGL / Baffinland 
#  Notes : 
#  1) Your main directory will differ. See 'base.dir' variable below to edit that path to match your system
#  2) POSIXct is a class for representing calendar dates and times (these must be input in a strictly decreasing unambigous order, i.e. "YYYY-MM-DD HH:MM:SS")
#  3) These data are also known as "Relative Abundance and Distribution" (RAD) data 
#
#====== +++ === === +++ === === +++ === ===
if (getOption("stringsAsFactors")) options(stringsAsFactors = FALSE) # set global option, don't want strings read as factors
getOption("stringsAsFactors") # check

library(plyr) # Hadley Wickham's "Plier" package for common tasks (e.g. summarizing) with data.frames

rm(list=ls()) # clear leftovers from previous workspace
foo = NULL
# load("~/Documents/2014 Work/Milne Inlet Narwhals/2014 Analysis/Code/MilneNarwhal.2014.RData") # Load workspace 

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

dat2014$datetime = with(dat2014, paste(Date, Time)) # Convert DateTime to POSIXct class
dat2014$datetime = as.POSIXct(dat2014$datetime) # not really necessary at this stage, but a place holder for possible future analysis / visualizing time series

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
# Summarize abundance by SubStratum (integrating over time)
#====== +++ === === +++ === === +++ === ===
tot.counts = group.size
i = sapply(tot.counts, is.factor) # intermediate step to convert columns that are presently factors to characters
tot.counts[i] <- lapply(tot.counts[i], as.character) # convert columns that are factors to character strings
tot.counts$TotalCount = as.numeric(tot.counts$GroupSize) * tot.counts$Freq # TotalCounts are product of group size and numbers of groups
tot.counts$Stratum = substring(tot.counts$SubStratum, first = 1, last = 1) # Create a vector with Stratum from SubStratum vector
tot.counts$SubStratum.num = substring(tot.counts$SubStratum, first = 2, last = 2) # Create a vector with SubStratum.num from SubStratum vector

tot.counts.subs = ddply(tot.counts, "SubStratum", summarise, TotalCount = sum(TotalCount)) # uses 'plyr' package, could also use function aggregate
# tot.counts.subs = aggregate(tot.counts.tmp$TotalCount, by = list(SubStratum = tot.counts.tmp$SubStratum), sum) # same result as line above

head(tot.counts.subs)
foo = tot.counts.subs
write.csv(file = "foo.csv", x = foo, row.names = FALSE); system("open foo.csv") # check

#====== +++ === === +++ === === +++ === ===
# Summarize by Stratum (integrating over time)
#====== +++ === === +++ === === +++ === ===
tot.counts.strat = ddply(tot.counts, "Stratum", summarise, TotalCount = sum(TotalCount)) # uses 'plyr' package, could also use function aggregate
head(tot.counts.strat) # check
tot.counts.strat

library(ggplot2)
ggplot(tot.counts.strat, aes(x = Stratum, y = TotalCount)) + geom_bar(stat = "identity")

#====== +++ === === +++ === === +++ === ===
# Save workspace image
#====== +++ === === +++ === === +++ === ===
save.image("~/Documents/2014 Work/Milne Inlet Narwhals/2014 Analysis/Code/MilneNarwhal.2014.RData")
