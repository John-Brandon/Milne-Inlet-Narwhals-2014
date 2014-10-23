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
#  2) POSIXlt is a class for representing calendar dates and times (in decreasing order to be unambiguous, i.e. "YYYY-MM-DD HH:MM:SS")
#  3) These data are also known as "Relative Abundance and Distribution" (RAD) data 
#
#====== +++ === === +++ === === +++ === ===
if (getOption("stringsAsFactors")) options(stringsAsFactors = FALSE) # set global option, don't want strings read as factors
getOption("stringsAsFactors") # check

#library(reshape2) # handy functions for data munging, via Hadley Wickham
#detach("package:reshape2", unload=TRUE)
#library(reshape)
library(plyr) # Hadley Wickham's "Plier" package for common tasks (e.g. summarizing) with data.frames

rm(list=ls()) # clear leftovers from previous workspace

load("~/Documents/2014 Work/Milne Inlet Narwhals/2014 Analysis/Code/MilneNarwhal.2014.RData") # Load workspace 

setwd("~/Documents/2014 Work/Milne Inlet Narwhals/Data/2014") # Set working directory for data
dfile = "2014.milne.inlet.narwhal.csv"
dat2014 = read.csv(file = dfile, header = TRUE, as.is = TRUE, na.strings = c("NA", "x", "X"))

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

dat2014$datetime = with(dat2014, paste(Date, Time)) # Convert DateTime to POSIXlt class
dat2014$datetime = as.POSIXlt(dat2014$datetime) # not really necessary at this stage, but a place holder for possible future analysis / visualizing time series

#====== +++ === === +++ === === +++ === ===
# Keep munging
#====== +++ === === +++ === === +++ === ===
length(unique(dat2014$datetime)) # number of "Relative Abundance and Distribution" (RAD) samples in 2014

#====== +++ === === +++ === === +++ === ===
# Start summarizing counts by making tables
# ?table
#====== +++ === === +++ === === +++ === ===
table(dat2014$Stratum, dat2014$GroupSize)
as.data.frame(table(dat2014$Stratum, dat2014$GroupSize))
group.size = table(dat2014$SubStratum, dat2014$GroupSize)
group.size = as.data.frame(group.size)
names(group.size) = c("SubStratum", "GroupSize", "Freq")
write.csv(file = "foo.csv", x = group.size); system("open foo.csv") # check

#====== +++ === === +++ === === +++ === ===
# Summarize abundance by SubStratum (integrating over time)
# ?table
#====== +++ === === +++ === === +++ === ===
tot.counts = group.size
i = sapply(tot.counts, is.factor) # intermediate step to convert from factor to character
tot.counts[i] <- lapply(tot.counts[i], as.character) # convert columns that are factors to character strings
tot.counts$TotalCount = as.numeric(tot.counts$GroupSize) * tot.counts$Freq # TotalCounts are product of group size and numbers of groups
tot.counts$Stratum = substring(tot.counts$SubStratum, first = 1, last = 1) # Create a vector with Stratum from SubStratum vector
tot.counts$SubStratum.num = substring(tot.counts$SubStratum, first = 2, last = 2) # Create a vector with SubStratum.num from SubStratum vector

tot.counts.subs = ddply(tot.counts, "SubStratum", summarise, TotalCount = sum(TotalCount)) # uses 'plyr' package, could also use function aggregate
# tot.counts.subs = aggregate(tot.counts.tmp$TotalCount, by = list(SubStratum = tot.counts.tmp$SubStratum), sum) # same result as line above

head(tot.counts.subs)
write.csv(file = "foo.csv", x = foo); system("open foo.csv") # check

#====== +++ === === +++ === === +++ === ===
# NEXT STEP: Summarize by Stratum (integrating over time)
#====== +++ === === +++ === === +++ === ===
tot.counts.strat = ddply(tot.counts, "Stratum", summarise, TotalCount = sum(TotalCount)) # uses 'plyr' package, could also use function aggregate
head(tot.counts.strat)
tot.counts.strat
library(ggplot2)
ggplot(tot.counts.strat, aes(x = Stratum, y = TotalCount)) + geom_bar(stat = "identity")

#####
######
?cast
ddply(tot.counts, "SubStratum", sum, Total = sum(TotalCount))
?transform

col1 = unique(dat2014$datetime) # make a colum
col.names = c("datetime", unique(dat2014$SubStratum), unique(dat2014$Stratum))
col.names

unique(dat2014$GroupSize)
table(dat2014$GroupSize)
gs = dat2014$GroupSize[-which(is.na(dat2014$GroupSize))] # Group size without blank observations due to weather
?merge
max(gs)

which(is.na(dat2014$GroupSize))

str(dat2014$GroupSize)
plot(dat2014$datetime, dat2014$GroupSize) 
hist(dat2014$GroupSize, breaks = 100, col = "grey")

# Save workspace image
save.image("~/Documents/2014 Work/Milne Inlet Narwhals/2014 Analysis/Code/MilneNarwhal.2014.RData")

# Scratch code below
library(reshape2)
smiths # example data table from Hadley Wickham
melt(smiths)
melt(smiths, id = c("subject", "time"), measured = c("age", "weight", "height"))
melt(smiths, id = c("subject", "time"))
melt(smiths, id = 1:2)
melt(smiths, measured = c("age", "weight", "height"))
melt(smiths)

trial <- data.frame(id = factor(1:4), A1 = c(1, 2, 1, 2), A2 = c(2, 1, 2, 1), B1 = c(3, 3, 3, 3))
trial

(trialm <- melt(trial))
(trialm <- cbind(trialm, colsplit(trialm$variable, names = c("treatment", "time"))))


names(airquality) <- tolower(names(airquality))
head(airquality)
aqm <- melt(airquality, id=c("month", "day"), na.rm=TRUE)
head(aqm)
str(aqm)
cast(aqm, day ~ month ~ variable)
cast(aqm, month ~ variable, mean)
cast(aqm, month ~ . | variable, mean)
cast(aqm, month ~ variable, mean, margins=c("grand_row", "grand_col"))
cast(aqm, day ~ month, mean, subset=variable=="ozone")
cast(aqm, month ~ variable, range)
cast(aqm, month ~ variable + result_variable, range)
cast(aqm, variable ~ month ~ result_variable,range)

baberuth <- subset(baseball, id == "ruthba01")
head(baberuth)
baberuth <- transform(baberuth, cyear = year - min(year) + 1)
head(baberuth)
baseball <- ddply(baseball, .(id), transform, cyear = year - min(year) + 1)
head(baseball)


head(baseball)
ddply(tot.counts, "SubStratum", summarise, sum(TotalCount))
foo = ddply(tot.counts, "SubStratum", summarise, TotalCount = sum(TotalCount))
head(foo)

str(tot.counts)
?by
?arrange
tot.counts.tmp = tot.counts
tot.counts.tmp$SubStratum = as.factor(tot.counts.tmp$SubStratum)
by(tot.counts.tmp$TotalCount, tot.counts.tmp$SubStratum, sum)
foo = aggregate(tot.counts.tmp$TotalCount, by = list(SubStratum = tot.counts.tmp$SubStratum), sum)
names(foo)
names(foo)[2] <- "TotalCount"

head(foo)
str(foo)

by(tot.counts$TotalCount, tot.counts$SubStratum, sum)
foo = aggregate(tot.counts$TotalCount, by = list(SubStratum = tot.counts$SubStratum), sum)

mydat <- data.frame(first = rpois(10,10), second = rpois(10,10), third = rpois(10,10), group = c(rep("a",5),rep("b",5)))
mydat
aggregate(mydat[,1:3], by=list(mydat$group), mean)
aggregate(mydat[,1:3], by=list(mydat$group), sum)
ddply(mydat, .(group), function(u) cor(u$first, u$second))


head(state.x77)
str(state.x77)
state.x77$state.region
aggregate(state.x77, list(Region = state.region), mean)
state.x77

