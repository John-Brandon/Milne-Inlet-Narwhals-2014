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
#  5) You will need to have ADMB installed (http://www.admb-project.org/)
#  6) The 'glmmADMB' package might require some special instructions to install
#   See: http://glmmadmb.r-forge.r-project.org/
#====== +++ === === +++ === === +++ === ===
rm(list=ls()) # clear leftovers from previous workspace

# Load workspace with Munged 2014 Data
load("~/Documents/2014 Work/Milne Inlet Narwhals/2014 Analysis/Code/MilneNarwhal.2014.RData") 

# Load workspace with munged 2013 AND 2014 Tide Data
load("~/Documents/2014 Work/Milne Inlet Narwhals/2014 Analysis/Code/TideData.MilneNarwhal.2014.RData")

# Load packages, this list is defined in Munging2014Data script
load.packages()

#====== +++ === === +++ === === +++ === ===
# Read 2013 RAD data 
#====== +++ === === +++ === === +++ === ===
setwd("~/Documents/2014 Work/Milne Inlet Narwhals/Data/2013") # Set working directory to 2013 data
dfile2013 = "2013.milne.inlet.narwhal.csv" # 2013 RAD data file, saved as comma delimited
dat2013 = read.csv(file = dfile2013, header = TRUE, as.is = TRUE, na.strings = c("NA", "x", "X")) # Read data file 

#====== +++ === === +++ === === +++ === ===
# Follow Scott Raborn's "filters" from 2013 report
#  After filters, there were 5 days that met criteria for inclusion in model: 13, 14, 21, 23 and 26 Aug (see Table D-1 of 2013 report)
#  Note: 2013 was the Pilot Study year. So, there was some on the fly learning, and data collection protocol evolved in field.
#   e.g. not all stratum surveyed (missing counts) and environmental data not recorded for each stratum during first few counts.
#
#   TODO : Simplify this code? 
#    There is a function in munging script called 'factor.count.quality'
#    That function assigns "Good" or "Poor" to count quality (by Count.id)
#    This filter could be re-written as a simple subset command, 
#     selecting only those Count.ids with "Good" count.quality
#====== +++ === === +++ === === +++ === ===
filter.sight = function(dat){
# This is a strict version of filtering; if only one stratum doesn't meet criteria, that count is ignored across all strata  
# filter out counts if any stratum is in (i) Poor sightability or (ii) Sightability was not recorded, i.e. NA (when rain).

  table_sight_countid = with(dat, table(Sightability, Count.id, useNA = "ifany"))
  table_sight_countid = as.data.frame(table_sight_countid)
  
  good.dat = with(table_sight_countid, Freq[which(Sightability == "P")]) # Returns vector with number of "P"s for each Count.id
  good.dat = ifelse(good.dat == 0, TRUE, FALSE) # Returns vector of TRUE / FALSE's
  count.keep.ii = which(good.dat == TRUE) # pre-filter for Poor Sightability

  good.dat.na = with(table_sight_countid, Freq[which(is.na(Sightability))]) # Returns vector with number of NAs for each Count.id
  good.dat.na = ifelse(good.dat.na == 0, TRUE, FALSE) # Returns vector of TRUE / FALSE's
  count.keep.na.ii = which(good.dat.na == TRUE) # pre-filter for NA Sightability
  
  count.keep.set = intersect(count.keep.ii, count.keep.na.ii) # count.id needs to meet both P and NA pre-filter conditions to be kept

  dat = filter(dat, Count.id %in% count.keep.set) # filter is a function from dplyr package -- selects rows that meet a criteria
  return(dat)
}

#====== +++ === === +++ === === +++ === ===
# Merge tide data into RAD count data.frame
#  Not sure if names in 2013 tide data are consistent, so naming this specifically for 2013
#====== +++ === === +++ === === +++ === ===
merge.2013.dat.tides = function(dat, dat.tides){ 
  dat.tides$datetime = as.POSIXct(dat.tides$datetime) # make sure datetime class is consistent with main data.frame's
  
  # just get the columns of tide data that are desired for merged data.frame
  dat.tides.2013.subset = subset(dat.tides, select = c(datetime, Elevation, highlow, delta, risingfalling, tidestate))
  dat = merge(x = dat, y = dat.tides.2013.subset, by.x = "datetime.rounded.to.five.min", by.y = "datetime")
  return(dat)
  # write.csv(x = dat2014, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check  
}

# Munge 2013 data 
#  TODO: Move this to Munging script?
dat2013 = remove.extraneous.columns(dat2013)
dat2013 = do.dates.and.ids(dat2013)
dat2013 = assign.vessel.boolean(dat2013)
dat2013 = factor.group.size(dat2013)
dat2013 = assign.strat.sight.2014(dat2013)
dat2013 = merge.2013.dat.tides(dat2013, dat.tides.2013) 

str(dat2013) # Check

filtered.dat2013 = filter.sight(dat2013) # filter sightability, returns only those counts entirely in Good or Excellence (no P or NA)
#write.csv(filtered.dat2013, "foo10.csv"); system("open foo10.csv") # Check

tot.counts.2013 = ddply(filtered.dat2013, .(Count.id, Stratum), summarise, 
                        value = sum(GroupSize, na.rm = TRUE), 
                        CountType = unique(CountType),
                        SeaState = ifelse(length(unique(SeaState)) > 0, paste(unique(SeaState), collapse = "," ), NA),
                        datetime = unique(datetime), 
                        where = unique(Where1)) # uses 'plyr' package, could also use function aggregate
cast(tot.counts.2013, Count.id + CountType + where + datetime ~ Stratum)

head(filtered.dat2013) # Check
# Have a look at Sightability and SeaState
# foo.ii = with(filtered.dat2013, which(SeaState > 2 & Sightability %in% c("G", "E")))
# length(foo.ii)
# filtered.dat2013$Count.id[foo.ii]

#====== +++ === === +++ === === +++ === ===
# 2014
#  'Munging2014Data' script already calls functions like 'do.dates.and.ids'
#  Loading 'MilneNarwhal.2014.RData' loads in a munged, filtered 2014 data.frame
#   So, just need to continue with processing at this stage
#====== +++ === === +++ === === +++ === ===
tot.counts.2014 = ddply(filtered.dat2014, .(Count.id, Stratum), summarise, 
                        value = sum(GroupSize, na.rm = TRUE), 
                        CountType = unique(CountType),
                        #SeaState = unique(SeaState), 
                        datetime = unique(datetime), 
                        where = unique(Where1)) # uses 'plyr' package, could also use function aggregate
dat.mat.2014 = cast(tot.counts.2014, Count.id + CountType + where + datetime ~ Stratum)
dat.mat.2014
head(filtered.dat2014)
foo = filtered.dat2014$GroupSize > 0
length(which(foo == TRUE))

#====== +++ === === +++ === === +++ === ===
# Join filtered.dat2014 and filtered.dat2013 data.frames
#====== +++ === === +++ === === +++ === ===
create.missing.columns = function(dat, column.names){
  tmp.df = matrix(nrow = nrow(dat), ncol = length(column.names)) 
  tmp.df = as.data.frame(tmp.df)
  names(tmp.df) = column.names
  dat = cbind(dat, tmp.df)
  return(dat)
}

columns.to.add.2013 = setdiff(names(filtered.dat2014), names(filtered.dat2013)) # names of column vectors that are found in dat 2014 but not dat 2013
filtered.dat2013 = create.missing.columns(filtered.dat2013, columns.to.add.2013)

columns.to.add.2014 = setdiff(names(filtered.dat2013), names(filtered.dat2014)) # names of column vectors that are found in dat 2013 but not dat 2014
filtered.dat2014 = create.missing.columns(filtered.dat2014, columns.to.add.2014)

# Create a table for Heather -- with group directions
View(filtered.dat2014)
names(filtered.dat2014)
filtered.no.vess.dat2014 = subset(filtered.dat2014, Vessel.related.count == FALSE & GroupSize > 0)
table(filtered.no.vess.dat2014$Direction, useNA = "always")
#
filtered.dat.join = rbind(filtered.dat2013, filtered.dat2014)
filtered.dat.join$Year = year(filtered.dat.join$datetime) # Create a column with Year
filtered.dat.join$Count.id.long = with(filtered.dat.join, paste(Year, Count.id, sep="."))
str(filtered.dat.join)

tot.counts.join = ddply(filtered.dat.join, .(Count.id.long, Stratum), summarise, 
                        value = sum(GroupSize, na.rm = TRUE), 
                        CountType = unique(CountType),
                        #SeaState = unique(SeaState), 
                        datetime = unique(datetime), 
                        julian.date = unique(julian.date),
                        hour = unique(dec.hour), 
                        tide.height = unique(Elevation),
                        tide.delta = unique(delta),
                        tide.state = unique(tidestate),
                        year = unique(Year), 
                        sea.state = max(SeaState)
                        ) # uses 'plyr' package, could also use function aggregate

# table distribution of counts by sea state for Val
View(subset(tot.counts.join, year == 2014 & disturbance == TRUE))
View(tot.counts.join)
counts.by.seastate = with(tot.counts.join, table(sea.state, value)) 
write.csv(counts.by.seastate, "counts.by.seastate.csv"); system("open counts.by.seastate.csv")
#

# do some prep for model input
names(tot.counts.join)
dates.tmp1.ii = which(tot.counts.join$julian.date == 235 & tot.counts.join$year == 2013)
dates.tmp2.ii = which(tot.counts.join$julian.date == 238 & tot.counts.join$year == 2013)
tot.counts.join$julian.date[dates.tmp1.ii] = 236
tot.counts.join$julian.date[dates.tmp2.ii] = 239
tot.counts.join = mutate(tot.counts.join, julian.date.scaled = scale(julian.date))
tot.counts.join = mutate(tot.counts.join, tide.delta = scale(tide.delta))
tot.counts.join = mutate(tot.counts.join, year = as.factor(year)) # year as factor
tot.counts.join = tot.counts.join[-which(tot.counts.join$Stratum == "A"),]  # drop Stratum A
tot.counts.join$grouped.strat = rep(NA, nrow(tot.counts.join))
tot.counts.join$grouped.strat[which(tot.counts.join$Stratum < "G")] = "BCDEF"
tot.counts.join$grouped.strat[which(tot.counts.join$Stratum > "F")] = "GHI"
tot.counts.join$grouped.strat = as.factor(tot.counts.join$grouped.strat)
tot.counts.join$disturbance = rep(FALSE, nrow(tot.counts.join))
tot.counts.join$disturbance[which(tot.counts.join$CountType %in% c("PRE", "C", "POST"))] = TRUE
tot.counts.join = mutate(tot.counts.join, 
                         year = as.factor(year),
                         Stratum = as.factor(Stratum),
                         factor.julian.date.scaled =factor(julian.date.scaled))
tot.counts.join = mutate(tot.counts.join, julian.date.scaled_2 = julian.date.scaled^2)
tot.counts.join = mutate(tot.counts.join, factor.julian.date = factor(julian.date),
                         julian.date_2 = julian.date^2)


str(tot.counts.join)
with(tot.counts.join, length(unique(Count.id.long))) # 116 counts
View(tot.counts.join)

unique(tot.counts.join$julian.date[which(tot.counts.join$year == 2013)])
unique(tot.counts.join$julian.date[which(tot.counts.join$year == 2014)])

unique(tot.counts.join$julian.date)
tab.counts = table(tot.counts.join$value)
tab.counts = as.data.frame(tab.counts)
names(tab.counts) = c("Observed", "Frequency"); head(tab.counts)
tab.counts = mutate(tab.counts, log.Frequency = log(Frequency))

brks = as.character(seq(from = 0, to = nrow(tab.counts)+5, by = 5))
brks = seq(from = 0, to = nrow(tab.counts)+5, by = 5)
library(scales)
ggplot(data = tab.counts, aes(x = Observed, y = log.Frequency)) + geom_bar(stat = "identity") +
  mytheme_bw + ylab("Ln (Frequency)") + scale_x_discrete(breaks = brks)


# nrow(tab.counts)
# # fit some preliminary models
# test.glmer1 = glmer.nb(data = tot.counts.join, value ~ tide.delta + (1 | Stratum))
# summary(test.glmer1)
# test.glmer2 = glmer.nb(data = tot.counts.join, value ~ tide.delta + CountType + (1 | Stratum))
# summary(test.glmer2)
# test.glmer3 = glmer.nb(data = tot.counts.join, value ~ CountType + (1 | Stratum))
# summary(test.glmer3)
# test.glmer4 = glmer.nb(data = tot.counts.join, value ~ CountType + (1 | Stratum) + julian.date.scaled)
# summary(test.glmer4)
# test.glmer5 = glmer.nb(data = tot.counts.join, value ~ CountType + (1 | Stratum) + (1 | year) + julian.date.scaled)
# summary(test.glmer5)
# as.factor(tot.counts.join$Stratum)

library("glmmADMB")
library(coefplot2) 
# === # === #
# Here is the model used for results in draft rep 22 Dec 2014
# View(tot.counts.join)
# write.csv(tot.counts.join, "tot.counts.join.csv"); system("open tot.counts.join.csv")
# file.show(system.file("tpl","glmmadmb.tpl",package="glmmADMB"))
# === # === #
test.glmmadmb11 = glmmadmb(value ~ disturbance + 
                             tide.delta + 
                             sea.state + 
                             julian.date.scaled +
                             julian.date.scaled_2 +
                             (1 | grouped.strat) + (1 | Stratum) + 
                             (1 | factor.julian.date.scaled), 
                           data = tot.counts.join, 
                           zeroInflation = TRUE, 
                           family="nbinom1")

summary(test.glmmadmb11)
str(test.glmmadmb11) # Check
# ranef(test.glmmadmb11)

fitted.11 = as.vector(test.glmmadmb11$fitted)
coefplot2(test.glmmadmb11, cex.main = 1.2, varnames = rev(c("Date Squared", "Date", "Sea State", "Tide Flow", "Large Vessel Presence")))

# confint(test.glmmadmb11)

# === # === #
# Here is plot used in Jan 2014 draft 
# === # === #
# plot(jitter(tot.counts.join$julian.date), tot.counts.join$value, col = alpha("blue", 0.75), xlab = "Julian Date", ylab = "Stratum Counts")
# points(jitter(tot.counts.join$julian.date), as.vector(test.glmmadmb11$fitted), col = alpha("red", 0.7), pch=4)
# === # === #
# Redo the plot with month / day instead of julian date
date.foo = c(215, 225, 235, 245)
date.foo = as.Date(date.foo, origin = "2013-01-01") 
date.foo = format(date.foo, "%b-%d")
plot(jitter(tot.counts.join$julian.date), 
     tot.counts.join$value, col = alpha("blue", 0.75), 
     xlab = "Date", 
     ylab = "Number of Narwhals (Stratum Counts)", 
     xaxt = "n", 
     cex = 1.5,
     cex.axis = 1.3,
     cex.lab = 1.3)
axis(1, at = c(215, 225, 235, 245), 
     labels = date.foo,
     cex.axis = 1.3)
points(jitter(tot.counts.join$julian.date), as.vector(test.glmmadmb11$fitted), col = alpha("red", 0.7), pch=4)
legend("topright", 
       legend = c("Observed", "Model Fit"), 
       col = c("blue", "red"), pch = c(1, 4),
       cex = 1.3)
text(216, 200, "(A)", cex = 1.5)
# === # === #
# Plot residuals -- this plot used in draft
# === # === #
predict(test.glmmadmb11, type = "response")
tmp.res = tot.counts.join$value - as.vector(test.glmmadmb11$fitted)
plot(log(fitted(test.glmmadmb11)), tmp.res,
     xlab = "Ln (Fitted)", ylab = "Residual",
     cex = 1.5,
     cex.axis = 1.3,
     cex.lab = 1.3) # log of observed residual plot
abline(h = 0)
text(-3.75, 150, "(B)", cex = 1.5)


# === # === #
# How many counts (over stratum) were there
# === # === #
counts.2013 = subset(tot.counts.join, year == 2013)
counts.2014 = subset(tot.counts.join, year == 2014)
with(counts.2013, length(unique(Count.id.long)))
with(counts.2014, length(unique(Count.id.long)))
with(tot.counts.join, length(unique(Count.id.long)))

# === # === #
# Look at percentages of zeros in 2013 v 2014
#  Report these numbers in results section
# === # === #
length(which(counts.2013$value == 0)); nrow(counts.2013)
length(which(counts.2014$value == 0)); nrow(counts.2014)
length(which(counts.2013$value == 0)) / nrow(counts.2013)
length(which(counts.2014$value == 0)) / nrow(counts.2014)
max(counts.2013$value); max(counts.2014$value)


save.image("~/Documents/2014 Work/Milne Inlet Narwhals/2014 Analysis/Code/MilneNarwhal.2014.RData") 

