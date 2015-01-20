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
dfile2013 = "2013.milne.inlet.narwhal.csv" # 2014 RAD data file, saved as comma delimited
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
#  Not sure if names in 2013 tide data are consistent, so naming this specifically for dat2014
#====== +++ === === +++ === === +++ === ===
merge.2013.dat.tides = function(dat, dat.tides){ 
  dat.tides$datetime = as.POSIXct(dat.tides$datetime) # make sure datetime class is consistent with main data.frame's
  
  # just get the columns of tide data that are desired for merged data.frame
  dat.tides.2013.subset = subset(dat.tides, select = c(datetime, Elevation, highlow, delta, risingfalling, tidestate))
  dat = merge(x = dat, y = dat.tides.2013.subset, by.x = "datetime.rounded.to.five.min", by.y = "datetime")
  return(dat)
  # write.csv(x = dat2014, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check  
}

dat2013 = remove.extraneous.columns(dat2013)
dat2013 = do.dates.and.ids(dat2013)
dat2013 = assign.vessel.boolean(dat2013)
dat2013 = factor.group.size(dat2013)
dat2013 = assign.strat.sight.2014(dat2013)
dat2013 = merge.2013.dat.tides(dat2013, dat.tides.2013) 

str(dat2013)
# dat2013 = rename(dat2013, CountType = WatchType) #Debugging
citation()
filtered.dat2013 = filter.sight(dat2013) # filter sightability, returns only those counts entirely in Good or Excellence (no P or NA)
#write.csv(filtered.dat2013, "foo10.csv"); system("open foo10.csv")

tot.counts.2013 = ddply(filtered.dat2013, .(Count.id, Stratum), summarise, 
                        value = sum(GroupSize, na.rm = TRUE), 
                        CountType = unique(CountType),
                        SeaState = ifelse(length(unique(SeaState)) > 0, paste(unique(SeaState), collapse = "," ), NA),
                        datetime = unique(datetime), 
                        where = unique(Where1)) # uses 'plyr' package, could also use function aggregate
cast(tot.counts.2013, Count.id + CountType + where + datetime ~ Stratum)

head(filtered.dat2013)
# Have a look at Sightability and SeaState
# foo.ii = with(filtered.dat2013, which(SeaState > 2 & Sightability %in% c("G", "E")))
# length(foo.ii)
# filtered.dat2013$Count.id[foo.ii]
R.Version()
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


nrow(tab.counts)
# fit some preliminary models
test.glmer1 = glmer.nb(data = tot.counts.join, value ~ tide.delta + (1 | Stratum))
summary(test.glmer1)
test.glmer2 = glmer.nb(data = tot.counts.join, value ~ tide.delta + CountType + (1 | Stratum))
summary(test.glmer2)
test.glmer3 = glmer.nb(data = tot.counts.join, value ~ CountType + (1 | Stratum))
summary(test.glmer3)
test.glmer4 = glmer.nb(data = tot.counts.join, value ~ CountType + (1 | Stratum) + julian.date.scaled)
summary(test.glmer4)
test.glmer5 = glmer.nb(data = tot.counts.join, value ~ CountType + (1 | Stratum) + (1 | year) + julian.date.scaled)
summary(test.glmer5)
as.factor(tot.counts.join$Stratum)

library("glmmADMB")
test.glmmadmb5 = glmmadmb(value ~ CountType + (1 | Stratum) + (1 | year) + julian.date.scaled, 
                           data=tot.counts.join, 
                           zeroInflation=TRUE, 
                           family="nbinom")
summary(test.glmmadmb5)
library(coefplot2) 
coefplot2(test.glmmadmb5)

test.glmer6 = glmer.nb(value ~ disturbance + (1 | grouped.strat) + (1 | year), data = tot.counts.join)
summary(test.glmer6)
test.glmmadmb6a = glmmadmb(value ~ disturbance + (1 | grouped.strat) + (1 | year), 
                          data=tot.counts.join, 
                          zeroInflation=TRUE, 
                          family="nbinom")

summary(test.glmmadmb6a)
hist(residuals(test.glmmadmb6a))
coefplot2(test.glmmadmb6a)


test.glmmadmb6b = glmmadmb(value ~ disturbance + (1 | grouped.strat) + (1 | year), 
                           data=tot.counts.join, 
                           zeroInflation=FALSE, 
                           family="nbinom")
summary(test.glmmadmb6b)
coefplot2(test.glmmadmb6b)
AIC(test.glmmadmb6a); AIC(test.glmmadmb6b)
ranef(test.glmmadmb6)
plot(test.glmmadmb6)
predict(test.glmmadmb6, type = "response")

test.glmmadmb7 = glmmadmb(value ~ disturbance 
                           (1 | grouped.strat) + (1 | Stratum), 
                           data=tot.counts.join, 
                           zeroInflation=TRUE, 
                           family="nbinom")
coefplot2(test.glmmadmb7)
summary(test.glmmadmb7)

test.glmmadmb8 = glmmadmb(value ~ disturbance + 
                             (1 | Stratum) + 
                             (1 | factor.julian.date.scaled), 
                           data=tot.counts.join, 
                           zeroInflation=TRUE, 
                           family="nbinom1")

coefplot2(test.glmmadmb8)
summary(test.glmmadmb8)
hist(residuals(test.glmmadmb8))


test.glmmadmb8a = glmmadmb(value ~ disturbance + 
                            (1 | grouped.strat) + (1 | Stratum) + 
                            (1 | factor.julian.date.scaled), 
                          data=tot.counts.join, 
                          zeroInflation=TRUE, 
                          family="nbinom1")

coefplot2(test.glmmadmb8a)
summary(test.glmmadmb8a)
hist(residuals(test.glmmadmb8a))

str(tot.counts.join)

# Try with different parameterization of NB distribution (here: Var = theta * mu)
test.glmmadmb8b = glmmadmb(value ~ disturbance + 
                            (1 | grouped.strat) + (1 | Stratum) + 
                            (1 | factor.julian.date.scaled), 
                          data=tot.counts.join, 
                          zeroInflation=TRUE, 
                          family="nbinom1") 

ranef(test.glmmadmb8b, condVar = TRUE)
coefplot2(test.glmmadmb8b)
summary(test.glmmadmb8b)
res8b = residuals(test.glmmadmb8b) # type = "response"
res8b = residuals(test.glmmadmb8b, type = "response") # 
hist(res8b)
plot(predict(test.glmmadmb8b, type = "response"), res8b)
qqplot(res8b)
?qqplot
?dotplot
?lattice

test.glmmadmb9 = glmmadmb(value ~ disturbance + 
                            tide.delta + 
                             (1 | grouped.strat) + 
                             (1 | Stratum) + 
                             (1 | factor.julian.date.scaled), 
                           data=tot.counts.join, 
                           zeroInflation=TRUE, 
                           family="nbinom1")

summary(test.glmmadmb9)
hist(residuals(test.glmmadmb9))
coefplot2(test.glmmadmb9)

test.glmmadmb10 = glmmadmb(value ~ disturbance + 
                            tide.delta + 
                            sea.state + 
                            (1 | grouped.strat) + (1 | Stratum) + 
                            (1 | factor.julian.date.scaled), 
                          data=tot.counts.join, 
                          zeroInflation=TRUE, 
                          family="nbinom1")

summary(test.glmmadmb10)
hist(residuals(test.glmmadmb10))
coefplot2(test.glmmadmb10)
coefplot2(test.glmmadmb10, cex.main = 1.2, varnames = rev(c("Sea State", "Tidal Flow", "Large Vessel Presence")))

# === # === #
# Try model used in results but without zero inflation for comparison
#  Zero inflation is estimated to be zero in model 11, which suggests
#  solution might not be ideal 
# === # === #
test.glmmadmb11.nozi = glmmadmb(value ~ disturbance + 
                             tide.delta + 
                             sea.state + 
                             julian.date.scaled +
                             + julian.date.scaled_2 +
                             (1 | grouped.strat) + (1 | Stratum) + 
                             (1 | factor.julian.date.scaled), 
                           data = tot.counts.join, 
                           zeroInflation = FALSE, 
                           family="nbinom1")
summary(test.glmmadmb11.nozi)
# === # === #
# Here is the model used for results in draft rep 22 Dec 2014
# View(tot.counts.join)
# write.csv(tot.counts.join, "tot.counts.join.csv"); system("open tot.counts.join.csv")
# file.show(system.file("tpl","glmmadmb.tpl",package="glmmADMB"))
# === # === #
test.glmmadmb.working = test.glmmadmb11
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
test.glmmadmb11 = test.glmmadmb11b
summary(test.glmmadmb11)
str(test.glmmadmb11)
ranef(test.glmmadmb11)

fitted.11 = as.vector(test.glmmadmb11$fitted)
coefplot2(test.glmmadmb11, cex.main = 1.2, varnames = rev(c("Date Squared", "Date", "Sea State", "Tide Flow", "Large Vessel Presence")))

confint(test.glmmadmb11)
methods(confint) # confit.admb
confint.default

# === # === #
# Here is plot used in Jan 2014 draft 
# === # === #
plot(jitter(tot.counts.join$julian.date), tot.counts.join$value, col = alpha("blue", 0.75), xlab = "Julian Date", ylab = "Stratum Counts")
points(jitter(tot.counts.join$julian.date), as.vector(test.glmmadmb11$fitted), col = alpha("red", 0.7), pch=4)
# === # === #
exp(-4)
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

plot(tot.counts.join$value[zeros.ii], fitted(test.glmmadmb11, type = "response")[zeros.ii])
abline(1,1)
cbind(tot.counts.join$value[zeros.ii], fitted(test.glmmadmb11, type = "response")[zeros.ii])
?fitted
AIC
rp = rpois(1000, 5)
mod.rp = glm(rp~1, family = poisson)
fitted(mod.rp)
# Have a look at sqrt of residuals (see Gelman and Hill p341, 15.10)
sqrt.res = sqrt(tot.counts.join$value) - sqrt(as.vector(test.glmmadmb11$fitted))
hist(sqrt.res)
plot(log(fitted(test.glmmadmb11)), sqrt.res,
     xlab = "Ln (Fitted)", ylab = "Residual",
     cex = 1.5,
     cex.axis = 1.3,
     cex.lab = 1.3) # log of observed residual plot
abline(h = 0)

plot(log(predict(test.glmmadmb11, type = "response")), tmp.res,
     xlab = "Ln (Predicted)", ylab = "Residual",
     cex = 1.5,
     cex.axis = 1.3,
     cex.lab = 1.3) # log of observed residual plot
abline(h = 0)

plot(log(tot.counts.join$value +0.01), tmp.res,
     xlab = "Ln (Observed)", ylab = "Residual",
     cex = 1.5,
     cex.axis = 1.3,
     cex.lab = 1.3) # log of observed residual plot
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

# === # === #
# intermediate / exploratory plots of count data
# === # === #
plot(jitter(tot.counts.join$julian.date[which(tot.counts.join$year == 2014)]), tot.counts.join$value[which(tot.counts.join$year == 2014)], col = "red", ylim =c(0, 250))
points(jitter(tot.counts.join$julian.date[which(tot.counts.join$year == 2013)]), tot.counts.join$value[which(tot.counts.join$year == 2013)], col = "blue")
with(tot.counts.join, hist(value))
with(tot.counts.join, max(value))
xx = seq(from = 0, to = 215, by = 1)
plot(xx, dnorm(xx), type = 'l', xlim = c(-3.1, 3.1), xaxs = 'i', yaxs = 'i')
range(resid(test.glmmadmb11, type = "response"))
?glmmADMB
test.glmmadmb11$phi # Matrix for converting from 'orthogonalized' to 'real' parameters: see Details
test.glmmadmb.foo = glmmadmb(value ~ (1 | factor.julian.date) + julian.date_2, 
                             data = tot.counts.join, 
                             family = "nbinom1", 
                             zeroInflation=TRUE)
summary(test.glmmadmb.foo)
test.glmmadmb.foo$b %*% test.glmmadmb.foo$phi
fitted(test.glmmadmb.foo)
names(tot.counts.join)
sessionInfo()
# === # === #
# plot residuals
# === # === #
res<-residuals(test.glmmadmb11, type="pearson")
plot(predict(test.glmmadmb11, type = "response"), res,
     xlab = "Predicted", ylab = "Residual") #for residual plot
plot(log(predict(test.glmmadmb11, type = "response")), res,
     xlab = "Predicted", ylab = "Residual", cex = 1.25, cex.lab = 1.2) #for residual plot
plot(fitted(test.glmmadmb11), res,
     xlab = "Observed", ylab = "Residual") #for residual plot

plot(log(fitted(test.glmmadmb11)), res,
     xlab = "Ln (Fitted)", ylab = "Residual") # log of observed residual plot

tmp.res = tot.counts.join$value - as.vector(test.glmmadmb11$fitted)
plot(log(fitted(test.glmmadmb11)), tmp.res,
     xlab = "Ln (Fitted)", ylab = "Residual") # log of observed residual plot
abline(h = 0)
cex.axis
# calculate deviance residuals
func1<-function(y, model.obj){
  2*model.obj$alpha*log((model.obj$alpha+ fitted(model.obj))/model.obj$alpha)
}
func2<-function(y, model.obj){
  2*y* log(y/fitted(model.obj))-2*(model.obj$alpha+ y)* log((model.obj$alpha+ y)/ (model.obj$alpha+ fitted(model.obj)))
} 
d2<-ifelse(tot.counts.join$value==0,func1(tot.counts.join$value, test.glmmadmb11),func2(tot.counts.join$value, test.glmmadmb11))
ei<-sign(tot.counts.join$value-fitted(test.glmmadmb11))*sqrt(d2)
plot(fitted(test.glmmadmb11), ei)
hist(ei)
plot(log(fitted(test.glmmadmb11)), ei)
plot(log(fitted(test.glmmadmb11)[-zeros.ii]), ei[-zeros.ii])
hist(ei[-zeros.ii])

summary(test.glmmadmb11)

?fitdistr
fitdistr(tot.counts.join$value, "negative binomial")
plot(xx, dnbinom(xx, size = 0.0565, mu = 7.07), type = 'l')
rnbinom(100, size = 0.0565, mu = 7.07) # check
?dnbinom
dev.off()
names(tot.counts.join)
View(data.frame(tot.counts.join$Count.id.long, tot.counts.join$value, predict(test.glmmadmb11, type = "response"), fitted(test.glmmadmb11)))
length(unique(predict(test.glmmadmb11)))
?predict.glmmadmb
methods(predict)
getAnywhere(predict.glmmadmb)
# work through code for predict.glmmadmb
object = test.glmmadmb11; summary(object)
object$random
as.character(object$fixed)
object$terms
foo = attr(object$terms, "offset")
!is.null(foo)
object$ilinkfun
X <- model.matrix(object, data = NULL)
View(X)
X %*% as.vector(object$b)
rm(object); rm(form); rm(X)


qqnorm(res) #for a qqplot
qqline(res)
abline(a = 0, b = 1)
range(fitted.11)
plot(tot.counts.join$value, fitted.11)
?residuals.glmmadmb
# === # === #
# Look at residuals of non-zero counts
# === # === #
zeros.ii = which(tot.counts.join$value == 0)
res2 = residuals(test.glmmadmb11, type="pearson")[-zeros.ii]
length(res2)
plot(predict(test.glmmadmb11, type = "response")[-zeros.ii], res2) #for residual plot
sum(res); sum(res2)
range(res); range(res2)
qqnorm(res2)
qqline(res2)

length(zeros.ii) # 710
length(which(round(fitted.11)==0)) # 268
hist(test.glmmadmb11$residuals[zeros.ii])
qlogis(test.glmmadmb11$pz)

with(test.glmmadmb11, range(fitted))
with(test.glmmadmb11, plot(fitted, residuals, ylim = c(-175, 175)))
abline(a = 0, b = -1)
with(test.glmmadmb11, plot(log(fitted), residuals)) # try log of fitted
qqnorm(test.glmmadmb11$residuals)
qqnorm(residuals(test.glmmadmb11, type = "response"))
qqline(residuals(test.glmmadmb11, type = "response"))
abline(a = 0, b = 1)
?predict.glmmadmb
library(gof)
cumres(test.glmmadmb11)

# === # === #
# Try some alternative models
#  Removed some parameters to see if this changes things
# === # === #
names(tot.counts.join)
# try same model as reported in draft without scaling julian.date
test.glmmadmb12 = glmmadmb(value ~ disturbance + 
                             year + 
                             julian.date_2 +
                             sea.state + 
                             tide.delta +
                             hour +
                             (1 | grouped.strat) +
                             (1 | Stratum) +
                             (1 | factor.julian.date), 
                            data = tot.counts.join, 
                            zeroInflation = TRUE, 
                            family="nbinom1")
summary(test.glmmadmb12); summary(test.glmmadmb11)
test.glmmadmb12$alpha
anova(test.glmmadmb11, test.glmmadmb12)
plot(jitter(tot.counts.join$julian.date), tot.counts.join$value, col = alpha("blue", 0.75), xlab = "Date", ylab = "Number of Narwhals (Stratum Counts)", xaxt = "n")
axis(1, at = c(215, 225, 235, 245), labels = date.foo)
points(jitter(tot.counts.join$julian.date), as.vector(test.glmmadmb12$fitted), col = alpha("red", 0.7), pch=4)


fitted.12 = as.vector(test.glmmadmb12$fitted)
coefplot.labs = c("Date Squared", "Date", "Sea State", "Tide Flow", "Large Vessel Presence", "Year")
coefplot2(test.glmmadmb12, cex.main = 1.2, varnames = rev(coefplot.labs))

res12 = residuals(test.glmmadmb12, type="pearson")
plot(predict(test.glmmadmb12, type = "response"), res12,
     xlab = "Predicted", ylab = "Residual") #for residual plot
plot(tot.counts.join$value, fitted(test.glmmadmb12), ylim = c(0,200), xlim = c(0,200))
abline(a = 0, b = 1)
abline(v=0)

# take a look at generalize poisson (longer right tail than NB)
install.packages("GPseq")
library("GPseq") 
yy.tmp = ddply(tot.counts.join, .(Count.id.long), summarise, 
               julian.date = unique(julian.date),
               hour = unique(as.integer(hour)),
               stratum.count = sum(value))
generalized_poisson_likelihood(yy.tmp$stratum.count)

#plot.glmmadmb(test.glmmadmb12)

confint(test.glmmadmb12)

plot(jitter(tot.counts.join$julian.date), tot.counts.join$value, col = alpha("blue", 0.75), xlab = "Julian Date", ylab = "Stratum Counts")
points(jitter(tot.counts.join$julian.date), as.vector(test.glmmadmb12$fitted), col = alpha("red", 0.7), pch=4)

qqnorm(residuals(test.glmmadmb12, type = "response"))
qqline(residuals(test.glmmadmb12, type = "response"))
abline(a = 0, b = 1)

zeros.11 = which(tot.counts.join$value == 0); length(zeros.11); nrow(tot.counts.join) - length(zeros.11)
which(fitted.11 == 0)

rle(tot.counts.join$julian.date)
View(unlist(rle(tot.counts.join$julian.date)))

table(tot.counts.join$value)
str(tot.counts.join)
?qqplot
drop.zero.obs = tot.counts.join$value[-zeros.11]
drop.zero.fit = fitted.11[-zeros.11]
plot(drop.zero.obs, drop.zero.fit)
drop.zero.resid = drop.zero.obs - drop.zero.fit
hist(drop.zero.resid)
qqnorm(drop.zero.resid)

resid(test.glmmadmb11[-zeros.11], type = "response")
qqnorm(resid(test.glmmadmb11[-zeros.11], type = "response"))
?resid

res = tot.counts.join$value - as.vector(test.glmmadmb11$fitted) 
summary(res)
hist(res)

with(tot.counts.join, unique(datetime.rounded.to.hr))
names(tot.counts.join)
ddply(tot.counts.join, .(datetime, value), summarise, 
      date = paste(month(datetime)))

tot.month = with(tot.counts.join, month(datetime))
tot.day = with(tot.counts.join, day(datetime))
month.day = data.frame(tot.month, tot.day)
month.day = arrange(month.day, tot.month, tot.day)
month.day$month.day = with(month.day, paste(tot.month, tot.day, sep = "-"))
unique(month.day$month.day)

# predict.11 = predict(test.glmmadmb11, type = "response")
# plot(tot.counts.join$value)
# points(predict.11, col = "red")

# Custom function to get predictions from glmmadmb models
# http://glmm.wikidot.com/dummy-html
predict.admb <- function(model, newdata, islog=TRUE)
{
  # Construct model matrix, nobs x np
  MM <- model.matrix(model$fixed, data=newdata, contrasts.arg=NULL)
  beta <- as.vector(model$b)
  phat <- MM %*% beta
  if (islog==TRUE) phat <- exp(phat)
  return (phat);
}
str(test.glmmadmb11)
as.vector(test.glmmadmb11$fitted)
plot(as.vector(test.glmmadmb11$fitted))
points(tot.counts.join$value, col = "red")
test.glmmadmb11$fitted - tot.counts.join$value
plot(unlist(test.glmmadmb11))
as.vector(test.glmmadmb11$b)
model.matrix(test.glmmadmb11) %*% as.vector(test.glmmadmb11$b)
MMM = model.matrix(test.glmmadmb11) %*% as.vector(test.glmmadmb11$b)
exp(MMM)
methods(glmmadmb)


names(tot.counts.join)
# ====
class(test.glmmadmb10)[1]
family(test.glmmadmb10)
f = family(test.glmer6)
f$linkfun
exp(predict(test.glmmadmb10, type = "response"))

do.call(f$family, list())
?do.call

install.packages("gof")
library(gof)
cumres(model = test.glmmadmb10, variable = c("predicted", colnames(model.matrix(test.glmmadmb10))),
       )
showMethods('cumres')
methods(cumres)
methods(cumres, class = "cumres.glm")
getAnywhere(cumres.glm)
sim1 <- function(n=100, f=function(x1,x2) {10+x1+x2^2}, sd=1, seed=1) {
  if (!is.null(seed))
    set.seed(seed)
  x1 <- rnorm(n);
  x2 <- rnorm(n)
  X <- cbind(1,x1,x2)
  y <- f(x1,x2) + rnorm(n,sd=sd)
  d <- data.frame(y,x1,x2)
  return(d)
}
d <- sim1(100); l <- lm(y ~ x1 + x2,d)
l
system.time(g <- cumres(l, R=100, plots=50))
g
plot(g)
g1 <- cumres(l, c("y"), R=100, plots=50)
g1
g2 <- cumres(l, c("y"), R=100, plots=50, b=0.5)
g2



?cumres
c("predicted", colnames(model.matrix(test.glmmadmb10))

?coefplot2
methods(coefplot2)
coefplot2.default


str(tot.counts.join)
with(tot.counts.join, unique(sea.state))

test.glmer7 = glmer.nb(value ~ disturbance + 
                         (1 | grouped.strat) + (1 | Stratum) +
                         (1 | factor.julian.date.scaled), data = tot.counts.join)
?glmer
summary(test.glmer7)


anova(test.glmer1, test.glmer2)

plot(test.glmer5)
str(test.glmer5)

# Plot counts by stratum - excluding zero counts
gg.counts.strat = ggplot(data = subset(tot.counts.join, value>0), aes(x = value)) + facet_grid(Stratum ~ .) + geom_bar()
gg.counts.strat + xlab("Number of Narwhals per Count") + ylab("Frequency")


mod.dat = cast(tot.counts.join, Count.id.long + CountType + datetime + julian.date + year + hour + tide.height + tide.delta + tide.state ~ Stratum)
mod.dat$total.count.abcdef = with(mod.dat, A + B + C + D + E + F)
mod.dat$total.count.ghi = with(mod.dat, G + H + I)
mod.dat$total.count = with(mod.dat, A + B + C + D + E + F + G + H + I)
View(mod.dat)
# mod.dat$julian.date.squared = mod.dat$julian.date * mod.dat$julian.date
# scale(mod.dat$julian.date.squared)
head(mod.dat, n=20)
str(mod.dat)
nrow(mod.dat)
with(mod.dat, unique(julian.date))
with(mod.dat, length(unique(julian.date)))

poly(mod.dat$julian.date, 2)[,1]
plot(poly(mod.dat$julian.date, 2)[,1], poly(mod.dat$julian.date, 2)[,2])
# ggplot(data = mod.dat, aes(x = total.count.ghi, y = total.count)) + geom_point() + geom_abline(intercept=0, slope=1, col = "gray")

# ggplot(data = mod.dat, aes(x = hour, y = tide.height)) + geom_point() + facet_grid(year ~ .)
# ggplot(data = mod.dat, aes(x = hour, y = tide.height, color = as.factor(year))) + geom_point() + scale_color_manual(values = c("red", "blue"))
# 
# ggplot(data = mod.dat, aes(x = tide.delta, y = total.count)) + geom_point() + facet_grid(year ~ .)
# ggplot(data = mod.dat, aes(x = tide.delta, y = total.count.ghi)) + geom_point() + facet_grid(year ~ .)
# 
# subset(mod.dat, CountType %in% c("PRE", "C", "POST"))

summary(glm.nb(data = mod.dat, formula = total.count ~ tide.state + julian.date.squared))
summary(glm.nb(data = mod.dat, formula = total.count ~ tide.state + julian.date + julian.date.squared))
summary(glm.nb(data = mod.dat, formula = total.count ~ tide.delta + julian.date + julian.date.squared))
summary(glm.nb(data = mod.dat, formula = total.count ~ tide.height + julian.date + julian.date.squared))
summary(glm.nb(data = mod.dat, formula = total.count ~ julian.date + julian.date.squared))
summary(glm.nb(data = mod.dat, formula = total.count ~ poly(julian.date, 2))) # + CountType))

mod.1 = glm.nb(data = mod.dat, formula = total.count ~ poly(julian.date, 2))
summary(mod.1)
plot(mod.1)

with(mod.dat, plot(julian.date, total.count, xlab = "Julian Date", ylab = "Total Count"))
jd = seq(from = min(mod.dat$julian.date)-1, to = max(mod.dat$julian.date)+1) #, length.out = 200)
lines(jd, predict.glm(mod.1, data.frame(julian.date = jd), type = "response")) # note type = "response" otherwise in ~logspace
mod.1.pred = predict.glm(mod.1, data.frame(julian.date = jd), type = "response", se.fit = TRUE)
mod.1.pred
mod.1.pred = as.numeric(mod.1.pred)
?dnbinom
mod.fit = data.frame(xx = as.numeric(jd), yy = mod.1.pred$fit, se.fit = mod.1.pred$se.fit, cv.fit = mod.1.pred$se.fit / mod.1.pred$fit)
g1 = ggplot() 
g1 = g1 + geom_point(data = mod.dat, aes(x = julian.date, y = total.count, colour = factor(year)))
g1 = g1 + scale_colour_manual(values = c("blue", "red"), name = "Year")
#g1 = g1 + geom_ribbon(data = mod.fit, aes(x = xx, ymin = yy - 2*se.fit, ymax = yy + 2*se.fit, color = "grey", alpha = 0.01))
g1 = g1 + geom_line(data = mod.fit, aes(x = xx, y = yy))
g1 = g1 + geom_ribbon(data = mod.fit, aes(x = xx, ymin = yy*exp(-1.96*cv.fit), ymax = yy*exp(1.96*cv.fit)), colour = "grey", alpha = 0.2)
g1 = g1 + ylab("Total Count") + xlab("Julian Date")
g1 + mytheme 



?scale_colour_manual
?geom_ribbon

?scale_fill_manual

summary(mod.1)
length(mod.1$fitted.values)
predict(mod.1)
fitted(mod.1)
plot(mod.1)
model.matrix(mod.dat$total.count ~ poly(mod.dat$julian.date, 2))
?poly
(z = poly(1:10, 3))
predict(z, seq(2, 4, 0.5))
plot(cars, xlab = "Speed (mph)", ylab = "Stopping distance (ft)",
     las = 1, xlim = c(0, 25))
d <- seq(0, 25, length.out = 200)
for(degree in 1:4) {
  fm <- lm(dist ~ poly(speed, degree), data = cars)
  assign(paste("cars", degree, sep = "."), fm)
  lines(d, predict(fm, data.frame(speed = d)), col = degree)
}
anova(cars.1, cars.2, cars.3, cars.4)
?assign
?model.matrix

require("foreign")
dat <- read.dta("http://www.ats.ucla.edu/stat/stata/dae/nb_data.dta")
str(dat)
dat <- within(dat, {
  prog <- factor(prog, levels = 1:3, labels = c("General", "Academic", "Vocational"))
  id <- factor(id)
})
dat$prog
# Rat=factor(rep(1:30,rep(5,30)))          # Rat identity
# filtered.dat2013 = subset(dat2013, Sightability %in% c("G", "E")) # filter out any sighting conditions that are not Good - Excellent
# str(filtered.dat2013) # check

# dates.to.remove = c("2013-08-10", "2013-08-11", "2013-08-12", "2013-08-17", "2013-08-18", "2013-08-19", "2013-08-22", "2013-08-25")
# filtered.dat2013 = subset(filtered.dat2013, ! Date %in% dates.to.remove) # filter dates
# 
# # filter times -- this is cluggy, but 2013 was a pilot study year and there was some growing pains in the data collection
# delete.ii = which(day(filtered.dat2013$datetime) == 13 & hour(filtered.dat2013$datetime) < 14)
# filtered.dat2013 = filtered.dat2013[-delete.ii,]
# delete.ii = which(day(filtered.dat2013$datetime) == 23 & hour(filtered.dat2013$datetime) == 10)
# filtered.dat2013 = filtered.dat2013[-delete.ii,]
# delete.ii = which(day(filtered.dat2013$datetime) == 26 & hour(filtered.dat2013$datetime) < 15)
# filtered.dat2013 = filtered.dat2013[-delete.ii,]
# View(filtered.dat2013)
# tot.counts.2013 = ddply(filtered.dat2013, .(Count.id, Stratum), summarise, 
#                         value = sum(GroupSize, na.rm = TRUE), 
#                         VesselPresence = unique(WatchType),
#                         SeaState = unique(SeaState), 
#                         datetime = unique(datetime)) 

# TODO: Combine annual data sets, filter, then run this to sum counts (if we want to model summed counts)
tot.counts.2013 = ddply(filtered.dat, .(Count.id, Stratum), summarise, 
                        value = sum(GroupSize, na.rm = TRUE), 
                        VesselPresence = unique(WatchType),
                        SeaState = unique(SeaState), 
                        datetime = unique(datetime)) # uses 'plyr' package, could also use function aggregate
cast(tot.counts.2013, Count.id + VesselPresence + datetime ~ Stratum)
View(tot.counts.2013)

# Plot raw counts by SeaState and add a GLM fit
gfoo = ggplot(data = tot.counts.2013, aes(x = SeaState, y = value)) 
gfoo = gfoo + geom_point(position = position_jitter(width = 0.25), alpha = 0.4) + ylab("Count") + xlab("Sea State") 
gfoo # + stat_smooth(method = glm, family = "poisson")

# make barplots like Figure D-1 in 2013 report (Appendix D)
foo = ddply(tot.counts.2013, .(SeaState), summarize, mean.count = mean(value)) # get mean number per stratum count
barplot(foo$mean.count, names.arg = foo$SeaState, xlab = "Sea State", ylab = "Mean number of narwhals") # , axis.lty=1 
(gfoo1 = ggplot(data = foo, aes(x = SeaState, y = mean.count)) + geom_bar(stat = "identity"))

dat.mat.2013 = cast(tot.counts.2013, datetime + VesselPresence ~ Stratum) # reshape the data.frame into Table D-1 from 2013 report (cast in Wickham's lexicon)

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

## TODO : Mutate certain variables to include in regression, e.g. consider making julian day out of datetime for the X's
# theme_set(theme_grey())

save.image("~/Documents/2014 Work/Milne Inlet Narwhals/2014 Analysis/Code/MilneNarwhal.2014.RData") 

#====== +++ === === +++ === === +++ === ===
## Scratch Code below
#====== +++ === === +++ === === +++ === ===
dir()
?data.matrix
?model.matrix
?model.frame

?gl # generate levels
dd <- data.frame(a = gl(3,4), b = gl(4,1,12)) # balanced 2-way
dd
options("contrasts")
model.matrix(~ a + b, dd)

DF <- data.frame(a = 1:3, b = letters[10:12],
                 c = seq(as.Date("2004-01-01"), by = "week", len = 3),
                 stringsAsFactors = TRUE)
data.matrix(DF[1:2])
data.matrix(DF)

#====== +++ === === +++ === === +++ === ===
## Check glmmadmb examples to see if admb estimates zeros
#====== +++ === === +++ === === +++ === ===
data("Owls")
Owls <- transform(Owls, 
                  Nest=reorder(Nest,NegPerChick), 
                  logBroodSize=log(BroodSize), 
                  NCalls=SiblingNegotiation)
hist(Owls$NCalls)
fit_zipoiss <- glmmadmb(NCalls~(FoodTreatment+ArrivalTime)*SexParent+ 
                          offset(logBroodSize)+(1|Nest), 
                        data=Owls, 
                        zeroInflation=TRUE, 
                        family="poisson")
summary(fit_zipoiss)
fitted_zipoiss = as.vector(fitted(fit_zipoiss))
predict_zipoiss = as.vector(predict(fit_zipoiss))
plot(fitted_zipoiss, Owls$NCalls, xlim = c(0,15))
plot(fitted_zipoiss, exp(predict_zipoiss), xlim = c(0,15), ylim = c(0,20))
abline(0,1)

#Check distribution of model residuals
#Build function first
# See this paper and electronic supplements
# http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3994649/#supp-5
install.packages("AICcmodavg")
library("AICcmodavg")
plot.glmer<-
  function(mer.fit,type="pearson",overdispersion.term=NULL){
    if(is.null(overdispersion.term))
    {
      print("Here0")
      Fitted<-as.vector(fitted(mer.fit))
      print("Herea")
      Residuals=as.vector(resid(mer.fit,type))
    } else
    {
      print("Hereb")
      response<-model.frame(mer.fit)[[1]]
      od.ranef<-lme4::ranef(mer.fit)[[overdispersion.term]][[1]]
      if(length(response)!=length(od.ranef) || fam.link.mer(mer.fit)$family!="nbinom1" || fam.link.mer(mer.fit)$link!="log")
        stop("Model is not lognormal-Poisson. Cannot use overdispersion term.")
      Fitted<-exp(log(fitted(mer.fit))-od.ranef)
      Residuals<-(response - Fitted)/sqrt(Fitted+(Fitted^2)*c(exp(lme4::VarCorr(mer.fit)[[overdispersion.term]])-1))
    }
    
    plot.data<-data.frame(Fitted=Fitted,Residuals=Residuals)
    print("Here")
    plot.data$loess.line<-predict(loess(Residuals~Fitted,data=plot.data))
    plot.data<-plot.data[order(plot.data$Fitted),]
    plot(plot.data[,c("Fitted","Residuals")])
    abline(h=0)
    points(plot.data[,c("Fitted","loess.line")],type="l",col="red")
    hist(plot.data$Residuals,xlab="Residuals",main="")
  }

plot.glmer(fit_zipoiss)
plot.glmer(test.glmmadmb11)
##Test for overdispersion on the glmmADMB model residuals
## Build function
overdisp_fun <- function(model) {
  ## number of variance parameters in 
  ##   an n-by-n variance-covariance matrix
  vpars <- function(m) {
    nrow(m)*(nrow(m)+1)/2
  }
  model.df <- sum(sapply(VarCorr(model),vpars))+length(fixef(model))
  rdf <- nrow(model.frame(model))-model.df
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}
overdisp_fun(fit_zipoiss)
overdisp_fun(test.glmmadmb11)

sim.dat = NULL; sim.dat$x = 1:100
sim.dat$y = rnbinom(max(sim.dat$x), mu = 20, size = 0.5); hist(sim.dat$y)
sim.mod = glmmadmb(y~x, data = sim.dat, family = "nbinom")
sim.mod$b %*% sim.mod$phi 
fitted(sim.mod); log(fitted(sim.mod))
getAnywhere(fitted.glmmadmb)
plot(sim.dat$x, fitted(sim.mod))
?rnbinom
