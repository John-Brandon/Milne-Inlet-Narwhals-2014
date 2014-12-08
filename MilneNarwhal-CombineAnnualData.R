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

dat2013 = do.dates.and.ids(dat2013)
dat2013 = assign.vessel.boolean(dat2013)
dat2013 = factor.group.size(dat2013)
dat2013 = assign.strat.sight.2014(dat2013)
dat2013 = merge.2013.dat.tides(dat2013, dat.tides.2013) 

str(dat2013)
# dat2013 = rename(dat2013, CountType = WatchType) #Debugging

filtered.dat2013 = filter.sight(dat2013) # filter sightability, returns only those counts entirely in Good or Excellence (no P or NA)
#write.csv(filtered.dat2013, "foo10.csv"); system("open foo10.csv")

tot.counts.2013 = ddply(filtered.dat2013, .(Count.id, Stratum), summarise, 
                        value = sum(GroupSize, na.rm = TRUE), 
                        CountType = unique(CountType),
                        #SeaState = unique(SeaState), 
                        datetime = unique(datetime)) # uses 'plyr' package, could also use function aggregate
cast(tot.counts.2013, Count.id + CountType + datetime ~ Stratum)

# Have a look at Sightability and SeaState
# foo.ii = with(filtered.dat2013, which(SeaState > 2 & Sightability %in% c("G", "E")))
# length(foo.ii)
# filtered.dat2013$Count.id[foo.ii]

#====== +++ === === +++ === === +++ === ===
# 2014
#  'Munging2014Data' script already calls functions like 'do.dates.and.ids'
#  Loading 'MilneNarwhal.2014.RData' loads in a munged, but unfiltered 2014 data.frame
#   So, just need to filter and continue with processing at this stage
#====== +++ === === +++ === === +++ === ===
filtered.dat2014 = filter.sight(dat2014)

tot.counts.2014 = ddply(filtered.dat2014, .(Count.id, Stratum), summarise, 
                        value = sum(GroupSize, na.rm = TRUE), 
                        CountType = unique(CountType),
                        #SeaState = unique(SeaState), 
                        datetime = unique(datetime)) # uses 'plyr' package, could also use function aggregate
cast(tot.counts.2014, Count.id + CountType + datetime ~ Stratum)

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

filtered.dat.join = rbind(filtered.dat2013, filtered.dat2014)
filtered.dat.join$Year = year(filtered.dat.join$datetime) # Create a column with Year
filtered.dat.join$Count.id.long = with(filtered.dat.join, paste(Year, Count.id, sep="."))
str(filtered.dat.join)

tot.counts.join = ddply(filtered.dat.join, .(Count.id.long, Stratum), summarise, 
                        value = sum(GroupSize, na.rm = TRUE), 
                        CountType = unique(CountType),
                        #SeaState = unique(SeaState), 
                        datetime = unique(datetime), 
                        hour = unique(dec.hour), 
                        tide.height = unique(Elevation),
                        tide.delta = unique(delta),
                        tide.state = unique(tidestate),
                        year = unique(Year)
                        ) # uses 'plyr' package, could also use function aggregate

mod.dat = cast(tot.counts.join, Count.id.long + CountType + datetime + year + hour + tide.height + tide.delta + tide.state ~ Stratum)
mod.dat$total.count.abcdef = with(mod.dat, A + B + C + D + E + F)
mod.dat$total.count.ghi = with(mod.dat, G + H + I)
mod.dat$total.count = with(mod.dat, A + B + C + D + E + F + G + H + I)
head(mod.dat, n=20)
str(mod.dat)

ggplot(data = mod.dat, aes(x = hour, y = tide.height)) + geom_point() + facet_grid(year ~ .)
ggplot(data = mod.dat, aes(x = hour, y = tide.height, color = as.factor(year))) + geom_point() + scale_color_manual(values = c("red", "blue"))

ggplot(data = mod.dat, aes(x = tide.delta, y = total.count)) + geom_point() + facet_grid(year ~ .)
ggplot(data = mod.dat, aes(x = tide.delta, y = total.count.ghi)) + geom_point() + facet_grid(year ~ .)

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
