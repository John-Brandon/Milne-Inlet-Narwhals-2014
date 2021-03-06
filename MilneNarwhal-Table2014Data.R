###==============
# Author: John R. Brandon
# eMail:  jbrandon at greeneridge [or jbrandon at gmail.com]
# Date :  Fall 2014
# OS   :  Mac OS 10.9.5 (x86_64-apple-darwin10.8.0 (64-bit))
# Language : R (ver 3.0.2 (2013-09-25) -- "Frisbee Sailing")
# IDE  : Rstudio v 0.98.484
###=== === +++ === === +++ === === +++ === ===
# Purpose : Code to table Milne Inlet narwhal data for LGL / Baffinland 
#  Notes : These routines work on data that has already been extracted and munged, so the first step is to load that pre-massaged data 
#  1) Your main directory will differ. You will have to edit the lines of code with directories to match your system
#  2) Loads existing workspace, that contains 2014 data (dat2014)
#  3) These data are also known as "Relative Abundance and Distribution" (RAD) data  
#  4) Includes code to make Figure 21: Time series of total counts
#   TODO : Move this to plotting script
#====== +++ === === +++ === === +++ === ===

rm(list=ls()) # clear leftovers from previous workspace

load("~/Documents/2014 Work/Milne Inlet Narwhals/2014 Analysis/Code/MilneNarwhal.2014.RData") # Load workspace 

# Load packages, this list is defined in Munging2014Data script
load.packages()

setwd("~/Documents/2014 Work/Milne Inlet Narwhals/Data/2014") # Set working directory for data, really for outputting tables (data has already been read)

#====== +++ === === +++ === === +++ === ===
# (0) Quick summary of counts
#====== +++ === === +++ === === +++ === ===
with(dat2014, length(which(GroupSizeLevel == "ZeroCount")))
with(dat2014, length(which(GroupSizeLevel == "PositiveCount")))
with(dat2014, sum(GroupSize[which(GroupSizeLevel == "PositiveCount")], na.rm = TRUE))

#====== +++ === === +++ === === +++ === ===
# (1) Table counts of group sizes by sub-stratum
#====== +++ === === +++ === === +++ === ===
table.group.size = function(dat){
  group.size = table(dat$SubStratum, dat$GroupSize) # ?table
  group.size = as.data.frame(group.size) # data.frame with group size frequencies in counts (e.g. 1 group of 34 in substratum F1, 0 groups of 33, etc.)
  names(group.size) = c("SubStratum", "GroupSize", "Freq") # make it easier to read
  
  group.size$GroupSize = as.numeric.factor(group.size$GroupSize) # convert GroupSize from factor to numeric
  group.size = group.size[order(group.size$GroupSize),] # sort table on GroupSize
  return(group.size)
}

#====== +++ === === +++ === === +++ === ===
# (2) Take frequencies of different group sizes in each substratum,
#     to get total numbers associated with different group sizes by substratum 
#====== +++ === === +++ === === +++ === ===
calc.intermediate.counts = function(group.size.table){
  tot.counts = group.size
  tot.counts$TotalCount = as.numeric(tot.counts$GroupSize) * tot.counts$Freq # TotalCounts are product of group size and numbers of groups
  tot.counts$Stratum = substring(tot.counts$SubStratum, first = 1, last = 1) # Create a vector with Stratum from SubStratum vector
  tot.counts$SubStratum.num = substring(tot.counts$SubStratum, first = 2, last = 2) # Create a vector with SubStratum.num from SubStratum vector
  return(tot.counts)  
}

#====== +++ === === +++ === === +++ === ===
# (3) Summarize abundance by SubStratum (integrating over time)
#====== +++ === === +++ === === +++ === ===
calc.total.counts.subs = function(tot.counts){
  # uses 'plyr' package, could also use function aggregate  
  tot.counts.subs = ddply(tot.counts, "SubStratum", summarise, TotalCount = sum(TotalCount, na.rm = TRUE)) 
  return(tot.counts.subs)
}

#====== +++ === === +++ === === +++ === ===
# (4) Summarize by Stratum (integrating over time) 
#   tot.counts.strat is plotted as histogram in plotting script
#====== +++ === === +++ === === +++ === ===
calc.total.counts.strat = function(tot.counts){
  # uses 'plyr' package, could also use function aggregate  
  tot.counts.strat = ddply(tot.counts, "Stratum", summarise, 
                           TotalCount = sum(TotalCount, na.rm = TRUE)) 
  return(tot.counts.strat)
}

#====== +++ === === +++ === === +++ === ===
# (5) Create a data.frame with TotalCount by SubStratum and Count.id
#  Note: using .(x, y) in ddply saves having to put quotes around "x" and "y"
#====== +++ === === +++ === === +++ === ===
calc.counts.by.sub.stratum = function(dat){
  counts.by.sub.stratum = ddply(dat, .(Count.id, SubStratum), summarise, 
                                datetime = unique(datetime), 
                                datetime.rounded.to.half.hr = unique(datetime.nearest.half.hr),                        
                                TotalCount.with.na = sum(GroupSize, na.rm = FALSE), 
                                TotalCount.without.na = sum(GroupSize, na.rm = TRUE),
                                Sightability = ifelse(length(unique(Sightability)) > 0, paste(unique(Sightability), collapse = "," ), NA),
                                #datetime.rounded.to.hr = unique(datetime.rounded.to.hr),
                                CountType = unique(CountType),
                                Vessel.related.count = unique(Vessel.related.count),
                                SeaState = ifelse(length(unique(SeaState)) > 0, paste(unique(SeaState), collapse = "," ), NA),
                                count.quality = unique(count.quality)
                                # Include.count = unique(Include.count)
  ) # returns numbers by sub-strata for each count.id
  
  counts.by.sub.stratum = extract.stratum(counts.by.sub.stratum) # creates new columns for Stratum (A,B,C,etc) and Strat.number
  return(counts.by.sub.stratum)
}

# Check
# write.csv(x = counts.by.sub.stratum, file = "counts.by.sub.stratum.csv", row.names = FALSE); system("open counts.by.sub.stratum.csv") # check

#====== +++ === === +++ === === +++ === ===
# (6) Collapse data.frame with TotalCount-by-SubStratum and Count.id, 
#     to a data.frame with count-by-stratum
#====== +++ === === +++ === === +++ === ===
calc.counts.by.strat = function(counts.by.sub.stratum){
  counts.by.stratum = ddply(counts.by.sub.stratum, .(Count.id, Stratum), summarise, 
                            datetime = unique(datetime), 
                            datetime.rounded.to.half.hr = unique(datetime.rounded.to.half.hr),                        
                            TotalCount.with.na = sum(TotalCount.with.na, na.rm = FALSE), 
                            TotalCount.without.na = sum(TotalCount.without.na, na.rm = TRUE),
                            Sightability = ifelse(length(unique(Sightability)) > 0, paste(unique(Sightability), collapse = "," ), NA),
                            #datetime.rounded.to.hr = unique(datetime.rounded.to.hr),
                            CountType = unique(CountType),
                            Vessel.related.count = unique(Vessel.related.count),
                            count.quality = unique(count.quality)
                            #Include.stratum.count = unique(Include.stratum.count)
  ) # returns numbers by sub-strata for each count.id
  return(counts.by.stratum)
}

# Check
# write.csv(x = counts.by.stratum, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check

#====== +++ === === +++ === === +++ === ===
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Call functions to create data.tables
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#====== +++ === === +++ === === +++ === ===
str(dat2014)
with(dat2014, length(unique(Count.id)))
group.size.2014 = table.group.size(dat2014)
tot.counts.2014 = calc.intermediate.counts(group.size.2014)
tot.counts.subs.2014 = calc.total.counts.subs(tot.counts.2014)
tot.counts.strat.2014 = calc.total.counts.strat(tot.counts.2014)

counts.by.sub.stratum.2014 = calc.counts.by.sub.stratum(dat2014) # takes a moment
str(counts.by.sub.stratum.2014)
with(counts.by.sub.stratum, table(Sightability, Vessel.related.count)) # table sightability vs. vessel.related.scans
with(counts.by.sub.stratum, unique(SeaState))
with(counts.by.sub.stratum, unique(Sightability))

counts.by.stratum.2014 = calc.counts.by.strat(counts.by.sub.stratum.2014) # takes a moment
str(counts.by.stratum.2014)
View(counts.by.stratum.2014)
write.csv(x = counts.by.stratum.2014, file = "counts.by.stratum.2014.csv", row.names = FALSE); system("open counts.by.stratum.2014.csv") # check
#====== +++ === === +++ === === +++ === ===
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#====== +++ === === +++ === === +++ === ===


#====== +++ === === +++ === === +++ === ===
# (8) 
# Join counts.by.stratum (now also includes total counts across stratum) 
#  with vector including time-stamps for full season
# This is a pre-cursor to plotting the time-series of effort / counts for the whole season 
#====== +++ === === +++ === === +++ === ===
str(half.hourly.timestamps) # check
time.stamps = force_tz(half.hourly.timestamps, tzone = tz(dat2014$datetime))
time.stamps = as.data.frame(half.hourly.timestamps)
str(time.stamps) # check

head(counts.by.stratum.2014) # check

counts.for.plot = ddply(counts.by.stratum.2014, .(Count.id), summarise, 
                        datetime.rounded.to.half.hr = unique(datetime.rounded.to.half.hr),
                        daynumber = yday(unique(datetime.rounded.to.half.hr)),
                  #      CountType = unique(CountType),
                        totalcount = sum(TotalCount.without.na),
                        Sightability = unique(count.quality)
)
head(counts.for.plot, n = 80)

all.season.dat = merge(counts.for.plot, time.stamps, by.x = "datetime.rounded.to.half.hr", by.y = "half.hourly.timestamps", all = TRUE) # outer join
head(all.season.dat)

# Check
# write.csv(x = all.season.dat, file = "foo2.csv", row.names = FALSE); system("open foo2.csv") # check

#====== +++ === === +++ === === +++ === ===
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# "Fig 21" Time series of total counts
# TODO : Move this to plotting script
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#====== +++ === === +++ === === +++ === ===
#Breaks for background rectangles
# dat.to.plot = subset(all.season.dat, daynumber %in% c(215:225))
dat.to.plot = all.season.dat
names(dat.to.plot)
# large.vessels = data.frame(xstart = all.season.dat$datetime.rounded.to.half.hr[4], xend = all.season.dat$datetime.rounded.to.half.hr[12])
# str(large.vessels)

add.large.vess.starts = as.POSIXct(c("2014-08-08 14:00",  # add additional (unobserved at time) large vess transits
                                     "2014-08-11 13:00", 
                                     "2014-08-12 00:00",
                                     "2014-08-19 18:30",
                                     "2014-08-30 00:00",
                                     "2014-09-08 02:00",
                                     "2014-09-08 04:30"), tz = "America/Iqaluit")
add.large.vess.stops = as.POSIXct(c("2014-08-08 16:00", 
                                     "2014-08-11 14:00", 
                                     "2014-08-12 01:00",
                                     "2014-08-19 19:00",
                                     "2014-08-30 20:00",
                                     "2014-09-08 03:00",
                                     "2014-09-08 05:30"), tz = "America/Iqaluit")

add.large.vess = data.frame(start.time = add.large.vess.starts, stop.time = add.large.vess.stops)
large.vess.times = extract.vessel.transit.times(dat2014) # extract large vess times from count data

large.vess.times$start.time = force_tz(large.vess.times$start.time, tzone = "America/Iqaluit")
large.vess.times$stop.time = force_tz(large.vess.times$stop.time, tzone = "America/Iqaluit")

large.vess.times[,1] - hours(4) 

#large.vess.times = rbind(large.vess.times, add.large.vess) # add extra large vessels
large.vess.times = rbind(add.large.vess, large.vess.times) # add extra large vessels
large.vess.times$daynumber = yday(large.vess.times$start.time) # assign day number
large.vess.times = arrange(large.vess.times, start.time) # sort by start.time

dat.to.plot$datetime.rounded.to.half.hr = force_tz(dat.to.plot$datetime.rounded.to.half.hr, tzone = "EDT")
tz(dat.to.plot$datetime.rounded.to.half.hr)
tz(dat.to.plot) # UTC ??

View(dat.to.plot)
# vess.to.plot = subset(large.vess.times, daynumber %in% c(215:225))
#gg = ggplot(dat.to.plot, aes(x = datetime.rounded.to.half.hr, y = totalcount, colour = Sightability))
gg = ggplot()
gg = gg + geom_rect(data = large.vess.times, aes(xmin = start.time - hours(4), xmax = stop.time - hours(4), ymin = 0, ymax = Inf), alpha = 0.70) #)
gg = gg + geom_point(data = dat.to.plot[!is.na(dat.to.plot$totalcount),], aes(x = datetime.rounded.to.half.hr, y = totalcount, group = daynumber, colour = Sightability), size = 4) # aes(group = daynumber)  
gg = gg + geom_line(data = dat.to.plot[!is.na(dat.to.plot$totalcount),], aes(x = datetime.rounded.to.half.hr, y = totalcount, group = daynumber, colour = Sightability), size = 0.75) 
gg = gg + xlab("Date") + ylab("Number of Narwhals per RAD Count")
gg = gg + scale_colour_manual(values = c("blue", "red"), labels = c("Good to Excellent", "Poor"))
gg = gg + mytheme_bw
gg = gg + theme(legend.justification=c(0,1), legend.position=c(0.025,0.925))

# Calls below make invididual plots for a given date range (defined by limits)
gg + scale_x_datetime(breaks = date_breaks("1 day"), # , minor_breaks = date_breaks("12 hour")
                      labels = date_format('%d-%b'), 
                      limits = as.POSIXct(c("2014-08-03","2014-08-11"), tz = "EDT")) +
      annotate("text", x=as.POSIXct("2014-08-03"), y=700, label="(A)", size = 8)

gg + scale_x_datetime(breaks = date_breaks("1 day"), 
                      labels = date_format('%d-%b'),
                      limits = as.POSIXct(c("2014-08-11","2014-08-19"), tz = "EDT")) + 
    annotate("text", x=as.POSIXct("2014-08-11"), y=700, label="(B)", size = 8)

gg + scale_x_datetime(breaks = date_breaks("1 day"), 
                      labels = date_format('%d-%b'),
                      limits = as.POSIXct(c("2014-08-19","2014-08-27"), tz = "EDT")) +
  annotate("text", x=as.POSIXct("2014-08-19"), y=700, label="(C)", size = 8)

gg + scale_x_datetime(breaks = date_breaks("1 day"), 
                      labels = date_format('%d-%b'),
                      limits = as.POSIXct(c("2014-08-27","2014-09-04"), tz = "EDT")) +
  annotate("text", x=as.POSIXct("2014-08-27"), y=700, label="(D)", size = 8)


#====== +++ === === +++ === === +++ === ===
# Look at breakdown of sightability during vessel counts
#====== +++ === === +++ === === +++ === ===
vessel.counts = subset(dat2014, CountType %in% c("PRE", "C", "POST")) # just counts associated with vessel transits
# write.csv(x = vessel.counts, file = "2014.vessel.counts.csv", row.names = FALSE); system("open 2014.vessel.counts.csv") # check

vessel.sightability = melt(table(vessel.counts$Sightability, vessel.counts$CountType)) # returns a data.frame
names(vessel.sightability) = c("Sightability", "CountType", "SubStratumCounts")

# write.csv(x = vessel.sightability, file = "2014.vessel.sightability.csv", row.names = FALSE); system("open 2014.vessel.sightability.csv") # check

arrange(vessel.sightability, Sightability) # 'arrange' is a wrapper for 'order' and sorts a data.frame by a given column(s)
vessel.sight.category.sums = ddply(vessel.sightability, "Sightability", summarise, SubStratumCounts = sum(SubStratumCounts))
vessel.sight.category.sums$Percentage = vessel.sight.category.sums$SubStratumCounts / sum(vessel.sight.category.sums$SubStratumCounts)
vessel.sight.category.sums$Percentage = round(vessel.sight.category.sums$Percentage, 2)
vessel.sight.category.sums

#====== +++ === === +++ === === +++ === ===
# Table X. Abundance data effort at Bruce Head in 2014, 
#  including the number of counts associated with the presence of large vessels. 
#  Includes counts made in all sighting conditions (poor, good and excellent)
#  Columns = Date, Start Time, End Time, Number of counts, Counts during vessel presence 
#====== +++ === === +++ === === +++ === ===

# For each unique day, get the start (earliest) and end (latest) time for counts, and sum the total number of counts
table.all.counts = ddply(dat2014, "Date", summarise, StartTime = substr(min(datetime), 12, 16), EndTime = substr(max(datetime), 12, 16), 
            Counts = length(unique(Time))) # , Counts.With.Vessel = would be nice to add this but do by hand for now

AddVesselCounts = function(dat){ 
  # dat is a vector with values for count types (e.g. "H" for hourly, "PRE", "C", and "POST" for counts associated with large vessels)
  # This calculates the number of counts associated with large vessels each day (when called by ddply below)
  count.codes = NULL; vessel.codes = NULL
  count.codes = unique(dat)
  vessel.codes = which(count.codes == "PRE" | count.codes == "C" | count.codes == "POST")
  vessel.codes = length(vessel.codes)
  return(vessel.codes)
}

start.end.daily.table = ddply(dat2014, "Date", summarise, StartTime = substr(min(datetime), 12, 16), EndTime = substr(max(datetime), 12, 16), 
                  Counts = length(unique(Time)), Counts.With.Vessel = AddVesselCounts(CountType)) 

# write.csv(x = start.end.daily.table, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check

#====== +++ === === +++ === === +++ === ===
# Table XX. Number of narwhals (individuals) per stratum, number of abundance counts, and
# mean number of narwhals per count considering all abundance data.
# Note: at present this just returns the total numbers per stratum. The number of counts was calculated above, 
#  and mean number of narwhals per count is currently calculated in the spreadsheet. 
#  The tallies done in the spreadsheet could propably been done here, to make the results more reproducible. 
#====== +++ === === +++ === === +++ === ===
table.all.numbers = ddply(dat2014, "Date", summarise, # TODO (jbrandon): again, horrible name for a table. Revise.
                   A = sum(GroupSize[which(Stratum=="A")], na.rm = TRUE), # this is pretty cluggy
                   B = sum(GroupSize[which(Stratum=="B")], na.rm = TRUE),
                   C = sum(GroupSize[which(Stratum=="C")], na.rm = TRUE),
                   D = sum(GroupSize[which(Stratum=="D")], na.rm = TRUE),
                   E = sum(GroupSize[which(Stratum=="E")], na.rm = TRUE),
                   F = sum(GroupSize[which(Stratum=="F")], na.rm = TRUE),
                   G = sum(GroupSize[which(Stratum=="G")], na.rm = TRUE),
                   H = sum(GroupSize[which(Stratum=="H")], na.rm = TRUE),
                   I = sum(GroupSize[which(Stratum=="I")], na.rm = TRUE)) # , Counts.With.Vessel = would be nice to add this but do by hand for now

# write.csv(x = table.all.numbers, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check

#====== +++ === === +++ === === +++ === ===
# Number of narwhals (individuals) in each stratum for each count
#====== +++ === === +++ === === +++ === ===
numbers.by.count.and.strata = ddply(dat2014, "Count.id", summarise, # TODO Move this function call to Table script
                                    Numbers = sum(GroupSize, na.rm = TRUE), 
                                    datetime = unique(datetime),
                                    datetime.nearest.half.hr = unique(datetime.nearest.half.hr),
                                    A = sum(GroupSize[which(Stratum=="A")], na.rm = TRUE), # this is pretty cluggy
                                    B = sum(GroupSize[which(Stratum=="B")], na.rm = TRUE),
                                    C = sum(GroupSize[which(Stratum=="C")], na.rm = TRUE),
                                    D = sum(GroupSize[which(Stratum=="D")], na.rm = TRUE),
                                    E = sum(GroupSize[which(Stratum=="E")], na.rm = TRUE),
                                    F = sum(GroupSize[which(Stratum=="F")], na.rm = TRUE),
                                    G = sum(GroupSize[which(Stratum=="G")], na.rm = TRUE),
                                    H = sum(GroupSize[which(Stratum=="H")], na.rm = TRUE),
                                    I = sum(GroupSize[which(Stratum=="I")], na.rm = TRUE)                                     
) 

# write.csv(x = numbers.by.count.and.strata, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check

#====== +++ === === +++ === === +++ === ===
# Number of narwhals (individuals) per sub-stratum 
#====== +++ === === +++ === === +++ === ===
# table.substram.numbers = ddply(dat2014, "Count.id", summarise, # TODO (jbrandon): again, horrible name for a table. Revise.
#                                for (ii in 1:length(unique(dat2014$SubStratum))) {
#                                  ii = sum(GroupSize[which(SubStratum==unique(SubStratum)[ii])], na.rm = TRUE)
#                                }) 
                               
table.substram.numbers = ddply(dat2014, "Count.id", summarise, # TODO (jbrandon): again, horrible name for a table. Revise.
                               A1 = sum(GroupSize[which(SubStratum=="A1")], na.rm = TRUE), # this is pretty cluggy
                               A2 = sum(GroupSize[which(SubStratum=="A2")], na.rm = TRUE),       
                               A3 = sum(GroupSize[which(SubStratum=="A3")], na.rm = TRUE),
                               B1 = sum(GroupSize[which(SubStratum=="B1")], na.rm = TRUE),
                               B2 = sum(GroupSize[which(SubStratum=="B2")], na.rm = TRUE),
                               B2 = sum(GroupSize[which(SubStratum=="B2")], na.rm = TRUE),
                               C1 = sum(GroupSize[which(SubStratum=="C1")], na.rm = TRUE),
                               C2 = sum(GroupSize[which(SubStratum=="C2")], na.rm = TRUE),
                               C3 = sum(GroupSize[which(SubStratum=="C3")], na.rm = TRUE),
                               D1 = sum(GroupSize[which(SubStratum=="D1")], na.rm = TRUE),
                               D2 = sum(GroupSize[which(SubStratum=="D2")], na.rm = TRUE),
                               D3 = sum(GroupSize[which(SubStratum=="D3")], na.rm = TRUE),
                               E1 = sum(GroupSize[which(SubStratum=="E1")], na.rm = TRUE),
                               E2 = sum(GroupSize[which(SubStratum=="E2")], na.rm = TRUE),
                               E3 = sum(GroupSize[which(SubStratum=="E3")], na.rm = TRUE),
                               F1 = sum(GroupSize[which(SubStratum=="F1")], na.rm = TRUE),
                               F2 = sum(GroupSize[which(SubStratum=="F2")], na.rm = TRUE),
                               F3 = sum(GroupSize[which(SubStratum=="F3")], na.rm = TRUE),
                               G1 = sum(GroupSize[which(SubStratum=="G1")], na.rm = TRUE),
                               G2 = sum(GroupSize[which(SubStratum=="G2")], na.rm = TRUE),
                               G3 = sum(GroupSize[which(SubStratum=="G3")], na.rm = TRUE),
                               H1 = sum(GroupSize[which(SubStratum=="H1")], na.rm = TRUE),
                               H2 = sum(GroupSize[which(SubStratum=="H2")], na.rm = TRUE),
                               H3 = sum(GroupSize[which(SubStratum=="H3")], na.rm = TRUE),
                               I1 = sum(GroupSize[which(SubStratum=="I1")], na.rm = TRUE),
                               I2 = sum(GroupSize[which(SubStratum=="I2")], na.rm = TRUE),
                               I3 = sum(GroupSize[which(SubStratum=="I3")], na.rm = TRUE)) # , Counts.With.Vessel = would be nice to add this but do by hand for now
                          

# write.csv(x = table.all.numbers, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check

save.image("~/Documents/2014 Work/Milne Inlet Narwhals/2014 Analysis/Code/MilneNarwhal.2014.RData")
