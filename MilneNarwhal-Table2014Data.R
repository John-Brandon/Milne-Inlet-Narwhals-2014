###==============
# Author: John R. Brandon
# eMail:  jbrandon greeneridge [or jbrandon@gmail.com]
# Date :  Fall 2014
# OS   :  Mac OS 10.9.5 (x86_64-apple-darwin10.8.0 (64-bit))
# Language : R (ver 3.0.2 (2013-09-25) -- "Frisbee Sailing")
# IDE  : Rstudio v 0.98.484
###=== === +++ === === +++ === === +++ === ===
# Purpose : Code to table Milne Inlet narwhal data for LGL / Baffinland 
#  Notes : These routines work on data that has already been extracted and munged, so the first step is to load that pre-massaged data 
#  1) Your main directory will differ. See 'base.dir' variable below to edit that path to match your system
#  2) 
#  3) These data are also known as "Relative Abundance and Distribution" (RAD) data 
#  4) Loads existing workspace, that contains 2014 data (dat2014)
#====== +++ === === +++ === === +++ === ===
library(plyr) # Hadley Wickham's "Plier" package for splitting data.frames and applying functions to each subset
# install.packages("dplyr")
library(dplyr)

rm(list=ls()) # clear leftovers from previous workspace

load("~/Documents/2014 Work/Milne Inlet Narwhals/2014 Analysis/Code/MilneNarwhal.2014.RData") # Load workspace 

setwd("~/Documents/2014 Work/Milne Inlet Narwhals/Data/2014") # Set working directory for data, really for outputting tables (data has already been read)

# write.csv(x = dat2014, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check

#====== +++ === === +++ === === +++ === ===
# Table counts of group sizes by sub-stratum
#====== +++ === === +++ === === +++ === ===
group.size = table(dat2014$SubStratum, dat2014$GroupSize) # ?table
group.size = as.data.frame(group.size) # data.frame with group size frequencies in counts (e.g. 1 group of 34 in substratum F1, 0 groups of 33, etc.)
names(group.size) = c("SubStratum", "GroupSize", "Freq") # make it easier to read

group.size$GroupSize = as.numeric.factor(group.size$GroupSize) # convert GroupSize from factor to numeric
group.size = group.size[order(group.size$GroupSize),] # sort table on GroupSize

# write.csv(file = "foo.csv", x = group.size, row.names = FALSE); system("open foo.csv") # check

#====== +++ === === +++ === === +++ === ===
# Take frequencies of different group sizes in each substratum to get total numbers associated with different group sizes in each substratum 
#====== +++ === === +++ === === +++ === ===
tot.counts = group.size
#ii = sapply(tot.counts, is.factor) # intermediate step to convert columns that are presently factors to characters
#tot.counts[ii] <- lapply(tot.counts[ii], as.character) # convert columns that are factors to character strings
tot.counts$TotalCount = as.numeric(tot.counts$GroupSize) * tot.counts$Freq # TotalCounts are product of group size and numbers of groups
tot.counts$Stratum = substring(tot.counts$SubStratum, first = 1, last = 1) # Create a vector with Stratum from SubStratum vector
tot.counts$SubStratum.num = substring(tot.counts$SubStratum, first = 2, last = 2) # Create a vector with SubStratum.num from SubStratum vector

#====== +++ === === +++ === === +++ === ===
# Summarize abundance by SubStratum (integrating over time)
#====== +++ === === +++ === === +++ === ===
tot.counts.subs = ddply(tot.counts, "SubStratum", summarise, TotalCount = sum(TotalCount, na.rm = TRUE)) # uses 'plyr' package, could also use function aggregate

write.csv(x = tot.counts.subs, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check

#====== +++ === === +++ === === +++ === ===
# Summarize by Stratum (integrating over time) -- tot.counts.strat is plotted as histogram in plotting script
#====== +++ === === +++ === === +++ === ===
tot.counts.strat = ddply(tot.counts, "Stratum", summarise, TotalCount = sum(TotalCount, na.rm = TRUE)) # uses 'plyr' package, could also use function aggregate
# head(tot.counts.strat) # check
# with(tot.counts.strat, sum(TotalCount)) # check

#====== +++ === === +++ === === +++ === ===
# Summarize by Sub-Stratum (integrating over time) -- tot.counts.strat is plotted as histogram in plotting script
#====== +++ === === +++ === === +++ === ===
tot.counts.substrat = ddply(tot.counts, "SubStratum", summarise, TotalCount = sum(TotalCount, na.rm = TRUE)) # uses 'plyr' package, could also use function aggregate
# head(tot.counts.substrat) # check
# with(tot.counts.substrat, sum(TotalCount)) # check

#====== +++ === === +++ === === +++ === ===
# TODO : Create a data.frame with TotalCount by SubStratum and Count.id
#  Note: using .(x, y) in ddply saves having to put quotes around "x" and "y" (e.g. see previous ddply with "SubStratum" example above)
#====== +++ === === +++ === === +++ === ===
head(dat2014)
str(dat2014)
total.counts.by.sub.stratum = ddply(dat2014, .(Count.id, SubStratum), summarise, 
            TotalCount.with.na = sum(GroupSize, na.rm = FALSE), 
            TotalCount.without.na = sum(GroupSize, na.rm = TRUE),
            sightabilities = ifelse(length(unique(Sightability)) > 0, paste(unique(Sightability), collapse = "," ), NA),
            datetime = unique(datetime), 
            datetime.rounded.to.hr = unique(datetime.rounded.to.hr),
            datetime.rounded.to.half.hr = unique(datetime.nearest.half.hr),                        
            CountType = unique(CountType),
            Vessel.related.count = unique(Vessel.related.count),
            Include.count = unique(Include.count)
                                    ) # returns numbers by sub-strata for each count.id

total.counts.by.sub.stratum$Stratum = substring(total.counts.by.sub.stratum$SubStratum, first = 1, last = 1) # Create a vector with Stratum from SubStratum vector
total.counts.by.sub.stratum$SubStratum.num = substring(total.counts.by.sub.stratum$SubStratum, first = 2, last = 2) # Create a vector with SubStratum.num from SubStratum vector   

write.csv(x = total.counts.by.sub.stratum, file = "total.counts.by.sub.stratum.csv", row.names = FALSE); system("open total.counts.by.sub.stratum.csv") # check

foo = ddply(total.counts.by.sub.stratum, "Count.id", summarise, 
            datetime.rounded.to.hr = unique(datetime.rounded.to.hr),
            datetime.rounded.to.half.hr = unique(datetime.rounded.to.half.hr)
            )
foo$duplicated = duplicated(foo$datetime.rounded.to.hr)
foo$duplicated.half.hr = duplicated(foo$datetime.rounded.to.half.hr)
foo$datetime.rounded.to.hr[duplicated(foo$datetime.rounded.to.hr)] 
foo$datetime.rounded.to.half.hr[duplicated(foo$datetime.rounded.to.half.hr)]

write.csv(x = foo, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check

?merge
foo2 = merge(x = total.counts.by.sub.stratum, y = foo, by.x = "Count.id", by.y = "Count.id")
write.csv(x = foo2, file = "foo2.csv", row.names = FALSE); system("open foo2.csv") # check

with(foo, sum(TotalCount, na.rm = TRUE)) # check -- 10463

names(dat2014)
unique(dat2014$Direction)
table(dat2014$Direction)

foo = ddply(dat2014, .(Count.id, Stratum), summarise, TotalCount = sum(GroupSize, na.rm = TRUE), datetime = unique(datetime))

write.csv(x = foo, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check

# with(airquality,
#      table(OzHi = Ozone > 80, Month, useNA = "ifany"))

#====== +++ === === +++ === === +++ === ===
# Look at breakdown of sightability during vessel counts
#====== +++ === === +++ === === +++ === ===
vessel.counts = subset(dat2014, CountType %in% c("PRE", "C", "POST")) # just counts associated with vessel transits
write.csv(x = vessel.counts, file = "2014.vessel.counts.csv", row.names = FALSE); system("open 2014.vessel.counts.csv") # check

vessel.sightability = melt(table(vessel.counts$Sightability, vessel.counts$CountType)) # returns a data.frame
names(vessel.sightability) = c("Sightability", "CountType", "SubStratumCounts")

write.csv(x = vessel.sightability, file = "2014.vessel.sightability.csv", row.names = FALSE); system("open 2014.vessel.sightability.csv") # check

arrange(vessel.sightability, Sightability)
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

write.csv(x = start.end.daily.table, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check

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

write.csv(x = table.all.numbers, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check

#====== +++ === === +++ === === +++ === ===
# Number of narwhals (individuals) in each stratum for each count
#====== +++ === === +++ === === +++ === ===
numbers.by.count.and.strata = ddply(dat2014, "Count.id", summarise, # TODO Move this function call to Table script
                                    Numbers = sum(GroupSize, na.rm = TRUE), 
                                    DateTime = unique(datetime),
                                    DateTime.rounded.to.hr = unique(datetime.rounded.to.hr),
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

tmp = rle(as.character(numbers.by.count.and.strata$DateTime.rounded.to.hr))
tmp$lengths
unique(tmp$lengths)
length(tmp$lengths)
which(tmp$lengths > 1) # these are the 
tmp$values[which(tmp$lengths > 1)]
write.csv(x = numbers.by.count.and.strata, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check

#====== +++ === === +++ === === +++ === ===
# Number of narwhals (individuals) per sub-stratum 
#====== +++ === === +++ === === +++ === ===
unique(dat2014$SubStratum) 

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
                          

write.csv(x = table.all.numbers, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check

#====== +++ === === +++ === === +++ === ===
# subset data to exclude abundance counts that included at least one strata with poor sightability
#====== +++ === === +++ === === +++ === ===
dat2014.keepers = subset(dat2014, IncludeCount == TRUE) # IncludeCount is column with TRUE / FALSE in each row

write.csv(x = dat2014.keepers, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check
unique(dat2014.keepers$IncludeCount)  # check -- should be TRUE
dim(dat2014.keepers); dim(dat2014) # check

#====== +++ === === +++ === === +++ === ===
# Table "Keepers" for the count by stratum data
#====== +++ === === +++ === === +++ === ===
keepers.table = ddply(dat2014.keepers, "Date", summarise, # TODO(jbrandon): come up wit more elegant way to sum over groups (i.e. when number of stratum is a variable)
                   A = sum(GroupSize[which(Stratum=="A")], na.rm = TRUE), # this is pretty cluggy
                   B = sum(GroupSize[which(Stratum=="B")], na.rm = TRUE),
                   C = sum(GroupSize[which(Stratum=="C")], na.rm = TRUE),
                   D = sum(GroupSize[which(Stratum=="D")], na.rm = TRUE),
                   E = sum(GroupSize[which(Stratum=="E")], na.rm = TRUE),
                   F = sum(GroupSize[which(Stratum=="F")], na.rm = TRUE),
                   G = sum(GroupSize[which(Stratum=="G")], na.rm = TRUE),
                   H = sum(GroupSize[which(Stratum=="H")], na.rm = TRUE),
                   I = sum(GroupSize[which(Stratum=="I")], na.rm = TRUE)) # , Counts.With.Vessel = would be nice to add this but do by hand for now

write.csv(x = keepers.table, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check

keepers.counts.to.include = ddply(dat2014.keepers, "Count.id", summarise, Include.count = CountInclude(Sightability)) # CountInclude function is coded above in this script

keepers.dates = keepers.table[, 1] # extract column with dates -- will add this back to data frame after summing across rows
keepers.table.counts = keepers.table[, -1] # remove column with dates for now, will recombine below
all.strata = rowSums(keepers.table.counts) # total numbers, across strata, for each day 
keepers.table = cbind(keepers.dates, keepers.table.counts, all.strata) # recombine

write.csv(x = keepers.table, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check

# add column to keepers.table with counts by day 
# For each day, get unique number of start times, which should be equal to the number of counts on that day
keepers.daily.counts = ddply(dat2014.keepers, "Date", summarise, number.of.counts = length(unique(datetime)))
keepers.table = cbind(keepers.table, keepers.daily.counts[,2])
names(keepers.table)[length(names(keepers.table))] = "number.of.counts" # rename last column to "number.of.counts"

keepers.table$mean.no.per.count = keepers.table$all.strata / keepers.table$number.of.counts # mean numbers per counts each day
keepers.table$mean.no.per.count = round(keepers.table$mean.no.per.count, digits = 1)

write.csv(x = keepers.table, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check

#====== +++ === === +++ === === +++ === ===
# Table number of narwhals per stratum during: 
#  (1) good or excellent sightability, and
#  (2) during periods of "PRE" "C" or "POST" vessel presence
#====== +++ === === +++ === === +++ === ===
names(dat2014.keepers) # these are the counts where each (and every) stratum was observed during good to excellent sightability 

dat2014.keepers.vessel = subset(dat2014.keepers, Vessel.related.count == TRUE)
write.csv(x = dat2014.keepers.vessel, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check


save.image("~/Documents/2014 Work/Milne Inlet Narwhals/2014 Analysis/Code/MilneNarwhal.2014.RData")
# # SCRATCH CODE BELOW
# ?rle
# rm(z)
# z <- c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE)
# rle(z)
# foorle <- rle(as.character(z))
# rep(seq_len(length(foorle$values)), times=foorle$lengths)
# rm(foorle)
# rle(dat2014$Date) # run length encoding
# ?seq_len
