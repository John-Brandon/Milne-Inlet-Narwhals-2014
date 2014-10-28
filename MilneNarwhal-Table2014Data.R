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

rm(list=ls()) # clear leftovers from previous workspace
load("~/Documents/2014 Work/Milne Inlet Narwhals/2014 Analysis/Code/MilneNarwhal.2014.RData") # Load workspace 
setwd("~/Documents/2014 Work/Milne Inlet Narwhals/Data/2014") # Set working directory for data, really for outputting tables (data has already been read)

write.csv(x = dat2014, file = "foo.csv"); system("open foo.csv") # check

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
# Designate survey.counts for Inclusion:
#  A complete survey.count is one that includes only stratum counts meeting the sightability criteria across all stratum during that survey.count.  
#  i.e. counts across each substratum were conducted during good to excellent sightability 
#  Any poor sightability, even if just one substratum, results in exclusion of that entire count under present criteria
#  Note also, that in addition to "P" for poor, if no attempt at a count was made for a sub-stratum (e.g. due to fog)
#    then those were entered as "x" (read into R as "NA"), so those counts with any "NA" or "P" will be excluded.
#====== +++ === === +++ === === +++ === ===
unique(dat2014$Sightability)
length(which(is.na(dat2014$Sightability)))
length(which(dat2014$Sightability == "P")) 
length(which(dat2014$Sightability == "L")) # TODO (hsmith): Data needs QC checking
length(which(dat2014$Sightability == 3))  # TODO (hsmith): Data needs QC checking

unique(dat2014$Count.id[which(is.na(dat2014$Sightability))]) # check to see which count.id's had NA's for no effort (due to fog, etc.)
unique(dat2014$Count.id[which(dat2014$Sightability == "P")]) # check to see which count.id's had P's for Poor sightability conditions

CountInclude = function(sightability){
  # Does this count meet the criteria of having all sub-stratum observed during Good or Excellent conditions?
  # 'sightability' here is a vector containing a code ("P", "G", "E", or NA) for each sub-stratum in a given count 
  # 'include.count' is returned as TRUE or FALSE
  unique.sight.codes = NULL; include.count = TRUE
  unique.sight.codes = unique(sightability)
  if ("P" %in% unique(sightability)) include.count = FALSE
  if (NA %in% unique(sightability)) include.count = FALSE
  return(include.count)
}

counts.to.include = ddply(dat2014, "Count.id", summarise, Include.count = CountInclude(Sightability)) # ! means NOT in R, so if no "P" then Include = TRUE 
write.csv(x = counts.to.include, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check

dat2014$IncludeCount = rep(FALSE, nrow(dat2014)) # create a new column, which will have logical values for including counts

for(ii in 1:nrow(counts.to.include)){ # expand from concise counts.to.include to full length column in data.frame 
  dat2014$IncludeCount[which(dat2014$Count.id == counts.to.include[ii,1])] = counts.to.include[ii,2] # gotta be a better way to code this
}
write.csv(x = dat2014, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check

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
