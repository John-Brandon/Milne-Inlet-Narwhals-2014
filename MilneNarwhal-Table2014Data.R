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
library(plyr) # Hadley Wickham's "Plier" package for common tasks (e.g. summarizing) with data.frames

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

AddVesselCounts = function(dat){ # TODO (jbrandon): horrible naming convention here (and foo.table), apologies
  foo = NULL; goo = NULL
  foo = unique(dat)
  goo = which(foo == "PRE" | foo == "C" | foo == "POST")
  goo = length(goo)
  return(goo)
}

foo.table = ddply(dat2014, "Date", summarise, StartTime = substr(min(datetime), 12, 16), EndTime = substr(max(datetime), 12, 16), 
                  Counts = length(unique(Time)), Counts.With.Vessel = AddVesselCounts(CountType)) 

write.csv(x = foo.table, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check

#====== +++ === === +++ === === +++ === ===
# Table XX. Number of narwhals (individuals) per stratum, number of abundance counts, and
# mean number of narwhals per count considering all abundance data.
# Note: at present this just returns the total numbers per stratum. The number of counts was calculated above, 
#  and mean number of narwhals per count is currently calculated in the spreadsheet. 
#  The tallies done in the spreadsheet could propably been done here, to make the results more reproducible. 
#====== +++ === === +++ === === +++ === ===
foo.table2 = ddply(dat2014, "Date", summarise, # TODO (jbrandon): again, horrible name for a table. Revise.
                   A = sum(GroupSize[which(Stratum=="A")], na.rm = TRUE), # this is pretty cluggy
                   B = sum(GroupSize[which(Stratum=="B")], na.rm = TRUE),
                   C = sum(GroupSize[which(Stratum=="C")], na.rm = TRUE),
                   D = sum(GroupSize[which(Stratum=="D")], na.rm = TRUE),
                   E = sum(GroupSize[which(Stratum=="E")], na.rm = TRUE),
                   F = sum(GroupSize[which(Stratum=="F")], na.rm = TRUE),
                   G = sum(GroupSize[which(Stratum=="G")], na.rm = TRUE),
                   H = sum(GroupSize[which(Stratum=="H")], na.rm = TRUE),
                   I = sum(GroupSize[which(Stratum=="I")], na.rm = TRUE)) # , Counts.With.Vessel = would be nice to add this but do by hand for now

write.csv(x = foo.table2, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check

#====== +++ === === +++ === === +++ === ===
# Make a column which assigns an ID number for each count (a single day may have multiple counts)
# TODO (jbrandon): Move this code to 'Munging' script
#====== +++ === === +++ === === +++ === ===
count.id = seq(from = 1, to = length(unique(dat2014$datetime))); count.id
ii = NULL
dat2014$Count.id = rep(-99, nrow(dat2014))
for(ii in 1:length(unique(dat2014$datetime))){ # probably a more elegant way to do this, rather than a loop.
  rec.numbers = NULL
  rec.numbers = which(dat2014$datetime == unique(dat2014$datetime)[ii])  
  dat2014$Count.id[rec.numbers] = count.id[ii]
}
foo = dat2014
write.csv(x = foo, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check

#====== +++ === === +++ === === +++ === ===
# Designate survey.counts for Inclusion:
#  A complete survey.count is, by definition, one which inlcudes counts, meeting the criteria, for all sub-stratum.  
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

unique(dat2014$Count.id[which(is.na(dat2014$Sightability))]) # check to see which count.id's had NA's
unique(dat2014$Count.id[which(dat2014$Sightability == "P")]) # check to see which count.id's had P's
foo = subset(dat2014, Count.id == 28)
dim(foo)

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

dat2014$IncludeCount = rep(FALSE, nrow(dat2014))

for(ii in 1:nrow(counts.to.include)){
  dat2014$IncludeCount[which(dat2014$Count.id == counts.to.include[ii,1])] = counts.to.include[ii,2]
}
write.csv(x = dat2014, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check

#====== +++ === === +++ === === +++ === ===
# subset data to exclude abundance counts that included at least one strata with poor sightability
#====== +++ === === +++ === === +++ === ===
dat2014.keepers = subset(dat2014, IncludeCount == TRUE)
write.csv(x = dat2014.keepers, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check
unique(dat2014.keepers$IncludeCount)  # check
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

is.data.frame(keepers.table) #check
keepers.table.counts = keepers.table[,-1]
keepers.table.counts 
# TODO (jbrandon): add columns to keepers.table with total counts by day; counts by day, and; mean # counts per day
