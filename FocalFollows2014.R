###=== === +++ === === +++ === === +++ === ===
# Author: John R. Brandon
# eMail:  jbrandon greeneridge [or jbrandon@gmail.com]
# Date :  Fall 2014
# OS   :  Mac OS 10.9.5 (x86_64-apple-darwin10.8.0 (64-bit))
# Language : R (ver 3.0.2 (2013-09-25) -- "Frisbee Sailing")
# IDE  : Rstudio v 0.98.484
###=== === +++ === === +++ === === +++ === ===
# Purpose : Summarize 2014 Milne Inlet LGL / Baffinland narwhal focal follow data
#            
#  Notes : 
#  1) Your main directory will differ. Update the base.dir variable.
#  2) Prior to running script, I manually changed format of Date in spreadsheet to "yyyy-mm-dd" to be consistent with POSIXct
#  3) 
#  4) 
#====== +++ === === +++ === === +++ === ===
library(reshape)
library(reshape2)
library(plyr) # for manipulating data
library(dplyr) 
library(lubridate) # for working with times
library(ggplot2) # for plotting

# Set Options
if (getOption("stringsAsFactors")) options(stringsAsFactors = FALSE) # set global option, don't want strings read as factors

# Initialize base data directory and file name
rm(list = ls())
base.dir = "~/Documents/2014 Work/Milne Inlet Narwhals/Data/2014"
fix.dfile = "FocalFollows_ByFix_2014.csv" # 2014 Focal Follows By Fix data, saved as comma delimited
tracksummary.dfile = "FocalFollows_TrackSummary_2014.csv" # 2014 Focal Follows By Fix data, saved as comma delimited

setwd(base.dir) # Set working directory for data

# Read data files 
fixdat2014 = read.csv(file = fix.dfile, header = TRUE, as.is = TRUE, na.strings = c("NA", "x", "X")) 
str(fixdat2014) # check structure of data.frame
View(fixdat2014)
# names(scandat2014)
trackdat2014 = read.csv(file = tracksummary.dfile, header = TRUE, as.is = TRUE, na.strings = c("NA", "x", "X")) 
str(trackdat2014) # check structure of data.frame

#====== +++ === === +++ === === +++ === ===
# 0.
# Do some munging
#====== +++ === === +++ === === +++ === ===
fixdat2014$Follow = gsub("F", "", fixdat2014$GroupFollow) # Remove the F before the follow ID number.
fixdat2014 = mutate(fixdat2014, Follow = as.numeric(Follow))
with(fixdat2014, class(Follow))

#====== +++ === === +++ === === +++ === ===
# 1. 
# For each follow/track, 
# I’d like to know how much time was spent doing each primary behaviour 
# (4 possibilities (T=traveling, M=milling, RB=resting with back exposed, RS= resting submerged).  
#  Maybe best to go with % here?
#====== +++ === === +++ === === +++ === ===
table.behav = with(fixdat2014, table(Follow, PrimaryBeh)) # make a table
table.behav = as.data.frame(table.behav) # melt table (from wide to long) to a data.frame
table.behav = mutate(table.behav, Follow = as.numeric(Follow), Freq = as.numeric(Freq)) # convert Follow ID number to numeric for sorting
# table.behav = rename(table.behav, c("Follow" = "FollowID")) # rename this column
table.behav = arrange(table.behav, Follow) # Sort on Follow ID number
table.behav = cast(table.behav, Follow ~ PrimaryBeh)
table.behav
table.behav.2 = subset(table.behav, select = -c(Follow))
table.behav.2 = as.matrix(table.behav.2) 
prop.table(table.behav.2, margin = 1) %>% round(. , 1) # get percentage activity state for each follow (and round to one dec place)

#====== +++ === === +++ === === +++ === ===
# 2. 
# For each track, for each of the 4 primary behaviour categories, 
#  I’d like a list of the secondary behaviours observed. 
#  (SS=side swimming, DI=diving, TU=tusking, RO=rolling, RU=rubbing, BR=bubble rings, N=none - can be excluded?)
#====== +++ === === +++ === === +++ === ===

table.beh.list = ddply(fixdat2014, .(Follow), summarise, PrimaryList = paste(unique(PrimaryBeh), collapse = ", "), 
      SecondaryList = paste(unique(SecondaryBeh), collapse = ", "))

table.beh.list = 
  ddply(fixdat2014, .(Follow, PrimaryBeh), summarise, SecondaryList = paste(unique(SecondaryBeh), collapse = " + "))

table.beh.list = mutate(table.beh.list, PrimaryList = gsub(", NA", "", table.beh.list$PrimaryList))
table.beh.list = mutate(table.beh.list, SecondaryList = gsub(", NA", "", table.beh.list$SecondaryList))
table.beh.list = mutate(table.beh.list, SecondaryList = gsub(", N", "", table.beh.list$SecondaryList))
table.beh.list = mutate(table.beh.list, SecondaryList = gsub("N, ", "", table.beh.list$SecondaryList))
table.beh.list = mutate(table.beh.list, SecondaryList = gsub("N", "", table.beh.list$SecondaryList))
table.beh.list

#====== +++ === === +++ === === +++ === ===
# 3. 
# For each track, what behaviour combination did the track end in? (primary and secondary)  
#  [Here I really just want to be able to comment on how many follows were terminated 
#  after the group dove and was not seen again].
#====== +++ === === +++ === === +++ === ===
ddply(fixdat2014, .(Follow), summarise, CountFixes = length(FixNum), TerminalBehavior = PrimaryBeh)
?ddply


save.image("~/Documents/2014 Work/Milne Inlet Narwhals/2014 Analysis/Code/FocalFollows2014.RData")
rm(list = ls())
load("~/Documents/2014 Work/Milne Inlet Narwhals/2014 Analysis/Code/FocalFollows2014.RData")
