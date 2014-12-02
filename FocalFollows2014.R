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
#  TODO : Why not read the data file in this function? 
#====== +++ === === +++ === === +++ === ===
munge.follow.dat = function(file.name){
  dat = read.csv(file = file.name, header = TRUE, as.is = TRUE, na.strings = c("NA", "x", "X")) 
  dat$Follow = gsub("F", "", dat$GroupFollow) # Remove the F before the follow ID number.
  dat = mutate(dat, Follow = as.numeric(Follow))
  dat = mutate(dat, DateTime = ymd_hms(DateTime))
  dat = mutate(dat, DateTime = force_tz(time = DateTime, tzone = "America/Iqaluit"))

  # Remove those follows with only one fix
  follows.to.delete = ddply(dat, .(Follow), summarise, follows.to.delete = (sum(LegTime) == 0))
  follows.to.delete = which(follows.to.delete$follows.to.delete)
  follows.to.delete.ii = which(dat$Follow %in% follows.to.delete)
  dat = dat[-follows.to.delete.ii, ]
  
  dat$PrimaryBeh[which(dat$PrimaryBeh == "M")] = "MI" # Correct a typo
  
  return(dat)
}

fixdat2014 = munge.follow.dat(fix.dfile)
View(fixdat2014)

#====== +++ === === +++ === === +++ === ===
# 1. 
# For each follow/track, 
# I’d like to know how much time was spent doing each primary behaviour 
# (4 possibilities (T=traveling, M=milling, RB=resting with back exposed, RS= resting submerged).  
#  Maybe best to go with % here?
# Note: functions lead() and lag() shift elements of vector by some offset (default shift is one element)
#====== +++ === === +++ === === +++ === ===

# Calculate the amount of time between fixes in each follow
# fixdat2014 = ddply(fixdat2014, .(Follow), transform, lagtime = lag(DateTime), 
#                    TimeDiff = difftime(DateTime, lag(DateTime), units = "secs")) # timediff=c(NA,diff(DateTime))

subset.fixdat = subset(fixdat2014, select = c(Follow, DateTime, LegTime, PrimaryBeh, SecondaryBeh)) # !is.na(PrimaryBeh),

assign.terminal.fix = function(follow){
  xx = NULL
  xx = length(follow)
  terminal.fix = rep(FALSE, xx)
  terminal.fix[xx] = TRUE
  return(terminal.fix)
}

# assign a column indicating which are the last fixes for each follow (True or False)
subset.fixdat = ddply(subset.fixdat, .(Follow), transform, terminal.fix = assign.terminal.fix(Follow)) # note the use of transform

subset.nonterminal.fix = subset(subset.fixdat, terminal.fix == FALSE) # extract non-terminal fixes
View(subset.nonterminal.fix)

subset.nonterminal.fix = mutate(subset.nonterminal.fix, value = LegTime)
cast(subset.nonterminal.fix, Follow ~ PrimaryBeh, sum) # output table with total amount of time each primary behavior observed during each follow

# cast(subset.nonterminal.fix, Follow ~ PrimaryBeh, sum, na.rm = TRUE, margins=c("grand_col", "grand_row")) 

# table.time.behav = ddply(subset.nonterminal.fix, .(Follow, PrimaryBeh), summarise, total.time = sum(LegTime, na.rm = TRUE))
# table.time.behav

# table.behav = with(fixdat2014, table(Follow, PrimaryBeh)) # make a table
# table.behav = as.data.frame(table.behav) # melt table (from wide to long) to a data.frame
# table.behav = mutate(table.behav, Follow = as.numeric(Follow), Freq = as.numeric(Freq)) # convert Follow ID number to numeric for sorting
# # table.behav = rename(table.behav, c("Follow" = "FollowID")) # rename this column
# table.behav = arrange(table.behav, Follow) # Sort on Follow ID number
# table.behav = cast(table.behav, Follow ~ PrimaryBeh)
# table.behav
# table.behav.2 = subset(table.behav, select = -c(Follow))
# table.behav.2 = as.matrix(table.behav.2) 
# prop.table(table.behav.2, margin = 1) %>% round(. , 1) # get percentage activity state for each follow (and round to one dec place)

#====== +++ === === +++ === === +++ === ===
# 2. 
# For each track, for each of the 4 primary behaviour categories, 
#  I’d like a list of the secondary behaviours observed. 
#  (SS=side swimming, DI=diving, TU=tusking, RO=rolling, RU=rubbing, BR=bubble rings, N=none - can be excluded?)
#====== +++ === === +++ === === +++ === ===

# table.beh.list = ddply(fixdat2014, .(Follow), summarise, PrimaryList = paste(unique(PrimaryBeh), collapse = ", "), 
#       SecondaryList = paste(unique(SecondaryBeh), collapse = ", "))

table.beh.list = ddply(fixdat2014, .(Follow, PrimaryBeh), summarise, SecondaryList = paste(unique(SecondaryBeh), collapse = " + "))
rm.these.records = which(is.na(table.beh.list$PrimaryBeh))
table.beh.list = table.beh.list[-rm.these.records,]

#table.beh.list = mutate(table.beh.list, PrimaryBeh = gsub(", NA", "", table.beh.list$PrimaryList))
table.beh.list = mutate(table.beh.list, SecondaryList = gsub(", NA", "", table.beh.list$SecondaryList))
table.beh.list = mutate(table.beh.list, SecondaryList = gsub(", N", "", table.beh.list$SecondaryList))
table.beh.list = mutate(table.beh.list, SecondaryList = gsub("N, ", "", table.beh.list$SecondaryList))
table.beh.list = mutate(table.beh.list, SecondaryList = gsub("N", "", table.beh.list$SecondaryList))
table.beh.list = mutate(table.beh.list, SecondaryList = gsub(" \\+ DI", "DI", table.beh.list$SecondaryList))
table.beh.list = mutate(table.beh.list, SecondaryList = gsub("DI \\+ ", "DI", table.beh.list$SecondaryList))
table.beh.list

with(table.beh.list, table(PrimaryBeh, SecondaryList))

install.packages("qdap")
?strip
#====== +++ === === +++ === === +++ === ===
# 3. 
# For each track, what behaviour combination did the track end in? (primary and secondary)  
#  [Here I really just want to be able to comment on how many follows were terminated 
#  after the group dove and was not seen again].
#====== +++ === === +++ === === +++ === ===
# ddply(fixdat2014, .(Follow), summarise, NumberOfFixes = length(FixNum), 
#       TerminalBehavior = ifelse(is.na(PrimaryBeh[NumberOfFixes]), PrimaryBeh[NumberOfFixes - 1], PrimaryBeh[NumberOfFixes]))

terminal.fixes = subset(subset.fixdat, terminal.fix == TRUE) # extract the terminal fix records
with(terminal.fixes, unique(PrimaryBeh)) # Note there are some NA for PrimaryBeh in terminal fixes during 2014

table.term.beh = with(terminal.fixes, table(SecondaryBeh, PrimaryBeh, useNA = "ifany")) 
#table.term.beh = as.data.frame(table.term.beh)
# table.term.beh = cast(table.term.beh, SecondaryBeh ~ PrimaryBeh)
# #arrange(table.term.beh, desc(DI))
table.term.beh

# SecondaryBeh M RB  T NA
# 1           DI 0  1 16  0
# 2            N 1  1 15  0
# 3           SS 0  0  1  0
# 4         <NA> 1  0  0  2

# PrimaryBeh   DI  N SS NA
# 1          T 16 15  1  0
# 2         RB  1  1  0  0
# 3          M  0  1  0  1
# 4       <NA>  0  0  0  2

save.image("~/Documents/2014 Work/Milne Inlet Narwhals/2014 Analysis/Code/FocalFollows2014.RData")
rm(list = ls())
load("~/Documents/2014 Work/Milne Inlet Narwhals/2014 Analysis/Code/FocalFollows2014.RData")
