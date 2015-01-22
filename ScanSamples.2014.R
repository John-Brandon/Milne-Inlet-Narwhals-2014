###=== === +++ === === +++ === === +++ === ===
# Author: John R. Brandon
# eMail (replace: " -> " with "@"):  jbrandon -> greeneridge.com [or jbrandon -> gmail.com]
# Date :  Fall 2014
# OS   :  Mac OS 10.9.5 (x86_64-apple-darwin10.8.0 (64-bit))
# Language : R (ver 3.0.2 (2013-09-25) -- "Frisbee Sailing")
# IDE  : Rstudio v 0.98.484
###=== === +++ === === +++ === === +++ === ===
# Purpose : Munge and table group composition data from Heather Smith
#           Milne Inlet LGL / Baffinland shore based (Bruce Head) narwhal counts
#  Notes : 
#  1) Your main directory will differ, so you'll need to change the 'base.dir' value
#  2) Some of the tabling for the 2014 report was completed in a spreadsheet. 
#====== +++ === === +++ === === +++ === ===
load.packages()

# Set Options
if (getOption("stringsAsFactors")) options(stringsAsFactors = FALSE) # set global option, don't want strings read as factors

# Initialize base data directory and file name
rm(list = ls())
base.dir = "~/Documents/2014 Work/Milne Inlet Narwhals/Data/2014"
scan.dfile = "ScanSamples.2014.csv" # 2014 Scan Sample data, saved as comma delimited

setwd(base.dir) # Set working directory for data

# Read data file 
scandat2014 = read.csv(file = scan.dfile, header = TRUE, as.is = TRUE, na.strings = c("NA", "x", "X")) 
str(scandat2014) # check structure of data.frame
with(scandat2014, unique(Formation))
# View(scandat2014)
# names(scandat2014)

#====== +++ === === +++ === === +++ === ===
# 0.
# Munge the behavioral scan data
#====== +++ === === +++ === === +++ === ===
# Relable Travel Directions from "N" / "S" -> "North" / "South

# munge.scan.dat = function(scan.dat){
#   
# }

# drop rows that do not have a recorded number for GroupSize
scandat2014 = subset(scandat2014, subset = !is.na(GroupSize))

scandat2014 = mutate(scandat2014, datetimeStart = paste(Date, BlockStartTime)) # work on times to get unique datetime
scandat2014 = mutate(scandat2014, datetimeStart = as.POSIXct(datetimeStart)) # convert from string to date time class
scandat2014 = mutate(scandat2014, datetimeStart = force_tz(datetimeStart, tzone = "America/Iqaluit")) # change to local timezone (doesn't change time)

scandat2014$GroupSpread = revalue(scandat2014$GroupSpread, 
                              c("L" = "Loose", 
                                "T" = "Tight")) # helps with plotting labels

scandat2014$Formation = revalue(scandat2014$Formation, 
                            c("N" = "", 
                              "C" = "Circular",
                              "P" = "Parallel",
                              "L" = "Linear")) # helps with plotting labels

scandat2014$Speed = revalue(scandat2014$Speed, 
                        c("S" = "Slow", 
                          "M" = "Medium",
                          "F" = "Fast")) # helps with plotting labels
with(scandat2014, table(Speed, TravelDirection))

scandat2014$DistanceAway = revalue(scandat2014$DistanceAway, 
                               c("I" = "Inner", 
                                 "O" = "Outer")) # helps with plotting labels

scandat2014$TravelDirection = revalue(scandat2014$TravelDirection, c("N" = "North", "S" = "South")) # helps with plotting labels

scandat2014$TravelDirection = as.factor(scandat2014$TravelDirection) # TODO : use 'lapply' (or 'apply') to make all relevant columns as.factor()
scandat2014$Formation = as.factor(scandat2014$Formation) 
scandat2014$TravelDirection = as.factor(scandat2014$TravelDirection)

# For groups where group composition is known (i.e. GroupSize = sum(YesTusk_A,YesTusk_J, NoTusk_A, NoTusk_J, NoTusk_C)
#  TODO: Move this function to munging script
#  TODO: See Focal follows script for how to do this with 'dplyr' and 'join'
assign.groupcomp.known = function(dat){
  dat = mutate(dat, sum.known.stages = YesTusk_A + YesTusk_J + NoTusk_A + NoTusk_J + NoTusk_C) # adds a new column, sum.known.stages
  
  ii.known = with(dat, which(GroupSize == sum.known.stages & (!is.na(sum.known.stages)))) # index of groups with known compositions
  
  dat$GroupCompKnown = rep(FALSE, nrow(dat))
  dat$GroupCompKnown[ii.known] = TRUE
  return(dat)
}

scandat2014 = assign.groupcomp.known(scandat2014)

# filter.scan.dat = function(dat = scandat2014, TravDir = "North", return.columns = c("")){
#   scandat = subset(scandat2014, TravelDirection %in% TravDir, select = select.columns)
#   return(scandat)
# }

select.columns = c("datetimeStart","GroupSize", "TravelDirection", "GroupSpread", "Formation", "Speed", "DistanceAway", 
                   "GroupCompKnown", "YesTusk_A", "YesTusk_J", "NoTusk_A", "NoTusk_J", "NoTusk_C") 
select.TravelDir = c("North", "South")

scandat = subset(x = scandat2014, select = select.columns) # scandat includes all travel direction

scandat.N = subset(x = scandat2014, TravelDirection == "North", select = select.columns)
scandat.S = subset(x = scandat2014, TravelDirection == "South", select = select.columns)
scandat.NS = subset(scandat, TravelDirection %in% c("North", "South")) # just look at those groups headed either North or South

scandat.NS = droplevels(scandat.NS) # Drops 'empty' factor levels after subset call above
levels(scandat.NS$TravelDirection)

str(scandat.NS)
#====== +++ === === +++ === === +++ === ===
# 1. 
# For all groups, what is the # of groups headed north, and # headed south? 
# Q – do we see more narwhals entering or exiting the inlet?
#====== +++ === === +++ === === +++ === ===
names(scandat.NS)
with(scandat.NS, table(TravelDirection))
with(scandat, table(TravelDirection)) # all directions, notice that some directions might be recorded as ""
# TravelDirection
# North South 
# 46   186 

#====== +++ === === +++ === === +++ === ===
# 2. 
#  For all groups, what is the range in group size – heading north, and heading south? 
#  Q – does group size differ for animals entering or exiting?
#====== +++ === === +++ === === +++ === ===
# groupsize.N = scandat.N$GroupSize
# groupsize.S = scandat.S$GroupSize

summary(scandat.N$GroupSize) # return summary statistics for GroupSize
summary(scandat.S$GroupSize) 

var(scandat.N$GroupSize) / mean(scandat.N$GroupSize) # look for evidence of overdispersion -- looks plausible in this case
var(scandat.S$GroupSize) / mean(scandat.S$GroupSize) # 3.9 , which suggests overdispersion

install.packages("qcc")
library(qcc)

qcc.overdispersion.test(scandat.N$GroupSize) # test for overdispersion (Null hypothesis = No overdispersion)
qcc.overdispersion.test(scandat.S$GroupSize) # p << 0.05

wilcox.test(scandat.N$GroupSize, scandat.S$GroupSize)


# **
#  Ramblings below -- just used non-parametric Wilcox in 2014
# Two things to keep in mind before doing a comparison of distribution of group sizes
#  (1) These are zero truncated data, so I think we would really need to use a special form of a GLM (zero-truncated)
#  (2) The 2014 data (both N and S) appear to be overdispersed relative to the Poisson
# This leads me to believe that the correct way to statistically compare group sizes in this case
#  would be to use something like a zero-truncated negative binomial vectorized GLM
#  REF: http://www.ats.ucla.edu/stat/r/dae/ztnb.htm
# **

# Do some plotting (box-plots) of distribution of group sizes by travel direction
gg = ggplot(scandat.NS, aes(x = TravelDirection, y = GroupSize)) 
gg.box = gg + geom_boxplot() + ylab("Group Size") + xlab("Travel Direction")
gg.box 

# Plot histograms of distribution of group sizes by travel direction
gg2 = ggplot(scandat.NS, aes(x = GroupSize))
gg2 = gg2 + geom_histogram(fill = "black", colour = "gray")
gg2 = gg2 + facet_grid(TravelDirection ~ .) + xlab("Group Size") + ylab("Frequency")
gg2 + mytheme_larger
#====== +++ === === +++ === === +++ === ===
# 3. 
# For all groups, I’d like to summarize the (a) group spread, (b) formation, (c) speed, and (d) distance away 
#  This could be further categorized by animals heading north vs south. 
#  Goal here is to generally characterize groups.  
#  These are all categorical variables – maybe mode is the best way to go?
#====== +++ === === +++ === === +++ === ===

#====== +++ === === +++ === === +++ === ===
# 3(a). 
# Group spread
#====== +++ === === +++ === === +++ === ===
group.spread.dat = subset(scandat.NS, GroupSpread %in% c("Tight","Loose")) # TODO: Move this line of code to munging

# Summarize GroupSpread
table.spread = table(group.spread.dat$GroupSpread, group.spread.dat$TravelDirection) 
table.spread

fisher.test(table.spread)
chisq.test(table.spread)
chisq.test(table.spread, simulate.p.value = TRUE)

# Use barplots to summarize GroupSpread
gg4 = ggplot(group.spread.dat, aes(x = GroupSpread))
gg4 = gg4 + geom_bar() + xlab("Group Spread") + ylab("Count")
gg4 = gg4 + facet_grid(TravelDirection ~ .)
gg4 + mytheme
gg4 + mytheme_larger
#====== +++ === === +++ === === +++ === ===
# 3(b). 
# Group Formation
#====== +++ === === +++ === === +++ === ===
formation.spread.dat = subset(scandat.NS, Formation != "") # TODO: Move this line of code to munging
View(formation.spread.dat)

levels(formation.spread.dat$Formation)
droplevels(formation.spread.dat$Formation)
levels(formation.spread.dat$TravelDirection)
table.formation = table(formation.spread.dat$Formation, formation.spread.dat$TravelDirection)
table.formation = table.formation[-1,]

chisq.test(table.formation)
chisq.test(table.formation[c(1,3),]) # ignore linear, because n < 5 for north and for south

# Use barplots to summarize Formation
with(formation.spread.dat, unique(Formation))
formation.spread.dat = subset(formation.spread.dat, Formation != "Non-directional Line")
gg5 = ggplot(formation.spread.dat, aes(x = Formation))
gg5 = gg5 + geom_bar() + xlab("Formation") + ylab("Count") 
gg5 = gg5 + facet_grid(TravelDirection ~ .)
gg5 + mytheme_larger

#====== +++ === === +++ === === +++ === ===
# 3(c). 
# Group Speed
#====== +++ === === +++ === === +++ === ===
table.speed = table(scandat.NS$Speed, scandat.NS$TravelDirection)
table.speed
#chisq.test(table.speed)
table.speed[c(2,3),] # just medium and slow, because not enough samples
chisq.test(table.speed[c(2,3),])
#fisher.test(table.speed[c(2,3),])
gg6 = ggplot(scandat, aes(x = Speed))
gg6 = gg6 + geom_bar() + xlab("Group Speed") + ylab("Count") 
gg6 = gg6 + facet_grid(TravelDirection ~ .) 
gg6 = gg6 + scale_x_discrete(limits=c("Slow","Medium","Fast"))
gg6 + mytheme_larger

#====== +++ === === +++ === === +++ === ===
# 3(d). 
# Group Distance Away
#====== +++ === === +++ === === +++ === ===
group.dist.away.dat = subset(scandat.NS, DistanceAway != "") # TODO: Move this line of code to munging

table.dist.away = table(group.dist.away.dat$DistanceAway, group.dist.away.dat$TravelDirection)
table.dist.away
chisq.test(table.dist.away)

group.dist.away.dat$DistanceAway
gg7 = ggplot(group.dist.away.dat, aes(x = DistanceAway))
gg7 = gg7 + geom_bar() + xlab("Group Distance Away") + ylab("Count") 
gg7 = gg7 + facet_grid(TravelDirection ~ .)
gg7 + mytheme_larger

#====== +++ === === +++ === === +++ === ===
# 4. Now going to take into account all groups, regardless of travel direction 
# How many groups have known composition?
#====== +++ === === +++ === === +++ === ===
# For groups where group composition is known (i.e. GroupSize = sum(YesTusk_A,YesTusk_J, NoTusk_A, NoTusk_J, NoTusk_C)
dat.GroupCompKnown = subset(scandat2014, GroupCompKnown == TRUE) # just groups with known composition
with(dat.GroupCompKnown, unique(TravelDirection))
# droplevels(dat.GroupCompKnown)
table(dat.GroupCompKnown$GroupCompKnown) 
with(scandat2014, unique(TravelDirection))
table(scandat2014$GroupCompKnown) 
addmargins(table(scandat2014$GroupCompKnown))

# For groups where group composition is known (i.e. GroupSize = sum(YesTusk_A,YesTusk_J, NoTusk_A, NoTusk_J, NoTusk_C)
#====== +++ === === +++ === === +++ === ===
# 5. 
# What is the number of groups with calves?
#====== +++ === === +++ === === +++ === ===
length(which(dat.GroupCompKnown$NoTusk_C > 0))

# For groups where group composition is known (i.e. GroupSize = sum(YesTusk_A,YesTusk_J, NoTusk_A, NoTusk_J, NoTusk_C)
#====== +++ === === +++ === === +++ === ===
# 6. 
# What is group composition of groups with calves? (is it animals without tusks, adults, a mix?)
#====== +++ === === +++ === === +++ === ===
dat.GroupCompKnown = mutate(dat.GroupCompKnown, GroupSize = YesTusk_A + YesTusk_J + NoTusk_A + NoTusk_J + NoTusk_C) # This could go under munging
with(dat.GroupCompKnown, which(GroupSize == 1))

dat.GroupsWithCalves = subset(dat.GroupCompKnown, NoTusk_C > 0) # subset the data to just get calves
dat.GroupsWithCalves = arrange(dat.GroupsWithCalves, desc(GroupSize)) # sort by group size in descending order

# Subset the data
groupcomp.calves = subset(dat.GroupsWithCalves, select = c(YesTusk_A, YesTusk_J, NoTusk_A, NoTusk_J, NoTusk_C, GroupSize))
groupcomp.calves$Group.ID = 1:nrow(groupcomp.calves)
head(groupcomp.calves)  
# View(groupcomp.calves)

# Some summary stats for group composition, not sure how insightful these are relative to the bar graph 
round(sapply(groupcomp.calves, mean), 2)
sapply(groupcomp.calves, sum)
round(sapply(groupcomp.calves, sd), 2)

# Plot group compositions for those groups including calves. 
groupcomp.calves.melt = melt(groupcomp.calves, id.vars = c("Group.ID", "GroupSize")) # melt data.frame from wide to long for plotting
# View(groupcomp.calves.melt) # check
gg8 = ggplot(groupcomp.calves.melt, aes(x = Group.ID, y = value, fill = variable)) 
gg8 = gg8 + ylab("Narwhal Group Size") + xlab("Group ID")
gg8 = gg8 + geom_bar(aes(order = desc(variable)), stat = "identity")
gg8 = gg8 + scale_y_discrete(breaks = seq(0, 30, by = 2))
gg8 = gg8 + scale_fill_brewer("Group Composition", palette="Set1", labels = c("Adult with Tusk", "Juvenile with Tusk", "Adult No Tusk", "Juvenile No Tusk", "Calf"))
gg8 = gg8 + mytheme_larger 
gg8 + theme(legend.position = c(1,1), legend.justification = c(1,1), legend.text = element_text(size = 22), legend.title = element_text(size = 22))

# For groups where group composition is known (i.e. GroupSize = sum(YesTusk_A,YesTusk_J, NoTusk_A, NoTusk_J, NoTusk_C)
#====== +++ === === +++ === === +++ === ===
# 7. 
# Are there mixed groups with tusks and notusks in the same group?
#====== +++ === === +++ === === +++ === ===

dat.GroupCompKnown = mutate(dat.GroupCompKnown, Tusks = ifelse(YesTusk_A + YesTusk_J > 0, TRUE, FALSE)) # This could go under munging
dat.GroupCompKnown = mutate(dat.GroupCompKnown, NoTusks = ifelse(NoTusk_A + NoTusk_J + NoTusk_C > 0, TRUE, FALSE)) # This could go under munging

with(dat.GroupCompKnown, table(Tusks, NoTusks))

dat.groupcomp.subset = subset(dat.GroupCompKnown, select = c(YesTusk_A, YesTusk_J, NoTusk_A, NoTusk_J, NoTusk_C, GroupSize))
dat.groupcomp.subset = arrange(dat.groupcomp.subset, desc(GroupSize)) # sort by group size in descending order
dat.groupcomp.subset = mutate(dat.groupcomp.subset, Group.ID = 1:nrow(dat.groupcomp.subset))
summary(dat.groupcomp.subset$GroupSize)

groupcomp.subset.melt = melt(dat.groupcomp.subset, id.vars = c("Group.ID", "GroupSize")) # melt data.frame from wide to long for plotting
# View(groupcomp.calves.melt) # check
gg9 = ggplot(groupcomp.subset.melt, aes(x = Group.ID, y = value, fill = variable))
gg9 = gg9 + ylab("Narwhal Group Size") + xlab("Group ID")
gg9 = gg9 + geom_bar(aes(order = desc(variable)), stat = "identity")
gg9 = gg9 + scale_y_discrete(breaks = seq(0, 100, by = 2))
gg9 = gg9 + scale_fill_brewer("Group Composition", palette="Set1", labels = c("Adult with Tusk", "Juvenile with Tusk", "Adult No Tusk", "Juvenile No Tusk", "Calf"))
gg9 = gg9 + mytheme_larger 
gg9 + theme(legend.position = c(1,1), legend.justification = c(1,1), legend.text = element_text(size = 22), legend.title = element_text(size = 22))

# For groups where group composition is known (i.e. GroupSize = sum(YesTusk_A,YesTusk_J, NoTusk_A, NoTusk_J, NoTusk_C)
#====== +++ === === +++ === === +++ === ===
# Designate into one of five "Super Groups"
# 1. NoTusk groups without calves: 
#     (i) NoTusk_J, (ii) NoTusk_A + NoTusk_J (iii) NoTusk_A
# 2. NoTusk groups with calves:
#     (i) NoTusk_A + NoTusk_C, (ii) NoTusk_A + NoTusk_J + NoTusk_C
# 3. YestTusk groups:
#     (i) YesTusk_A, (ii) YesTusk_J, YesTuskA + YesTusk_J
# 4. mixed tusk groups without calves: 
#     (i) YT_A + YT_J + NT_J, (ii) YT_A + NT_J, (iii) YT_A + NT_A + NT_J, (iv) YT_A + NT_A, (v) YT_J + NT_A + NT_J
# 5. mixed tusk groups with calves:
#     (i) YT_J + NT_A + NT_J + NT_C, (ii) YT_J + NT_A + NT_C, (iii) YT_A + NT_A + NT_J + NT_C
#====== +++ === === +++ === === +++ === ===
View(dat.GroupCompKnown)
names(dat.GroupCompKnown)

# TODO: Test this function on dat.GroupCompKnown, then replace code below with function call
assign.supergroup.bits = function(dat){
  dat$YesTusk_A_tf = rep("", nrow(dat))
  dat$YesTusk_J_tf = rep("", nrow(dat))
  dat$NoTusk_A_tf = rep("", nrow(dat))
  dat$NoTusk_J_tf = rep("", nrow(dat))
  dat$NoTusk_C_tf = rep("", nrow(dat))
  
  dat$YesTusk_A_tf = with(dat, ifelse(YesTusk_A > 0, 1, 0))
  dat$YesTusk_J_tf = with(dat, ifelse(YesTusk_J > 0, 1, 0))
  dat$NoTusk_A_tf = with(dat, ifelse(NoTusk_A > 0, 1, 0))
  dat$NoTusk_J_tf = with(dat, ifelse(NoTusk_J > 0, 1, 0))
  dat$NoTusk_C_tf = with(dat, ifelse(NoTusk_C > 0, 1, 0))
  
  dat$super.group.bits = with(dat, paste(YesTusk_A_tf, YesTusk_J_tf, NoTusk_A_tf, NoTusk_J_tf, NoTusk_C_tf, sep=""))
  # with(dat, unique(super.group.bits))
  
  # YesTusk_A ; YesTusk_J ; NoTusk_A ; NoTusk_J ; NoTusk_C
  SuperGroup1 = c("00010", "00110", "00100") # (i) NoTusk_J, (ii) NoTusk_A + NoTusk_J (iii) NoTusk_A
  SuperGroup2 = c("00101", "00111") # (i) NoTusk_A + NoTusk_C, (ii) NoTusk_A + NoTusk_J + NoTusk_C
  SuperGroup3 = c("10000", "01000", "11000") # (i) YesTusk_A, (ii) YesTusk_J, (iii) YesTuskA + YesTusk_J
  SuperGroup4 = c("11010", "10010", "10110", "10100", "01110", "11110") # (i) YT_A + YT_J + NT_J, (ii) YT_A + NT_J, (iii) YT_A + NT_A + NT_J, (iv) YT_A + NT_A, (v) YT_J + NT_A + NT_J, (vii) "11110"
  SuperGroup5 = c("01111", "01101", "10111", "11101") # (i) YT_J + NT_A + NT_J + NT_C, (ii) YT_J + NT_A + NT_C, (iii) YT_A + NT_A + NT_J + NT_C, (iv) "11101"
  
  dat$SuperGroup = rep("", nrow(dat))
  ii = NULL
  for(ii in 1:nrow(dat.GroupCompKnown)){
    if(dat$super.group.bits[ii] %in% SuperGroup1) dat$SuperGroup[ii] = 1
    if(dat$super.group.bits[ii] %in% SuperGroup2) dat$SuperGroup[ii] = 2
    if(dat$super.group.bits[ii] %in% SuperGroup3) dat$SuperGroup[ii] = 3
    if(dat$super.group.bits[ii] %in% SuperGroup4) dat$SuperGroup[ii] = 4
    if(dat$super.group.bits[ii] %in% SuperGroup5) dat$SuperGroup[ii] = 5
  }  
  return(dat)
}


dat.GroupCompKnown$YesTusk_A_tf = rep("", nrow(dat.GroupCompKnown))
dat.GroupCompKnown$YesTusk_J_tf = rep("", nrow(dat.GroupCompKnown))
dat.GroupCompKnown$NoTusk_A_tf = rep("", nrow(dat.GroupCompKnown))
dat.GroupCompKnown$NoTusk_J_tf = rep("", nrow(dat.GroupCompKnown))
dat.GroupCompKnown$NoTusk_C_tf = rep("", nrow(dat.GroupCompKnown))

dat.GroupCompKnown$YesTusk_A_tf = with(dat.GroupCompKnown, ifelse(YesTusk_A > 0, 1, 0))
dat.GroupCompKnown$YesTusk_J_tf = with(dat.GroupCompKnown, ifelse(YesTusk_J > 0, 1, 0))
dat.GroupCompKnown$NoTusk_A_tf = with(dat.GroupCompKnown, ifelse(NoTusk_A > 0, 1, 0))
dat.GroupCompKnown$NoTusk_J_tf = with(dat.GroupCompKnown, ifelse(NoTusk_J > 0, 1, 0))
dat.GroupCompKnown$NoTusk_C_tf = with(dat.GroupCompKnown, ifelse(NoTusk_C > 0, 1, 0))

dat.GroupCompKnown$super.group.bits = with(dat.GroupCompKnown, paste(YesTusk_A_tf, YesTusk_J_tf, NoTusk_A_tf, NoTusk_J_tf, NoTusk_C_tf, sep=""))
with(dat.GroupCompKnown, unique(super.group.bits))

# YesTusk_A ; YesTusk_J ; NoTusk_A ; NoTusk_J ; NoTusk_C
SuperGroup1 = c("00010", "00110", "00100") # (i) NoTusk_J, (ii) NoTusk_A + NoTusk_J (iii) NoTusk_A
SuperGroup2 = c("00101", "00111") # (i) NoTusk_A + NoTusk_C, (ii) NoTusk_A + NoTusk_J + NoTusk_C
SuperGroup3 = c("10000", "01000", "11000") # (i) YesTusk_A, (ii) YesTusk_J, (iii) YesTuskA + YesTusk_J
SuperGroup4 = c("11010", "10010", "10110", "10100", "01110") # (i) YT_A + YT_J + NT_J, (ii) YT_A + NT_J, (iii) YT_A + NT_A + NT_J, (iv) YT_A + NT_A, (v) YT_J + NT_A + NT_J
SuperGroup5 = c("01111", "01101", "10111", "11101") # (i) YT_J + NT_A + NT_J + NT_C, (ii) YT_J + NT_A + NT_C, (iii) YT_A + NT_A + NT_J + NT_C

dat.GroupCompKnown$SuperGroup = rep("", nrow(dat.GroupCompKnown))
ii = NULL
for(ii in 1:nrow(dat.GroupCompKnown)){
  if(dat.GroupCompKnown$super.group.bits[ii] %in% SuperGroup1) dat.GroupCompKnown$SuperGroup[ii] = 1
  if(dat.GroupCompKnown$super.group.bits[ii] %in% SuperGroup2) dat.GroupCompKnown$SuperGroup[ii] = 2
  if(dat.GroupCompKnown$super.group.bits[ii] %in% SuperGroup3) dat.GroupCompKnown$SuperGroup[ii] = 3
  if(dat.GroupCompKnown$super.group.bits[ii] %in% SuperGroup4) dat.GroupCompKnown$SuperGroup[ii] = 4
  if(dat.GroupCompKnown$super.group.bits[ii] %in% SuperGroup5) dat.GroupCompKnown$SuperGroup[ii] = 5
}
with(dat.GroupCompKnown, unique(SuperGroup))

str(dat.GroupCompKnown)
length(which(dat.GroupCompKnown$GroupSpread == "N"))
length(which(dat.GroupCompKnown$SuperGroup == "")) # One type of GroupComp missing (n=1)! TODO: fix sets
table(dat.GroupCompKnown$SuperGroup)
write.csv(dat.GroupCompKnown, "dat.GroupCompKnown.csv"); system("open dat.GroupCompKnown.csv")

#====== +++ === === +++ === === +++ === ===
# Create table for appendix with Group Comp x Group Size
#====== +++ === === +++ === === +++ === ===
View(dat.GroupCompKnown)
addmargins(with(dat.GroupCompKnown, table(super.group.bits, GroupSize)))
foo = with(dat.GroupCompKnown, table(super.group.bits, GroupSize))
foo = addmargins(foo)
foo = as.data.frame(foo)
foo = cast(foo, super.group.bits ~ GroupSize)
foo = arrange(foo, desc(Sum))

#====== +++ === === +++ === === +++ === ===
# 8. 
# For predominant group types, I’m guessing these would be 
#  1. NoTusk_A and NoTusk_C   
#  2. YesTusk_A only   
#  3. YesTusk_A and YesTusk_J  
#  4. NoTusk_A and YesTusk_J 
#  5. NoTusk_A and YesTusk_J and NoTusk_C), 
#
#  I’d like to summarize group spread, formation, speed, and distance away (as in #3)
#====== +++ === === +++ === === +++ === ===
with(dat.GroupCompKnown, table(SuperGroup, GroupSpread, useNA = "ifany")) # Table Spread
with(dat.GroupCompKnown, table(SuperGroup, Formation, useNA = "ifany")) # Table Formation
with(dat.GroupCompKnown, table(SuperGroup, Speed, useNA = "ifany")) # Table Speed
with(dat.GroupCompKnown, table(SuperGroup, DistanceAway, useNA = "ifany")) # Table Distance Away
?group_by
# Use barplots to summarize GroupSpread
gg10 = ggplot(subset(dat.GroupCompKnown, GroupSpread %in% c("Tight", "Loose")), aes(x = GroupSpread))
gg10 + geom_bar() + xlab("Group Spread") + ylab("Count") + facet_grid(SuperGroup ~ .)

# Use barplots to summarize Formation
gg11 = ggplot(subset(dat.GroupCompKnown, Formation %in% c("Circular", "Linear", "Parallel")), aes(x = Formation))
gg11 + geom_bar() + xlab("Formation") + ylab("Count") + facet_grid(SuperGroup ~ .)

# Use barplots to summarize Speed
gg12 = ggplot(dat.GroupCompKnown, aes(x = Speed))
gg12 + geom_bar() + xlab("Group Speed") + ylab("Count") + facet_grid(SuperGroup ~ .) + scale_x_discrete(limits=c("Slow","Medium","Fast"))

# Use barplots to summarize Distance Away
gg13 = ggplot(dat.GroupCompKnown, aes(x = DistanceAway))
gg13 + geom_bar() + xlab("Group Distance Away") + ylab("Count") + facet_grid(SuperGroup ~ .)

# dat.GroupCompTable = table(dat.GroupCompKnown$GroupComp, dat.GroupCompKnown$GroupSize)
# dat.GroupCompTable = as.data.frame(dat.GroupCompTable)
# 
# TotalGroups = ddply(dat.GroupCompTable, .(Var1), summarise, TotalGroups = sum(Freq))
# 
# table.group.comp = cast(dat.GroupCompTable, Var1 ~ Var2)
# table.group.comp$TotalGroups = TotalGroups$TotalGroups
# table.group.comp = arrange(table.group.comp, desc(TotalGroups))
# table.group.comp
# 
# head(dat.GroupCompKnown)
# gsub(',,', '', dat.GroupCompKnown$GroupComp[1]) # substitutes first argument with second argument, in the string in third argument

# For groups where group composition is known (i.e. GroupSize = sum(YesTusk_A,YesTusk_J, NoTusk_A, NoTusk_J, NoTusk_C)
#====== +++ === === +++ === === +++ === ===
# 8a. 
# For predominant group types, I’m guessing these would be 
#  Summarize group spread
#====== +++ === === +++ === === +++ === ===
View(dat.GroupCompKnown)
str(dat.GroupCompKnown)
with(dat.GroupCompKnown, table(GroupComp, Formation))

# For groups where group composition is known (i.e. GroupSize = sum(YesTusk_A,YesTusk_J, NoTusk_A, NoTusk_J, NoTusk_C)
#====== +++ === === +++ === === +++ === ===
# 8b. 
# For predominant group types, I’m guessing these would be 
#  Summarize group formation
#====== +++ === === +++ === === +++ === ===


# For groups where group composition is known (i.e. GroupSize = sum(YesTusk_A,YesTusk_J, NoTusk_A, NoTusk_J, NoTusk_C)
#====== +++ === === +++ === === +++ === ===
# 8c. 
# For predominant group types, I’m guessing these would be 
#  Summarize group speed
#====== +++ === === +++ === === +++ === ===


# For groups where group composition is known (i.e. GroupSize = sum(YesTusk_A,YesTusk_J, NoTusk_A, NoTusk_J, NoTusk_C)
#====== +++ === === +++ === === +++ === ===
# 8d. 
# For predominant group types, I’m guessing these would be 
#  Summarize group distance
#====== +++ === === +++ === === +++ === ===

# Define a new column with a pasted list of group-types

save.image("~/Documents/2014 Work/Milne Inlet Narwhals/2014 Analysis/Code/ScanSamples2014.RData")
rm(list = ls())
load("~/Documents/2014 Work/Milne Inlet Narwhals/2014 Analysis/Code/ScanSamples2014.RData")



