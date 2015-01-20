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
load.packages() # loads a batch of packages -- should move these into .Rprofile

# Set Options
if (getOption("stringsAsFactors")) options(stringsAsFactors = FALSE) # set global option, don't want strings read as factors

# Initialize base data directory and file name
rm(list = ls())
base.dir = "~/Documents/2014 Work/Milne Inlet Narwhals/Data/2014"
fix.dfile = "FocalFollows_ByFix_2014.csv" # 2014 Focal Follows By Fix data, saved as comma delimited
tracksummary.dfile = "FocalFollows_TrackSummary_2014.csv" # 2014 Focal Follows By Fix data, saved as comma delimited
getwd()
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
str(fixdat2014)
View(fixdat2014)

fixdat2014 = assign.groupcomp.known(fixdat2014)
with(fixdat2014, unique(GroupFollow)); with(fixdat2014, length(unique(GroupFollow)))

with(fixdat2014, unique(GroupFollow[GroupCompKnown]))

# write.csv(fixdat2014, "foo1.csv"); system("open foo1.csv") # check

# If one of the fixes has a known GroupComp, then GroupCompKnown = TRUE
#  TODO: Introduce a check here for unknown stage classes in any of the fixes -- which would imply the GroupComp is not strictly known
#  TODO: Replace code in 'assign.groupcomp.known' function with ddply and join calls below
# CompKnown = ddply(fixdat2014, .(GroupFollow), summarise, 
#       GroupCompKnown = ifelse(any(GroupCompKnown), TRUE, FALSE))
# 
# foo2 = join(fixdat2014, CompKnown, by = "GroupFollow")
# write.csv(foo2, "foo2.csv"); system("open foo2.csv") # check

# Assign "Super Group" Type to each follow of known GroupComp

#====== +++ === === +++ === === +++ === ===
# Work on table with known group types (SuperGroups)
#  - Want primary and secondary behaviour budgets
#  - Take shortcut that Tracking data (at least for 2014) has compiled GroupComps
#  NOTE: The last steps for this task were done by hand in spreadsheet:
#
#====== +++ === === +++ === === +++ === ===
trackdat2014 # names(trackdat2014)
filtered.trackdat2014 = subset(trackdat2014, !is.na(TrackDuration)) # remove tracks without multiple fixes
filtered.trackdat2014 = subset(filtered.trackdat2014, GroupFollow != "F15") # Drop this follow, bad data, from 2014
with(filtered.trackdat2014, unique(GroupFollow)) # check

sub.filtered.trackdat2014 = subset(filtered.trackdat2014, 
                                   select = c(GroupFollow, Anthro, TrackDuration, 
                                      YesTusk_A, YesTusk_J, 
                                      YesTusk_U, NoTusk_U, 
                                      NoTusk_A, NoTusk_J, 
                                      NoTusk_C, NoTusk_U,
                                      UTusk_A, UTusk_J, UTusk_U))
sub.filtered.trackdat2014 # note: for 2014 we've dropped two focal follows, each had only one fix

# how many uknown categories are also unobserved (NA)
unknown.categories = with(sub.filtered.trackdat2014, 
                     data.frame(YesTusk_U, NoTusk_U,  UTusk_A, UTusk_J, UTusk_U))
head(unknown.categories) #str(unknown.categories)

unknown.follows = rowSums(unknown.categories, na.rm = TRUE) # consider NA == 0
unknown.follows # check

index.unknown.follows = which(is.na(unknown.follows) | unknown.follows > 0)
index.unknown.follows # check

known.follows = filtered.trackdat2014[ -index.unknown.follows , ]

known.follows = assign.supergroup.bits(known.follows) # Assign SuperGroups
with(known.follows, SuperGroup)  # check

sub.known.follows = subset(known.follows, select = c(GroupFollow, Anthro, SuperGroup))

setdiff(names(sub.known.follows), names(fixdat2014))

# Filter out follows from detailed fix data 
follows.to.remove = setdiff(fixdat2014$GroupFollow, sub.known.follows$GroupFollow)
index.follows.to.remove = which(fixdat2014$GroupFollow %in% follows.to.remove)
filtered.fixdat2014 = fixdat2014[ - index.follows.to.remove, ]
filtered.fixdat2014 = join(fixdat2014, sub.known.follows, by = "GroupFollow")
filtered.fixdat2014 = filtered.fixdat2014[ - which(is.na(filtered.fixdat2014$SuperGroup)), ]
  
View(filtered.fixdat2014)
with(filtered.fixdat2014, table(SuperGroup))

# Sum total time each group followed and add column (note 'transform' argument)
filtered.fixdat2014 = ddply(filtered.fixdat2014, .(Follow), transform,
                        tot.time = sum(LegTime))
View(filtered.fixdat2014)

# Total follow times by SuperGroup
ddply(filtered.fixdat2014, .(SuperGroup), summarise, 
      tot.time = sum(unique(tot.time)))

# Want the percentage of SuperGroups that spent XX % of time in primary behavior X
ddply(filtered.fixdat2014, .(SuperGroup, PrimaryBeh), summarise, 
      tot.time = sum(unique(tot.time)))


ddply(filtered.fixdat2014, .(SuperGroup, PrimaryBeh), summarise, 
      time = sum(unique(tot.time)))

ddply(filtered.fixdat2014, .(SuperGroup, SecondaryBeh), summarise, 
      time = sum(unique(tot.time)))

table.tot.time = ddply(filtered.fixdat2014, .(Follow), summarise, # Checking
      super.group = unique(SuperGroup),
      tot.time = sum(LegTime), 
      prime.beh = paste(unique(PrimaryBeh), collapse = " "),
      secondary.beh = paste(unique(SecondaryBeh), collapse = " "))
table.tot.time = arrange(table.tot.time, super.group, prime.beh)

write.csv(table.tot.time, "table.tot.time.csv"); system("open table.tot.time.csv")

follow.ii = table.tot.time$Follow[which(table.tot.time$prime.beh != "T")] 
follow.ii
complex.follows = filtered.fixdat2014[which(filtered.fixdat2014$Follow %in% follow.ii),]
sub.complex.follows = subset(complex.follows,
       select = c(Follow, PrimaryBeh, LegTime, Anthro, SuperGroup, tot.time))

sub.complex.follows$percent.time = sub.complex.follows$LegTime / sub.complex.follows$tot.time
sub.complex.follows$percent.time = round(sub.complex.follows$percent.time, 2)
ddply(sub.complex.follows, .(Follow, PrimaryBeh), summarise, percent.time = sum(percent.time))

View(sub.complex.follows)

head(known.follows, n=10); nrow(known.follows)

# foo.known = subset(trackdat2014.keep, select = c(YesTusk_A, YesTusk_J,  
#                                                  NoTusk_A, NoTusk_J, 
#                                                  NoTusk_C))
# head(foo.known)
# foo.known$groupsize = rowSums(foo.known, na.rm = FALSE)
# foo.known = subset(foo.known, !is.na(groupsize)); nrow(foo.known)
# 
# 
# foo.test = foo.known[which(is.na(foo.known))]
# 
# 
# with(foo1, which(YesTusk_U < 1 & NoTusk_U < 1 & ))
# 
# foo = subset(foo1, YesTusk_U | YesTusk_U < 1)
# 
# ddply(foo0, .(GroupFollow))
# 
# foo = assign.supergroup.bits(trackdat2014)
# 
# fix.groupcomp.known = subset(fixdat2014, GroupCompKnown)
# fix.groupcomp.known = subset(fix.groupcomp.known, LegTime>0)
# fix.groupcomp.known; nrow(fix.groupcomp.known)
# with(fix.groupcomp.known, unique(GroupFollow)); with(fix.groupcomp.known, length(unique(GroupFollow)))
# with(fix.groupcomp.known, unique(GroupFollow))
# 
# foo = assign.supergroup.bits(fix.groupcomp.known)

#====== +++ === === +++ === === +++ === ===
# Summary table given (summary) of track data
#  Number of 
#====== +++ === === +++ === === +++ === ===
View(filtered.trackdat2014)
nrow(filtered.trackdat2014); nrow(trackdat2014) # Check
names(filtered.trackdat2014)

filtered.trackdat2014$VessPrescence = rep(FALSE, nrow(filtered.trackdat2014))
# filtered.trackdat2014$VessPrescence[which()] = TRUE
vess.ii = which(filtered.trackdat2014$Anthro %in% c("MV", "SV"))
vess.ii
with(filtered.trackdat2014, table(Anthro))
filtered.trackdat2014$VessPrescence[vess.ii] = TRUE
View(filtered.trackdat2014)
filtered.trackdat2014$Linearity

foo = ddply(filtered.trackdat2014, .(VessPrescence), summarise, 
      no.fixes = sum(NoOfFixes), 
      min.track.duration = min(TrackDuration),
      mean.track.duration = mean(TrackDuration),
      max.track.duration = max(TrackDuration),
      min.tot.distance = min(TotDistance), 
      mean.tot.distance = mean(TotDistance),
      max.tot.distance = max(TotDistance),
      min.linearity = min(Linearity),
      mean.linearity = mean(Linearity),
      max.linearity = max(Linearity),
      min.reorientation.rate = min(ReorientationRate),
      mean.reorientation.rate = mean(ReorientationRate),
      max.reorientation.rate = max(ReorientationRate),
      min.min.legspeed = min(MinLegSpeed),
      mean.min.legspeed = mean(MinLegSpeed),
      max.min.legspeed = max(MinLegSpeed),
      min.max.legspeed = min(MaxLegSpeed),
      mean.max.legspeed = mean(MaxLegSpeed),
      max.max.legspeed = max(MaxLegSpeed)
      )

# foo = subset(foo, select = c(Anthro, no.fixes, mean.track.duration, 
#                       mean.tot.distance, mean.linearity,
#                       mean.reorientation.rate, 
#                       mean.min.legspeed,
#                       mean.max.legspeed))

foo 
foo1 = melt(foo, id = "VessPrescence")
foo1
foo2 = cast(foo1,  variable ~ VessPrescence)
foo2 = mutate(foo2, GroupsWithVess = MV + SV, GroupsNoVess = N)
foo2

foo3 = subset(foo2, select = c(variable, GroupsWithVess, GroupsNoVess))
foo3$GroupsWithVes = with(foo3, round(GroupsWithVess, 2))
foo3

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

table.beh.list

#table.beh.list = mutate(table.beh.list, PrimaryBeh = gsub(", NA", "", table.beh.list$PrimaryList))
table.beh.list = mutate(table.beh.list, SecondaryList = gsub("\\+ NA", "", table.beh.list$SecondaryList))
table.beh.list = mutate(table.beh.list, SecondaryList = gsub(" \\+ N", "", table.beh.list$SecondaryList))
table.beh.list = mutate(table.beh.list, SecondaryList = gsub("N \\+ ", "", table.beh.list$SecondaryList))
table.beh.list = mutate(table.beh.list, SecondaryList = gsub("N", "", table.beh.list$SecondaryList))
table.beh.list = mutate(table.beh.list, SecondaryList = gsub("A", "", table.beh.list$SecondaryList))
table.beh.list = mutate(table.beh.list, SecondaryList = gsub(" \\+ DI", "DI", table.beh.list$SecondaryList)) # Note: Back-slashes in front of plus sign (+)
table.beh.list = mutate(table.beh.list, SecondaryList = gsub("DI \\+ ", "DI", table.beh.list$SecondaryList))
table.beh.list

with(table.beh.list, table(SecondaryList, PrimaryBeh))

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
