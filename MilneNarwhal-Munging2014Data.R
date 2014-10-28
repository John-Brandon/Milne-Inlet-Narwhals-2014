###==============
# Author: John R. Brandon
# eMail:  jbrandon greeneridge [or jbrandon@gmail.com]
# Date :  Fall 2014
# OS   :  Mac OS 10.9.5 (x86_64-apple-darwin10.8.0 (64-bit))
# Language : R (ver 3.0.2 (2013-09-25) -- "Frisbee Sailing")
# IDE  : Rstudio v 0.98.484
###=== === +++ === === +++ === === +++ === ===
# Purpose : Visualize and analyze Milen Inlet narwhal data for LGL / Baffinland 
#  Notes : 
#  1) Your main directory will differ. See 'base.dir' variable below to edit that path to match your system
#  2) POSIXct is a class for representing calendar dates and times (these must be input in a strictly decreasing unambigous order, i.e. "YYYY-MM-DD HH:MM:SS")
#  3) These data are also known as "Relative Abundance and Distribution" (RAD) data 
#
#====== +++ === === +++ === === +++ === ===
if (getOption("stringsAsFactors")) options(stringsAsFactors = FALSE) # set global option, don't want strings read as factors
getOption("stringsAsFactors") # check

library(plyr) # Hadley Wickham's "Plier" package for common tasks (e.g. summarizing) with data.frames

rm(list=ls()) # clear leftovers from previous workspace
foo = NULL
# load("~/Documents/2014 Work/Milne Inlet Narwhals/2014 Analysis/Code/MilneNarwhal.2014.RData") # Load workspace 

setwd("~/Documents/2014 Work/Milne Inlet Narwhals/Data/2014") # Set working directory for data
dfile = "2014.milne.inlet.narwhal.csv" # 2014 RAD data file, saved as comma delimited
dat2014 = read.csv(file = dfile, header = TRUE, as.is = TRUE, na.strings = c("NA", "x", "X")) # Read data file 

#====== +++ === === +++ === === +++ === ===
# Clean up some typos
#====== +++ === === +++ === === +++ === ===
typos.ii = which(dat2014$SubStratum == "13") 
dat2014$SubStratum[typos.ii] = "I3"

#====== +++ === === +++ === === +++ === ===
# Start munging
#====== +++ === === +++ === === +++ === ===
#dat2014 = cbind(dat2014, colsplit(dat2014$SubStratum, split="", names = c("Stratum", "SubStratum.num")))
dat2014$Stratum = substring(dat2014$SubStratum, first = 1, last = 1) # Create a vector with Stratum from SubStratum vector
dat2014$SubStratum.num = substring(dat2014$SubStratum, first = 2, last = 2) # Create a vector with SubStratum.num from SubStratum vector

dat2014$datetime = with(dat2014, paste(Date, Time)) # Convert DateTime to POSIXct class
dat2014$datetime = as.POSIXct(dat2014$datetime) # not really necessary at this stage, but a place holder for possible future analysis / visualizing time series

length(unique(dat2014$datetime)) # number of "Relative Abundance and Distribution" (RAD) samples in 2014

#====== +++ === === +++ === === +++ === ===
# Keep munging:
# Table counts of group sizes by sub-stratum
#====== +++ === === +++ === === +++ === ===
group.size = table(dat2014$SubStratum, dat2014$GroupSize) # ?table
group.size = as.data.frame(group.size) # data.frame with group size frequencies in counts (e.g. 1 group of 34 in substratum F1, 0 groups of 33, etc.)
names(group.size) = c("SubStratum", "GroupSize", "Freq") # make it easier to read
write.csv(file = "foo.csv", x = group.size, row.names = FALSE); system("open foo.csv") # check

#====== +++ === === +++ === === +++ === ===
# Add a column to data.frame, assigning TRUE or FALSE to vessel count
#  If CountType is "PRE", "C" or "POST" vessel count is TRUE, otherwise FALSE
#====== +++ === === +++ === === +++ === ===
dat2014$Vessel.related.count = rep(FALSE, nrow(dat2014))
Vessel.related.count.ii = which(dat2014$CountType %in% c("PRE", "C", "POST")) # which CountType records are "PRE", "C" or "POST"
dat2014$Vessel.related.count[Vessel.related.count.ii] = TRUE

write.csv(x = dat2014, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check

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
# Summarize abundance by SubStratum (integrating over time)
#====== +++ === === +++ === === +++ === ===
tot.counts = group.size
i = sapply(tot.counts, is.factor) # intermediate step to convert columns that are presently factors to characters
tot.counts[i] <- lapply(tot.counts[i], as.character) # convert columns that are factors to character strings
tot.counts$TotalCount = as.numeric(tot.counts$GroupSize) * tot.counts$Freq # TotalCounts are product of group size and numbers of groups
tot.counts$Stratum = substring(tot.counts$SubStratum, first = 1, last = 1) # Create a vector with Stratum from SubStratum vector
tot.counts$SubStratum.num = substring(tot.counts$SubStratum, first = 2, last = 2) # Create a vector with SubStratum.num from SubStratum vector

tot.counts.subs = ddply(tot.counts, "SubStratum", summarise, TotalCount = sum(TotalCount)) # uses 'plyr' package, could also use function aggregate
# tot.counts.subs = aggregate(tot.counts.tmp$TotalCount, by = list(SubStratum = tot.counts.tmp$SubStratum), sum) # same result as line above

head(tot.counts.subs)
foo = tot.counts.subs
write.csv(file = "foo.csv", x = foo, row.names = FALSE); system("open foo.csv") # check

#====== +++ === === +++ === === +++ === ===
# Summarize by Stratum (integrating over time)
#====== +++ === === +++ === === +++ === ===
tot.counts.strat = ddply(tot.counts, "Stratum", summarise, TotalCount = sum(TotalCount)) # uses 'plyr' package, could also use function aggregate
head(tot.counts.strat) # check
tot.counts.strat

library(ggplot2)
ggplot(tot.counts.strat, aes(x = Stratum, y = TotalCount)) + geom_bar(stat = "identity")

#====== +++ === === +++ === === +++ === ===
# Save workspace image
#====== +++ === === +++ === === +++ === ===
save.image("~/Documents/2014 Work/Milne Inlet Narwhals/2014 Analysis/Code/MilneNarwhal.2014.RData")

#====== +++ === === +++ === === +++ === ===
#====== +++ === === +++ === === +++ === ===
# %%% 
# SCRATCH CODE BELOW
# %%% 
#====== +++ === === +++ === === +++ === ===
pp <- function (n,r=4) {
  x <- seq(-r*pi, r*pi, len=n)
  df <- expand.grid(x=x, y=x)
  df$r <- sqrt(df$x^2 + df$y^2)
  df$z <- cos(df$r^2)*exp(-df$r/6)
  df
}
str(pp(10))


# SCRATCH CODE BELOW
(dat <- data.frame(time = 1:10, s1 = rnorm(10), s2 = rnorm(10), s3 = rnorm(10)))
library('reshape2')
library('ggplot2')

# melt the data (wide -> long)
(dm <- melt(dat, id = 'time'))

str(dm)

head(dm)

# Plot the series together, distinguish by color:
ggplot(dm, aes(x = time, y = value, colour = variable)) +
  geom_path(size = 1)
# Plot the series in separate panels
ggplot(dm, aes(x = time, y = value)) + geom_path(size = 1) +
  facet_wrap(~ variable, ncol = 1)
#=== === === === ===
library(reshape)
library(ggplot2)
# Using ggplot2 0.9.2.1

nba <- read.csv("http://datasets.flowingdata.com/ppg2008.csv")
nba$Name <- with(nba, reorder(Name, PTS))
nba.m <- melt(nba)
nba.m <- ddply(nba.m, .(variable), transform, value = scale(value))
?scale
# Convert the factor levels to numeric + quanity to determine size of hole.
nba.m$var2 = as.numeric(nba.m$variable) + 15

# Labels and breaks need to be added with scale_y_discrete.
y_labels = levels(nba.m$variable)
y_breaks = seq_along(y_labels) + 15

p2 = ggplot(nba.m, aes(x=Name, y=var2, fill=value)) +
  geom_tile(colour="white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  ylim(c(0, max(nba.m$var2) + 0.5)) +
  scale_y_discrete(breaks=y_breaks, labels=y_labels) +
  coord_polar(theta="x") +
  theme(panel.background=element_blank(),
        axis.title=element_blank(),
        panel.grid=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks=element_blank(),
        axis.text.y=element_text(size=5))

p2
