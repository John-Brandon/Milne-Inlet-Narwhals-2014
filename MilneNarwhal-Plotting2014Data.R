###==============
# Author: John R. Brandon
# eMail:  jbrandon greeneridge [or jbrandon@gmail.com]
# Date :  Fall 2014
# OS   :  Mac OS 10.9.5 (x86_64-apple-darwin10.8.0 (64-bit))
# Language : R (ver 3.0.2 (2013-09-25) -- "Frisbee Sailing")
# IDE  : Rstudio v 0.98.484
###=== === +++ === === +++ === === +++ === ===
# Purpose : Plotting Milne Inlet narwhal data for LGL / Baffinland 
#  Notes : These routines work on data that has already been extracted and munged, so the first step is to load that pre-massaged data 
#  1) Your main directory will differ. You will have to edit the lines of code with directories to match your system
#  2) Loads existing workspace, that contains 2014 data (dat2014) and some tables created in 'MilneNarwhal-Table2014Data.R' script
#  3) These data are also known as "Relative Abundance and Distribution" (RAD) data 
#====== +++ === === +++ === === +++ === ===
library(plyr) # for working with data
library(mgcv) # provides capability to plot smoothed functions 
library(ggplot2) # for plotting
library(RColorBrewer) # for creating color palettes

rm(list = ls())
load("~/Documents/2014 Work/Milne Inlet Narwhals/2014 Analysis/Code/MilneNarwhal.2014.RData") # Load workspace 
setwd("~/Documents/2014 Work/Milne Inlet Narwhals/Data/2014") # Set working directory for data, really for outputting tables (data has already been read)

# Initialize some plottig parameters -- create custom plotting theme for ggplot2
mytheme = theme_grey() + theme(axis.title.x = element_text(size = rel(1.75), vjust = 0.0), 
                               axis.title.y = element_text(size = rel(1.75), vjust = 1.0), 
                               axis.text = element_text(size = rel(1.5), colour = "black"),
                               plot.title = element_text(size = rel(2.5)),
                               legend.text = element_text(size=14), 
                               legend.title = element_text(size = 14),
                               strip.text.x = element_text(size = 14), 
                               strip.text.y = element_text(size = 14))                               ) # , plot.margin=unit(c(1,1,1,1),"cm")

mytheme_larger = theme_grey() + theme(axis.title.x = element_text(size = rel(2.25), vjust = 0.0), 
                               axis.title.y = element_text(size = rel(2.25), vjust = 1.0), 
                               axis.text = element_text(size = rel(2.0), colour = "black"),
                               plot.title = element_text(size = rel(2.75)),
                               legend.text = element_text(size=16), 
                               legend.title = element_text(size = 16),
                               strip.text.x = element_text(size = 24), 
                               strip.text.y = element_text(size = 24))                     

mytheme_bw = theme_bw() + theme(axis.title.x = element_text(size = rel(1.75), vjust = 0.0), 
                               axis.title.y = element_text(size = rel(1.75), vjust = 1.0), 
                               axis.text = element_text(size = rel(1.5), colour = "black"),
                               plot.title = element_text(size = rel(2.5)),
                               legend.text = element_text(size=20), 
                               legend.title = element_text(size = 20),
                               panel.grid.major = element_line(colour = "darkgray"),
                               strip.text.x = element_text(size = 14), 
                               strip.text.y = element_text(size = 14))
                               #panel.grid.minor = element_line(colour = "gray")) # , plot.margin=unit(c(1,1,1,1),"cm")
#====== +++ === === +++ === === +++ === ===
# Histogram of total counts by strata 
#  Note: tot.counts.strat are for all sighting conditions AND include periods when large vessels were associated with counts
#====== +++ === === +++ === === +++ === ===
View(tot.counts.strat)
filtered.no.vess.dat2014
filtered.tot.counts.strat = ddply(tot.counts, "Stratum", summarise, 
                                  TotalCount = sum(TotalCount, na.rm = TRUE))

ggplot(tot.counts.strat, aes(x = Stratum, y = TotalCount)) + 
  geom_bar(stat = "identity") + ylab("Total Number of narwhals") + mytheme

fig.14.dat = table.group.size(filtered.dat2014.less.vessels)
fig.14.dat = calc.intermediate.counts(fig.14.dat)
fig.14.dat = calc.total.counts.strat(fig.14.dat)
ggplot(fig.14.dat, aes(x = Stratum, y = TotalCount)) + 
  geom_bar(stat = "identity") + ylab("Total Number of Narwhals") + mytheme

#====== +++ === === +++ === === +++ === ===
# Histograms of the distribution of count sizes (excluding zero counts) by strata 
#  These include vessel associated counts, but have filtered out poor sightability
#====== +++ === === +++ === === +++ === ===
positive.counts.by.stratum = subset(counts.by.stratum.keepers, TotalCount.without.na > 0)
ggplot(positive.counts.by.stratum, aes(x = TotalCount.without.na)) + geom_histogram() + facet_grid(Stratum ~ .) + xlab("Count") + ylab("Frequency")

#====== +++ === === +++ === === +++ === ===
# Histogram of group sizes in sub-strata
#  NOTE: use of "origin" in geom_histogram()
#====== +++ === === +++ === === +++ === ===
g = ggplot(data = filtered.dat2014.less.vessels, aes(x = GroupSize)) 
g = g + geom_histogram(fill = "gray40", colour = "black", binwidth = 1, na.rm = TRUE, origin = -0.5)
g = g + facet_grid(Stratum ~ SubStratum.num)
g + xlim(c(-0.5, 6.5)) + xlab("Narwhal Group Size") + ylab("Frequency of Narwhal Group Size") + mytheme + scale_y_continuous(breaks = c(50, 150))

#====== +++ === === +++ === === +++ === ===
# Histogram of group sizes in Strata
#  NOTE: use of "origin" in geom_histogram()
#====== +++ === === +++ === === +++ === ===
g = ggplot(data = filtered.dat2014.less.vessels, aes(x = GroupSize))
g = g + geom_histogram(fill = "gray40", colour = "black", binwidth = 1, na.rm = TRUE, origin = -0.50) # NOTE: origin
g = g + facet_grid(Stratum ~ .) 
# g = g + scale_x_discrete(breaks = 0:7)
g + xlim(c(-0.5, 6.5)) + xlab("Group Size") + ylab("Frequency") + mytheme + scale_y_continuous(breaks = c(200, 400))

#====== +++ === === +++ === === +++ === ===
# Histograms of (1) total and (2) mean counts by hour, over all strata
#  Exclude counts associated with large vessels
#====== +++ === === +++ === === +++ === ===
# Use ddply to bin (1) number of counts, and (2) narwhals by hourly blocks -- do without large vessel counts
write.csv(x = dat2014, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check

without.vessels.dat2014 = subset(dat2014, (CountType != "PRE" & CountType != "C" & CountType != "POST"))

fig.numbers.by.hour = ddply(without.vessels.dat2014, "rounded.hour", summarise, 
                            Numbers = sum(GroupSize, na.rm = TRUE), 
                            Counts = length(unique(Count.id))
                            ) 

names(without.vessels.dat2014)
sum(fig.numbers.by.hour$Numbers)
sum(fig.numbers.by.hour$Counts)

fig.numbers.by.hour$Mean.number = fig.numbers.by.hour$Numbers / fig.numbers.by.hour$Counts

write.csv(x = fig.numbers.by.hour, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check

# Bar graph of numbers (or average numbers) by hour
every.four.hrs = c("00:00", "04:00", "08:00", "12:00", "16:00", "20:00")
#ggplot(fig.numbers.by.hour, aes(x = rounded.hour, y = Counts)) + geom_bar(stat = "identity") # total counts

# total counts
ggplot(fig.numbers.by.hour, aes(x = rounded.hour, y = Numbers)) + geom_bar(stat = "identity") + mytheme_larger + 
  ylab("Total Number of Narwhals") + xlab("Time of Day") + scale_x_discrete(breaks=every.four.hrs) +
  annotate("text", x = Inf, y = Inf, label = "(A)", hjust = 1.25, vjust = 2, family = "serif", size = 14)

# Mean number of narwhals per count  
ggplot(fig.numbers.by.hour, aes(x = rounded.hour, y = Mean.number)) +
  geom_bar(stat = "identity") + mytheme_larger + ylab("Mean Number of Narwhals Per Count") + xlab("Time of Day") +
  scale_x_discrete(breaks=every.four.hrs) +
  annotate("text", x = Inf, y = Inf, label = "(B)", hjust = 1.25, vjust = 2, family = "serif", size = 14)

#====== +++ === === +++ === === +++ === ===
# Try boxplot of distribution of numbers by hour
#====== +++ === === +++ === === +++ === ===
# numbers.dist.by.hour = ddply(without.vessels.dat2014, "Count.id", summarise, 
#                             Numbers = sum(GroupSize, na.rm = TRUE), 
#                             Hour = unique(rounded.hour)
# ) 
# 
# write.csv(x = numbers.dist.by.hour, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check
# 
# ggplot(numbers.dist.by.hour, aes(x = as.factor(Hour), y = Numbers)) + geom_boxplot() + scale_x_discrete(breaks=every.four.hrs) + ylim(c(0,200))
# 
# ggplot(numbers.dist.by.hour, aes(x = as.factor(Hour), y = Numbers)) + geom_boxplot() + geom_jitter(alpha=0.75)# 
# ggplot(numbers.dist.by.hour, aes(x = as.factor(Hour), y = Numbers)) + geom_point(alpha=0.75)# 

#====== +++ === === +++ === === +++ === ===
# Try heatmap showing numbers in each strata for each count
#  TODO : Also want to condition this on only "G" and "Excellent" Sightability
#====== +++ === === +++ === === +++ === ===
heat.dat = subset(numbers.by.count.and.strata, select = -c(Numbers, DateTime)) # simplify data.frame (debugging)
heat.dat = melt(heat.dat, id = 'Count.id') # go from wide to long data.frame format -- use function melt()
head(heat.dat)
names(heat.dat) = c("Count.ID", "Stratum", "Numbers")
write.csv(x = heat.dat, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check

qplot(x = Count.ID, y = Stratum, data = heat.dat, fill = Numbers, geom = "raster", ylab = "Stratum") # Looks like a first step!!

# col.ramp.Function = colorRampPalette(c("black", "grey70")) # create a color palette
# palettesize = max(heat.dat$Numbers)
# col.ramp = col.ramp.Function(palettesize)

(heat.map.strata = ggplot(heat.dat, aes(x = Count.ID, y = Stratum, fill = Numbers)) + geom_tile())
heat.map.strata = heat.map.strata + scale_y_discrete(limits=rev(levels(heat.dat$Stratum))) # reverse y-axis to coincide with orientation of strata
heat.map.strata

# heat.map.strata = heat.map.strata + scale_fill_gradient2(low = col.ramp[1],
#                                                           mid = col.ramp[palettesize / 2], 
#                                                           high = col.ramp[palettesize],
#                                                           midpoint = (max(heat.dat$Numbers) + min(heat.dat$Numbers)) / 2, 
#                                                           name = "Numbers")
# heat.map.strata
# heat.map.strata = heat.map.strata + coord_cartesian(xlim = range(heat.dat$Count.ID))
# heat.map.strata

#====== +++ === === +++ === === +++ === ===
# Heatmap showing numbers in each strata for each count
#  Condition this on only "G" and "Excellent" Sightability
#====== +++ === === +++ === === +++ === ===
numbers.by.count.and.strata.include = ddply(dat2014.include, "Count.id", summarise, 
                                    Numbers = sum(GroupSize, na.rm = TRUE), 
                                    DateTime = unique(datetime),
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

write.csv(x = numbers.by.count.and.strata.include, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check

# Get data.frame above into format for plotting ggplot heat map
heat.dat = subset(numbers.by.count.and.strata.include, select = -c(Numbers)) # simplify data.frame (debugging)
heat.dat$Count.id2 = 1:nrow(heat.dat)
heat.dat.melted = melt(heat.dat, id = c('Count.id', 'Count.id2','DateTime')) # go from wide to long data.frame format -- use function melt()

names(heat.dat.melted) = c("Count.ID", "Count.ID2","DateTime","Stratum", "Numbers") 

(heat.map.strata = ggplot(heat.dat.melted, aes(x = Count.ID2, y = Stratum, fill = Numbers)) + geom_tile())
heat.map.strata = heat.map.strata + scale_y_discrete(limits=rev(levels(heat.dat.melted$Stratum))) # reverse y-axis to coincide with orientation of strata
heat.map.strata = heat.map.strata + scale_x_discrete(breaks = brks, labels = labls) + xlab("Date")
heat.map.strata

#====== +++ === === +++ === === +++ === ===
# Have a look at faceted scatterplots showing numbers in sub-stratum and tide.delta
#====== +++ === === +++ === === +++ === ===
# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
man.palette = c("Red", "Blue", "Green", "Orange")
# start working with 'included' data here
names(filtered.dat2014)
sightings.dat2014 = subset(filtered.dat2014, GroupSize > 0)

# This shows group size as a function of tidal flow (delta)
#qplot(x = delta, y = GroupSize, data = dat2014.include, facets = Stratum ~ SubStratum.num) # first draft with qplot
qplot(x = delta, y = GroupSize, data = sightings.dat2014, facets = Stratum ~ SubStratum.num) # first draft with qplot

# head(dat2014.include)
# g1 = ggplot(dat2014.include, aes(delta, GroupSize, colour = GroupSizeLevel)) # Recreate with full ggplot (easier to customize plots with ggplot than qplot)
# g1 = g1 + geom_point() + facet_grid(Stratum ~ SubStratum.num) # geom_smooth(method = "lm")
# g1 + scale_y_continuous(limits = c(0, 15)) + xlab("Tidal Rate") + ylab("Group Size")
# 
# g2 = ggplot(dat2014.include, aes(delta, GroupSize, shape = GroupSizeLevel)) # Recreate with full ggplot (easier to customize plots with ggplot than qplot)
# g2 = g2 + geom_point() + facet_grid(Stratum ~ SubStratum.num) # geom_smooth(method = "lm")
# g2 + scale_y_continuous(limits = c(0, 15)) + scale_shape_manual(values = c(19,4))
# 
# g2 = ggplot(dat2014, aes(x = delta, y = GroupSize, colour = Vessel.related.count)) # Recreate with full ggplot (easier to customize plots with ggplot than qplot)
# g2 + geom_point() + facet_grid(Stratum ~ SubStratum.num) 
# 
# # Plot tide elevation on x-axis instead of 'delta'
# gElev = ggplot(dat2014.include, aes(x = Elevation, y = GroupSize)) # Recreate with full ggplot (easier to customize plots with ggplot than qplot)
# gElev + geom_point() + facet_grid(Stratum ~ SubStratum.num) 

library(scales)
View(filtered.dat2014)
# Recreate with full ggplot (easier to customize plots with ggplot than qplot)
# g3 = ggplot(subset(dat2014.include, subset = !is.na(Direction)), aes(x = delta, y = GroupSize, colour = Direction)) 
g3 = ggplot(subset(filtered.dat2014, subset = !is.na(Direction)), aes(x = delta, y = GroupSize, colour = Direction)) 
g3 = g3 + facet_grid(Stratum ~ SubStratum.num) 
g3 = g3 + scale_colour_manual("Narwhal\nTravel\nDirection", values=man.palette, labels=c("East", "North", "South", "West")) 
g3 = g3 + xlab("Tide Flow") + ylab("Group Size") 
g3 = g3 + scale_y_continuous(limits = c(0, 15)) 
g3 = g3 + geom_jitter(size = 4, alpha = 0.75) 
g3 = g3 + mytheme
g3 + theme(legend.text = element_text(size = 20), legend.title = element_text(size = 20))
