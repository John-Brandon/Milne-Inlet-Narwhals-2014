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
#  1) Your main directory will differ. See 'base.dir' variable below to edit that path to match your system
#  2) 
#  3) These data are also known as "Relative Abundance and Distribution" (RAD) data 
#  4) Loads existing workspace, that contains 2014 data (dat2014) and some tables created in 'MilneNarwhal-Table2014Data.R' script
#====== +++ === === +++ === === +++ === ===
library(plyr) # for working with data
library(mgcv) # provides capability to plot smoothed functions 
library(ggplot2) # for plotting
library(RColorBrewer) # for creating color palettes

# base.dir = "~/Documents/2014 Work/Milne Inlet Narwhals/2014 Analysis/Code"
# wrk.space = "MilneNarwhal.2014.RData"
# wrk.space = paste(base.dir, wrk.space, sep"/")
# load(wrk.space)
load("~/Documents/2014 Work/Milne Inlet Narwhals/2014 Analysis/Code/MilneNarwhal.2014.RData") # Load workspace 
setwd("~/Documents/2014 Work/Milne Inlet Narwhals/Data/2014") # Set working directory for data, really for outputting tables (data has already been read)

# Initialize some plottig parameters -- create custom plotting theme for ggplot2
mytheme = theme_grey() + theme(axis.title.x = element_text(size = rel(1.75), vjust = 0.0), 
                               axis.title.y = element_text(size = rel(1.75), vjust = 1.0), axis.text = element_text(size = rel(1.5), colour = "black"),
                               plot.title = element_text(size = rel(2.5))) # , plot.margin=unit(c(1,1,1,1),"cm")


#====== +++ === === +++ === === +++ === ===
# Histogram of total counts by strata 
#  Note: tot.counts.strat are for all sighting conditions AND include periods when large vessels were associated with counts
#====== +++ === === +++ === === +++ === ===
ggplot(tot.counts.strat, aes(x = Stratum, y = TotalCount)) + geom_bar(stat = "identity") + ylab("Number of narwhals") + mytheme

#====== +++ === === +++ === === +++ === ===
# Histogram of group sizes in sub-strata
#====== +++ === === +++ === === +++ === ===
g = ggplot(data = dat2014, aes(x = GroupSize)) + geom_histogram(fill = "gray40", colour = "black", binwidth = 1, na.rm = TRUE)
g = g + facet_grid(Stratum ~ SubStratum.num)
g + xlim(c(0, 7)) 

#====== +++ === === +++ === === +++ === ===
# Histogram of group sizes in Strata
#====== +++ === === +++ === === +++ === ===
g = ggplot(data = dat2014, aes(x = GroupSize)) + geom_histogram(fill = "gray40", colour = "black", binwidth = 1, na.rm = TRUE)
g = g + facet_grid(Stratum ~ .)
g + xlim(c(0, 5)) 

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

fig.numbers.by.hour$Mean.number = fig.numbers.by.hour$Numbers / fig.numbers.by.hour$Counts

write.csv(x = fig.numbers.by.hour, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check

# Bar graph of numbers (or average numbers) by hour
every.four.hrs = c("00:00", "04:00", "08:00", "12:00", "16:00", "20:00")
#ggplot(fig.numbers.by.hour, aes(x = rounded.hour, y = Counts)) + geom_bar(stat = "identity") # total counts

# total counts
ggplot(fig.numbers.by.hour, aes(x = rounded.hour, y = Counts)) + geom_bar(stat = "identity") + mytheme + 
  ylab("Number of narwhals") + xlab("Time of Day") + scale_x_discrete(breaks=every.four.hrs)

# Mean number of narwhals per count  
ggplot(fig.numbers.by.hour, aes(x = rounded.hour, y = Mean.number)) +
  geom_bar(stat = "identity") + mytheme + ylab("Mean number of narwhals per count") + xlab("Time of Day") +
  scale_x_discrete(breaks=every.four.hrs)


#====== +++ === === +++ === === +++ === ===
# Try boxplot of distribution of numbers by hour
#====== +++ === === +++ === === +++ === ===
numbers.dist.by.hour = ddply(without.vessels.dat2014, "Count.id", summarise, 
                            Numbers = sum(GroupSize, na.rm = TRUE), 
                            Hour = unique(datetime.rounded.to.hr)
) 

write.csv(x = numbers.dist.by.hour, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check

ggplot(numbers.dist.by.hour, aes(x = as.factor(Hour), y = Numbers)) + geom_boxplot() + geom_jitter(alpha=0.75)# 
ggplot(numbers.dist.by.hour, aes(x = as.factor(Hour), y = Numbers)) + geom_point(alpha=0.75)# 

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

# brks = seq(1, 151, by = 50)
# labls = numbers.by.count.and.strata$DateTime[brks] # Want to strip out Month and Day , ie. "14 Aug" for tick marks on x-axis
# labls = format(labls, format = "%d %b") # see Crawley's book p. 92 for full list of formats
# 
# (heat.map.strata = ggplot(heat.dat.melted, aes(x = Count.ID, y = Stratum, fill = Numbers)) + geom_tile())
# heat.map.strata = heat.map.strata + scale_y_discrete(limits=rev(levels(heat.dat.melted$Stratum))) # reverse y-axis to coincide with orientation of strata
# heat.map.strata = heat.map.strata + scale_x_discrete(breaks = brks, labels = labls) + xlab("Date")
# heat.map.strata 

#====== +++ === === +++ === === +++ === ===
# Have a look at faceted scatterplots showing numbers in sub-stratum and tide.delta
#====== +++ === === +++ === === +++ === ===
# start working with 'included' data here
names(dat2014.include)

# This shows group size as a function of tidal flow (delta)
qplot(x = delta, y = GroupSize, data = dat2014.include, facets = Stratum ~ SubStratum.num) # first draft with qplot

head(dat2014.include)
g = ggplot(dat2014.include, aes(delta, GroupSize, colour = GroupSizeLevel)) # Recreate with full ggplot (easier to customize plots with ggplot than qplot)
g = g + geom_point() + facet_grid(Stratum ~ SubStratum.num) # geom_smooth(method = "lm")
g + scale_y_continuous(limits = c(0, 15))

g = ggplot(dat2014.include, aes(delta, GroupSize, shape = GroupSizeLevel)) # Recreate with full ggplot (easier to customize plots with ggplot than qplot)
g = g + geom_point() + facet_grid(Stratum ~ SubStratum.num) # geom_smooth(method = "lm")
g + scale_y_continuous(limits = c(0, 15)) + scale_shape_manual(values = c(19,4))


g = ggplot(dat2014.include, aes(x = delta, y = GroupSize, colour = Vessel.related.count)) # Recreate with full ggplot (easier to customize plots with ggplot than qplot)
g + geom_point() + facet_grid(Stratum ~ SubStratum.num) 

# Plot tide elevation on x-axis instead of 'delta'
gElev = ggplot(dat2014.include, aes(x = Elevation, y = GroupSize)) # Recreate with full ggplot (easier to customize plots with ggplot than qplot)
gElev + geom_point() + facet_grid(Stratum ~ SubStratum.num) 

g = ggplot(dat2014.include, aes(x = delta, y = GroupSize, colour = Direction)) # Recreate with full ggplot (easier to customize plots with ggplot than qplot)
g + geom_point() + facet_grid(Stratum ~ SubStratum.num) + scale_colour_brewer(palette = "Set1")

unique(dat2014.include$Direction)
unique(dat2014.include$Vessel.related.count)

#====== +++ === === +++ === === +++ === ===
# TODO jbrandon : Show total numbers per count 
head(numbers.by.count.and.strata)
head(numbers.by.count.and.strata.include)
write.csv(x = dat2014, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check


#====== +++ === === +++ === === +++ === ===
# First Draft at Map 
#====== +++ === === +++ === === +++ === ===
setwd("~/Documents/2014 Work/Milne Inlet Narwhals/Data")
load("CAN_adm0.RData")
can.map.dat = gadm; rm(gadm)
can.map.dat = fortify(can.map.dat) # this will take awhile, maybe a cup of coffee (or two) long time
autoplot(can.map.dat)
ggplot(can.map.dat, aes(x = long, y = lat)) + geom_path()

library(maps)
library(ggplot)
canada.map = map_data("world", region = "Canada")
ggplot(canada.map, aes(x=long, y = lat, group = group, fill = region)) + coord_map("mercator") + geom_polygon(colour = "black")
ggplot(canada.map, aes(x=long, y = lat, group = group, fill = region)) + coord_map("mercator") + geom_path()

# TRY MAP WITH OPENSTREETMAP IN R
install.packages("OpenStreetMap")
library(OpenStreetMap)
map <- openmap(upperLeft = c(lat = 72.17, lon = -81),
               lowerRight = c(lat = 71.83, lon = -80.33),
               minNumTiles=4,type="bing")
plot(map)
autoplot(map)

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
