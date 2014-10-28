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
library(plyr)
library(ggplot2)
# base.dir = "~/Documents/2014 Work/Milne Inlet Narwhals/2014 Analysis/Code"
# wrk.space = "MilneNarwhal.2014.RData"
# wrk.space = paste(base.dir, wrk.space, sep"/")
# load(wrk.space)
load("~/Documents/2014 Work/Milne Inlet Narwhals/2014 Analysis/Code/MilneNarwhal.2014.RData") # Load workspace 
setwd("~/Documents/2014 Work/Milne Inlet Narwhals/Data/2014") # Set working directory for data, really for outputting tables (data has already been read)

# Rounding 'datetime' (class POSIXct) to nearest hour
datetime.rounded.to.hr = dat2014$datetime # create a new column that will contain datetime rounded to the nearest hour 
datetime.rounded.to.hr = format(round(datetime.rounded.to.hr, units="hours"), format="%H:%M") # seems to work
str(datetime.rounded.to.hr) # returns a character string
dat2014$datetime.rounded.to.hr = datetime.rounded.to.hr # append the rounded hours to data.frame

write.csv(x = dat2014, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check

# Use ddply to bin (1) number of counts, and (2) narwhals by hourly blocks -- do without large vessel counts
without.vessels.dat2014 = subset(dat2014, (CountType != "PRE" & CountType != "C" & CountType != "POST"))
fig.numbers.by.hour = ddply(without.vessels.dat2014, "datetime.rounded.to.hr", summarise, 
                            Numbers = sum(GroupSize, na.rm = TRUE), 
                            Counts = length(unique(Count.id))
                            ) 

fig.numbers.by.hour$Mean.number = fig.numbers.by.hour$Numbers / fig.numbers.by.hour$Counts

write.csv(x = fig.numbers.by.hour, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check

# Bar graph of numbers (or average numbers) by hour
ggplot(fig.numbers.by.hour, aes(x = datetime.rounded.to.hr, y = Counts)) + geom_bar(stat = "identity") # total counts
ggplot(fig.numbers.by.hour, aes(x = datetime.rounded.to.hr, y = Mean.number)) + geom_bar(stat = "identity") # average number per count

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
#  Also want to condition this on only "G" and "Excellent" Sightability
#====== +++ === === +++ === === +++ === ===
numbers.by.count.and.strata = ddply(dat2014, "Count.id", summarise, 
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

write.csv(x = numbers.by.count.and.strata, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check

# TODO: get data.frame above into format for plotting ggplot heat map
# Think we're going to need to 'melt' this data.frame
heat.dat = subset(numbers.by.count.and.strata, select = -c(Numbers, DateTime)) # simplify data.frame (debugging)
heat.dat = melt(heat.dat, id = 'Count.id') # go from wide to long data.frame format
head(heat.dat)
write.csv(x = heat.dat, file = "foo.csv", row.names = FALSE); system("open foo.csv") # check

qplot(x = Count.id, y = variable, data = heat.dat, fill = value, geom = "raster") # Looks like a first step!!


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
