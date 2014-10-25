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

# Use ddply to bin number of narwhals by hourly blocks
ddply()


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
