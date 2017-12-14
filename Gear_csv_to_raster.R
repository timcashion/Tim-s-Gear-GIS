# ----------------
# Tim's Gear maps
# Convert CSV data to rasters (and maybe also shapefiles if u want)
# CSV data contain industrial allocated catch by geartype per cell. Each CSV contains all gears and catch aggregated into 5 year groups.
# I've manually used Excel's vlookup to match up cellID to x, y coordinates, but in the future this can be done in R as well. 
# This code disaggregates each CSV by geartype into a raster map for each geartype & time period.
# This Datacarpentry workshop is overall super helpful: http://www.datacarpentry.org/R-spatial-raster-vector-lesson/ 
# ----------------

# ----------------
# 01 INITIAL SETUP
# ----------------

# For first timers:
# install.packages("raster")
# install.packages("sf")
# install.packages("rgdal")

# Setup
library(raster) # raster work
library(sf) # shapefile work
library(stringr) # replace spaces w "_". Used in file naming.
setwd('C:/Users/spopov/Documents/GIS/Tim/Gear maps')

# ----------------
# 02 CSV READ AND DATAFRAME CREATION
# ----------------

# CSV read
# 3 files: 
#   1. 1950-1954
#   2. 1980-1984
#   3. 2010-2014

c1950 <- read.csv('Data/IndustrialAllocatedGear_1950_1954_Aggregated.csv')
c1980 <- read.csv('Data/IndustrialAllocatedGear_1980_1984_Aggregated.csv')
c2010 <- read.csv('Data/IndustrialAllocatedGear_2010_2014_Aggregated.csv')

# Create list of different gear types, using 2010-2014 data. This works bc we have same gears in all data. Ideally would want to loop through all CSVs and get unique values from all of them. 
geartypes <- unique(c2010$gear_type)

# Disaggregate gear types
# USING A FANCY FOR LOOP!!

# 1950-1954
df1950_list <- list() # create empty list. This will be filled with the df names we create in the following forloop. Later, we can use this list to call each df when we create our rasters. 
for (gear in geartypes) {
  print(gear)
  dfname <- paste(gear, 1950) # create data frame names for 1950s data in the form: "<geartype> 1950"
  dfname <- str_replace_all(dfname,"[[:punct:]\\s]+","_") # replace spaces w "_" (*1)
  df1950_list[[dfname]] <- assign(paste(dfname), data.frame(c1950[c1950$gear_type == gear,])) # create and assign these new dataframes the above created dataframe names (dfname), then populate it with rows that contain the correct (gear). THEN, chuck all these dataframes into one 1950s list. 
}

# OMGAD YASSS I CAN'T BELIEVE THAT WORKED

# 1980-1984 
df1980_list <- list()
for (gear in geartypes) {
  print(gear)
  dfname <- paste(gear, 1980) 
  dfname <- str_replace_all(dfname,"[[:punct:]\\s]+","_") # replace spaces w "_" (*1)
  df1980_list[[dfname]] <- assign(paste(dfname), data.frame(c1980[c1980$gear_type == gear,]))
}

# 2010-2014
df2010_list <- list()
for (gear in geartypes) {
  print(gear)
  dfname <- paste(gear, 2010) 
  dfname <- str_replace_all(dfname,"[[:punct:]\\s]+","_") # replace spaces w "_" (*1)
  df2010_list[[dfname]] <- assign(paste(dfname), data.frame(c2010[c2010$gear_type == gear,]))
}

# ----------------
# 03 RASTERIZE THE DATA
# ----------------
# 
# The "raster" libary has a really easy little function called "rasterFromXYZ" which will convert any dataframe in the format [x-coord,y-coord,value] to a nice raster grid. 
# First, create 'xyz' df by pulling the x, y, and Catch columns from each dataframe created in the above for loops. 
# Then, create raster plots for each. (*2) 
# Chuck it all in a for loop so it does this for each df we created above. 
# We'll use each "df<year>_list" to tell the for loop which df to create a raster for.  

# Create directories by geartype 
#for (gear in geartypes) {
#  setwd('C:/Users/spopov/Documents/GIS/Tim/Gear maps/Rasters/R TEST')
#  geardir <- paste(gear)
#  dir.create(geardir)
#}

# Manually created 1950, 1980, 2010 directories for this. Everything below this essentially could be MUCH better. 

# 1950-1954 rasters
setwd('./1950-1954/')
names1950 <- names(df1950_list)
for (i in (1:length(df1950_list))){
  df <- df1950_list[[i]]
  filename <- paste0(names1950[[i]])
  xyz <- df[,c('x','y','Catch')]
  r <- rasterFromXYZ(xyz,crs="+proj=utm +ellps=WGS84 +datum=WGS84 +units=m +no_defs", digits=2)
  writeRaster(r, filename, format = "GTiff")
}

# 1980-1984 rasters
setwd('../1980-1984/')
names1950 <- names(df1950_list)
for (i in (1:length(df1950_list))){
  df <- df1950_list[[i]]
  filename <- paste0(names1950[[i]])
  xyz <- df[,c('x','y','Catch')]
  r <- rasterFromXYZ(xyz,crs="+proj=utm +ellps=WGS84 +datum=WGS84 +units=m +no_defs", digits=2)
  writeRaster(r, filename, format = "GTiff")
}

# 2010-2014 rasters
setwd('../2010-2014/')
names2010 <- names(df2010_list)
for (i in (1:length(df2010_list))){
  df <- df2010_list[[i]]
  filename <- paste0(names2010[[i]])
  xyz <- df[,c('x','y','Catch')]
  r <- rasterFromXYZ(xyz,crs="+proj=utm +ellps=WGS84 +datum=WGS84 +units=m +no_defs", digits=5)
  writeRaster(r, filename, format = "GTiff")
}

setwd('..')

# ---------------
# 04 FIGURE PLOTTING
# ---------------

# Create beautiful figs(*3) in R instead of messing around in QGIS! 
# Later: Somehow create separate color scales for each unique "gear" automatically??

theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Ubuntu Regular", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      # panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.border = element_blank(),
      ...
    )
}


# TESTING FOR GITHUB.

# ----------------
# FOOTNOTES
# ----------------

# *1. https://stackoverflow.com/questions/13985215/replace-special-characters-along-with-the-space-in-list-of-strings
# *2. https://www.rdocumentation.org/packages/raster/versions/2.6-7/topics/rasterFromXYZ 
# *3. https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/ 