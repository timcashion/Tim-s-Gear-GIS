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

# Setup

# Note this part only works if you are in RStudio. If not using RStudio, set wd manually to wherever this script is stored.
# Set working directory to directory this R script is stored in.
set_wd <- function() {
  if (!require(rstudioapi)) {
    install.packages("rstudioapi", repos = "http://cran.stat.sfu.ca/")
    require(rstudioapi)
  }
  current_path <- getActiveDocumentContext()$path 
  setwd(dirname(current_path))
  print(getwd())
}

set_wd()


# Install and load all other necessary packages
if (!require(rgeos)) {
  install.packages("rgeos", repos = "http://cran.stat.sfu.ca/")
  require(rgeos)
}
if (!require(rgdal)) {
  install.packages("rgdal", repos = "http://cran.stat.sfu.ca/")
  require(rgdal)
}
if (!require(raster)) {
  install.packages("raster", repos = "http://cran.stat.sfu.ca/")
  require(raster)
}
if (!require(sf)) {
  install.packages("sf", repos = "http://cran.stat.sfu.ca/")
  require(sf)
}
if (!require(maps)) {
  install.packages("maps", repos = "http://cran.stat.sfu.ca/")
  require(maps)
}
if (!require(rasterVis)) {
  install.packages("rasterVis", repos = "http://cran.stat.sfu.ca/")
  require(rasterVis)
}
if (!require(stringr)) {
  install.packages("stringr", repos = "http://cran.stat.sfu.ca/")
  require(stringr)
}
if (!require(ggplot2)) {
  install.packages("ggplot2", repos = "http://cran.stat.sfu.ca/")
  require(ggplot2)
}
if(!require(viridis)) {
  install.packages("viridis", repos="http://cran.stat.sfu.ca/")
  require(viridis)
}
# WARNING: extrafont is great for making pretty plots with nice fonts, but can take awhile to download and load up all the fonts the first time you install & load it. 
# if (!require(extrafont)) {
#  install.packages("extrafont", repos = "http://cran.stat.sfu.ca/")
#  require(extrafont)
# } 

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

# Create list of different gear types, using 2010-2014 data. This works bc we have same gears in all data. Ideally would want to loop through all CSVs and get unique values from all of them
geartypes <- unique(c2010$gear_type)

# Disaggregate gear types
# USING A FANCY FOR LOOP!!

# 1950-1954
df1950_list <- list() # create empty list. This will be filled with the df names we create in the following forloop. Later, we can use this list to call each df when we create our rasters. 
for (gear in geartypes) {
  print(gear)
  dfname <- paste(gear, "1950-1954") # create data frame names for 1950s data in the form: "<geartype> 1950"
  dfname <- str_replace_all(dfname,"[\\s]+","_") # replace spaces w "_" (*1)
  df1950_list[[dfname]] <- assign(paste(dfname), data.frame(c1950[c1950$gear_type == gear,])) # create and assign these new dataframes the above created dataframe names (dfname), then populate it with rows that contain the correct (gear). THEN, chuck all these dataframes into one 1950s list. 
}

# OMGAD YASSS I CAN'T BELIEVE THAT WORKED

# 1980-1984 
df1980_list <- list()
for (gear in geartypes) {
  print(gear)
  dfname <- paste(gear, "1980-1984") 
  dfname <- str_replace_all(dfname,"[\\s]+","_") # replace spaces w "_" (*1)
  df1980_list[[dfname]] <- assign(paste(dfname), data.frame(c1980[c1980$gear_type == gear,]))
}

# 2010-2014
df2010_list <- list()
for (gear in geartypes) {
  print(gear)
  dfname <- paste(gear, "2010-2014") 
  dfname <- str_replace_all(dfname,"[\\s]+","_") # replace spaces w "_" (*1)
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

# Manually created 1950, 1980, 2010 directories within the "Rasters" directory for this. Everything below this essentially could be MUCH better. 

# 1950-1954 rasters
setwd('./Rasters/1950-1954/')
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
names1980 <- names(df1980_list)
for (i in (1:length(df1980_list))){
  df <- df1980_list[[i]]
  filename <- paste0(names1980[[i]])
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

setwd('..') # Back to "Rasters" directory.

# ---------------
# 04 FIGURE PLOTTING
# ---------------

# Create beautiful figs(*3) in R instead of messing around in QGIS! 
# Later: Somehow create separate color scales for each unique "gear" automatically??

# Going to use gillnets from 2010 to test out pretty plots. 
xyz <- `gillnets_2010-2014`[,c('x', 'y', 'Catch')]
r_gn <- rasterFromXYZ(xyz,crs="+proj=utm +ellps=WGS84 +datum=WGS84 +units=m +no_defs", digits=5)
r_gn_spdf <- rasterToPoints(r_gn) # convert raster to points
r_gn_df <- as.data.frame(r_gn_spdf) # convert points to dataframe. ggplot can only plot vector based graphics, not rasters.
log_catch <- r_gn_df # Make dataframe for the log of catch
log_catch$Catch <- log(log_catch$Catch) # Take log of Catch
# ULTIMATELY, CAN PROBABLY JUST READ THE CSVs AND SKIP THE RASTERIZING STEPS ALTOGETHER


land <- map_data("world") # note: if we want Pacific ocean centered, wrap to 23 & 383 longitudes.

theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Open Sans", color = "#22211d"),
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

# Define color scale
pal <- scale_fill_viridis(
  option = "viridis",
  name = "log(Average catch (tonnes))",
  guide = guide_colorbar(
    direction = "horizontal",
    barheight = unit(2, units = "mm"),
    barwidth = unit(50, units = "mm"),
    draw.ulin = F,
    title.position = "top",
    title.hjust = 0.5,
    label.hjust = 0.5))

pal <- scale_fill_viridis(
  option = "viridis",
  name = "log(Average catch (tonnes))",
  guide = guide_colorbar(
    direction = "horizontal",
    barheight = unit(2, units = "mm"),
    barwidth = unit(50, units = "mm"),
    draw.ulin = F,
    title.position = "top",
    title.hjust = 0.5,
    label.hjust = 0.5))

# TEST USING LEVELPLOT
# levelplot (r_gn,
#           col.regions=colr)
#  + layer(sp.polygons(land))

# TEST USING GGPLOT
p <- ggplot() +
  # Land polygons
  geom_polygon(data = land, 
               aes(
                 x = long, 
                 y = lat,
                 group = group
                 )) +
  # Gillnets raster 
  geom_raster(data = log_catch,
                aes(
                  x = x,
                  y = y,
                  fill=Catch),
              interpolate = TRUE) +
  # Previously defined theme
  theme_map() +
  # Catch color scale
  pal +
  # Labels
  labs(title = "Industrial catch by gillnets",
       subtitle = "log(Average catch, 2010-2014)",
       caption = "Data: Sea Around Us") +
  # Legend
  theme(legend.position = "bottom")

# # -- TESTING GGALT AND COORD_PROJ FUNCTION
# # Install devtools in order to install a github brance of 'ggalt' from someone
# # NOTE: if you have not installed devtools before, you will need to RESTART R STUDIO for the install_github line to work!
# if(!require(devtools)) {
#   install.packages("devtools", repos="http://cran.stat.sfu.ca/")
#   require(devtools)
# }
# # Install branch of ggalt from rplzzz. NOTE: need to RESTART R STUDIO if this first time installing devtools.
# install_github("rplzzz/ggalt", ref='ggp221')
# library(ggalt)
# 
# # ggalt will now let us use 'coord_proj' which means we can change the projection of our plot wihtout actually having to mess around with the projection of the spatial data itself.
# p <- p + coord_proj("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

plot(p)

# See histogram of the raster values
hist <- hist(r_gn)

# ----------------
# FOOTNOTES
# ----------------

# *1. https://stackoverflow.com/questions/13985215/replace-special-characters-along-with-the-space-in-list-of-strings
# *2. https://www.rdocumentation.org/packages/raster/versions/2.6-7/topics/rasterFromXYZ 
# *3. https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/ 