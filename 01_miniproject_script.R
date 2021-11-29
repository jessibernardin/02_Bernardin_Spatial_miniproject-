
library(knitr)
library(rmarkdown)
library(bookdown)
library(knitcitations)
library(formatR)
library(devtools)
library(rgbif)
library(raster)
library(sf)
library(ggplot2)
library(tidyverse)
library(Rcpp)
library(terra)
library(dplyr)
setwd("~/Desktop/R_spatial_Fall 2021/miniproject/01_Bernardin_Spatial_miniproject")

#dependent variable, location of iNat purple pitcher plant observations from GBIF
pitcher <- read.csv("gbif_sarracenia.csv", sep = "\t")

#predictor variable, elevation data for the US
elevation <- read.table("Elevation.US.txt", header = TRUE,
                 sep = "|",
                 na.strings = "",
                 comment.char = "",
                 quote = "\"",
fill = FALSE)

#predictor variable, land use data for the US
landuse <- read.csv("MajorLandUse.csv", header = TRUE)

#using worldclim data instead
r <- getData("worldclim",var="bio",res=10)
temp.rast <- r[[1]]
names(temp.rast) <- "Temp"

precip.rast <- r[[12]]
names(precip.rast) <- "Prec"

plot(temp.rast)
plot(precip.rast)

#watersheds
#Projected CRS: Sphere_ARC_INFO_Lambert_Azimuthal_Equal_Area
watersheds <- st_read("NA_Watersheds_Shapefile/watershed_p_v2.shp")


#state boundaries
#NAD83
state <- st_read("us_state_20m/cb_2018_us_state_20m.shp")

#census data
#https://data.ers.usda.gov/reports.aspx?ID=17827

population <- read.csv("state_population.csv")

########### Making the Database ##############

#summarize county elevation to state ave elevation
state.elevation <- elevation %>% dplyr::group_by(STATE_ALPHA) %>%
  dplyr::summarize(mean_elevation = mean(ELEV_IN_M, na.rm = TRUE))

names(state.elevation)[1] <- "Code"

st.elev.pop <- left_join(population, state.elevation, by = "Code")

st.elev.pop <- distinct(st.elev.pop, state, .keep_all = TRUE)

#filter the land use data to only year 2007
landuse_07 <- filter(landuse, Year == "2007")
landuse_07 <- landuse_07 %>%
  dplyr::rename(state = Region.or.State)

#combine with other state data
state.df <- left_join(st.elev.pop, landuse_07, by = c("state"))

# check geometries for polygons
st_is_valid(state) # TRUE
st_make_valid(watersheds) # TRUE

#plot(st_geometry(state))
#plot(st_geometry(watersheds))

#check crs
st_crs(state) == st_crs(watersheds) #FALSE
#reproject
state <- state %>% 
  st_transform(., crs = st_crs(watersheds))

#recheck
st_crs(state) == st_crs(watersheds) #TRUE

#Bind state sf with the state tabular data

class(state.df$Total.cropland)
class(state.df$Total.land)
state.df$Total.cropland <- as.integer((state.df$Total.cropland))
state.df %>%  dplyr::mutate(., prop_crop = Total.cropland / Total.land)
state.df.sf <- dplyr::left_join(state, state.df, by = c("STUSPS" = "Code"))

#make pitcher data a shape file

pitcher.sf <- st_as_sf(pitcher, coords = c("decimalLongitude", "decimalLatitude"), crs = "EPSG:4326")

plot(st_geometry(pitcher.sf))
pitcher.sf.t <- st_transform(pitcher.sf, crs = st_crs(r))

plot(temp.rast)
plot(st_geometry(pitcher.sf.t),add=T, pch = 20, cex = .2)

#reproject pitcher.sf
st_crs(state) == st_crs(pitcher.sf)

pitcher.sf <- pitcher.sf %>% 
  st_transform(., crs = st_crs(watersheds))

st_crs(watersheds) == st_crs(pitcher.sf) #TRUE

#pitcher.sf point geometry
#watershed sf polygons
#state.df.sf state info (population, elevation, land use, state polygons)

#get all the data to the raster crs
state.df.sf <- state.df.sf %>% 
  st_transform(., crs = st_crs(temp.rast))

st_crs(temp.rast) == st_crs(state.df.sf)

st_crs(temp.rast) == st_crs(watersheds)
watersheds <- watersheds %>% 
  st_transform(., crs = st_crs(temp.rast))
st_crs(temp.rast) == st_crs(watersheds)


st_crs(temp.rast) == st_crs(pitcher.sf)
pitcher.sf <- pitcher.sf %>% 
  st_transform(., crs = st_crs(temp.rast))
st_crs(temp.rast) == st_crs(pitcher.sf)


#raster extract
#temp and precip
temp.extract <- terra::extract(temp.rast, pitcher.sf, fun = mean, na.rm=TRUE)
pitcher.sf$temp <- temp.extract
precip.extract <- extract(precip.rast, pitcher.sf, fun = mean, na.rm=TRUE)
pitcher.sf$precip <- precip.extract


#temp.wshd <- terra::extract(temp.rast, watersheds, fun = mean, na.rm=TRUE)
#watersheds$temp <- temp.wshd
#precip.wshd <- extract(precip.rast, watersheds, fun = mean, na.rm=TRUE)
#watersheds$precip <- precip.wshd

#Adding the extracted raster data back to the main dataset
#state.df.sf (has state polygons, pop, elev., landuse data)
#watershed (watershed polygons)
#pitcher.sf (pitcher points, temp data, precip data from rasters)


watersheds$pt_count <- lengths(st_intersects(watersheds, pitcher.sf))
watersheds.f <- watersheds %>%
  filter_at(vars(starts_with("pt_count")), any_vars(. > 0))
