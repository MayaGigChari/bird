
remotes::install_github("crazycapivara/h3-r")
install.packages("rjson")
install.packages("geojsonR")
install.packages("rgbif")



#step 1: convert california shapefile into geojson 

library(h3)
library(rjson)
library(geojsonR)
library(sf)
library(rgbif)


#need to get the latlong information. 
california <- sf::read_sf("ca-state-boundary/CA_State_TIGER2016.shp") %>%
  sf::st_transform(4326)

h3_indexes <- polyfill(california[1, ], res = 6)
#get the indexes

geog_boundaries<- h3_to_geo_boundary(h3_indexes, format_as_geojson = TRUE) #check to make sure these match. 


#need to populate some kind of table. 


#step 2: need to query the GBIF database for each hexagon. 

#need to convert these boundaries to "well known text" format. 


?occ_data
?h3_to_geo_boundary


#used california shape file to make polygons. 

?polyfill
?st_read
