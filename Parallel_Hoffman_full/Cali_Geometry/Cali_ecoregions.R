#this is a script for processing the cali ecoregion data into shape files. 

library(sf)

ecoregion_shapefile <- st_read("Cali_geometry/ca_eco_l3/ca_eco_l3.shp") #this is an sf object of all the geometries in california. 

#can join the ecoregion shapefiles by the california geometries/ ranges. 