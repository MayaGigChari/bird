
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

current_directory  = getwd()
tree_trimming_path<-file.path(current_directory, "App_functions_bird.R")

source(tree_trimming_path)

california_true = FROM_GeoJson("Cali_Geometry/CA_hexbinned@6.json")
#pulled from github
#need to get the latlong information. 


california <- sf::read_sf("Cali_Geometry/ca-state-boundary/CA_State_TIGER2016.shp") %>%
  sf::st_transform(4326)

h3_indexes <- polyfill(california[1, ], res = 6)

#get the indexes

#this just gives a list of lists of latitude and longitude coordiantes
geog_boundaries <- h3_to_geo_boundary(h3_indexes) 

#this gives an sf file with a POLYGON wkt field 
polygons <- h3_to_geo_boundary_sf(h3_indexes)

#create a list the size of the number of polygons 


#do not run this again. This is just for birds here. Takes several hours
#function that takes in the scientific identifier (ex: "Aves") and outputs taxa list for each pixel as specified above. 

popPixel<- function(scientific_identifier, len = length(geog_boundaries), nms = h3_indexes)
{
  occurrence_df <-list(len)
  for(i in 10497: length(geog_boundaries))
  {
    coordinate_data_polygon = st_as_text(polygons[[2]][[i]])
    birds_temp_polygon = occ_data(geometry = coordinate_data_polygon, scientificName = scientific_identifier, hasGeospatialIssue = "False")
    species_data<-birds_temp_polygon$data$species
    species_data<- read_df(species_data)
    occurrence_df[i]<- species_data
    print(i)
  }
  names(occurrence_df)<- nms
  return(occurrence_df)
  
}


#here I write the RDS file to Cali_Geometry folder 
#also attempt to read the same file to make sure it wokrs. 
saveRDS(occurrence_df, "Cali_Geometry/occurrence_birds_polygons.rds")
cali_birds<- readRDS("Cali_Geometry/occurrence_birds_polygons.rds")




#### NOT USEFUL
#rationale through the development of the above function.
#example: walking through the steps for one polygon
#can view each geometry with this. now I will try to retrieve a species list with one of these.  
polygon_text = st_as_text(polygons[[2]][[1]])
#will attempt here to use one polygon to get gbif occurrence data. 
polygon_temp = writeLines(polygon_text, "Cali_Geometry/temp_polygon.wkt")
#created a wkt file with one polygon. 
polygon_temp_data = readLines("Cali_Geometry/temp_polygon.wkt")
#may potentially have an issue with filtering by name = Aves. 
#TODO: do we want to preserve all observational geographic information so that we can throw it on the site? 
birds_temp_polygon = occ_data(geometry = polygon_temp_data, scientificName = "Aves", hasGeospatialIssue = "False")
species_data<-birds_temp_polygon$data$species
write.table(species_data, file = "Cali_Geometry/species_data_temp.txt")


#need to convert these boundaries to "well known text" format. 

?occ_data
?h3_to_geo_boundary

#used california shape file to make polygons. 

?polyfill
?st_read
