#this is a script to extract all hexagonal data from each UC reserve. 

library(dplyr)
library(h3)
library(sf)
california <- sf::read_sf("Cali_Geometry/ca-state-boundary/CA_State_TIGER2016.shp") %>%
  sf::st_transform(4326)

h3_indexes <- polyfill(california[1, ], res = 6)


#list of uc reserve files. 
files_uc_reserves <- list.dirs(path="Cali_Geometry/uc_reserves_geog", full.names=TRUE, recursive=FALSE)


uc_reserve_shapefiles_list<- list()
for (i in files_uc_reserves) 
{
  current_length<- length(uc_reserve_shapefiles_list)
  uc_reserve_shapefiles_list <- append(uc_reserve_shapefiles_list, list.files(i, pattern="(?i)Boundary.*\\.shp$", full.names=TRUE)) # (?i) makes the pattern case insensitive
  if(current_length == length(uc_reserve_shapefiles_list))
  {
    uc_reserve_shapefiles_list <- append(uc_reserve_shapefiles_list, list.files(i, pattern="(?i)(.shp)$", full.names = TRUE)) # Also making this pattern case insensitive
  }
}

#uc_reserve_shapefiles_list<- unlist(uc_reserve_shapefiles_list)

#this creates a list of lists of geometries of the different shape files. 
#saves the geometries of all the shape files 

uc_reserve_shapefiles_list_sf<- lapply(uc_reserve_shapefiles_list, sf::read_sf)
uc_reserve_shapefiles_list_sf <- lapply(uc_reserve_shapefiles_list_sf, function(x) {
  sf_transformed <- sf::st_transform(x, 4326)  # Transform each sf object
  return(sf_transformed)  # Return the transformed sf object
})

names(uc_reserve_shapefiles_list_sf)<- unlist(uc_reserve_shapefiles_list)
saveRDS(uc_reserve_shapefiles_list_sf, file = "Cali_Geometry/uc_reserve_shapefiles_sf_objects")
#this contains all the filenames for the shape files. 
#can use this to populate bird PD/MPD/MNTD data in uc_reserves_main file. 

#the resolution of 6 is not granular enough. For the purposes of this project it is probably better to just intersect with the original polygons than to assign hex values. 


#testing various resolutions to fill in the UC reserves. 10 is the most useful, although it is extremely granular. 
#resolution of 10 does not work for very large reserves. will revisit this. below is for borrego, produces way too large of a dataset. 
#this I will come back to 
reserve_test<-  sf::read_sf(uc_reserve_shapefiles_list[30]) %>%
  sf::st_transform(4326)

plot(reserve_test)

reserve_test<- st_make_valid(reserve_test)

h3_indexes <- polyfill(reserve_test, res = 10)

h3_boundaries<- h3_to_geo_boundary_sf(h3_indexes)
plot(h3_boundaries)
