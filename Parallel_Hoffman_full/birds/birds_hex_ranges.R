##This file is located in birds. 
#This file takes the hexagonal information from the h3 library and the california shape file
#from Cali_goemetry, and parses california into indexes
#TODO in bird_ranges: change ecoregions to wgs84 instead of NAD 83!!!
#this is a script for getting species lists for all the hexagons within california and within ecoregions. 

#the geometry asociated with indexes are then used to creates species list and trees for birds based on pre-defined range
#data from range_gen. 
library(h3)
library(sf)
library(dplyr)
library(rlist)

#generate geometry for california 
california <- sf::read_sf("Cali_Geometry/ca-state-boundary/CA_State_TIGER2016.shp") %>%
  sf::st_transform(4326)
h3_indexes <- polyfill(california[1, ], res = 6)
polygons <- h3_to_geo_boundary_sf(h3_indexes)

#get geometry for ecoregions 
ecoregions<- st_read("Cali_geometry/ca_eco_l3", packlage = "sf")
ecoregions<- st_transform(ecoregions, "WGS84")
ecoregions_L3codes<- ecoregions$US_L3CODE


#load the bird range data. 
#make sure everything is valid and force validity. 
bird_ranges_touse<- st_read("birds/birds_consolidated_geometries_0507.shp")
sf_use_s2(FALSE)
bird_ranges_touse<- st_make_valid(bird_ranges_touse)

#need to merge everythign with hex data. 

bird_ranges_per_hex<- st_join(polygons, bird_ranges_touse)


#load data for intersection: make a function to do this. 

#this is a function that takes the range data and the hexagonal data and writes an rds file for the corresponding geographic area 
#will make another function that takes rds list data and makes trees out of it. 
#need to check abbreviations. 
#will transfer genHexRaw to app_functions so that this can be cleaner. 

#this also needs to be edited. TODO next.
genHexRaw<- function(range_data, sf_polygons) #the indexes will be h3_indexes usually 
{
  #want to select just the species and the index. But can do this later. 
  #should always have an h3_index field. This is necessary for the filter statement 
  #joining the species that could exist in that area with the polygons in that area! I think this might work... 
  hex_species<- list()
  for(i in sf_polygons$h3_index)
  {
    poly_temp_db<- range_data %>%
      filter(h3_index == i)
    poly_species<- unique(poly_temp_db$sci_nam)
    hex_species[i] <- list(poly_species)
    print(i)
  }
  print("hexRaw done")
  return(hex_species)
}

#for california birds. 
#hopefully this is now right. 
cali_birds_complete_hex_list<- genHexRaw(bird_ranges_per_hex, polygons)

names(cali_birds_complete_hex_list)<- h3_indexes

saveRDS(cali_birds_complete_hex_list, file = "birds/occurrence_birds_polygons_fromRangeData_0507")

#this gives us polygon data for birds for overlappign ranges intersecting with each polygon. 



#THIS IS OBSOLETE
#ecoregion 9 is complete but may be different.
#these seem to be incorrect. 
#the ecoregion_range_temp is NOT the actual range of that particular ecoregion! that's why this doesn't freaking work
#TODO: check all polyfills!
#TODO: compare the output of this to the output where all california ranges are used. should be the same!
for(i in 1: length(dir_list_ecoregions))
{
  #each hexagon has its set species list! Don't need to keep mf recalculating it! 
  #ecoregion_range_temp<- st_read(paste(dir_list_ecoregions[i], "/ecoregion.shp", sep = ""))
  #sf_use_s2(FALSE) #this may not be necessary. should probably run a check first. 
  #ecoregion_range_temp<- st_make_valid(ecoregion_range_temp)
  #filter for the polygons that we want. 
  
  ecoregion_outline_temp<- ecoregions %>%
    filter(US_L3CODE == ecoregions_L3codes[i])
  #find all polygons that overlap 
  indexes_in_ecoregion<- polyfill(ecoregion_outline_temp, res = 6)
  polygons_in_ecoregion <- h3_to_geo_boundary_sf(indexes_in_ecoregion)
  
  #transform geometry so that it is the same. need to double check this doesn't remove any important information. 
  #ecoregion_range_temp<- st_transform(ecoregion_range_temp, crs = st_crs(polygons_in_ecoregion))
  #ecoregion_range_rds<- genHexRaw(ecoregion_range_temp, polygons_in_ecoregion)
  ecoregion_range_rds<- cali_birds_complete_hex_list[indexes_in_ecoregion]
  saveRDS(ecoregion_range_rds, file = paste(dir_list_ecoregions[i], "/hex_range_polygon_data.rds", sep  = ""))
  print(i)
}


