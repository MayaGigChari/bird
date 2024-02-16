
#need to generalize this. right now just for plant.s 
#install.packages("sf")
#devtools::install_github("crazycapivara/h3-r")
library(sf)
library(tibble)
library(geojsonsf)
library(h3)
library(dplyr)
# Read the GeoJSON file
#this is for the azathoth rstudio remote server



# Validate GeoJSON file
geojson_file <- "bien_ranges.geojson"
bird_ranges <- st_read(geojson_file)

#this originally evaluated to false 
validity <- st_is_valid(bird_ranges)

#need to make all the bird ranges valid before running more things.
bird_ranges <- st_make_valid(bird_ranges)

#these are all valid now. 


st_crs(bird_ranges) <- st_crs(4326) 

#bounding box for california and polygon type in sf package
#california_bbox <- st_bbox(c(xmin = -124.41060660766607, xmax = -114.13445790587905, ymin = 32.5342307609976, ymax = 42.00965914828148))
#california_bbox_polygon <- st_as_sfc(st_bbox(california_bbox), crs = st_crs(bird_ranges))

#read california shape file 
california <- sf::read_sf("Cali_Geometry/ca-state-boundary/CA_State_TIGER2016.shp") %>%
  sf::st_transform(4326)

# filters geojson for only those that intersect with bounding box or califrornia shape file
#this is misleading, it's actually plants I just mislabeled everything as birds. 
birds_in_california <- st_intersection(bird_ranges, california) 

st_write(birds_in_california, "Plants/california_plants.shp")

#this doesn't work. need to figure it out. 
sf_geojson(birds_in_california, "Plants/california_plants.geojson")

write.csv(birds_in_california$species, "Plants/california_plant_species_list.csv ")


#written the csv. can now pull this csv onto local.

#now need to get all the plants that intersect with each hexcode 


#get the whole plant observation dataset
#this is a huge file so it might not actua.ly be possible to read it in 

#plants in cali is the occurrence dataset of plants seen in california. 
#these are ALL THE SEEN PLANTS!
plants_in_cali<- st_read("Plants/bien_occurrence_california.geojson")


#these might be mostly empty. 
plants_in_cali$geometry

geog_plant_observations = plants_in_cali[!st_is_empty(plants_in_cali),,drop=FALSE]


#index california in the same way. 
#each index should be associated with some kind of geometry 
h3_indexes <- polyfill(california[1, ], res = 6)
polygons <- h3_to_geo_boundary_sf(h3_indexes)


#popPixel is redundant in California_partition 
#need to get 

#list for each hexagon
taxon_ids <- list()
# Iterate through each polygon
for (i in 1:3) 
  {
  current_polygon <- polygons[i, ]
  intersects <- st_filter(geog_plant_observations, current_polygon)
  print(intersects$taxon_id)
  taxon_observation_ids[i]<- intersects$taxon_id
  }

#basically want to iterate through each polygon and see which points are within the polygon (which individual tuple)

polygons[[2]]

popPixel<- function(len = length(polygons), nms = h3_indexes)
{
  occurrence_df <-list(len)
  for(i in 1: 10)
  {
    coordinate_data_polygon = st_as_text(polygons[[2]][[i]]) #the polygon geometry 
    print(coordinate_data_polygon)
    plants_temp_polygon = st_intersects(plants_in_cali, polygon)
    print(plants_temp_polygon)
    #species_data<- read_df(species_data)
    #occurrence_df[i]<- species_data
    #print(i)
  }
  #names(occurrence_df)<- nms
  #return(occurrence_df)
  
}

popPixel()


