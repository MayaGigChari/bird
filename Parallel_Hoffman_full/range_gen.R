
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


#for range data




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


#don't need to do a groupby. 
#now going to run this with the full data. 

#I think this is done with the full data. 

#do a join on st_within, whatever that means. 
joined_data<-st_join (geog_plant_observations, polygons, join = st_within)%>% 
  select(h3_index,taxonobservation_id,taxon_id, matched_taxonomic_status, scrubbed_taxon_name_no_author)%>%
  filter(is.na(h3_index) == FALSE)

hex_species_plants<- list()
for(i in 1: length(h3_indexes))
{
  poly_temp_db<- joined_data %>%
    filter(h3_index == h3_indexes[i])
  poly_species<- unique(poly_temp_db$scrubbed_taxon_name_no_author)
  hex_species_plants[i] <- list(poly_species)
}

#also has taxonomic status data!


#need to get the species information!
#attempt with st_join#attempt with st_joinh3_indexes

#basically want to iterate through each polygon and see which points are within the polygon (which individual tuple)
  
#notes: st_intersectionc creates a shared geometry between x and y!

popPixel()

#need to update this. 

