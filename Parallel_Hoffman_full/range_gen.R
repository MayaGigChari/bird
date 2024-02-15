
#need to generalize this. right now just for plant.s 
#install.packages("sf")
library(sf)
library(tibble)
library(geojsonsf)
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
birds_in_california <- st_intersects(bird_ranges, california) 

st_write(birds_in_california, "Plants/california_plants.shp")

#this doesn't work. need to figure it out. 
sf_geojson(birds_in_california, "Plants/california_plants.geojson")

write.csv(birds_in_california$species, "Plants/california_plant_species_list.csv ")


#written the csv. can now pull this csv onto local. 

