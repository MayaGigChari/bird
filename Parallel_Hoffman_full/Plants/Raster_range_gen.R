#this is a file for generating plant ranges from the raster files from https://datadryad.org/stash/dataset/doi:10.6078/D1QQ2S

#either can load all the data at once 

#cannot install rgdal.
install.packages("raster")
library(raster)
install.packages("stars")
install.packages("inlmisc")
library(Grid2Polygons)
library(terra)
library(sf)
library(ggplot2)
library(dplyr)
library(h3)
#install.packages("RGDAL")
#library(RGDAL)

rastlist <- list.files(path = "/Users/mayachari/Desktop/bird/Parallel_Hoffman_full/Plants/binary_maxent/", pattern='.tif$', 
                       all.files=TRUE, full.names=TRUE)

#read in all rasters
allrasters <- lapply(rastlist, raster)


#want to load and plot california. 


#from this, it looks like the SECOND geomtetry (1) is where the range actually happens. 
california<- st_read("Cali_Geometry/ca-state-boundary")%>%
  st_transform("WGS84")



allshapes<- sapply(allrasters,  as, Class = "SpatRaster")

allshapes<- sapply(allshapes, as.polygons)

allshapes<- lapply(allshapes, st_as_sf)

allshapes<- lapply(allshapes, st_transform, crs = "WGS84")




adjust_sf_type<- function(sf_object)
{
  test<- sf_object
  test<- test[-1,]
  test[1,1]<- colnames(test)[1]
  colnames(test)[1]<- "sci_name"
  return(test)
}

allshapes_corrected<- lapply(allshapes, adjust_sf_type)

#now we have a list of corrected shapes 

#combine all sf objects into one object. 



allshapes_corrected_1<- allshapes_corrected[1:500]
allshapes_corrected_2<- allshapes_corrected[501:1000]
allshapes_corrected_3<- allshapes_corrected[1001:1500]
allshapes_corrected_4<- allshapes_corrected[1501:2000]
allshapes_corrected_5<- allshapes_corrected[2001:2500]
allshapes_corrected_6<- allshapes_corrected[2501:3000]
allshapes_corrected_7<- allshapes_corrected[3001:3500]
allshapes_corrected_8<- allshapes_corrected[3501:4000]
allshapes_corrected_9<- allshapes_corrected[4001:4500]
allshapes_corrected_10<- allshapes_corrected[4501:5000]
allshapes_corrected_11<- allshapes_corrected[5001:5220]



combined_sf_1<- do.call(rbind, allshapes_corrected_1)
combined_sf_2<- do.call(rbind, allshapes_corrected_2)
combined_sf_3<- do.call(rbind, allshapes_corrected_3)
combined_sf_4<- do.call(rbind, allshapes_corrected_4)
combined_sf_5<- do.call(rbind, allshapes_corrected_5)
combined_sf_6<- do.call(rbind, allshapes_corrected_6)
combined_sf_7<- do.call(rbind, allshapes_corrected_7)
combined_sf_8<- do.call(rbind, allshapes_corrected_8)
combined_sf_9<- do.call(rbind, allshapes_corrected_9)
combined_sf_10<- do.call(rbind, allshapes_corrected_10)
combined_sf_11<- do.call(rbind, allshapes_corrected_11)


list_combined_objects<- list(combined_sf_1, combined_sf_2, combined_sf_3, combined_sf_4, combined_sf_5, combined_sf_6,
                             combined_sf_7, combined_sf_8, combined_sf_9, combined_sf_10, combined_sf_11)

total_combination_shapes<- do.call(rbind, list_combined_objects)

st_write(total_combination_shapes, "Plants/total_combined_range_shapes.shp")




#there are 5162 features that have intersection with the california shape file.

#maybe california plants loses some information. 
california_plants<- st_intersection(california, total_combination_shapes)

#make ecoregion stuff. 

Plants_intersect_california<- st_read("Plants/total_combined_range_shapes.shp")

sf_use_s2(FALSE)
Plants_intersect_california<-st_make_valid(Plants_intersect_california)

ecoregions<- st_read("Cali_Geometry/ca_eco_l3/ca_eco_l3.shp")


ecoregions<- st_transform(ecoregions, "WGS84")

#THIS IS AN ARBITRARY THRESHOLD: if < 1% of an ecoregion is covered by the range, then we will remove that range. 
ecoregions$area <- st_area(ecoregions) #Take care of units
ecoregions$min_thresh<- 0.02*ecoregions$area #can always amend this minimum threshold by just accessing the area. 



ecoregions_Plants_data_filtered<- st_intersection(ecoregions, Plants_intersect_california)

#within this ecoregions thing there are 45187 tuples. 

st_write(ecoregions_Plants_data_filtered, "Plants/total_combined_range_shapes_with_ecoregions_Mishler.shp")


#get the areas of all the intersections 
#started at 7:59 pm 
ecoregions_Plants_data_filtered$eco_area<- st_area(ecoregions_Plants_data_filtered)


#filter the data so that the overlapped area is greater than the minimum threshold. 
#the geometry of this is: for each tuple species i, ecoregion j, the geometry is the overlap of species 
# i only in ecoregion j. 
#note: each species is uniuqe and only has one geometry. 

#maybe I shouldn't filter things out because these ranges are so much more explicit. 
ecoregions_Plants_data_filtered_with_restrictions<- ecoregions_Plants_data_filtered%>%
  filter(eco_area>= min_thresh | eco_area >= 0.10*area)

#test the range for zostera marina. 
test_Abies.amabilis<- ecoregions_Plants_data_filtered_with_restrictions%>%
  filter(sci_name == "Zostera.marina")

library(ggplot2)
p<- ggplot() +
  geom_sf(data = ecoregions) +
  geom_sf(data = test_Abies.amabilis,aes(fill = US_L3CODE))

ggsave("Plants/images/test_Zostera.marina_range_with_filtering.png", plot = p, width = 10, height = 8, units = "in")

dev.off()
#do a test: 
#going to use the filtered data. However, for each hexagon I will populate with the nonfiltered data? but then it doesn't make a ton of sense to build the trees for each ecoregion. 
#the trees would have to be specific to the larger california area and I would have to simulate pd for the larger area which I DO anyways ok. 

renamed_sci_name<- gsub("\\.", "_", ecoregions_Plants_data_filtered_with_restrictions$sci_name)
Plants_data_filtered_with_restrictions$sci_name<- renamed_sci_name

st_write(ecoregions_Plants_data_filtered_with_restrictions, "Plants/Plants_complete_species_lists_by_ecoregion_with_geom_restrictions_MISHLER.shp", append = FALSE)


ecoregion_codes<- ecoregions$US_L3CODE

#honestly I don't know, I need to go to sleep. 

#do not need to rerun this. already have all the ecoregion species stuff. 
#ecoregion bird data is already merged! This is basically good to go for the entire ecoregion. but also need to do the other thing with the ecoregions (hex data stuff)
#these shape files are good for the entire overlapping species list for a region, need to be able to populate hex data as well.

#edited this to only select some fields, but actually only edited it for one ecoregion. 
#make a folder called ecoregion_dataa_2 that houses all of this. need to amend this in future used scripts. 
for(code in ecoregion_codes) {
  folder_path <- file.path("Plants/ecoregion_data_2", code)
  
  # Check if folder exists, if not, create it
  if (!file.exists(folder_path)) {
    dir.create(folder_path, recursive = TRUE)
    cat("Created folder:", folder_path, "\n")
  }
  
  #also selects the geometry. 
  temp <- ecoregions_Plants_data_filtered_with_restrictions %>%
    filter(US_L3CODE == code) %>%
    dplyr::select(sci_name, US_L3CODE, US_L3NAME, L1_KEY)
  
  filepath_shape <- file.path("Plants/ecoregion_data_2", code, "ecoregion.shp")
  filepath_checklist <- file.path("Plants/ecoregion_data_2", code, "checklist.csv")
  
  # Check if shapefile already exists
  if(file.exists(filepath_shape)) {
    print(paste("Shapefile", filepath_shape, "already exists. Deleting..."))
    file.remove(filepath_shape)
  }
  
  sf::st_write(temp, filepath_shape)
  write.csv(unique(temp$sci_name), file = filepath_checklist)
}
#in this file I have now generated all the Plants ranges. However, the checklists all have * * instead of *_* which might become an issue.  

#now I will make a large file with the polygons and the species ranges to use in later analyses. This will be massive, just a heads up. 


h3_indexes <- polyfill(california[1, ], res = 6)
polygons <- h3_to_geo_boundary_sf(h3_indexes)

#this will probably take forever. 
all_california_sci_name<- gsub("\\.","_", Plants_intersect_california$sci_name)
Plants_intersect_california$sci_name<- all_california_sci_name


polygons_plant_species_ranges_MISHLER<- st_join(polygons, Plants_intersect_california)

polygons_plant_species_ranges_MISHLER$sci_name

st_write(polygons_plant_species_ranges_MISHLER, "Plants/polygons_plant_species_ranges_MISHLER.shp")

st_read("Plants/polygons_plant_species_ranges_MISHLER.shp")

#now I need to separate out the per-hexagon species. 


genHexRaw_prejoined<- function(hex_range_joined_data, indexes) #the indexes will be h3_indexes usually 
{
  #want to select just the species and the index. But can do this later. 
  #should always have an h3_index field. This is necessary for the filter statement 
  #joining the species that could exist in that area with the polygons in that area! I think this might work... 
  hex_species<- list()
  #need to iterate through hexes. not i's 
  for(i in 1:length(indexes))
  {
    poly_temp_db<- hex_range_joined_data %>%
      filter(h3_index == indexes[i])
    poly_species<- unique(poly_temp_db$sci_name)
    hex_species[i] <- list(poly_species)
    print(indexes[i])
  }
  print("hexRaw done")
  return(hex_species)
}

Mammal_hex_species_list<- genHexRaw_prejoined(polygons_plant_species_ranges_MISHLER, h3_indexes)

names(Mammal_hex_species_list)<- h3_indexes

Plants_hex_species_list<- Mammal_hex_species_list

saveRDS(Plants_hex_species_list, file = "Plants/plants_hex_species_list_MISHLER_0505")

#now we have hexagon-specific plant species data. 

#need to run the main. 