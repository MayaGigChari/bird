#this is a file analogous to bird_ranges

#step 1: already generated in the range_gen script: plants that have ranges that interesect California? 

#what about non-native plants? what did we end up deciding? 


#here I will try to get a list of all california natives (like in birds)

#this part can be done on the local. 
install.packages("rvest")
install.packages("httr")
library(httr)
library(rvest)
library(dplyr)
library(sf)
library(ggplot2)

# cannot find a mammal checklist so will work directly with the range data. 

#CALIFORNIA DEPARTMENT OF FISH AND WILDLIFE MIGHT HAVE BETTER RANGE MAPS!
mammal_ranges<- st_read("Mammals/MAMMALS_TERRESTRIAL_ONLY/MAMMALS_TERRESTRIAL_ONLY.shp")

sf_use_s2(FALSE)
mammal_ranges<- st_make_valid(mammal_ranges)

california<- st_read("Cali_geometry/ca-state-boundary/CA_State_TIGER2016.shp")

california<- st_transform(california, crs = "WGS84")

mammals_intersect_california<- st_intersection(mammal_ranges, california)


p<- ggplot() +
  geom_sf(data = california)+
  geom_sf(data = mammals_intersect_california[3,][1], fill = "red")
  
#plotted and spot checked a few species, and it looks good with the ranges. 


#get all the mammal ranges and their intersection with california. 
#will not cross reference this with any kind of list because I can't find one. 


############
#Now: We have a list of mammals that overlap with california but also that exist in the california species checklist of native plants. 
#it looks like a bunch of subspecies were removed.
#SWITCH BACK TO LOCAL FOR ECOREGIONS STUFF!
############

#from California
#apply the ecoregion thresholding thing. 
#these are all the california species (only about 4,000 of them)
install.packages("lwgeom")
library(lwgeom)
mammals_intersect_california$range_area<- st_area(mammals_intersect_california)

st_write(mammals_intersect_california, "Mammals/full_california_mammal_ranges.shp")
ecoregions<- st_read("Cali_Geometry/ca_eco_l3/ca_eco_l3.shp")


ecoregions<- st_transform(ecoregions, "WGS84")

#THIS IS AN ARBITRARY THRESHOLD: if < 1% of an ecoregion is covered by the range, then we will remove that range. 
ecoregions$area <- st_area(ecoregions) #Take care of units
ecoregions$min_thresh<- 0.02*ecoregions$area #can always amend this minimum threshold by just accessing the area. 



#TODO: preprocess the ecoregion data so that the range is not included if the ecoregion area in a certain range is below a threshold. 
#require that at least 5% of the ecoregion be covered in order for it to be included in the range. 

#for real. this will take forever to run. 
#running this again to force myself to have a break. 
#saving ecoregions_plant_data_intersection just in case. 
#this should assign species with particular ranges to each ecoregion. HOWEVER, it might only preserve the ecoregion gometry not the actual species range geometry!
ecoregions_mammal_data_filtered<- st_intersection(ecoregions, mammals_intersect_california)

#do a test:
ecoregions_mammal_data_filtered
plot(ecoregions_mammal_data_filtered[100,])
#get the areas of all the intersections 
#started at 7:59 pm 
ecoregions_mammal_data_filtered$eco_area<- st_area(ecoregions_mammal_data_filtered)
#filter the data so that the overlapped area is greater than the minimum threshold. 
#the geometry of this is: for each tuple species i, ecoregion j, the geometry is the overlap of species 
# i only in ecoregion j. 
#note: each species is uniuqe and only has one geometry. 
ecoregions_mammal_data_filtered<- ecoregions_mammal_data_filtered%>%
  filter(eco_area>= min_thresh | eco_area >= 0.10*range_area)

st_write(ecoregions_mammal_data_filtered, "Mammals/mammal_complete_species_lists_by_ecoregion.shp")


test<- ecoregions_mammal_data_filtered %>%
  filter(sci_name == "Odocoileus virginianus") 
library(ggplot2)

# Plot the first plot using ggplot2 and color by US_L3CODE

#the species Cymopterus_panamintensis has been fully removed! 

#may have to filter on if the threshold of the range is below a certain threshold of the whole species range? 
#as an artifact of this probram, some species with poorly-defined small ranges are removed. 

#if we filter out the ecoregion that a hexagon is in, we should just get the part of the range of species n that is within that ecoregion. 
p<- ggplot() +
  geom_sf(data = test[1], aes(fill = US_L3CODE)) +
  geom_sf(data = ecoregions_mammal_data_filtered %>%
            filter(sci_name == "Odocoileus virginianus"), 
          color = "red", alpha = 0.1)

ggsave("Mammals/Odocoileus_virginianus_example_ecoregion_range_sep.png", plot = p,width = 10, height = 8,  dpi = 300)
#made a directory called ecoregion_data to store shape files and lists. 
ecoregion_codes<- ecoregions$US_L3CODE

#honestly I don't know, I need to go to sleep. 

#do not need to rerun this. already have all the ecoregion species stuff. 
#ecoregion bird data is already merged! This is basically good to go for the entire ecoregion. but also need to do the other thing with the ecoregions (hex data stuff)
#these shape files are good for the entire overlapping species list for a region, need to be able to populate hex data as well.

#edited this to only select some fields, but actually only edited it for one ecoregion. 
for(code in ecoregion_codes) {
  folder_path <- file.path("Mammals/ecoregion_data", code)
  
  # Check if folder exists, if not, create it
  if (!file.exists(folder_path)) {
    dir.create(folder_path, recursive = TRUE)
    cat("Created folder:", folder_path, "\n")
  }
  
  #also selects the geometry. 
  temp <- ecoregions_mammal_data_filtered %>%
    filter(US_L3CODE == code) %>%
    select(sci_name, US_L3CODE, US_L3NAME, L1_KEY)
  
  filepath_shape <- file.path("Mammals/ecoregion_data", code, "ecoregion.shp")
  filepath_checklist <- file.path("Mammals/ecoregion_data", code, "checklist.csv")
  
  # Check if shapefile already exists
  if(file.exists(filepath_shape)) {
    print(paste("Shapefile", filepath_shape, "already exists. Deleting..."))
    file.remove(filepath_shape)
  }
  
  sf::st_write(temp, filepath_shape)
  print(unique(temp$sci_name))
  write.csv(unique(temp$sci_name), file = filepath_checklist)
}
#in this file I have now generated all the mammal ranges. However, the checklists all have * * instead of *_* which might become an issue.  