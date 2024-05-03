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

# cannot find a butterflies checklist so will work directly with the range data. 
#probably need to combine geometries before we move on to the range area thing. 
#CALIFORNIA DEPARTMENT OF FISH AND WILDLIFE MIGHT HAVE BETTER RANGE MAPS!
butterflies_ranges<- st_read("butterflies/ButterflyRanges/na_rm_albers.shp")
butterflies_ranges<- st_transform(butterflies_ranges, crs = "WGS84")

sf_use_s2(FALSE)
butterflies_ranges<- st_make_valid(butterflies_ranges)

california<- st_read("Cali_geometry/ca-state-boundary/CA_State_TIGER2016.shp")

california<- st_transform(california, crs = "WGS84")

#there are a surprising number of butterflies that intersect with california! yay!
butterflies_intersect_california<- st_intersection(butterflies_ranges, california)


#unite all the butterflies grouping by their binomial 
#now all the geometries are consolidated for the butterflies. 
butterflies_intersect_consolidated_geometries<- butterflies_intersect_california %>%
  group_by(binomial) %>% 
  summarize(geometry = st_union(geometry))


butterflies_intersect_consolidated_geometries<- st_as_sf(butterflies_intersect_consolidated_geometries)


install.packages("lwgeom")
library(lwgeom)


butterflies_intersect_consolidated_geometries$range_area<- st_area(butterflies_intersect_consolidated_geometries)

st_write(butterflies_intersect_consolidated_geometries, "butterflies/full_california_butterflies_ranges.shp")
ecoregions<- st_read("Cali_Geometry/ca_eco_l3/ca_eco_l3.shp")


ecoregions<- st_transform(ecoregions, "WGS84")

#THIS IS AN ARBITRARY THRESHOLD: if < 1% of an ecoregion is covered by the range, then we will remove that range. 
ecoregions$area <- st_area(ecoregions) #Take care of units
ecoregions$min_thresh<- 0.02*ecoregions$area #can always amend this minimum threshold by just accessing the area. 



ecoregions_butterflies_data_filtered<- st_intersection(ecoregions, butterflies_intersect_consolidated_geometries)

#do a test:
ecoregions_butterflies_data_filtered

plot(ecoregions_butterflies_data_filtered[100,]) 
#get the areas of all the intersections 
#started at 7:59 pm 
ecoregions_butterflies_data_filtered$eco_area<- st_area(ecoregions_butterflies_data_filtered)
#filter the data so that the overlapped area is greater than the minimum threshold. 
#the geometry of this is: for each tuple species i, ecoregion j, the geometry is the overlap of species 
# i only in ecoregion j. 


#removed 1939-1806 tuples here. 
ecoregions_butterflies_data_filtered<- ecoregions_butterflies_data_filtered%>%
  filter(eco_area>= min_thresh | eco_area >= 0.10*range_area)


ecoregion_codes<- ecoregions$US_L3CODE


#edited this to only select some fields, but actually only edited it for one ecoregion. 
for(code in ecoregion_codes) {
  folder_path <- file.path("butterflies/ecoregion_data", code)
  
  # Check if folder exists, if not, create it
  if (!file.exists(folder_path)) {
    dir.create(folder_path, recursive = TRUE)
    cat("Created folder:", folder_path, "\n")
  }
  
  #also selects the geometry. 
  temp <- ecoregions_butterflies_data_filtered %>%
    filter(US_L3CODE == code) %>%
    dplyr::select(binomial, US_L3CODE, US_L3NAME, L1_KEY)
  
  filepath_shape <- file.path("butterflies/ecoregion_data", code, "ecoregion.shp")
  filepath_checklist <- file.path("butterflies/ecoregion_data", code, "checklist.csv")
  
  # Check if shapefile already exists
  if(file.exists(filepath_shape)) {
    print(paste("Shapefile", filepath_shape, "already exists. Deleting..."))
    file.remove(filepath_shape)
  }
  
  sf::st_write(temp, filepath_shape)
  print(unique(temp$binomial))
  write.csv(unique(temp$binomial), file = filepath_checklist)
}

#saved the butterfly data. 
#by ecoregion it's almost too small. Might have to figure out a way to deal with this properly. 
#in this file I have now generated all the butterflies ranges. However, the checklists all have * * instead of *_* which might become an issue.  