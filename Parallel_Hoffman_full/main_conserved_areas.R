library(sf)
library(dplyr)


#read in the shape files and convert to the geometry that we want. 
conserved_areas<- st_read("Cali_Geometry/conserved_areas_CA_NAT_RESOURCES/30x30_Conserved_Areas%2C_Terrestrial_(2023).shp")

conserved_areas<- st_as_sf(conserved_areas)

selected_columns <- conserved_areas %>%
  dplyr::select(MNG_AGNCY, OBJECTID, cpad_PARK_)

selected_columns$GEOMETRY<- NULL

#select only managers that have at least 20 parks 
managers_of_interest <- selected_columns %>%
  group_by(MNG_AGNCY) %>%
  summarise(count = n()) %>%
  filter(count > 50)