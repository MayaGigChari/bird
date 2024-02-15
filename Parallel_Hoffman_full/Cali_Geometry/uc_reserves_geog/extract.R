#install.packages("sf")
library(sf)

shapefile_path <- "Angelo/Angelo_boundary.shp"
shapefile <- st_read(shapefile_path)

wkt <- st_as_text(shapefile$geometry)
output_file <- "Angelo_boundary.wkt"
writeLines(wkt, output_file)

# install.packages("rgbif")
library(rgbif)
reserve_birds = occ_data(geometry = wkt, geom_big = "axe", scientificName = "Aves", hasGeospatialIssue = "False")

#remove duplicates, get species names (remove spaces), turn into csv
species <- reserve_birds$data[!duplicated(reserve_birds$data$scientificName), "scientificName", drop = FALSE]
species <- data.frame(gsub(" ", "", species$scientificName))

write.csv(species, "Angelo_bird_species.csv", row.names = FALSE)