
#need to generalize username from mchari

print(.libPaths())

#new_libPaths <- c(paste('/u/home/mchari/R/library', sep = ""), .libPaths())

# Update library paths
#.libPaths(new_libPaths)

# Packages to install
#packages_to_install <- c("sf")

# Loop through packages to install
#for (package in packages_to_install) {
  # Check if package is available
#  if (!require(package, character.only = TRUE)) {
    # If not available, install the package
#    install.packages(package, lib = new_libPaths)
#  }
#}

#print("done")
# Load the installed packages
#library(sf, lib.loc = new_libPaths)

#install.packages("sf")
library(sf)
# Read the GeoJSON file
geojson_file <- "bien_ranges.geojson"
bird_ranges <- st_read(geojson_file)

st_crs(bird_ranges) <- st_crs(4326) 

#bounding box for california and polygon type in sf package
california_bbox <- st_bbox(c(xmin = -124.41060660766607, xmax = -114.13445790587905, ymin = 32.5342307609976, ymax = 42.00965914828148))

california_bbox_polygon <- st_as_sfc(st_bbox(california_bbox), crs = bird_ranges_crs)

# filters geojson for only those that intersect with bounding box 
birds_in_california <- bird_ranges[st_intersects(bird_ranges, california_bbox_polygon), ]

print("found birds in california")