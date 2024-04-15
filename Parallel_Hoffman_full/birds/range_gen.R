
new_libPaths = .libPaths(c(paste('/u/home/m/',username, '/R', sep = ""),.libPaths())) #library path for cluster
.libPaths(new_libPaths)
packages_to_install <- c("sf")

for (package in packages_to_install) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, repos = "http://cran.us.r-project.org", lib = new_libPaths)
  }
  library(package)
}

#this is for plants. 
# Read the GeoJSON file
geojson_file <- "bien_ranges.geojson"
bird_ranges <- st_read(geojson_file)

#bounding box for california and polygon type in sf package
california_bbox <- st_bbox(c(xmin = -124.41060660766607, xmax = -114.13445790587905, ymin = 2.5342307609976, ymax = 42.00965914828148))
california_bbox_polygon <- st_as_sfc(st_bbox(california_bbox))

# filters geojson for only those that intersect with bounding box 
birds_in_california <- bird_ranges[st_intersects(bird_ranges, california_bbox_polygon), ]

print("found birds in california")