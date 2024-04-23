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

# URL of the webpage
url <- "https://ucjeps.berkeley.edu/cgi-bin/get_JM_name_data"

# Make an HTTP POST request to the webpage without specifying the NAME parameter
response <- POST(url)

# Extract the HTML content of the response
html_content <- content(response, as = "text")

# Parse the HTML content
page <- read_html(html_content)

# Extract the data you need using CSS selectors or XPath
# For example, if the data is in a table, you can use:
data <- page %>% html_table()

data_useful<- data[[4]]

#there are 7,392 species that are technically california natives. 
data_useful_only_natives<- data_useful%>%
  filter(nativity == "NATIVE")

#write this to a csv or an rds file; might still be too big. 

species_data_useful<- data_useful_only_natives$name_minus_authors

write.csv(species_data_useful, file = "Plants/species_native_to_Cali.csv")

# If the data is in a different format or structure, you may need to use different functions
# to extract it. You can use functions like html_nodes() to select specific HTML elements and
# html_text() to extract the text content.

# Once you have extracted the data, you can further process it or save it to a file.
# For example, to save it to a CSV file:


#########Everything above this was done on the local. now going to properly prune the big data. 
species_of_interest<- read.csv("Plants/species_native_to_Cali.csv")
species_of_interest$X<- NULL
colnames(species_of_interest)<- "species"
species_of_interest<- data.frame(species_of_interest)
species_of_interest$species <- gsub(" ", "_", species_of_interest$species)
#the next step with this is to trim the regional data to only birds that are supposed to be in california 


plants_range_california_only<- st_read("Plants/california_plants.shp")


#there are 12574 

#this join statement might have been used incorrectly in other scripts

#there are a total of 3,918 species of plants in this file that are California natives and also are represented with range data overlapping california. 
#is it possible that I should have been using inner joins all along? 
plants_range_california_only_natives<- st_as_sf(inner_join(species_of_interest, plants_range_california_only))
      
st_write(plants_range_california_only_natives, "Plants/California_species_shapes/plants_range_california_only_natives_POTW.shp")


#sanity check to make sure that we have the correct number of tuples. 
plants_names_both<- which(plants_range_california$species %in% species_of_interest$species)

#always do an inner join. never do a left join in this project ever. 


############
#Now: We have a list of birds that overlap with california but also that exist in the california species checklist of native plants. 
#it looks like a bunch of subspecies were removed.
#BELOW: do the same for ecoregions. 
############





