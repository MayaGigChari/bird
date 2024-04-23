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

# If the data is in a different format or structure, you may need to use different functions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

# to extract it. You can use functions like html_nodes() to select specific HTML elements and
# html_text() to extract the text content.

# Once you have extracted the data, you can further process it or save it to a file.
# For example, to save it to a CSV file:
write.csv(data, "all_data.csv", row.names = FALSE)