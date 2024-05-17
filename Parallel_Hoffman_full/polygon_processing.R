#this script takes a set of data for hexagons and processes them statistically
#need to add ecoregion data to this. 

#should rejoin this with the hexagonal geometry data and make an sf object. 
#don't have hex name information
data_temp<- readRDS("birds/raw_hex_stats_from_ranges")


data_temp$species_in_tree

species<- data_temp$species_in_tree
richness<- lapply(species, length)
richness<- unlist(richness)

pd<- unlist(data_temp$pd_values)

mpd<- unlist(data_temp$mpd_values)

mntd<- unlist(data_temp$mntd_values)

 

df_stats<- data.frame(pd)
colnames(df_stats)<- "pd"
df_stats$mpd<- mpd
df_stats$mntd <- mntd
df_stats$richness<- richness


ggplot(df_stats, aes(x = pd))+geom_histogram(bins = 100)
ggplot(df_stats, aes(x = mpd))+geom_histogram(bins = 100)
ggplot(df_stats, aes(x = mntd))+geom_histogram(bins = 100)
ggplot(df_stats, aes(x = richness))+geom_histogram(bins = 100)
library(ggplot2)
library(dplyr)

# Count the occurrences of each richness value
richness_counts <- df_stats %>%
  count(richness)

# Filter richness values that occur fewer than 5 times
richness_filtered <- richness_counts %>%
  filter(n >= 10) %>% 
  filter(richness == 160 | richness == 170) %>%
  dplyr::select(richness)

# Filter df_stats based on richness_filtered
df_filtered <- df_stats %>%
  semi_join(richness_filtered, by = "richness")

# Create histograms for each unique richness value
p <- df_filtered %>%
  ggplot(aes(x = pd)) +
  geom_histogram(bins = 100) +
  labs(x = "pd", y = "Frequency") +
  facet_wrap(~ richness, ncol = 2) +  # Create facets based on richness with 11 columns
  ggtitle("Histogram of pd for different richness levels")

# Display the plot
print(p)
