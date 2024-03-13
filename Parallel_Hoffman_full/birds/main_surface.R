packages_to_install <- c("picante", "ape", "stringr", "dplyr", "phytools", "data.table", "drc", "jsonlite", "tidyverse")

# Load necessary packages
for (pkg in packages_to_install) {
  # Check if the package is not already installed
  if (!requireNamespace(pkg, quietly = TRUE)) {
    # Install the package only if not already installed
    install.packages(pkg)
  }
  # Load the package
  library(pkg, character.only = TRUE)
}


clade = "birds_ranges"

current_directory <- getwd()

Model_gen_path <- file.path(current_directory, "Model_generation.R")

source(Model_gen_path)

#get the data
#pd_data <- retrieve_data(clade, "pd")
#mpd_data <- retrieve_data(clade, "mpd")
#mntd_data <- retrieve_data(clade, "mntd")

#generate the models
#pd_model<-surfaceGen(pd_data, "pd")  
#mpd_model<-surfaceGen(mpd_data, "mpd")
#mntd_model<-surfaceGen(mntd_data, "mntd")

#for the ranges: 

pd_data<- read.csv("birds/rangesCI_pd_output_bootstrap_bird.csv")
mpd_data<- read.csv("birds/rangesCI_mpd_output_bootstrap_bird.csv")
mntd_data<- read.csv("birds/rangesCI_mntd_output_bootstrap_bird.csv")

#range models
#this doesn't work at all. 

#need to figure out how to write this to json. 
pd_model<-as_tibble(t(surfaceGen(pd_data, "pd")), rownames = "key") #need to make models. 
mpd_model<-t(surfaceGen(mpd_data, "mpd"))
mntd_model<-t(surfaceGen(mntd_data, "mntd"))

#plot(mpd_model$tree_sizes, mpd_model$low)

#wite model coefficients to csv files for visualization. 
write.csv(pd_model, "birds/pd_model_params.csv", row.names = TRUE)
write.csv(mpd_model, "birds/mpd_model_params.csv", row.names = TRUE)
write.csv(mntd_model, "birds/mntd_model_params.csv", row.names = TRUE)

json_pd<- toJSON(x = pd_model, dataframe = 'rows', pretty = F)
write(json_pd, file = "birds/pd_model_params.json")

json_mpd<- toJSON(x = mpd_model, dataframe = 'rows', pretty = F)
write(json_mpd, file = "birds/mpd_model_params.json")

json_mntd<- toJSON(x = mntd_model, dataframe = 'rows', pretty = F)
write(json_mntd, file = "birds/mntd_model_params.json")



