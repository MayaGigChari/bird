
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


clade = "amphibians"

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

pd_data<- read.csv("amphibians/CI_pd_output_bootstrap_bird.csv")
mpd_data<- read.csv("amphibians/CI_mpd_output_bootstrap.csv")
mntd_data<- read.csv("amphibians/CI_mntd_output_bootstrap.csv")

#range models
#this doesn't work at all. 

#need to figure out how to write this to json. 
pd_model<-as_tibble(t(surfaceGen(pd_data, "pd")), rownames = "key") #need to make models. 
mpd_model<-as_tibble(t(surfaceGen(mpd_data, "mpd")), rownames = "key")
mntd_model<-as_tibble(t(surfaceGen(mntd_data, "mntd")), rownames = "key")
surfaceGen
#plot(mpd_model$tree_sizes, mpd_model$low)

#wite model coefficients to csv files for visualization. 
write.csv(pd_model, "amphibians/pd_model_params.csv", row.names = TRUE)
write.csv(mpd_model, "amphibians/mpd_model_params.csv", row.names = TRUE)
write.csv(mntd_model, "amphibians/mntd_model_params.csv", row.names = TRUE)

json_pd<- toJSON(x = pd_model, dataframe = 'rows', pretty = F)
write(json_pd, file = "amphibians/pd_model_params.json")

json_mpd<- toJSON(x = mpd_model, dataframe = 'rows', pretty = F)
write(json_mpd, file = "amphibians/mpd_model_params.json")

json_mntd<- toJSON(x = mntd_model, dataframe = 'rows', pretty = F)
write(json_mntd, file = "amphibians/mntd_model_params.json")


#surface generation for ecoregions: 

dir_list_ecoregions <- list.dirs("amphibians/ecoregion_data",recursive = FALSE)  

#these values have definitely chagned. 

#these ecoregion surfaces are not working.
#check iususes here in morning 
#now I have 
for(i in dir_list_ecoregions)
{
  
  pd_data_temp<- read.csv(paste(i, "/ecoregionsCI_pd_output_bootstrap_bird.csv", sep = ""))
  mpd_data_temp<- read.csv(paste(i, "/ecoregionsCI_mpd_output_bootstrap.csv", sep = ""))
  mntd_data_temp<- read.csv(paste(i,"/ecoregionsCI_mntd_output_bootstrap.csv", sep = ""))
  
  print(i)
  
  pd_model_temp<-as_tibble(t(surfaceGen(pd_data_temp_, "pd")), rownames = "key") #need to make models. 
  mpd_model_temp<-as_tibble(t(surfaceGen(mpd_data_temp_, "mpd")), rownames = "key")
  mntd_model_temp<-as_tibble(t(surfaceGen(mntd_data_temp_, "mntd")), rownames = "key")
  
  json_pd<- toJSON(x = pd_model_temp, dataframe = 'rows', pretty = F)
  write(json_pd, file = paste(i, "/pd_model_params.json", sep = ""))
  
  json_mpd<- toJSON(x = mpd_model_temp, dataframe = 'rows', pretty = F)
  write(json_mpd, file = paste(i,"/mpd_model_params.json", sep = ""))
  
  json_mntd<- toJSON(x =mntd_model_temp, dataframe = 'rows', pretty = F)
  write(json_mntd, file = paste(i, "/mntd_model_params.json", sep = ""))
  
}

#need to plot 

#now I have saved all the surfaces and can populate the hexagons. 


#also need to get the pd, mpd and mntd statistics for each ecoregion like was done for the hex data. 

