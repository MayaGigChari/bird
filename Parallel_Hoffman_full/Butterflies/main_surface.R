#for butterflies (species or genus)

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


clade = "butterflies"

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

pd_data<- read.csv("butterflies/GENUSCI_pd_output_bootstrap_bird.csv")
mpd_data<- read.csv("butterflies/GENUSCI_mpd_output_bootstrap.csv")
mntd_data<- read.csv("butterflies/GENUSCI_mntd_output_bootstrap.csv")

#range models
#this doesn't work at all. 

#need to figure out how to write this to json. 


pdf(file=paste( clade, "/pd_genus_fit_baro5.pdf", sep = ""))
pd_model<-as_tibble(t(surfaceGen(pd_data, "pd", func = baro5())), rownames = "key") #need to make models. 
dev.off()
pdf(file=paste(clade, "/mpd_genus_fit_baro5.pdf", sep = ""))
mpd_model<-as_tibble(t(surfaceGen(mpd_data, "mpd", func = baro5())), rownames = "key")
dev.off()
pdf(file=paste(clade, "/mntd_genus_fit_baro5.pdf", sep = ""))
mntd_model<-as_tibble(t(surfaceGen(mntd_data, "mntd", func = baro5())), rownames = "key")
dev.off()
surfaceGen

prediction_regression(pd_data, metric = "pd", func = baro5(), clade = "butterflies")
prediction_regression(mpd_data, metric = "mpd", func = baro5(), clade = "butterflies")
prediction_regression(mntd_data, metric = "mntd", func = baro5(), clade = "butterflies")

#plot(mpd_model$tree_sizes, mpd_model$low)

#wite model coefficients to csv files for visualization. 
write.csv(pd_model, "butterflies/GENUSpd_model_params_baro5.csv", row.names = TRUE)
write.csv(mpd_model, "butterflies/GENUSmpd_model_params_baro5.csv", row.names = TRUE)
write.csv(mntd_model, "butterflies/GENUSmntd_model_params_baro5.csv", row.names = TRUE)

json_pd<- toJSON(x = pd_model, dataframe = 'rows', pretty = F)
write(json_pd, file = "butterflies/GENUSpd_model_params_baro5.json")

json_mpd<- toJSON(x = mpd_model, dataframe = 'rows', pretty = F)
write(json_mpd, file = "butterflies/GENUSmpd_model_params_baro5.json")

json_mntd<- toJSON(x = mntd_model, dataframe = 'rows', pretty = F)
write(json_mntd, file = "butterflies/GENUSmntd_model_params_baro5.json")


#surface generation for ecoregions: 

dir_list_ecoregions <- list.dirs("butterflies/ecoregion_data",recursive = FALSE)  

#these values have definitely chagned. 

#these ecoregion surfaces are not working.
#check iususes here in morning 
#now I have 
for(i in dir_list_ecoregions)
{
  
  pd_data_temp_mam<- read.csv(paste(i, "/ecoregions_GENUSCI_pd_output_bootstrap.csv", sep = ""))
  mpd_data_temp_mam<- read.csv(paste(i, "/ecoregions_GENUSCI_mpd_output_bootstrap.csv", sep = ""))
  mntd_data_temp_mam<- read.csv(paste(i,"/ecoregions_GENUSCI_mntd_output_bootstrap.csv", sep = ""))
  
  #print(i)
  pdf(file=paste( i, "/pd_genus_fit_baro5.pdf", sep = ""))
  pd_model_temp_mam<-as_tibble(t(surfaceGen(pd_data_temp_mam, "pd", func = baro5())), rownames = "key") #need to make models. 
  print(i)
  pdf(file=paste( i, "/mpd_genus_fit_baro5.pdf", sep = ""))
  mpd_model_temp_mam<-as_tibble(t(surfaceGen(mpd_data_temp_mam, "mpd", func = baro5())), rownames = "key")
  dev.off()
  pdf(file=paste( i, "/mntd_genus_fit_baro5.pdf", sep = ""))
  mntd_model_temp_mam<-as_tibble(t(surfaceGen(mntd_data_temp_mam, "mntd", func = baro5())), rownames = "key")
  dev.off()
  
  print(i)
  json_pd<- toJSON(x = pd_model_temp_mam, dataframe = 'rows', pretty = F)
  write(json_pd, file = paste(i, "/GENUSpd_model_params_baro5.json", sep = ""))
  
  json_mpd<- toJSON(x = mpd_model_temp_mam, dataframe = 'rows', pretty = F)
  write(json_mpd, file = paste(i,"/GENUSmpd_model_params_baro5.json", sep = ""))
  
  json_mntd<- toJSON(x =mntd_model_temp_mam, dataframe = 'rows', pretty = F)
  write(json_mntd, file = paste(i, "/GENUSmntd_model_params_baro5.json", sep = ""))
  
  #print(i)
  
}


#want to try to visualize the surface parameterization with the actual data. 
#done with model params. 