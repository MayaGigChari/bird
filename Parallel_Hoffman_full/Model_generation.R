library(picante)
library(ape)
library(stringr)
library(dplyr)
library(phytools) 
library(data.table)
install.packages("drc")
library(drc)


current_directory = getwd()

#need to somehow get out of this without aborting the r session in the else statement
#retrieve the data . 
retrieve_data <- function(clade, metric) {
  filename <- switch(metric,
                     "pd" = "CI_pd_output_bootstrap_bird.csv",
                     "mpd" = "CI_mpd_output_bootstrap_bird.csv",
                     "mntd" = "CI_mntd_output_bootstrap_bird.csv",
                     stop("Invalid metric"))
  
  filepath <- file.path(clade, filename)
  
  if (!file.exists(filepath)) {
    stop("File not found:", filepath)
  }
  
  data <- read.csv(filepath)
  
  return(data)
}

# Example usage:
#pd_data <- retrieve_data("squamate", "pd")
#mpd_data <- retrieve_data("squamate", "mpd")
#mntd_data <- retrieve_data("squamate", "mntd")



#can adapt this to do more best fit things and actually calculate error statistics but right now don't have to.
#can change to using the MM.3() fit for the model
surfaceGen<- function(data_input, metric)
{
  
  print(metric)
  data_sim <- data_input
  rownames(data_sim)<-data_sim$X
  data_sim$X<- NULL
  data_sim<- data.frame(t(data_sim))
  data_sim$tree_size <- as.numeric(gsub(metric, "", rownames(data_sim)))
  
  if(metric == "mpd")
  {
    model_low <- drm(data_sim$Low ~ data_sim$tree_size, fct = LL.4())
    model_high <- drm(data_sim$High ~ data_sim$tree_size, fct = LL.4())
  }
  
  if(metric == "mntd")
  {
    model_low <- drm(data_sim$Low ~ data_sim$tree_size, fct = LL.4())
    model_high <- drm(data_sim$High ~ data_sim$tree_size, fct = LL.4())
  }
  
  if(metric == "pd")
  {
    model_low <- drc::drm(data_sim$Low ~ data_sim$tree_size, fct = LL.4())
    model_high <- drc::drm(data_sim$High ~ data_sim$tree_size, fct= LL.4())
  }
  
  plot(High~tree_size, data = data_sim, ylim = c(min(data_sim$Low), max(data_sim$High)))
  points(Low~tree_size, data = data_sim)
  lines(predict(model_low)~tree_size, data = data_sim, type = "l", col = "blue")
  lines(predict(model_high)~tree_size, data = data_sim, type = "l", col = "blue")
  #print(summary(model_low))
  #print(summary(model_high))
  #relatively low standard error. 
  summary_low <- summary(model_low)$coefficients
  summary_high <- summary(model_high)$coefficients
  
  # Create a data frame with row names and coefficients
  result <- data.frame(row.names = c("b", "c", "d", "e"),
                       low = summary_low[, 1],
                       high = summary_high[, 1])
  
  return(result)
  
}


#pd_model<-surfaceGen(pd_data, "pd")
#mpd_model<-surfaceGen(mpd_data, "mpd")
#mntd_model<-surfaceGen(mntd_data, "mntd")



