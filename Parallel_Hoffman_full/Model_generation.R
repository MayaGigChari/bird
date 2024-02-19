library(picante)
library(ape)
library(stringr)
library(dplyr)
library(phytools) 
library(data.table)
install.packages("drc")
library(drc)
install.packages('polyreg')
library(polyreg)

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
#pd_data <- retrieve_data("Plants", "pd")
#mpd_data <- retrieve_data("Plants", "mpd")
#mntd_data <- retrieve_data("Plants", "mntd")

#model fitting: need to try different models. 

#data clean isn't that useful. 

data_clean<- function(data_input, metric)
{
  seq_to_pred<- seq(0,650, by = 2)
  data_sim <- data_input
  rownames(data_sim)<-data_sim$X
  data_sim$X<- NULL
  data_sim<- data.frame(t(data_sim))
  data_sim$tree_size <- as.numeric(gsub(metric, "", rownames(data_sim)))
  return(data_sim)
}

#baro5 function tends to fit best. 
#no asymptotic behaviro 

data_sim
#only take the 10 and above. 

data_sim<- data_clean(mpd_data[,-2], "mpd")
model_low <- drm(data_sim$Low ~ data_sim$tree_size, fct = baro5())
model_high <- drm(data_sim$High ~ data_sim$tree_size, fct = baro5())
summary(model_low)
plot(High~tree_size, data = data_sim, ylim = c(min(data_sim$Low), max(data_sim$High)))
points(Low~tree_size, data = data_sim)
lines(predict(model_low)~tree_size,data = data_sim, type = "l", col = "blue")
lines(predict(model_high)~tree_size, data = data_sim, type = "l", col = "blue")

model_low$coefficients
model_high$coefficients
#barro 5 is technically the best fit for everything. 

## Model selection
#mselect(model_high, list(LL.3(), LL.5(), W1.3(), W1.4(), W2.4(), baro5()), linreg = TRUE)
#polyFit(data_sim$Low, data_sim$tree_size)

#can adapt this to do more best fit things and actually calculate error statistics but right now don't have to.
#can change to using the MM.3() fit for the model


#for now maybe just use baro5
#could use LL.3/4 and baro5 only when necessary? 
surfaceGen<- function(data_input, metric, outType = "list", model)
{
  
  data_sim<-data_clean(data_input)
  
  if(metric == "mpd")
  {
    model_low <- drm(data_sim$Low ~ data_sim$tree_size, fct = LL.4())
    model_high <- drm(data_sim$High ~ data_sim$tree_size, fct = LL.4())
  }
  
  if(metric == "mntd")
  {
    model_low <- drm(data_sim$Low ~ data_sim$tree_size, fct = LL.4())
    model_high <- drm(data_sim$High ~ data_sim$tree_size, fct = LL.4 ())
  }
  
  if(metric == "pd")
  {
    model_low <- drc::drm(data_sim$Low ~ data_sim$tree_size, fct = LL.3())
    model_high <- drc::drm(data_sim$High ~ data_sim$tree_size, fct= LL.3())
  }

  plot(High~tree_size, data = data_sim, ylim = c(min(data_sim$Low), max(data_sim$High)))
  points(Low~tree_size, data = data_sim)
  lines(predict(model_low)~tree_size,data = data_sim, type = "l", col = "blue")
  lines(predict(model_high)~tree_size, data = data_sim, type = "l", col = "blue")
  #print(summary(model_low))
  #print(summary(model_high))
  #relatively low standard error. 
  #print(summary(model_low))
  #plot(model_low)
  #plot(model_high)
  
  summary_low <- summary(model_low)$coefficients
  summary_high <- summary(model_high)$coefficients

  
  # Create a data frame with row names and coefficients
  result_coef <- data.frame(row.names = c("b", "c", "d", "e"),
                       low = summary_low[, 1],
                       high = summary_high[, 1])
  
  
  #need to add tree sizes. 
  result_model<- data.frame(predict(model_low, data = seq_to_pred))
  colnames(result_model)<- "low"
  result_model$high <- predict(model_high, data = seq_to_pred)
  result_model$tree_sizes <- data_sim$tree_size

  
  if(outType == "coef")
  {
    return(result_coef)
  }
  else if(outType == "list")
  {
    return(result_model)
  }
  
}


#need to more cohesively fit nonlinear models. 


