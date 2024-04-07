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

data_clean<- function(data_input, metric, min, max, by)
{
  seq_to_pred<- seq(min,max, by)
  data_sim <- data_input
  rownames(data_sim)<-data_sim$X
  data_sim$X<- NULL
  data_sim<- data.frame(t(data_sim))
  data_sim$tree_size <- as.numeric(gsub(metric, "", colnames(data_sim)))
  return(data_sim)
}

data_clean_quartiles<- function(data_input, metric)
{
  rownames(data_input)<-data_input$X
  data_input$X<- NULL
  rows<- rownames(data_input)
  cols<- colnames(data_input)
  data_input<- data.frame(t(data_input))
  rownames(data_input)<- cols
  colnames(data_input)<- rows
  data_input$tree_size <- as.numeric(gsub(metric, "", rownames(data_input)))
  return(data_input)
}
#baro5 function tends to fit best. 
#no asymptotic behaviro 

#data_sim
#only take the 10 and above. 


#ill use the LL.3 function 
#data_sim<- data_clean_quartiles(mntd_data, "mntd")
#model_low <- drm(data_sim$Low ~ data_sim$tree_size, fct = LL.4())
#model_high <- drm(data_sim$High ~ data_sim$tree_size, fct = LL.4())
#summary(model_low)
#plot(High~tree_size, data = data_sim, ylim = c(min(data_sim$Low), max(data_sim$High)))
#points(Low~tree_size, data = data_sim)
#lines(predict(model_low)~tree_size,data = data_sim, type = "l", col = "blue")
#lines(predict(model_high)~tree_size, data = data_sim, type = "l", col = "blue")

#model_low$coefficients
#model_high$coefficients
#barro 5 is technically the best fit for everything. 

## Model selection
#mselect(model_high, list(LL.3(), LL.5(), W1.3(), W1.4(), W2.4(), baro5()), linreg = TRUE)
#polyFit(data_sim$Low, data_sim$tree_size)

#can adapt this to do more best fit things and actually calculate error statistics but right now don't have to.
#can change to using the MM.3() fit for the model


#for now maybe just use baro5
#could use LL.3/4 and baro5 only when necessary? 

#just use LL.4 for simplicity. 

#can change to LL.3 later
surfaceGen<- function(data_input, metric, outType = "coef", datatype = "normal")
{
  
  if(datatype == "quartiles")
  {
    data_sim<-data_clean_quartiles(data_input, metric)
  }
  else
  {
    data_sim<- data_clean(data_input, metric)
  }

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
    model_low <- drc::drm(data_sim$Low ~ data_sim$tree_size, fct = LL.4())
    model_high <- drc::drm(data_sim$High ~ data_sim$tree_size, fct= LL.4())
  }

  #plot(High~tree_size, data = data_sim, ylim = c(min(data_sim$Low), max(data_sim$High)))
  #points(Low~tree_size, data = data_sim)
  #lines(predict(model_low)~tree_size,data = data_sim, type = "l", col = "blue")
  #lines(predict(model_high)~tree_size, data = data_sim, type = "l", col = "blue")
  #print(summary(model_low))
  #print(summary(model_high))
  #relatively low standard error. 
  #print(summary(model_low))
  #plot(model_low)
  #plot(model_high)
  
  summary_low <- model_low$coefficients
  summary_high <- model_high$coefficients

  
  # Create a data frame with row names and coefficients
  result_coef <- data.frame(row.names = names(summary_low),
                       low = summary_low,
                       high = summary_high)
  

  
  if(outType == "coef")
  {
    return(result_coef)
  }
  else if(outType == "list")
  {
    #need to add tree sizes. 
    result_model<- data.frame(predict(model_low, data = seq_to_pred))
    colnames(result_model)<- "low"
    result_model$high <- predict(model_high, data = seq_to_pred)
    result_model$tree_sizes <- data_sim$tree_size
    return(result_model)
  }
  
}


#need to more cohesively fit nonlinear models. 


