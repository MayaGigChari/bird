
#check environmental variables for a job ID
JobID<- (Sys.getenv(x = "JOB_ID"))

array_outputs<- paste(JobID, "array_runs", sep  = "_")
stats<-paste(JobID, "statistics", sep = "_")


#here I will check to see if there already exists a file with that JOBID, else create one. 
if (file.exists(array_outputs)) {

 cat("The folder already exists")

} else {

 dir.create(array_outputs)

}
if (file.exists(stats)) {

 cat("The folder already exists")

} else {

 dir.create(stats)

}


