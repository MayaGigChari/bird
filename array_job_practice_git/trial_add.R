string_wd = getwd()

jobID = Sys.getenv(x = "JOB_ID")

string_wd_mod = paste(getwd(),"/", jobID,"_array_runs", sep = "")

setwd(string_wd_mod)


fileConn<-file("output.txt")
writeLines(c("Hello","World"), fileConn)
close(fileConn)
