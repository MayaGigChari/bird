#!/bin/bash
#$ -cwd #uses current working directory
# error = Merged with joblog
#$ -o /u/home/m/mchari/bird/joblog_$JOB_ID/joblog.$JOB_ID.$TASK_ID #creates a file called joblog.jobidnumber.taskidnumber to write to.
#$ -j y
#$ -l h_rt=0:30:00,h_data=2G #requests 30 minutes, 2GB of data (per core)
#$ -pe shared 2 #requests 2 cores
# Email address to notify
#$ -M $USER@mail #don't change this line, finds your email in the system
# Notify when
##$ -m bea #sends you an email (b) when the job begins (e) when job ends (a) when job is aborted (error)
#$ -t 1:17200 # with step size of 1, each node runs 5 randomizations.

#sleep $(( RANDOM % 250))  #added this line to try to stagger jobs. currently  commented out. 

# load the job environment:. 
. /u/local/Modules/default/init/modules.sh
module load R


echo ${SGE_TASK_ID}

# run julia code
echo Running phylo_code for n = ${SGE_TASK_ID} #prints this quote to joblog.jobidnumber
Rscript Hoffman_parallel_option2.R ${SGE_TASK_ID}  > /u/home/m/mchari/bird/joblog_$JOB_ID/output.$JOB_ID.${SGE_TASK_ID}






