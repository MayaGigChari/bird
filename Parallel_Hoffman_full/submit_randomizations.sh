#!/bin/bash
# Get the directory of the script
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Construct directory paths relative to the script location
JOB_LOG_DIR="$SCRIPT_DIR/joblog_$JOB_ID"
OUTPUT_DIR="$SCRIPT_DIR/joblog_$JOB_ID/output.$JOB_ID"

#$ -cwd
# error = Merged with joblog
#$ -o $JOB_LOG_DIR/joblog.$JOB_ID.$TASK_ID
#$ -j y
#$ -l h_rt=0:30:00,h_data=2G
#$ -pe shared 2
# Email address to notify
#$ -M $USER@mail
# Notify when
##$ -m bea
#$ -t 1:240

#!/bin/bash
# Get the directory of the script
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Construct directory paths relative to the script location
JOB_LOG_DIR="$SCRIPT_DIR/joblog_$JOB_ID"
OUTPUT_DIR="$SCRIPT_DIR/joblog_$JOB_ID/output.$JOB_ID"

#$ -cwd
# error = Merged with joblog
#$ -o $JOB_LOG_DIR/joblog.$JOB_ID.$TASK_ID
#$ -j y
#$ -l h_rt=0:30:00,h_data=2G
#$ -pe shared 2
# Email address to notify
#$ -M $USER@mail
# Notify when
##$ -m bea
#$ -t 1:240

#sleep $(( RANDOM % 250))

# load the job environment
. /u/local/Modules/default/init/modules.sh
module load R

echo ${SGE_TASK_ID}

# run julia code
echo Running phylo_code for n = ${SGE_TASK_ID}
Rscript Hoffman_parallel_bird.R ${SGE_TASK_ID} > $OUTPUT_DIR.${SGE_TASK_ID}



