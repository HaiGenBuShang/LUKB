#/bin/bash

echo "01 01 * * * cd $(pwd)/Results && find . -name \"*\" -mtime +1 -type f -exec rm -rf {} \;" > remove_file.crontab

crontab remove_file.crontab #remove old file

kill -9 `cat Running_logs/UKB_shiny_pid.txt 2> /dev/null` > /dev/null 2>&1 #stop last running
nohup R -e "shiny::runApp('./')" > Running_logs/UKB_shiny.log 2>&1 & #run app
echo $! > Running_logs/UKB_shiny_pid.txt


rm remove_file.crontab
