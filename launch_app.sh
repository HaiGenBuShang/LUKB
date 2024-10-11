#!/bin/bash

echo "01 01 * * * cd $(pwd)/Results && find . -name \"*\" -mtime +1 -type f -exec rm -rf {} \;" > remove_file.crontab

crontab remove_file.crontab #remove old file

kill -9 `cat Running_logs/LUKB_shiny_pid.txt 2> /dev/null` > /dev/null 2>&1 #stop last running
sed -i '$d' app.R

echo 'shinyApp(ui, server,options = list(host = "0.0.0.0",port = 1111))' >> app.R #the default port for LUKB is 1111

nohup R -e "shiny::runApp('./')" > Running_logs/LUKB_shiny.log 2>&1 & #run app
echo $! > Running_logs/LUKB_shiny_pid.txt


rm remove_file.crontab
