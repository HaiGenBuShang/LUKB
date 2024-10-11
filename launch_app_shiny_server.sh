#!/bin/bash

echo "01 01 * * * cd $(pwd)/Results && find . -name \"*\" -mtime +1 -type f -exec rm -rf {} \;" > remove_file.crontab

crontab remove_file.crontab #remove old file

kill -9 `cat Running_logs/LUKB_shiny_server_pid.txt 2> /dev/null` > /dev/null 2>&1 #stop last running 
sed -i "s/run_as.*/run_as\ $USER;/g" shiny_server_conf/shiny-server.conf #Use current user to run the shiny_server
mkdir -p shiny_app
cd shiny_app

rm -f ./LUKB
current_dir=$(pwd)
ln -s ${current_dir%/*} ./LUKB
cd ..

rm -f Running_logs/LUKB-*-*log

sed -i '$d' app.R
echo "shinyApp(ui, server)" >> app.R

nohup shiny-server shiny_server_conf/shiny-server.conf > Running_logs/LUKB_shiny_server.log 2>&1 & #run app through shiny-server
echo $! > Running_logs/LUKB_shiny_server_pid.txt

rm remove_file.crontab
