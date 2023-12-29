mkdir ../UKB_accounts -p

if [ ! -a ../UKB_accounts/accounts ]; then
	printf "user\tpassword\tauthorised\n" > ../UKB_accounts/accounts 
fi

read -p "Type the user name: " user_name
read -p "Type the password: " passwd
read -p "Give data donwload permision? [TRUE/FALSE] " permission

printf "${user_name}\t${passwd}\t${permission}\n" >> ../UKB_accounts/accounts

chmod 600 ../UKB_accounts/accounts

kill -9 `cat Running_logs/UKB_shiny_pid.txt 2> /dev/null` > /dev/null 2>&1
chmod +x launch_app.sh
./launch_app.sh
