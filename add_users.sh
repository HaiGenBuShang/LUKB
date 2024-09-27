mkdir ../UKB_accounts -p

if [ ! -f ../UKB_accounts/accounts ]; then
        printf "user\tpassword\tauthorised\n" > ../UKB_accounts/accounts
fi

while :
do
        read -p "user name (max length 25): " user_name
        [ ${#user_name} -le 25 ] && [ -n "${user_name}" ] && break
done

read -p "Type the password: " passwd
read -p "Give data donwload permision? [TRUE/FALSE] " permission

stored_passwd=$(Rscript utilities/password_storing.R ${passwd})

printf "${user_name}\t${stored_passwd}\t${permission}\n" >> ../UKB_accounts/accounts

chmod 600 ../UKB_accounts/accounts

which shiny-server > /dev/null 2>&1

if [ $? -eq 0 ]; then
	echo "LUKB will start within Shiny Server and can be accessed by the link https://your_ip:3838/LUKB or the link configured in the Nginx server"
	chmod +x launch_app_shiny_server.sh
	./launch_app_shiny_server.sh
else
	echo "LUKB will start directory, and can be accessed by the link http://your_ip:1111 or http://your_ip:the_port_you_point"
	chmod +x launch_app.sh
	./launch_app.sh
fi


