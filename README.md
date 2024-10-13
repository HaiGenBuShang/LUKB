# LUKB

LUKB is a freely deployable R Shiny-based web tool, which empowers researchers to prepare UK Biobank data efficiently, and thus maximizing the potential of UK Biobank data.

First of all, we thank all the authors who contributed to the R shiny or related packages.

This tool is used to prepare your local UK Biobank data to analysis-ready status.

## Dependencies

This tool requires R, best with version 4.2.3 or higher and some dependent R packages.

To install R, please refer to https://www.r-project.org/.

To install the dependent R packages, execute: Rscript required_packages.R. 

To run LUKB in ***https*** mode, **Nginx**, **Shiny Server** and a **SSL certificate** are required. 

To install **Nginx**, execute:

sudo apt install nginx (Ubuntu)

or

sudo yum install nginx (CentOS)

To install **Shiny Server**, please refer to https://posit.co/download/shiny-server/.

To get a **SSL certificate** for a local IP address, execute:

sudo openssl req -x509 -nodes -days 365 -newkey rsa:2048 -keyout path_to_store_the.key -out path_to_store_the_certificate.crt.

You will need to manually install *openssl* if it is not installed along with Nginx.

## Configure Nginx (Strongly Recommended)

If you want to run LUKB in ***https*** mode, which we **strongly** recommend, especially if you deploy LUKB through the internet. 

Firstly, you should modify the configuration file nginx_conf/shiny_server.conf.

You should replace the IP address with your IP address or domain on line 3 and 12 of the file. If you would like Shiny Server run in a different port, replace the port 3838 to the port you like.

After this, you need to copy the nginx_conf/shiny_server.conf to the directory that contain the configuration files for Nginx. The default directory is /etc/nginx/sites-enabled/ (Ubuntu) and /etc/nginx/conf.d/ (CentOS).

Then you should test the configuration file and restart Nginx by excuting:

sudo nginx -t && systemctl restart nginx.service


## Add Users and Start LUKB
You can start the tool, execute: chmod +x add_users.sh && ./add_users.sh
You wiil need to add one user name, password and his data downloading permision.

With Nginx and Shiny Server installed and configured correctly, ensure that the port used by Shiny Sever is allowed through your firewall. To allow access to the default Shiny Server port (3838) on Ubuntu, execute the fellowing:

sudo iptables -I INPUT -p tcp --dport 3838 -j ACCEPT (Ubuntu)

This step is not needed for CentOS. Then you can access LUKB via https://your_server_ip/LUKB/.

If Nginx and Shiny Server are not being used, the default port used by LUKB (1111) should be added to the firewall rules. To allow access to port 1111, execute:

sudo iptables -I INPUT -p tcp --dport 1111 -j ACCEPT (Ubuntu)

or

sudo firewall-cmd --add-port=1111/tcp (CentOS)

Then you can play with the tool by opening the link: http://fill.your.ip.address:1111 with the added user information.
If you want to change the default port 1111, change "port = 1111" to "port = the_port_you_want" in the app.R file.

If you like this tool, please cite:

Xiangnan Li, Yaqi Huang, Shuming Wang, Meng Hao, Yi Li, Hui Zhang and Zixin Hu. LUKB: Preparing Local UK Biobank Data for Analysis. https://github.com/HaiGenBuShang/LUKB

If you have any questions, please contact:
xiangnan_li@fudan.edu.cn
