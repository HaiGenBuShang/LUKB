# LUKB
LUKB is a freely deployable R Shiny-based web tool, which empowers researchers to overcome these hurdles and maximize the potential of UK Biobank data.

First of all, we thank all the authors who contributed to the R shiny or related packages.

This tool is used to prepare your local UK Biobank data to analysis-ready status.

This tool requires R, best with version 4.2.3 or higher and some dependent R packages.

To install R, please refer to https://www.r-project.org/.

To install the dependent R packages, execute: Rscript required_packages.R.

After this you can start the tool, execute: chmod +x add_users.sh && ./add_users.sh
You wiil need to add one user name, password and his data downloading permision.
You need to open the port 1111, execute:

sudo iptables -I INPUT -p tcp --dport 1111 -j ACCEPT (Ubuntu)

or

sudo firewall-cmd --add-port=1111/tcp (CentOS)

Then you can play with the tool by opening the link: http://fill.your.ip.address:1111 with the added user information.
If you want to change the default port 1111, change "port = 1111" to "port = the_port_you_want" in the app.R file.

If you like this tool, please cite:
Xiangnan Li, Shuming Wang, Hui Zhang and Zixin Hu. LUKB: Preparing Local UK Biobank Data for Analysis. https://github.com/HaiGenBuShang/LUKB

If you have any questions, please contact:
xiangnan_li@fudan.edu.cn
