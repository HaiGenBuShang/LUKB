library(shiny)
library(tidyverse)
library(data.table)
library(rvest)
library(ukbwranglr)

library(shinymanager)

inactivity <- "function idleTimer() {
var t = setTimeout(logout, 120000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"

credentials <- read.table("../UKB_accounts//accounts",header = TRUE,sep = "\t",stringsAsFactors = FALSE)
authorised_user <- credentials %>% filter(authorised==TRUE) %>% pull(user)

UKB_data_dict <- get_ukb_data_dict(path = "UKB_data/Data_Dictionary_Showcase.tsv")
UKB_codings <- get_ukb_codings("UKB_data/Codings.tsv")



ui <- fluidPage(
  titlePanel("Welcome to explore UKB data!"),
  tabsetPanel(
    tabPanel("Data Extraction",
             DatasetUI("Dataset")),
    tabPanel("Data Mapping",
             DataCleaningUI("Datacleaning")),
    tabPanel("Add Dataset",
             UKB_data_addUI("Add_basket")),
    tabPanel("Shared Data",
             View_shared_filesUI("Shared_data")),
  ),
)

ui <- secure_app(
  ui
)





server <- function(input, output, session) {
  auth_res <- secure_server(check_credentials = check_credentials(credentials),timeout = 30,keep_token = TRUE)
  
  basket_success <- UKB_data_addServer("Add_basket",authorised_user = authorised_user,auth_res = auth_res)
  
  data_success <- DatasetServer("Dataset",authorised_user = authorised_user,auth_res = auth_res,
                                success_info = basket_success)
  DataCleaningServer("Datacleaning",authorised_user = authorised_user,auth_info = auth_res,success_info = data_success,
                     UKB_data_dict = UKB_data_dict,UKB_codings=UKB_codings)
  
  View_shared_filesServer("Shared_data",auth_info = auth_res,authorised_user = authorised_user)
  
}


shinyApp(ui, server,options = list(host = "0.0.0.0",port = 1111))