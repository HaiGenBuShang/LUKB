options(shiny.maxRequestSize=30*1024^3)

UKB_data_addUI <- function(id) {
  tagList(
    sidebarLayout(
      sidebarPanel(
        shinyjs::useShinyjs(),
        fileInput(NS(id,"UKB_enc"),label = "Choose the \".enc\" file corresponding to your dataset",accept = ".enc"),
        fileInput(NS(id,"UKB_keyfile"),label = "Choose the \".key\" file corresponding to your dataset",accept = ".key"),
        textInput(NS(id,"md5_string"),label = "Type the MD5 checksum corresponding to your dataset"),
        actionButton(NS(id,"Data_prepare"),"Start preparing your data"),
        verbatimTextOutput(NS(id,"no_permisions")),
        verbatimTextOutput(NS(id,"preparing_log")),
        hr(),
        textInput(NS(id,"dataset_file_string"),"Check your dataset preparing status",
                  placeholder = "Please enter the name of the log file:\n"),
        actionButton(NS(id,"check_status"),"Check data preparing status"),
      ),
      mainPanel(
        verbatimTextOutput(NS(id,"data_status")),
      ),
    ),
  )
}


UKB_data_addServer <- function(id,authorised_user,auth_res) {
  moduleServer(id, function(input, output, session) {
    
    user <- reactive({
      reactiveValuesToList(auth_res)$user 
    })
    
    task_info <- reactiveVal()
    observeEvent(input$Data_prepare,{
      req(input$UKB_enc)
      req(input$UKB_keyfile)
      req(input$md5_string)
      
      
      if(user()%in%authorised_user){
        shinyjs::disable("Data_prepare")
        system(paste("mv",input$UKB_enc$datapath,paste0("UKB_data/",input$UKB_enc$name)))
        
        task_info(prepare_main(data_file = paste0("UKB_data/",input$UKB_enc$name),key_file = input$UKB_keyfile$datapath,
                               file_md5 = input$md5_string,
                               log_file=c(paste0("UKB_data/",input$UKB_enc$name) %>% 
                                            str_replace_all("\\.enc","_prepare.log"))))
      }else{
        output$no_permisions <- renderText({"You do not have the permission!"})
      }
      
    })
    
    
    prepare_log_info <- eventReactive(task_info(),{
      
      req(input$UKB_enc)
      req(input$UKB_keyfile)
      req(input$md5_string)
      paste0("Preparing started.\nPlease use this log file to check the dataset preparing status:\n",
             input$UKB_enc$name %>% str_replace_all("\\.enc","_prepare.log"))
    })
    
    output$preparing_log <- renderText(prepare_log_info())
    
    
    observeEvent(input$check_status,{
      if(identical(task_info()$get_exit_status(),as.integer(0)))
        shinyjs::enable("Data_prepare")
    })
    
    Dataset_status_info <- eventReactive(input$check_status,{
      if(identical(task_info()$get_exit_status(),as.integer(0))&
        !system(paste0("grep 'Check and unpack finished!' ",paste0("UKB_data/",input$UKB_enc$name) %>% 
                        str_replace_all("\\.enc","_prepare.log")," > /dev/null"))){
        c("Your dataset has been added!")
      }else{
        system(paste0("cat UKB_data/",input$dataset_file_string %>% str_trim(side = "both") %>% 
                        str_replace_all("\\.enc","_prepare.log")),intern = TRUE) %>% as.data.frame()
      }
    })
    
    output$data_status <- renderPrint(Dataset_status_info())
    
    success_info <- eventReactive(input$check_status,{
      if_else(identical(task_info()$get_exit_status(),as.integer(0))&
                !system(paste0("grep 'Check and unpack finished!' ",paste0("UKB_data/",input$UKB_enc$name) %>% 
                                str_replace_all("\\.enc","_prepare.log")," > /dev/null")),1,0)
    })
    
    success_info
  })
}





