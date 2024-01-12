library(tidyverse)
library(shiny)

ShowCurrentJobUI <- function(id) {
  tagList(
    h5(strong("Your current data extraction file")),
    verbatimTextOutput(NS(id,"extraction_file")),
    h5(strong("Status")),
    verbatimTextOutput(NS(id,"extraction_status")),
  )
}

ShowCurrentJobServer <- function(id,user) {
  moduleServer(id, function(input, output, session) {
    log_file <- eventReactive(user(),{
      req(user())
      paste0("Results/",user(),"_extraction_file_info.log")
    })
    
    log_file_status <-reactive({
      on.exit(invalidateLater(5000),add = TRUE)
      file.exists(log_file())
    })
    
    extraction_info <- eventReactive(log_file_status(),{
      if(log_file_status()){
        system(paste0("cat ",log_file()),intern = TRUE) %>% str_c(collapse = "\n")
      }else{
        "You currently have no data extraction job.\nYou can extract data now."
      }
    })
    
    output$extraction_file <- renderText(extraction_info())
    
    file_to_be_downloaded <- eventReactive(extraction_info(),{
      if(extraction_info()=="You currently have no data extraction job.\nYou can extract data now."){
        validate()
      }else{
        paste0("Results/",extraction_info() %>% str_replace_all(paste0(".*(UKB_",user(),"_.*\\.csv)\n.*\n.*\n.*"),"\\1"))
      }
    })
    
    extraction_dat <- reactive({
      req(file_to_be_downloaded(),cancelOutput = TRUE)
      reactiveFileReader(5000,session,
                         file_to_be_downloaded() %>% str_replace_all("\\.csv",".log"),
                         read_delim,col_names = FALSE,delim="\n",show_col_types = FALSE,progress=FALSE,quote="")
    })
    
    
    extraction_status <- eventReactive(extraction_dat()(),{
      extraction_dat()()
    })
    
    out_extraction_status <- eventReactive(extraction_status(),{
      if(extraction_status() %>% nrow() == 0){
        ""
      }else if(extraction_status() %>% tail(n=1) %>% t() %>% str_detect("Output finished")){
        paste0(file_to_be_downloaded() %>% str_remove_all(".*/")," finished!")
      }else{
        extraction_status()
      }
    })

    output$extraction_status <- renderPrint({
      req(out_extraction_status())
      if(is.character(out_extraction_status())){
        out_extraction_status()
      }else{
        out_extraction_status() %>% print.AsIs()
      }
    })

    success_info <- eventReactive(extraction_status(),{
      if((out_extraction_status() %>% tail(n=1) %>% as.matrix())[1] %>% str_detect("finished!")){
        system(paste0("rm ",log_file()),ignore.stdout = TRUE,ignore.stderr = TRUE)
        1
      }else{
        0
      }
    })
    
    success_info
    
  })
}


# ui <- fluidPage(
#   ShowCurrentJobUI("test"),
# )
# 
# server <- function(input,output,session){
#   x <- ShowCurrentJobServer("test",user = reactive("abc"),file_to_be_downloaded = reactive("../Results/abc.csv"))
#   observeEvent(x(),{
#     print(x())
#     print(Sys.time())
#   })
# }
# 
# shinyApp(ui,server)













