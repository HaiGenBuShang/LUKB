DatadownloadUI <- function(id) {
  tagList(
    h5("Please note:\nThe downloads of your account will be recorded.\n"),
    uiOutput(NS(id,"choose_file_ui")),
    textOutput(NS(id,"downloaded_file")),
    downloadButton(NS(id,"download"),"Download"),
    Data_shareUI(NS(id,"data_sharing")),
  )
}

DatadownloadServer <- function(id,auth_user,auth_info,success_info) {
  moduleServer(id, function(input, output, session) {
    user <- reactive({
      reactiveValuesToList(auth_info)$user 
    })
    
    ui <- reactive({
      req(user())
      ns <- session$ns
      selectInput(ns("choosed_file"),"Choose your data file to download",
                  choices = finished_data_extraction(data_dir = "Results/",user = user()))
    })
    
    output$choose_file_ui <- renderUI({
      ui()
    })
    
    observeEvent(success_info(),{
      req(success_info())
      if(success_info()==1)
        updateSelectInput(session,"choosed_file","Choose your data file to download",
                          choices = finished_data_extraction(data_dir = "Results/",user = user()))
    })
    
    file_to_be_downloaded <- eventReactive(input$choosed_file,{
      paste0("Results/",input$choosed_file)
    })
    
    output$downloaded_file <- renderText({
      paste0("Download file: ",file_to_be_downloaded() %>% str_remove("Results/")) 
    })
    
    sharing_info <- Data_shareServer("data_sharing",shared_file = file_to_be_downloaded)
    output$download <- download_file(file_name = file_to_be_downloaded,user = user,authorised_user = auth_user)
    sharing_info
  })
}
