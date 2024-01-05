View_shared_filesUI <- function(id) {
  tagList(
    sidebarLayout(
      sidebarPanel(
        selectInput(NS(id,"shared_files"),"Select one shared data file to preview",
                    choices = list.files("Shared_data/",pattern = "csv")),
        "Data file Remarks",
        verbatimTextOutput(NS(id,"shared_remarks")),
        hr(),
        h5("Please note:\nThe downloads of your account will be recorded.\n"),
        downloadButton(NS(id,"download"),"Download This file!")
      ),
      mainPanel(
        data_previewUI(NS(id,"preview_shared")),
      )
    ),
  )
}

View_shared_filesServer <- function(id,auth_info,authorised_user,sharing_success_info) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(session$clientData,{
      updateSelectInput(session,"shared_files","Select one shared data file to preview",
                        choices = list.files("Shared_data/",pattern = "csv"))
    })
    
    observeEvent(sharing_success_info(),{
      req(sharing_success_info())
      if(sum(sharing_success_info())>0)
        updateSelectInput(session,"shared_files","Select one shared data file to preview",
                          choices = list.files("Shared_data/",pattern = "csv"))
    })
    
    user <- reactive({
      reactiveValuesToList(auth_info)$user 
    })
    
    files_to_download <- reactive({
      req(input$shared_files)
      paste0("Shared_data/",input$shared_files)
    })
    
    shared_remarks <- reactive({
      req(input$shared_files)
      system(paste0("cat Shared_data/",input$shared_files %>% str_replace_all("\\.csv",".remark")),intern = TRUE)
    })
    
    output$shared_remarks <- renderText(shared_remarks())
    
    data_previewServer(id = "preview_shared",
                       preview_files = files_to_download,preview_text="Data preview")
    
    output$download <- download_file(file_name = files_to_download,user = user,authorised_user = authorised_user)
    
  })
}
