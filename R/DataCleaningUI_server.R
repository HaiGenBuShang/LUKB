DataCleaningUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        fileInput(NS(id,"up_file"),"Upload your dataset"),
        selectInput(NS(id,"choosed_file"),"Or Choose your dataset",
                    choices = list.files("Results/",pattern = "[0-9]\\.csv")),
        waiter::use_waiter(),
        actionButton(NS(id,"mapping"),"Data map"),
        verbatimTextOutput(NS(id,"mapping_status")),
      ),
      mainPanel(
        data_previewUI(NS(id,"preview_mapped")),
      ),
    ),
    h5("Please note:\nDownloads of your account will be recorded.\n"),
    textOutput(NS(id,"mapped_file")),
    downloadButton(NS(id,"download_mapped"),"Download"),
    Data_shareUI(NS(id,"mapped_data_sharing")),
    
    hr(),
    h3("Ignore This If the Mapped Dataset Satisfies You."),
    sidebarLayout(
      sidebarPanel(
        Code_remappingUI(NS(id,"remapping")),
      ),
      mainPanel(
        data_previewUI(NS(id,"preview_remapping")),
      ),
    ),
    h5("Please note:\nDownloads of your account will be recorded.\n"),
    textOutput(NS(id,"remapped_file")),
    downloadButton(NS(id,"download_remapped"),"Download"),
    Data_shareUI(NS(id,"remapped_data_sharing")),
  )
}

DataCleaningServer <- function(id,auth_info,authorised_user,success_info,UKB_data_dict,UKB_codings) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(session$clientData,{
      updateSelectInput(session,"choosed_file","Or Choose your dataset",
                        choices = list.files("Results/",pattern = "[0-9]\\.csv"))
    })
    
    observeEvent(success_info(),{
      req(success_info)
      if(success_info()==1)
        updateSelectInput(session,"choosed_file","Or Choose your dataset",
                          choices = list.files("Results/",pattern = "[0-9]\\.csv"))
    })
    
    
    dat_file_for_cleaning <- reactive({
      if(input$up_file %>% is.null()){
        req(input$choosed_file)
        data.frame(name=input$choosed_file,datapath=paste0("Results/",input$choosed_file))
      }else{
        req(input$up_file)
        input$up_file
      } 
    })
    
    
    format_pass <- eventReactive(input$mapping,{
      if(!check_file_format(file = dat_file_for_cleaning()$datapath)){
        validate("File did not contain correct header!")
      }else{
        dat_file_for_cleaning
      }
    })
    
    observeEvent(format_pass(),{
      
      
      waiter <- waiter::Waiter$new()
      waiter$show()
      on.exit(waiter$hide())
      
      mapped_dat <- read_ukb(path = dat_file_for_cleaning()$datapath,delim = ",",
                             ukb_data_dict = UKB_data_dict,
                             ukb_codings = UKB_codings) %>% as_tibble()
      write.table(mapped_dat,file = paste0("Results/",dat_file_for_cleaning()$name %>% 
                                             str_remove(".csv") %>% str_c("_mapped.csv")),
                  col.names = TRUE,row.names = FALSE,sep = ",")
    })
    
    mapped_file <- eventReactive(format_pass(),{
      req(format_pass)
      paste0("Results/",dat_file_for_cleaning()$name %>% 
               str_remove(".csv") %>% str_c("_mapped.csv"))
    })
    
    mapping_status <- eventReactive(mapped_file(),{
      "Cleaning Complete!\nYour Dataset will be deleted in at most 48 hours, proceed in time."
    })
    output$mapping_status <- renderText(mapping_status())
    
    user <- reactive({
      reactiveValuesToList(auth_info)$user 
    })
    
    output$mapped_file <- renderText({
      paste0("Download file: ",mapped_file() %>% str_remove("Results/")) 
    })
    output$download_mapped <- download_file(file_name = mapped_file,user = user,authorised_user = authorised_user)
    
    ####
    Data_shareServer("mapped_data_sharing",shared_file = mapped_file)
    ####
    
    data_previewServer("preview_mapped",preview_files = mapped_file,preview_text="Data preview")
    
    remapped_file <- Code_remappingServer("remapping",remapping_file=mapped_file)
    
    data_previewServer("preview_remapping",preview_files = remapped_file,preview_text="Data preview")
    
    output$remapped_file <- renderText({
      paste0("Download file: ",remapped_file() %>% str_remove("Results/")) 
    })
    
    ####
    Data_shareServer("remapped_data_sharing",shared_file = remapped_file)
    ####
    output$download_remapped <- download_file(file_name = remapped_file,user = user,authorised_user = authorised_user)
    
  })
}
