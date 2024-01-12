modal_confirm <- function(session){
  ns <- session$ns
  modalDialog(
    HTML("You can only generate one dataset one time,<br>which means you have to generate dataset one by one.<br>
         Please double check your fields."),
    title = "Generate dataset",
    footer = tagList(
      actionButton(ns("cancel"), "Go back to double check"),
      actionButton(ns("ok"), "I want to continue", class = "btn btn-danger"),
    )
  )
}





GenerateDatasetUI <- function(id) {
  tagList(
    shinyjs::useShinyjs(),
    actionButton(NS(id,"generate"),"Extract data!"),
    verbatimTextOutput(NS(id,"Dataset_file")),
    hr(),
    
    ShowCurrentJobUI(NS(id,"current_job")),
    
  )
}





GenerateDatasetServer <- function(id,field_data,ukb_basket_file,auth_info) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$generate,{
      showModal(modal_confirm(session))
    })
    
    observeEvent(input$cancel, {
      removeModal()
    })
    
    observeEvent(input$ok,{
      showNotification("Fields submitted!")
      removeModal()
    })
    
    user <- reactive({
      reactiveValuesToList(auth_info)$user 
    })
    
    date_file <- eventReactive(input$ok,{
      paste0("Results/UKB_",user(),"_",field_data()$FieldID[1],"_",sys_time())
    })
    
    observeEvent(date_file(),{
      shinyjs::disable("generate")
      generate_file(UKB_file = paste0("./UKB_data/",ukb_basket_file(),"_ukb"),
                    UKB_field = field_data()$FieldID,
                    date_file = date_file())
    })
    
    Dataset_file <- eventReactive(date_file(),{
      paste0("The name of your dataset will be: ",date_file() %>% str_remove(".*/"),".csv\n",
             "Use your dataset name to check the dataset status.\n",
             "Included fields:\n",paste0(field_data()$field,collapse = ", "))
    })
    
    observeEvent(Dataset_file(),{
      system(paste0("echo '",Dataset_file(),"' > ","Results/",user(),"_extraction_file_info.log"))
    })
    
    
    file_to_be_downloaded <- eventReactive(date_file(),{
      paste0("Results/",date_file() %>% str_remove(".*/"),".csv")
    })
    
    success_info <- ShowCurrentJobServer("current_job",user = user)
    
    observeEvent(success_info(),{
      if(success_info()==1){
        shinyjs::enable("generate")
      }else{
        shinyjs::disable("generate")
      }
    })
    
    reactiveValues(success_info=success_info,file_to_be_downloaded=file_to_be_downloaded)

  })
}


