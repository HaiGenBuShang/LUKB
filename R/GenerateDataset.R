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
    textInput(NS(id,"dataset_file_string"),"Check your dataset status",
              placeholder = "Please enter the name of your dataset:\n"),
    actionButton(NS(id,"check_status"),"Check extraction status"),
    verbatimTextOutput(NS(id,"Dataset_status")),
  )
}





GenerateDatasetServer <- function(id,field_data,ukb_basket_file) {
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
    
    date_file <- eventReactive(input$ok,{
      paste0("Results/UKB_",field_data()$FieldID[1],"_",format(Sys.time(), "%Y%m%d_%H%M%S"))
    })
    
    
    
    task_info <- eventReactive(input$ok,{
      shinyjs::disable("generate")
      process_x <- generate_file(UKB_file = paste0("./UKB_data/",ukb_basket_file(),"_ukb"),
                                 UKB_field = field_data()$FieldID,
                                 date_file = date_file())
      #must do this or the command might not reponse
      process_x
    })
    
    observeEvent(task_info(),{
      task_info()$get_exit_status()
    })
    
    Dataset_file <- eventReactive(date_file(),{
      paste0("The name of your dataset will be: ",date_file() %>% str_remove(".*/"),".csv\n",
             "Use your dataset name to check the dataset status.\n",
             "Included fields:\n",paste0(field_data()$field,collapse = ", "))
    })
    
    output$Dataset_file <- renderText({
      Dataset_file()
    })
    
    observeEvent(input$check_status,{
      if(identical(task_info()$get_exit_status(),as.integer(0)))
        shinyjs::enable("generate")
    })
    
    
    Dataset_status_info <- eventReactive(input$check_status,{
      if(!system(paste0("grep 'Output finished' Results/",
                        paste0(input$dataset_file_string %>% str_trim(side = "both") %>% str_remove("\\..*"),
                               ".log > /dev/null")))){
        c("Your dataset has been extracted!","Your data will be deleted in at most 48 hours, proceed in time.")
      }else{
        read.table(paste0("Results/",input$dataset_file_string %>% str_trim(side = "both") %>% str_remove("\\..*"),
                          ".log"),sep = "\n",header = FALSE)
      }
    })
    
    success_info <- eventReactive(input$check_status,{
      if_else(!system(paste0("grep 'Output finished' Results/",
                             paste0(input$dataset_file_string %>% str_trim(side = "both") %>% str_remove("\\..*"),
                                    ".log > /dev/null"))),1,0)
    })
    
    output$Dataset_status <- renderPrint(Dataset_status_info())
    
    file_to_be_downloaded <- eventReactive(input$check_status,{
      paste0("Results/",input$dataset_file_string)
    })
    
    reactiveValues(success_info=success_info,file_to_be_downloaded=file_to_be_downloaded)
    
  })
}


