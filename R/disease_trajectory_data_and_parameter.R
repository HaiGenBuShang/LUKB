disease_trajectory_data_and_parameters_UI <- function(id) {
  tagList(
    
    fileInput(NS(id,"medical_history"),label = "Please upload your medical history file"),
    DT::dataTableOutput(NS(id,"preview_tab")),
    
    selectInput(NS(id,"target_disease"),label = "Please choose one disease as target disease",choices = NULL),
    numericInput(NS(id,"min_prevalence"),label = "Minum prevalence to include diseases",value = 0.01),
    numericInput(NS(id,"min_disease_gap"),label = "Minum gap between two diseases",value = 0,min = 0),
    dateInput(NS(id,"start_date"),label = "Please provide the start date for your study"),
    dateInput(NS(id,"end_date"),label = "Please provide the end date for your study"),
    selectInput(NS(id,"variable_for_cox"),label = "Please choose the variables for cox regression to adjust",
                choices = NULL,multiple = TRUE),
    selectInput(NS(id,"variable_for_ccwc"),label = "Please choose the variables for case-control analysis to match",
                choices = NULL,multiple = TRUE),
    numericInput(NS(id,"n_controls"),label = "How many controls for one case?",value = 2,min = 0,step = 1),
    waiter::use_waiter(),
    actionButton(NS(id,"parameters"),label = "Comfirm parameters"),
    verbatimTextOutput(NS(id,"file_prefix")),
    
  )
}

disease_trajectory_data_and_parameters_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    input_dat_preview <- eventReactive(input$medical_history,{
      # req(input$medical_history)
      # browser()
      validate(
        need(input$medical_history,"Please provide input data")
      )
      fread(file = input$medical_history$datapath,header = TRUE,nrows = 100)
    })
    
    
    data_info <- eventReactive(input_dat_preview(),{
      req(input_dat_preview())
      # browser()
      diseases <- fread(file = input$medical_history$datapath,header = TRUE,select = 2) %>% pull(1) %>% unique()
      dates <- fread(file = input$medical_history$datapath,header = TRUE,select = 3) %>% pull(1) %>% unique() %>% as_date()
      col_names <- colnames(input_dat_preview())
      list(diseases=diseases,min_dates=dates %>% min,max_dates=dates %>% max,col_names=col_names)
    })
    
    #*************
    #*update input
    #*************
    observeEvent(data_info(),{
      updateSelectInput(session,"target_disease",label = "Please choose one disease as target disease",choices = data_info()$diseases)
      updateDateInput(session,"start_date",label = "Please provide the start date for your study",value = data_info()$min_dates,
                      min = data_info()$min_dates,max = data_info()$max_dates)
      updateDateInput(session,"end_date",label = "Please provide the end date for your study",value = data_info()$max_dates,
                      min = data_info()$min_dates,max = data_info()$max_dates)
      updateSelectInput(session,"variable_for_cox",label = "Please choose the variables for cox regression to adjust",
                        choices = data_info()$col_names)
      updateSelectInput(session,"variable_for_ccwc",label = "Please choose the variables for case-control analysis to match",
                        choices = data_info()$col_names)
    })
    
    
    #************************
    #*show input data preview
    #************************
    output$preview_tab <- DT::renderDataTable({
      req(input_dat_preview())
      input_dat_preview()
    },options = list(scrollX = TRUE,pageLength = 5))
    
    #*************************
    #*save analysis parameters
    #*************************
    
    #assign file prefix
    file_prefix <- eventReactive(input$parameters,{
      
      validate(
        need(input$medical_history,"Please provide input data")
      )
      
      waiter <- waiter::Waiter$new()
      waiter$show()
      on.exit(waiter$hide())
      
      
      file_prefix <- paste0(format(Sys.time(), "%Y_%m_%d_%H%M%S"),"_",sample(letters,10,replace = TRUE) %>% paste(collapse = ""))
      
      start_date <- input$start_date
      end_date <- input$end_date
      N_control <- input$n_controls
      min_prevalence <- input$min_prevalence
      min_two_disease_gap <- input$min_disease_gap
      variable_for_cox <- input$variable_for_cox
      variable_for_ccwc <- input$variable_for_ccwc
      t_disease <- input$target_disease
      # number_of_core <- input$number_of_cores
      
      data_for_HR <- fread(file = input$medical_history$datapath,header = TRUE) %>% as_tibble()
      
      save(start_date,end_date,N_control,min_prevalence,min_two_disease_gap,variable_for_cox,
           variable_for_ccwc,t_disease,
           # number_of_core,
           file = paste0("Results/",file_prefix,"_parameters.RData"))
      # save(data_for_HR,
      #      file = paste0("Results/",file_prefix,"_input_data.RData"))
      
      system(paste("Rscript utilities/save_input_data.R",input$medical_history$datapath,
                   paste0("Results/",file_prefix,"_input_data.RData"),sep = " "))
      
      
      file_prefix
    })
    
    output$file_prefix <- renderText({
      paste0("Please remember and use this file prefix to track your task:\n",file_prefix())
    })
    
    module_secuss <- file_prefix
  })
}


# ui <- fluidPage(
#   disease_trajectory_data_and_parameters_UI("test")
# )
# 
# server <- function(input, output, session) {
#   disease_trajectory_data_and_parameters_Server("test")
# }
# 
# shinyApp(ui, server)
