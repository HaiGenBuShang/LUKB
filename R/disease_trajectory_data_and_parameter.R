disease_trajectory_data_and_parameters_UI <- function(id) {
  tagList(
    
    fileInput(NS(id,"medical_history"),label = "Please upload your medical history file"),
    DT::dataTableOutput(NS(id,"preview_tab")),
    hr(),
    
    sidebarLayout(
      sidebarPanel(
        selectInput(NS(id,"target_disease"),label = "Please choose one disease as target disease",choices = NULL),
        dateInput(NS(id,"start_date"),label = "Please provide the start date for your study"),
        dateInput(NS(id,"end_date"),label = "Please provide the end date for your study"),
        numericInput(NS(id,"min_prevalence"),label = "Minum prevalence to include diseases",value = 0.01),
        numericInput(NS(id,"min_disease_gap"),label = "Minum gap between two diseases",value = 0,min = 0),
        selectInput(NS(id,"variable_for_cox"),label = "Please choose the variables for cox regression to adjust",
                    choices = NULL,multiple = TRUE),
        selectInput(NS(id,"variable_for_ccwc"),label = "Please choose the variables for case-control analysis to match",
                    choices = NULL,multiple = TRUE),
        numericInput(NS(id,"n_controls"),label = "How many controls for one case?",value = 2,min = 0,step = 1),
        waiter::use_waiter(),
        
        fluidRow(
          column(width = 4,actionButton(NS(id,"preview"),label="Preview"),align="left"),
          column(width = 8,actionButton(NS(id,"parameters"),label = "Comfirm parameters"),align="right"),
        ),
        
        verbatimTextOutput(NS(id,"file_prefix")),
        width = 4
      ),
      mainPanel(
        fluidRow(
          column(width = 6,plotOutput(NS(id,"target_disease_num"))),
          column(width = 6,plotOutput(NS(id,"include_diseases"))),
        ),
        fluidRow(
          column(width = 6,plotOutput(NS(id,"disease_per_ind"))),
          column(width = 6,plotOutput(NS(id,"case_control_num"))),
        )
      )
    )
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
    
    
    
    #***************************
    #*Preview the data structure
    #***************************
    
    data_preview <- eventReactive(input$preview,{
      validate(
        need(input$medical_history,"Please provide input data")
      )
      
      waiter <- waiter::Waiter$new()
      waiter$show()
      on.exit(waiter$hide())
      
      start_date <- as_date(input$start_date)
      end_date <- as_date(input$end_date)
      N_control <- input$n_controls
      min_prevalence <- input$min_prevalence
      min_two_disease_gap <- input$min_disease_gap
      variable_for_cox <- input$variable_for_cox
      variable_for_ccwc <- input$variable_for_ccwc
      t_disease <- input$target_disease
      
      data_for_HR <- fread(file = input$medical_history$datapath,header = TRUE) %>% as_tibble() %>% 
        rename(eid=1,pid=2,value=3) %>% mutate(value=as_date(value))
      
      disease_num <- target_disease_number(dat = data_for_HR,target_disease = t_disease,start_date = start_date,end_date = end_date)
      include_diseases <- include_disease(dat = data_for_HR,min_prevalence = min_prevalence)
      diseases_per_ind <- individual_disease_counted(dat = data_for_HR,start_date = start_date,end_date = end_date,
                                                     min_disease_gap = min_two_disease_gap,target_disease = t_disease)
      cc_num <- case_control_num(dat = data_for_HR,target_disease = t_disease,start_date = start_date,end_date = end_date,
                                 n_controls = N_control)
      
      list(disease_num=disease_num,include_diseases=include_diseases,diseases_per_ind=diseases_per_ind,cc_num=cc_num,
           data_for_HR=data_for_HR)
      
    })
    
    output$target_disease_num <- renderPlot({
      req(data_preview())
      data_preview()$disease_num
    },res = 144)
    
    
    # output$target_disease_num <- renderImage({
    #   
    #   # browser()
    #   # width  <- session$clientData$output_target_disease_num_width
    #   # height <- session$clientData$output_target_disease_num_height
    #   width  <- 275.3281
    #   height <- 400
    #   # A temp file to save the output.
    #   outfile <- tempfile(fileext='.png')
    #   
    #   png(outfile, width=width, height=height)
    #   data_preview()$include_diseases
    #   dev.off()
    #   
    #   list(src = outfile,
    #        width = width,
    #        height = height,
    #        alt = "This is alternate text")
    #   
    # },deleteFile = TRUE)
    
    
    output$include_diseases <- renderPlot({
      req(data_preview())
      data_preview()$include_diseases
    },res = 144)
    
    output$disease_per_ind <- renderPlot({
      req(data_preview())
      data_preview()$diseases_per_ind
    },res = 144)
    
    output$case_control_num <- renderPlot({
      req(data_preview())
      data_preview()$cc_num
    },res = 144)
    
    
    
    #*************************
    #*save analysis parameters
    #*************************
    
    #assign file prefix
    file_prefix <- eventReactive(input$parameters,{
      
      validate(
        need(input$preview,"Please preview your data first")
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
      
      data_for_HR <- data_preview()$data_for_HR
      
      save(start_date,end_date,N_control,min_prevalence,min_two_disease_gap,variable_for_cox,
           variable_for_ccwc,t_disease,
           file = paste0("Results/",file_prefix,"_parameters.RData"))
      
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
