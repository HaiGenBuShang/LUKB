library(data.table)
library(ukbtools)

options(shiny.maxRequestSize=30*1024^3)

ICD_summary_UI <- function(id) {
  tagList(
    fluidRow(
      column(6, align = "left",
             selectInput(NS(id,"choosed_file"),"Or Choose your mapped data file",
                         choices = list.files("Results/",pattern = "mapped\\.csv") %>%
                           str_subset("preview",negate = TRUE),width = "100%")),
      column(6, align = "left",
             fileInput(NS(id,"up_file"),"Upload your extracted data after code mapping file",width = "100%")),
    ),
    tagList(
      waiter::use_waiter(),
      h3("The file you are working with is:\n",strong(textOutput(NS(id,"file_name")))),
      sidebarLayout(
        sidebarPanel(
          h4(strong("Disease case counting")),
          uiOutput(NS(id,"ICD_codes_length")), #ICD code length should define in this UI
          
        ),
        mainPanel(
          DT::dataTableOutput(NS(id,"disease_count")),
        )
      ),
      sidebarLayout(
        sidebarPanel(
          h4(strong("Disease prevalence")),
          uiOutput(NS(id,"dis_prevlence")), #ICD code length should define in this UI
        ),
        mainPanel(
          DT::dataTableOutput(NS(id,"disease_prevalence")),
        )
      ),
      sidebarLayout(
        sidebarPanel(
          h4(strong("Prevalence figure")),
          uiOutput(NS(id,"ICD_plots")),
          
        ),
        mainPanel(
          plotOutput(NS(id,"freq_plot")),
        )
      ),
    )
  )
  
}

ICD_summary_Server <- function(id,success_info) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    observeEvent(session$clientData,{
      updateSelectInput(session,"choosed_file","Or Choose your mapped data file",
                        choices = list.files("Results/",pattern = "mapped\\.csv") %>% str_subset("preview",negate = TRUE))
    })
    
    observeEvent(success_info(),{
      req(success_info)
      if(success_info()==1)
        updateSelectInput(session,"choosed_file","Or Choose your mapped data file",
                          choices = list.files("Results/",pattern = "mapped\\.csv") %>% str_subset("preview",negate = TRUE) )
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
    
    
    all_fields <- eventReactive(dat_file_for_cleaning(),{
      fread(dat_file_for_cleaning()$datapath,nrows = 2) %>% colnames()
    })
    
    
    
    file_pass <- eventReactive(all_fields(),{
      if(!any(all_fields() %>% str_detect(paste("^diagnoses.*icd", 10, sep = "")))){
        validate(paste0("This file: ",dat_file_for_cleaning()$name," do not contain ICD10 data!"))
      } else {
        dat_file_for_cleaning()
      }
    })
    
    file_name <- eventReactive(file_pass(),{
      
      paste0(file_pass()$name)
    })
    output$file_name <- renderText(file_name())
    
    
    #*************************
    #For Disease case counting
    #*************************
    
    ICD_codes_length <- eventReactive(file_pass(),{
      tagList(
        sliderInput(ns("n_codes"),"Select ICD Code Length",min = 1,max = 5,value = 1),
        selectInput(ns("decreasing"),"Table order",choices = c("Decreasing","Increasing"),multiple = FALSE),
        actionButton(ns("counting"),"Count"),
      )
    })
    
    output$ICD_codes_length <- renderUI(ICD_codes_length())
    
    UKB_ICD_dat <- eventReactive(file_pass(),{
      
      waiter <- waiter::Waiter$new()
      waiter$show()
      on.exit(waiter$hide())
      
      # fread(file_pass()$datapath) %>% dplyr::select(eid,matches(paste("^diagnoses.*icd", 10, sep = "")))
      res <- fread(file_pass()$datapath)
      
      res
      
    })
    
    
    dis_long_tab <- eventReactive(UKB_ICD_dat(),{
      
      waiter <- waiter::Waiter$new()
      waiter$show()
      on.exit(waiter$hide())
      
      res <- UKB_ICD_dat() %>% dplyr::select(eid,matches(paste("^diagnoses.*icd", 10, sep = ""))) %>% 
        pivot_longer(cols = -1,values_drop_na = TRUE) %>% filter(value!="") %>% distinct(eid,value)
      
      res
      
    })
    
    dis_cont_tab <- eventReactive(input$counting,{
      
      req(input$n_codes)
      dis_long_tab() %>% 
        mutate(n_codes=str_sub(value,1,input$n_codes)) %>% count(n_codes) %>% 
        rename(ICD_codes=1) %>% 
        arrange(ifelse(input$decreasing=="Decreasing",-1,1)*n)
    })
    
    
    output$disease_count <- DT::renderDT({
      req(dis_cont_tab())
      dis_cont_tab()
    })
    
    
    #**********************
    #For Disease prevalence
    #**********************
    
    dis_prevlence <- eventReactive(file_pass(),{
      tagList(
        textAreaInput(ns("icd_codes"),"Type The ICD codes you want to inquire",value = "I\nI10",
                      placeholder = "One line for one ICD codes",height = "100px"),
        fluidRow(
          column(4,actionButton(ns("calculate"),"Inquire")),
          column(8,downloadButton(ns("download_p"),"Download table"), align = 'right'),
        ),
      )
    })
    
    
    output$dis_prevlence <- renderUI(dis_prevlence())
    
    icd_codes <- reactive({
      req(input$icd_codes)
      input$icd_codes %>% str_split_1(pattern = "\n") %>% str_trim(side = "both")
    })
    
    icd_codes_for_plot <- reactive({
      req(input$icd_codes_for_plot)
      input$icd_codes_for_plot %>% str_split_1(pattern = "\n") %>% str_trim(side = "both")
    })
    
    
    prev_dat <- eventReactive(input$calculate,{
      
      waiter <- waiter::Waiter$new()
      waiter$show()
      on.exit(waiter$hide())
      
      # cl <- parallel::makeCluster(parallel::detectCores())
      # doParallel::registerDoParallel(cl)
      # # dx_freq <- df %>% dplyr::mutate(freq = purrr::map(dx, code_freq, 
      # #                                                   icd.code,icd.labels)) %>% tidyr::unnest(freq)
      # res <- tibble(icd_code = icd_codes()) %>%
      #   mutate(prevalence=map_dbl(icd_code,~ukb_icd_prevalence(UKB_ICD_dat(),.x))) %>% rename(ICD_codes=1)
      # doParallel::stopImplicitCluster()
      # parallel::stopCluster(cl)
      # 
      # res
      
      res <- tibble(icd_code = icd_codes()) %>%
        mutate(prevalence=map_dbl(icd_code,~ukb_icd_prevalence(UKB_ICD_dat(),.x))) %>% rename(ICD_codes=1)
      
      res
      
    })
    
    output$disease_prevalence <- DT::renderDT({
      req(prev_dat())
      prev_dat()
    })
    
    output$download_p <- downloadHandler(
      filename = function() {
        paste0("Inquired_prevalence", ".csv")
      },
      content = function(file) {
        vroom::vroom_write(prev_dat(), file,delim = ",")
      }
    )
    
    
    
    #**********************
    #For prevalence plot
    #**********************
    ICD_plots <- eventReactive(file_pass(),{
      tagList(
        selectInput(ns("ref_var"),"Reference Variable",
                    choices = all_fields() %>% 
                      setdiff(c("eid",all_fields() %>% str_subset(paste("^diagnoses.*icd", 10, sep = ""))))),
        verbatimTextOutput(ns("no_numeric")),
        textAreaInput(ns("icd_codes_for_plot"),"Type The ICD codes",value = c("I\nI10"),
                      placeholder = "One line for one ICD codes",height = "100px"),
        
        numericInput(ns("n_groups"),"Number of groups",value = 10,min = 1),
        
        
        fluidRow(
          column(4,actionButton(ns("plot"),"Generate plot")),
          column(8,downloadButton(ns("download_figure_p"),"Download figure data"), align = 'right'),
        ),
        
        
      )
    })
    
    output$ICD_plots <- renderUI(ICD_plots())
    
    non_numeric_fields <- eventReactive(UKB_ICD_dat(),{
      which(!(UKB_ICD_dat() %>% sapply(is.numeric))) %>% names()
    })
    
    
    
    observeEvent(UKB_ICD_dat(), {
      
      # non_numeric_fields <- which(!(UKB_ICD_dat() %>% sapply(is.numeric))) %>% names()
      updateSelectInput(session,"ref_var","Reference Variable",
                        choices = all_fields() %>% 
                          setdiff(c("eid",all_fields() %>% str_subset(paste("^diagnoses.*icd", 10, sep = "")),
                                    non_numeric_fields())))
    })
    
    
    no_numeric <- eventReactive(non_numeric_fields(),{
      # browser()
      if(length(all_fields() %>% 
                setdiff(c("eid",all_fields() %>% str_subset(paste("^diagnoses.*icd", 10, sep = "")),
                          non_numeric_fields())))==0){
        validate("The Reference variable column must contain numeric data!\nAnd No numeric column exists in your data file!")
      } else {
        NULL
      }
    })
    
    output$no_numeric <- renderText({
      # browser()
      req(no_numeric())
      no_numeric()
    })
    
    
    icd_freq_plot <- eventReactive(input$plot,{
      
      waiter <- waiter::Waiter$new()
      waiter$show()
      on.exit(waiter$hide())
      
      res <- LUKB_ukb_icd_freq_by(data = UKB_ICD_dat(),reference.var = input$ref_var,
                                  icd.code = icd_codes_for_plot(),n.groups = input$n_groups,
                                  freq.plot = TRUE)
      res
      
    })
    
    output$freq_plot <- renderPlot({
      icd_freq_plot()$figure
    },res = 144)
    
    
    output$download_figure_p <- downloadHandler(
      filename = function() {
        paste0("Figure_data", ".csv")
      },
      content = function(file) {
        vroom::vroom_write(icd_freq_plot()$dat, file,delim = ",")
      }
    )
    
    
  })
}



# library(shiny)
# library(tidyverse)
# 
# 
# ui <- fluidPage(
#   ICD_summary_UI("test_summary")
# )
# 
# server <- function(input, output, session) {
#   ICD_summary_Server("test_summary",success_info = reactive({0}))
# }
# 
# 
# shinyApp(ui, server)






