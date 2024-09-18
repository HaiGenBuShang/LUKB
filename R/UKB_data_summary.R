library(data.table)
library(ukbtools)

data_summary_UI <- function(id) {
  tagList(
    fluidRow(
      column(6, align = "left",
             selectInput(NS(id,"choosed_file"),"Or Choose your mapped data file",
                         choices = c("",list.files("Results/",pattern = "mapped\\.csv") %>%
                                       str_subset("preview",negate = TRUE)),
                         selected = "",width = "100%")),
      column(6, align = "left",
             fileInput(NS(id,"up_file"),"Upload your extracted data after code mapping",width = "100%")),
    ),
    
    waiter::use_waiter(),
    sidebarLayout(
      sidebarPanel(
        h4(strong("Figure Parameters")),
        uiOutput(NS(id,"choose_fields")), #reference var and other params are in its corresponding server function
        
      ),
      mainPanel(
        plotOutput(NS(id,"summary_plot")),
        column(12,tableOutput(NS(id,"summary_table")),align="center"),
      )
    )
  )
}

data_summary_Server <- function(id,success_info) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    observeEvent(session$clientData,{
      updateSelectInput(session,"choosed_file","Or Choose your mapped data file",
                        choices = c("",list.files("Results/",pattern = "mapped\\.csv") %>%
                                      str_subset("preview",negate = TRUE)),
                        selected = "")
    })
    
    observeEvent(success_info(),{
      req(success_info)
      if(success_info()==1)
        updateSelectInput(session,"choosed_file","Or Choose your mapped data file",
                          choices = c("",list.files("Results/",pattern = "mapped\\.csv") %>%
                                        str_subset("preview",negate = TRUE)),
                          selected = "")
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
    
    file_info <- eventReactive(all_fields(),{
      tagList(
        selectInput(ns("Field"),"Choose one UK Biobank Field",choices = all_fields() %>% setdiff("eid"), multiple = FALSE),
        
        selectInput(ns("subset_var"),"Choose one subset variable",
                    choices = all_fields() %>% setdiff(c("eid")),multiple = FALSE),
        uiOutput(ns("sub_thres")),
        
        # selectInput(ns("proportion_or_count"),"Display Count or proportion",
        #             choices = c("proportion","count"),multiple = FALSE),
        selectInput(ns("legend_position"),"Set the legend position",
                    choices = c("top","bottom","left","right","none")),
        textInput(ns("x_lab"),label = "X-axis label",value = "The Choosed Field"),
        selectInput(ns("NA_"),"Remove NA values in the the subset variable?",choices = c(TRUE,FALSE),multiple = FALSE),
        actionButton(ns("plot"),"Summary the data!")
      )
      
    })
    
    output$choose_fields <- renderUI({
      file_info() 
    })
    
    
    observeEvent(input$Field, {
      updateSelectInput(session,"subset_var","Choose one subset variable",
                        choices = all_fields() %>% setdiff(c("eid",input$Field)))
    })
    
    sub_thres <- eventReactive(input$subset_var,{
      subset_var_class <- UKB_dat() %>% pull(input$subset_var) %>% class()
      
      if(subset_var_class == "character"){
        tagList(
          selectInput(ns("subset_thres"),"Choose one or more values to subset (subset only contain choosed values)",
                      choices = UKB_dat() %>%  pull(input$subset_var ) %>% unique(),multiple = TRUE),
          selectInput(ns("proportion_or_count"),"Display Count or proportion",
                      choices = c("proportion","count"),multiple = FALSE),
        )
      } else {
        tagList(
          numericInput(ns("subset_thres"),
                       "Set the threshold for subsetting (value_in_subset >= threshold)",value = 20),
        )
      }
      
    })
    output$sub_thres <- renderUI({
      sub_thres()
    })
    
    
    
    UKB_dat <- eventReactive(dat_file_for_cleaning(),{
      
      waiter <- waiter::Waiter$new()
      waiter$show()
      on.exit(waiter$hide())
      
      fread(dat_file_for_cleaning()$datapath)
    })
    
    summary_plot <- eventReactive(input$plot,{
      
      waiter <- waiter::Waiter$new()
      waiter$show()
      on.exit(waiter$hide())
      
      dat_summary(UKB_dat = UKB_dat(),selected_var = input$Field,subset_var = input$subset_var,
                  subset_thres = input$subset_thres,statistic_count_or_proportion = input$proportion_or_count,
                  legend_position = input$legend_position,x_lab = input$x_lab,remove_na = as.logical(input$NA_))
    })
    
    output$summary_plot <- renderPlot({
      summary_plot()
    },res = 144)
    
    summary_table <- eventReactive(input$plot,{
      
      UKB_dat() %>% select(!!input$Field,!!input$subset_var) %>% base::summary()
    })

    output$summary_table <- renderTable({
      req(summary_table())
      summary_table() %>% as.data.frame.matrix() %>% as_tibble()
    })
    
    
  })
}



# library(shiny)
# library(tidyverse)
# 
# 
# ui <- fluidPage(
#   data_summary_UI("test_summary")
# )
# 
# server <- function(input, output, session) {
#   data_summary_Server("test_summary",success_info = reactive({0}))
# }
# 
# 
# shinyApp(ui, server)
