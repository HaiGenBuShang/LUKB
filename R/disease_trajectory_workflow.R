library(shiny)
library(data.table)
library(lubridate)

options(shiny.maxRequestSize=30*1024^3)

Disease_trajectoryUI <- function(id) {
  tagList(
    disease_trajectory_data_and_parameters_UI(NS(id,"data_and_parameter")),
    
    
    uiOutput(NS(id,"HR_button")),
    
    
    uiOutput(NS(id,"direction_test")),
    
    
    uiOutput(NS(id,"ccwc")),
    
    uiOutput(NS(id,"final_tra")),
    
  )
}

Disease_trajectoryServer <- function(id,user,authorised_user) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    #*************************
    #*Save data and parameters
    #*************************
    
    module_1_success <- disease_trajectory_data_and_parameters_Server("data_and_parameter")
    
    #************************
    #*Cox regression analysis
    #************************
    HR_analysis <- eventReactive(module_1_success(),{
      tagList(
        hr(),
        disease_trajectory_HR_analysis_UI(ns("HR_analysis_workflow")),
      )
    })
    
    
    output$HR_button <- renderUI({
      HR_analysis()
    })
    
    HR_res_plot <- disease_trajectory_HR_analysis_Server("HR_analysis_workflow",user = user,authorised_user = authorised_user)
    
    
    #******************************
    #*Disease pair binomial and directional test
    #******************************

    direction_test_ui <- eventReactive(HR_res_plot(),{
      tagList(
        hr(),
        disease_trajectory_disease_pair_direction_UI(ns("disease_pair_direction_workflow")),
      )
    })

    output$direction_test <- renderUI({
      req(HR_res_plot())
      direction_test_ui()
    })

    direction_tes_res_graph <- disease_trajectory_disease_pair_direction_Server("disease_pair_direction_workflow",
                                                                                last_step_success = HR_res_plot,
                                                                                user = user,authorised_user = authorised_user)

    
    #**************************************************************
    #*case control dataset and conditional logistic regression test
    #**************************************************************
    ccwc_ui <- eventReactive(direction_tes_res_graph(),{
      tagList(
        hr(),
        disease_trajectory_case_control_analysis_UI(ns("ccwc_workflow"))
      )
    })

    output$ccwc <- renderUI({
      ccwc_ui()
    })

    ccwc_res_graph <- disease_trajectory_case_control_analysis_Server("ccwc_workflow",last_step_success = direction_tes_res_graph,
                                                                      user = user,authorised_user = authorised_user)
    
    
    #***********************************************************
    #*Final disease trajectories started from the target disease
    #***********************************************************
    
    fina_tra_ui <- eventReactive(ccwc_res_graph(),{
      tagList(
        hr(),
        disease_trajectory_final_trajectory_UI(ns("final_tra_workflow"))
      )
    })
    
    output$final_tra <- renderUI({
      fina_tra_ui()
    })
    
    disease_trajectory_final_trajectory_Server("final_tra_workflow",last_step_success = ccwc_res_graph,
                                               user = user,authorised_user = authorised_user)
    
    
  })
}



# library(shiny)
# library(tidyverse)
# 
# 
# ui <- fluidPage(
#   Disease_trajectoryUI("test_Dt"),
# )
# 
# server <- function(input, output, session) {
#   Disease_trajectoryServer("test_Dt",user = reactiveVal("abcd"),authorised_user = "abcd")
# }
# 
# shinyApp(ui, server)
# shinyApp(ui, server,options = list(host = "0.0.0.0",port = 2222))
