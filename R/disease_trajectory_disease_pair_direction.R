disease_trajectory_disease_pair_direction_UI <- function(id) {
  tagList(
    sidebarLayout(
      sidebarPanel(
        textInput(NS(id,"direction_test_prefix"),label = "Please provide the file prefix of your task",
                  placeholder = "yyy_mm_dd_abcdefghij"),
        waiter::use_waiter(),
        actionButton(NS(id,"direction_test_start"),"Start direction test"),
        hr(),
        actionButton(NS(id,"direction_test_show"),"Display disease pair graph"),
      ),
      mainPanel(
        uiOutput(NS(id,"direction_test_show_parameter")),
        plotOutput(NS(id,"direction_test_graph"),height = "1200px"),
        
        uiOutput(NS(id,"direction_test_download_ui")),
        
      )
    ),
  )
}

disease_trajectory_disease_pair_direction_Server <- function(id,last_step_success,user,authorised_user) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$direction_test_start,{
      req(last_step_success())
      
      waiter <- waiter::Waiter$new()
      waiter$show()
      on.exit(waiter$hide())
      
      system(paste("nohup Rscript utilities/disease_pair_binomial_and_directional_test.R",input$direction_test_prefix,
                   ">",paste0("Results/",input$direction_test_prefix,"_disease_pair_test.log 2>&1 &")))
    })
    
    direction_test_status <- eventReactive(input$direction_test_show,{
      # browser()
      track_task_status(directory = "Results/",
                        log_file_prefix = input$direction_test_prefix,log_file_regexpr = "_disease_pair_test.log",
                        needed_log_file_number = 1,log_content_for_success = "Disease pair analysis finished!",
                        message_for_still_running = "Disease pair analysis is still running!",
                        message_for_finished = "Disease pair analysis finished!")
      
    })
    
    direction_tes_res_graph <- eventReactive(direction_test_status(),{
      # browser()
      files <- paste0("Results/",input$direction_test_prefix,"_disease_pair_test_graph.RData")
      load(files)
      g
    })
    
    output$direction_test_graph <- renderPlot({
      req(direction_tes_res_graph())
      # browser()
      igraph::plot.igraph(direction_tes_res_graph(),edge.lty=1.5,edge.arrow.size=0.3)
    },res = 144)
    
    
    parameter_ui <- eventReactive(direction_test_status(),{
      ns <- session$ns
      tagList(
        disease_trajectory_show_parameters_UI(ns("show_parameter")),
      )
    })
    
    output$direction_test_show_parameter <- renderUI({
      parameter_ui()
    })
    
    parameter_file <- eventReactive(direction_test_status(),{
      paste0("Results/",input$direction_test_prefix,"_parameters.RData")
    })
    
    disease_trajectory_show_parameters_Server("show_parameter",
                                              parameter_file = parameter_file)
    
    
    download_ui <- eventReactive(direction_test_status(),{
      ns <- session$ns
      tagList(
        download_RData_UI(ns("Rdata_download"),download_label = "Download disease pairs RData"),
      )
    })
    
    output$direction_test_download_ui <- renderUI(
      download_ui()
    )
    
    Rdata_file <- eventReactive(direction_test_status(),{
      paste0("Results/",input$direction_test_prefix,"_disease_pair_test_pairs.RData")
    })
    
    download_RData_Server("Rdata_download",RData_file = Rdata_file,user = user,authorised_user = authorised_user)
    
    
    
    module_secuss <- direction_tes_res_graph
  })
}


# ui <- fluidPage(
#   disease_trajectory_disease_pair_direction_UI("test")
# )
# 
# server <- function(input, output, session) {
#   disease_trajectory_disease_pair_direction_Server("test",reactiveVal(1),user = reactiveVal("XXX"),authorised_user = "XXX")
# }
# 
# shinyApp(ui, server)


