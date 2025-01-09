disease_trajectory_final_trajectory_UI <- function(id) {
  tagList(
    sidebarLayout(
      sidebarPanel(
        textInput(NS(id,"final_tra_prefix"),label = "Please provide the file prefix of your task",
                  placeholder = "yyy_mm_dd_abcdefghij"),
        hr(),
        waiter::use_waiter(),
        actionButton(NS(id,"final_tra_show"),"Display disease trajectories"),
      ),
      mainPanel(
        uiOutput(NS(id,"final_tra_show_parameter")),
        visNetworkOutput(NS(id,"final_tra_graph"),height = "800px"),
        
        uiOutput(NS(id,"final_tra_download_ui")),
        
      )
    )
  )
}

disease_trajectory_final_trajectory_Server <- function(id,last_step_success,user,authorised_user) {
  moduleServer(id, function(input, output, session) {
    
    final_tra_status <- eventReactive(input$final_tra_show,{
      validate(
        need(input$final_tra_prefix,"Please provided file prefix")
      )
      track_task_status(directory = "Results/",
                        log_file_prefix = input$final_tra_prefix,log_file_regexpr = "_ccwc_part.*log$",
                        needed_log_file_number = list.files(path = "Results/",
                                                            pattern = paste0(input$final_tra_prefix,"_ccwc_part[0-9]*.RData"),
                                                            full.names = TRUE) %>% length(),
                        log_content_for_success = "CCWC analysis.*finished!",
                        message_for_still_running = "Waiting for the finishment of C-c matching and clogistic!",
                        message_for_finished = "Case-control matching and conditional logistic regression finished!")
    })
    
    final_tra_res_graph <- eventReactive(final_tra_status(),{
      # browser()
      
      waiter <- waiter::Waiter$new()
      waiter$show()
      on.exit(waiter$hide())
      
      files <- paste0("Results/",input$final_tra_prefix,"_disease_pair_test_graph.RData")
      parameter_file <- paste0("Results/",input$final_tra_prefix,"_parameters.RData")
      load(files);load(parameter_file)
      sub_g_nodes <- igraph::subcomponent(g,v = t_disease,mode = "out")
      final_disease_trajectories <- igraph::subgraph(g,sub_g_nodes)
      save(final_disease_trajectories,file = paste0("Results/",input$final_tra_prefix,"_final_disease_trajectories.RData"))
      final_disease_trajectories
      
      # sub_g <- igraph::subgraph(g,sub_g_nodes)
      # sub_g
    })
    
    output$final_tra_graph <- renderVisNetwork({
      req(final_tra_res_graph())
      # browser()
      # igraph::plot.igraph(final_tra_res_graph(),edge.lty=1.5,edge.arrow.size=0.3)
      set.seed(12345678)
      vis_dat <- toVisNetworkData(final_tra_res_graph())
      
      load(paste0("Results/",input$final_tra_prefix,"_parameters.RData"))
      
      vis_dat$nodes$color <- if_else(vis_dat$nodes$id==t_disease,"red","lightblue")
      visNetwork(nodes = vis_dat$nodes, edges = vis_dat$edges) %>% visIgraphLayout(layout = "layout_with_fr") %>% 
        visEdges(arrows = list(to=list(enabled=TRUE,scaleFactor = 0.5)),color="grey") %>% 
        visNodes(font = list(size=30,face="Arial",color="black",vadjust=-10))
      
      
    })
    
    
    
    parameter_ui <- eventReactive(final_tra_status(),{
      ns <- session$ns
      tagList(
        disease_trajectory_show_parameters_UI(ns("show_parameter")),
      )
    })
    
    output$final_tra_show_parameter <- renderUI({
      parameter_ui()
    })
    
    parameter_file <- eventReactive(final_tra_status(),{
      paste0("Results/",input$final_tra_prefix,"_parameters.RData")
    })
    
    disease_trajectory_show_parameters_Server("show_parameter",
                                              parameter_file = parameter_file)
    
    
    download_ui <- eventReactive(final_tra_status(),{
      ns <- session$ns
      tagList(
        download_RData_UI(ns("Rdata_download"),download_label = "Download final trajectories RData"),
      )
    })
    
    output$final_tra_download_ui <- renderUI(
      download_ui()
    )
    
    Rdata_file <- eventReactive(final_tra_status(),{
      paste0("Results/",input$final_tra_prefix,"_final_disease_trajectories.RData")
    })
    
    download_RData_Server("Rdata_download",RData_file = Rdata_file,user = user,authorised_user = authorised_user)
    
    
    
    module_secuss <- final_tra_res_graph
    
  })
}




# ui <- fluidPage(
#   disease_trajectory_final_trajectory_UI("test")
# )
# 
# server <- function(input, output, session) {
#   disease_trajectory_final_trajectory_Server("test",reactiveVal(1),user = reactiveVal("XXX"),authorised_user = "XXX")
# }
# 
# shinyApp(ui, server)
