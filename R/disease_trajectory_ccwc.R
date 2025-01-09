disease_trajectory_case_control_analysis_UI <- function(id) {
  tagList(
    sidebarLayout(
      sidebarPanel(
        textInput(NS(id,"ccwc_prefix"),label = "Please provide the file prefix of your task",
                  placeholder = "yyy_mm_dd_abcdefghij"),
        numericInput(NS(id,"ccwc_number_for_HR"),label = paste0("How many cores used for c-logistic regression ",
                                                                "(Much memory required for each core)"),
                     value = 2,min = 1),
        
        waiter::use_waiter(),
        actionButton(NS(id,"ccwc_start"),"Start matching and clogistic"),
        
        verbatimTextOutput(NS(id,"ccwc_start_info")),
        hr(),
        actionButton(NS(id,"ccwc_show"),"Display disease pair graph"),
      ),
      mainPanel(
        uiOutput(NS(id,"ccwc_show_parameter")),
        visNetworkOutput(NS(id,"ccwc_graph"),height = "800px"),
        
        uiOutput(NS(id,"ccwc_download_ui")),
        
      )
    )
  )
}

disease_trajectory_case_control_analysis_Server <- function(id,last_step_success,user,authorised_user) {
  moduleServer(id, function(input, output, session) {
    
    start_info <- eventReactive(input$ccwc_start,{
      # browser()
      waiter <- waiter::Waiter$new()
      waiter$show()
      on.exit(waiter$hide())
      
      validate(
        need(input$ccwc_prefix,"Please provide file prefix")
      )
      req(last_step_success())
      
      system(paste("Rscript utilities/launch_ccwc.R", input$ccwc_prefix, input$ccwc_number_for_HR))
      
      "Case-control matching and conditional logistic regression started!"
    })
    
    output$ccwc_start_info <- renderText({
      start_info()
    })
    
    task_status <- eventReactive(input$ccwc_show,{
      # browser()
      track_task_status(directory = "Results/",
                        log_file_prefix = input$ccwc_prefix,log_file_regexpr = "_ccwc_part.*log$",
                        needed_log_file_number = list.files(path = "Results/",
                                                            pattern = paste0(input$ccwc_prefix,"_ccwc_part[0-9]*.RData"),
                                                            full.names = TRUE) %>% length(),
                        log_content_for_success = "CCWC analysis.*finished!",
                        message_for_still_running = "Case-control matching and conditional logistic regression are still running!",
                        message_for_finished = "Case-control matching and conditional logistic regression finished!")
      
    })
    
    ccwc_passed_pairs <- eventReactive(task_status(),{
      ccwc_res_files <- list.files(path = "Results/",pattern = paste0(input$ccwc_prefix,"_ccwc_part[0-9]*_clog_res.RData"),full.names = TRUE)
      c_log_res <- lapply(ccwc_res_files,function(x){
        load(x);get("c_log_res")
      }) %>% bind_rows() %>% mutate(clog_p_adj=p.adjust(clog_p,method="bon")) %>% 
        mutate(right_direction=clog_HR>1) %>% filter(clog_p_adj<0.05,right_direction)
      
      save(c_log_res,file = paste0("Results/",input$ccwc_prefix,"_ccwc_combined_clog_res.RData"))
      c_log_res
      
    })
    
    ccwc_g <- eventReactive(ccwc_passed_pairs(),{
      g <- igraph::graph_from_data_frame(ccwc_passed_pairs() %>% 
                                           mutate(clog_p_adj=p.adjust(clog_p,method="bon")) %>% 
                                           mutate(right_direction=clog_HR>1) %>% filter(clog_p_adj<0.05,right_direction)%>% 
                                           select(disease_A,disease_B))
    })
    
    output$ccwc_graph <- renderVisNetwork({
      req(ccwc_g())
      # igraph::plot.igraph(ccwc_g(),edge.lty=1.5,edge.arrow.size=0.3)
      set.seed(12345678)
      vis_dat <- toVisNetworkData(ccwc_g())
      
      load(paste0("Results/",input$ccwc_prefix,"_parameters.RData"))
      
      vis_dat$nodes$color <- if_else(vis_dat$nodes$id==t_disease,"red","lightblue")
      visNetwork(nodes = vis_dat$nodes, edges = vis_dat$edges) %>% visIgraphLayout(layout = "layout_with_fr") %>% 
        visEdges(arrows = list(to=list(enabled=TRUE,scaleFactor = 0.5)),color="grey") %>% 
        visNodes(font = list(size=30,face="Arial",color="black",vadjust=-10))
      
    })
    
    
    
    parameter_ui <- eventReactive(task_status(),{
      ns <- session$ns
      tagList(
        disease_trajectory_show_parameters_UI(ns("show_parameter")),
      )
    })
    
    output$ccwc_show_parameter <- renderUI({
      parameter_ui()
    })
    
    parameter_file <- eventReactive(task_status(),{
      paste0("Results/",input$ccwc_prefix,"_parameters.RData")
    })
    
    disease_trajectory_show_parameters_Server("show_parameter",
                                              parameter_file = parameter_file)
    
    
    download_ui <- eventReactive(task_status(),{
      ns <- session$ns
      tagList(
        download_RData_UI(ns("Rdata_download"),download_label = "Download C-C and Clogistic RData"),
      )
    })
    
    output$ccwc_download_ui <- renderUI(
      download_ui()
    )
    
    Rdata_file <- eventReactive(task_status(),{
      paste0("Results/",input$ccwc_prefix,"_ccwc_combined_clog_res.RData")
    })
    
    download_RData_Server("Rdata_download",RData_file = Rdata_file,user = user,authorised_user = authorised_user)
    
    
    
    module_secuss <- ccwc_g
  })
}


# ui <- fluidPage(
#   disease_trajectory_case_control_analysis_UI("test")
# )
# 
# server <- function(input, output, session) {
#   disease_trajectory_case_control_analysis_Server("test",reactiveVal(1),user = reactiveVal("XXX"),authorised_user = "XXX")
# }
# 
# shinyApp(ui, server)
