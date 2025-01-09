disease_trajectory_HR_analysis_UI <- function(id) {
  tagList(
    sidebarLayout(
      sidebarPanel(
        textInput(NS(id,"HR_file_prefix"),label = "Please provide the file prefix of your task",
                  placeholder = "yyy_mm_dd_abcdefghij"),
        numericInput(NS(id,"core_number_for_HR"),label = "How many cores used for cox analysis",value = 4,min = 1),
        waiter::use_waiter(),
        actionButton(NS(id,"HR_analysis"),"Start Cox regression"),
        hr(),
        actionButton(NS(id,"Cox_res"),"Display Cox results"),
      ),
      mainPanel(
        uiOutput(NS(id,"Cox_show_parameter")),
        plotlyOutput(NS(id,"Cox_plot")),
        
        uiOutput(NS(id,"Cox_download_ui")),
        
      )
    ),
  )
}

disease_trajectory_HR_analysis_Server <- function(id,user,authorised_user) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$HR_analysis,{
      req(input$HR_file_prefix)
      
      waiter <- waiter::Waiter$new()
      waiter$show()
      on.exit(waiter$hide())
      
      system(paste("Rscript utilities/launch_HR_analysis.R", input$HR_file_prefix, input$core_number_for_HR))
    })
    
    HR_task_status <- eventReactive(input$Cox_res,{
      
      track_task_status(directory = "Results/",log_file_prefix = input$HR_file_prefix,log_file_regexpr = "_HR_disease_part.*log",
                        needed_log_file_number = list.files(path = "Results/",
                                                            pattern = paste0(input$HR_file_prefix,"_HR_disease_part.*RData"),
                                                            full.names = TRUE) %>% length(),
                        log_content_for_success = "HR analysis.*finished",
                        message_for_still_running = "Cox regression analysis is still running!",
                        message_for_finished = "Cox regression finished!")
    })
    
    HR_res <- eventReactive(HR_task_status(),{
      HR_res_dat <- list.files(path = "Results/",pattern = paste0(input$HR_file_prefix,"_HR_analysis_part.*RData"),full.names = TRUE)
      HR_res <- lapply(HR_res_dat,function(x){
        load(x);get("HR_res")
      }) %>% bind_rows() %>% 
        mutate(HR_p_adj=p.adjust(HR_p,method="bon"),p_adj=p.adjust(pvalue,method="bon"),
               f_p_adj=p.adjust(f_pvalue,method="bon"))
      save(HR_res,file = paste0("Results/",input$HR_file_prefix,"_HR_analysis_combined.RData"))
      HR_res
    })
    
    HR_res_plot <- eventReactive(HR_res(),{
      dat_for_ggplot <- HR_res() %>% #select(-contains("CI")) %>% 
        mutate(row_n=row_number()) %>% 
        
        mutate(shape=if_else(HR_p_adj<0.05&p_adj<0.05,19,3),
               
               #if the color were changed, then 
               #!!! scale_color_identity() !!! label parameter must be checked!!!!!
               color=if_else(HR_p_adj<0.05&p_adj<0.05,"<0.05","≥0.05")) %>% 
        
        #**********************************************
        #*This line used to produce the chapter initial
        #**********************************************
        mutate(Chapter_initial=str_sub(pid,1,1))
      
      gg_vline_dat <- dat_for_ggplot %>% group_by(Chapter_initial) %>% 
        summarise(max_row_n=max(row_n),min_row_n=min(row_n)) %>% mutate(lag=lag(max_row_n)) %>% 
        mutate(mean=map2_dbl(min_row_n,lag,~(mean(c(.x,.y))))) %>% 
        mutate(mean_label=map2_dbl(max_row_n,min_row_n,~mean(c(.x,.y))))
      
      # browser()
      dat_for_ggplot %>% 
        ggplot() +
        geom_point(mapping = aes(x=row_n,y=log(HR),color=color,
                                 text=paste0("ICD-10:&nbsp;",pid,
                                             "<br>HR:&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",round(HR,digits = 2),
                                             "<br>HR CI:&nbsp;&nbsp;",
                                             CI_low %>% as.numeric() %>% round(digits = 2) %>% format(nsmall=2)," - ",
                                             CI_upp %>% as.numeric() %>% round(digits = 2) %>% format(nsmall=2)))) +
        # scale_color_identity(guide = guide_legend(title = "Adjust P"),labels=c(">0.05","<0.05"))+
        scale_color_manual(values = c("≥0.05"="black","<0.05"="red"),name="Adjusted P")+
        geom_hline(yintercept = 0) + geom_vline(xintercept = gg_vline_dat$mean[-1],linetype=2)+
        scale_x_continuous(breaks = gg_vline_dat$mean_label,labels = gg_vline_dat$Chapter_initial) + 
        xlab("ICD-10 Chapter Initials") +
        theme(#axis.title.x = element_text(face = "bold"),
          axis.title = element_text(face = "bold",family = "Arial"))
    })
    
    output$Cox_plot <- renderPlotly({
      ggplotly(HR_res_plot(),tooltip = c("text"),labelfont=2)
    })
    
    
    parameter_ui <- eventReactive(HR_task_status(),{
      ns <- session$ns
      tagList(
        disease_trajectory_show_parameters_UI(ns("show_parameter")),
      )
    })
    
    output$Cox_show_parameter <- renderUI({
      parameter_ui()
    })
    
    parameter_file <- eventReactive(HR_task_status(),{
      paste0("Results/",input$HR_file_prefix,"_parameters.RData")
    })
    
    disease_trajectory_show_parameters_Server("show_parameter",
                                              parameter_file = parameter_file)
    
    
    download_ui <- eventReactive(HR_task_status(),{
      ns <- session$ns
      tagList(
        download_RData_UI(ns("Rdata_download"),download_label = "Download Cox RData"),
      )
    })
    
    output$Cox_download_ui <- renderUI(
      download_ui()
    )
    
    Rdata_file <- eventReactive(HR_task_status(),{
      paste0("Results/",input$HR_file_prefix,"_HR_analysis_combined.RData")
    })
    
    download_RData_Server("Rdata_download",RData_file = Rdata_file,user = user,authorised_user = authorised_user)
    
    
    module_secuss <- HR_res_plot
  })
}




# ui <- fluidPage(
#   disease_trajectory_HR_analysis_UI("test")
# )
# 
# server <- function(input, output, session) {
#   disease_trajectory_HR_analysis_Server("test",user = reactiveVal("XXX"),authorised_user = "XXX")
# }
# 
# shinyApp(ui, server)
