disease_trajectory_show_parameters_UI <- function(id) {
  tagList(
    h4(HTML("<b>","Analyzing Parameters","</b>")),
    # h4("Analyzing Parameters"),
    DT::DTOutput(NS(id,"show_parameters_table")),
  )
}

disease_trajectory_show_parameters_Server <- function(id,parameter_file) {
  moduleServer(id, function(input, output, session) {
    parameter_dat <- eventReactive(parameter_file(),{
      # browser()
      load(parameter_file())
      
      paras <- sapply(ls(),function(x)get(x))[c(6,5,1,2,3,8,4,7)]
      max_paras <- sapply(paras,length) %>% max
      
      paras %>% map(~c(.x,rep(NA,max_paras-length(.x)))) %>% as_tibble()
      
    })
    
    output$show_parameters_table <- DT::renderDT({
      parameter_dat()
    # },options=list(scrollX=TRUE),caption = "Analyzing parameters")
    },options=list(scrollX=TRUE))
  })
}


# ui <- fluidPage(
#   disease_trajectory_show_parameters_UI("test")
# )
# 
# server <- function(input, output, session) {
#   disease_trajectory_show_parameters_Server("test",parameter_file = reactiveVal("Results/2024_11_30_112147_iqufcynuyr_parameters.RData"))
# }
# 
# shinyApp(ui, server)
