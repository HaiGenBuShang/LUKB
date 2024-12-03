example_file <- "./UKB_data/antigenic_example_data.txt"

Antigenic_mapUI <- function(id) {
  tagList(
    tags$div(HTML("<b>This component is desgined for Antigenic analysis, ",
                  "to draw antigenic map and calculate the antigenic distance.</b>"),
             HTML("<br>One example file can be "),
             downloadLink(NS(id,"example_file"),"downloaded,"),
             "which contents comes from one of our publications (Yuanchen Liu, et al. Nature Communications, 2024)."),
    sidebarLayout(
      sidebarPanel(
        textAreaInput(NS(id,"ID_50_data"),label = "Please paste your ID50 data",
                      placeholder = paste("Column names are not allowed, and data separated by '\t'",
                                          "Details please see the example file",
                                          "E.g.",
                                          "BA.2\t8138.4\t5734.8\t...",
                                          "HK.3\t1246.5\t2066.3\t...",sep = "\n"),height = "200px",width = "100%"),
        width = 8
      ),
      mainPanel(
        DT::dataTableOutput(NS(id,"preview_tab")),
        width = 4
      ),
    ),
    actionButton(NS(id,"analyzing"),"Confirm data and start analysis"),
    waiter::use_waiter(),
    verbatimTextOutput(NS(id,"analysis_status")),
    hr(),
    sidebarLayout(
      sidebarPanel(
        HTML("<b>X-axis limit</b>"),
        fluidRow(
          column(numericInput(NS(id,"x_lim_min"),"min",value=-5),width = 6),
          column(numericInput(NS(id,"x_lim_max"),"max",value=5),width = 6),
        ),
        # hr(),
        HTML("<b>Y-axis limit</b>"),
        fluidRow(
          column(numericInput(NS(id,"y_lim_min"),"min",value=-5),width = 6),
          column(numericInput(NS(id,"y_lim_max"),"max",value=5),width = 6),
        ),
        hr(),
        numericInput(NS(id,"rotate_angle"),"Roration angle",value = 0,min = 0),
        selectInput(NS(id,"show_axis"),label = "Show axis annotations",choices = c(FALSE,TRUE),multiple = FALSE),
        numericInput(NS(id,"point_size"),"Size for points",value = 5,min = 1),
        numericInput(NS(id,"label_size"),"Size for labels",value = 5,min = 1),
        numericInput(NS(id,"pdf_height"),"height for output pdf",value = 7,min = 1),
        numericInput(NS(id,"pdf_width"),"Width for output pdf",value = 7,min = 1),
        fluidRow(
          column(actionButton(NS(id,"generate_plot"),"Plot!"),width = 3),
          column(downloadButton(NS(id,"download_pdf"),"Download pdf file"),width = 9,align = 'right'),
        )
      ),
      mainPanel(
        plotOutput(NS(id,"antigenic_plot"),height = 650),
        
        uiOutput(NS(id,"antigenic_distance_ui")),
        
      ),
    ),
  )
}




Antigenic_mapServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    output$example_file <- downloadHandler(
      filename = function(){
        example_file %>% str_remove_all(".*/")
      },
      content = function(file){
        file.copy(example_file, file)
      },
      contentType = "txt"
    )
    
    downloadHandler(
      filename = function(){
        pdf_file_name() %>% str_remove_all(".*/") %>% str_replace_all(".pdf",".txt")
      },
      content = function(file){
        write.table(antigenic_distance_dat(),file = file,sep = "\t",row.names = FALSE,col.names = TRUE)
      },
      contentType = "txt"
    )
    
    
    
    ID_50_data <- reactive({
      req(input$ID_50_data)
      dat_1 <-  str_split_1(input$ID_50_data,"\n") %>% str_split("\t")
      data <- dat_1[!dat_1 %>% sapply(function(x){all(x=="")})] %>% do.call(rbind,.) %>% as.data.frame()
      data %>% mutate(across(.cols = -1,.fns = as.numeric))
      
    })
    
    output$preview_tab <- DT::renderDataTable({
      ID_50_data()
    },options = list(scrollX = TRUE,pageLength = 5))
    
    color_for_each_lineage <- eventReactive(ID_50_data(),{
      each_lineage_color <- ID_50_data() %>% select(1) %>% rename(lineage=1) %>% arrange(lineage) %>% 
        mutate(colors=(scales::hue_pal()(nrow(.))))
      ID_50_data() %>% select(1) %>% rename(lineage=1) %>% 
        left_join(each_lineage_color,by="lineage") %>% pull(colors)
    })
    
    antigenic_obj <- eventReactive({
      req(input$analyzing)
      # req(color_for_each_lineage())
    },{
      waiter <- waiter::Waiter$new()
      waiter$show()
      on.exit(waiter$hide())
      antigenic_lineage_coor(ID_50_data(),color_for_each_lineage())
    })
    
    analysis_status <- eventReactive(antigenic_obj(),{
      "Analyzing finished!"
    })
    output$analysis_status <- renderPrint(analysis_status())
    

    antigenic_map <- eventReactive({
      req(input$generate_plot)
    },{
      # browser()
      if(input$show_axis){
        antigenic_rotate(lineage_coor_obj = antigenic_obj(),title = "Antigenic Map",
                         rotate_angle = input$rotate_angle,
                         x_lim = c(input$x_lim_min,input$x_lim_max),y_lim = c(input$y_lim_min,input$y_lim_max),
                         point_size = input$point_size,lab_size = input$label_size) + theme(text = element_text())
      }else{
        antigenic_rotate(lineage_coor_obj = antigenic_obj(),title = "Antigenic Map",
                         rotate_angle = input$rotate_angle,
                         x_lim = c(input$x_lim_min,input$x_lim_max),y_lim = c(input$y_lim_min,input$y_lim_max),
                         point_size = input$point_size,lab_size = input$label_size)
      }
      
    })

    output$antigenic_plot <- renderPlot({
      req(antigenic_map())
      antigenic_map() %>% print() 
    },res = 144)
    
    pdf_file_name <- eventReactive(req(antigenic_map()),{
      paste0("Results/",format(Sys.time(), "%Y_%m_%d_%H%M%S") %>% str_replace_all("-","_"),"_antigenic_map_rotate_",
             input$rotate_angle,"_point_size_",input$point_size,"_label_size_",input$label_size,".pdf")
    })

    observeEvent({
      req(pdf_file_name())
    },{
      # browser()
      pdf(pdf_file_name(),
          width = input$pdf_width,height = input$pdf_width)
      antigenic_map() %>% print()
      dev.off()
    })

    output$download_pdf <- downloadHandler(
      filename = function(){
        pdf_file_name() %>% str_remove_all(".*/")
      },
      content = function(file){
        file.copy(pdf_file_name(), file)
      },
      contentType = "txt"
    )
    
    
    download_antigenic <- eventReactive(req(input$generate_plot),{
      tagList(
        fluidRow(
          column(downloadButton(NS(id,"antigenic_distance"),"Antigenic distance"),width = 12,align = "right"),
        ),
      )
    })
    
    output$antigenic_distance_ui <- renderUI(
      download_antigenic()
    )
    
    antigenic_distance_dat <- eventReactive(req(input$generate_plot),{
      antigenic_distance(lineage_coor_obj = antigenic_obj())
    })
    
    output$antigenic_distance <- downloadHandler(
      filename = function(){
        pdf_file_name() %>% str_remove_all(".*/") %>% str_replace_all(".pdf",".txt")
      },
      content = function(file){
        write.table(antigenic_distance_dat(),file = file,sep = "\t",row.names = FALSE,col.names = TRUE)
      },
      contentType = "txt"
    )
    
  })
}


# library(shiny)
# library(tidyverse)
# 
# 
# ui <- fluidPage(
#   Antigenic_mapUI("test_summary"),
# )
# 
# server <- function(input, output, session) {
#   Antigenic_mapServer("test_summary")
# }
# 
# shinyApp(ui, server)
# shinyApp(ui, server,options = list(host = "0.0.0.0",port = 2222))







