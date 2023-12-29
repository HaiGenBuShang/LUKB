choosefieldsUI <- function(id) {
  tagList(
    uiOutput(NS(id,"choose_fields")),
    UKB_previewUI(NS(id,"preview_choosed")),
  )
}

choosefieldsServer <- function(id,ukb_table,ukb_basket_file) {
  moduleServer(id, function(input, output, session) {
    # browser()
    
    fields_info <- eventReactive(ukb_table(),{
      req(ukb_basket_file())
      ns <- session$ns
      sidebarLayout(
        sidebarPanel(
          selectInput(ns("Field"),"Choose UK Biobank Fields",
                      choices = ukb_table()$FieldID, multiple = TRUE),
          textAreaInput(ns("Pasted_fields"),"Or paste your Field IDs",
                        placeholder = "One line for one Field ID",height = "100px"),
        ),
        mainPanel(
          tableOutput(ns("field_info")),
        ),
      )
    })
    
    output$choose_fields <- renderUI({
      fields_info() 
    })
    
    
    field <- reactive({
      ukb_table() %>% filter(FieldID%in%input$Field)
    })
    
    
    field_2 <- reactive({
      pasted_fields <- input$Pasted_fields %>% str_split_1(pattern = "\n") %>% str_trim(side = "both")
      ukb_table() %>% filter(FieldID%in%pasted_fields)
    })
    
    field_info <- eventReactive({
      field_2()
      field()
    },{
      bind_rows(field(),field_2()) %>% select(field,FieldID,n) %>% distinct(field,.keep_all = TRUE) %>% 
        arrange(FieldID)
    })
    output$field_info <- renderTable(field_info(),width = "100%")
    
    ukb_preview_file <- eventReactive(ukb_basket_file(),{
      req(ukb_basket_file())
      paste0("./UKB_data/preview/",ukb_basket_file(),"_ukb_preview")
    })
    
    UKB_previewServer("preview_choosed",field_info=field_info,
                      preview_UKB_file=ukb_preview_file)
    
    field_info
  })
}

