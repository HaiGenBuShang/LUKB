html_file <- "./UKB_data/ukb_coding.zip"

Code_remappingUI <- function(id) {
  tagList(
    tags$div("Still some fields not mapping? See UKB ", 
             tags$a(href="https://biobank.ndph.ox.ac.uk/showcase/help.cgi?cd=data_coding","Data Coding section"),
             "and find your corresponding coding file in this ",
             downloadLink(NS(id,"coding_file"),"HTML file."),
    ),
    "Provide coding file and filed ID below.",
    tags$div("coding file examples:",
             tags$a(href="https://biobank.ndph.ox.ac.uk/showcase/coding.cgi?id=6","Data-Coding 6.")),
    fluidRow(
      column(
        6,
        fileInput(NS(id,"coding_file"),"Upload your coding file"),
      ),
      column(
        6,
        textInput(NS(id,"fields"),"Field ID corresponding to coding file")
      ),
    ),
    waiter::use_waiter(),
    actionButton(NS(id,"mapping"),"Data remap"),
    verbatimTextOutput(NS(id,"remapping_status")),
    hr(),
    actionButton(NS(id,"add_more_fields"),"I have more fileds to remap"),
    
    uiOutput(NS(id,"more_fields")),
    
  )
}

Code_remappingServer <- function(id,remapping_file) {
  moduleServer(id, function(input, output, session) {
    
    remapped_filename <- reactive({
      remapping_file() %>% str_remove("_mapped.csv") %>% str_c("_remapped.csv")
    })
    
    recodes <- reactiveVal(NA)
    
    observeEvent(input$mapping,{
      req(input$coding_file,input$fields)
      
      waiter <- waiter::Waiter$new()
      waiter$show()
      on.exit(waiter$hide())
      
      recodes(try(recode_file(file_to_be_recoded = remapping_file(),field_ID = input$fields,
                              coding_file = input$coding_file$datapath),silent = TRUE))
    })
    
    
    more_fields <- eventReactive(input$add_more_fields,{
      req(remapping_status())
      ns <- session$ns
      tagList(
        "If you have multiple fileds to remap, please remap them multiple times.",
        fluidRow(
          column(
            6,
            fileInput(ns("coding_file_2"),"Upload another coding file"),
          ),
          column(
            6,
            textInput(ns("fields_2"),"Field ID corresponding to coding file")
          ),
        ),
        actionButton(ns("mapping_2"),"Remapping with another coding file"),
        
        verbatimTextOutput(ns("remapping_status_2")), 
      )
    })
    
    output$more_fields <- renderUI(more_fields())
    
    
    
    
    observeEvent(input$mapping_2,{
      req(input$coding_file_2,input$fields_2)
      
      waiter <- waiter::Waiter$new()
      waiter$show()
      on.exit(waiter$hide())
      
      recodes(try(recode_file(file_to_be_recoded = remapped_filename(),field_ID = input$fields_2,
                              coding_file = input$coding_file_2$datapath),silent = TRUE))
    })
    
    
    remapping_status <- eventReactive(recodes(),{
      req(recodes())
      if("try-error"%in%class(recodes())){
        validate("Invalid coding file or Field IDs")
      }else{
        fwrite(recodes(),file = remapped_filename(),
               col.names = TRUE,row.names = FALSE,sep = ",")
        "Remapping Complete!\nYour Dataset will be deleted in at most 48 hours, proceed in time."
      }
    })
    
    
    remapping_info <- eventReactive(remapping_status(),{
      "Remapping Complete!\nYour Dataset will be deleted in at most 48 hours, proceed in time."
    })
    
    output$remapping_status <- renderText({
      remapping_info()
    })
    
    remapping_info_2 <- eventReactive(input$mapping_2,{
      paste0("Field: \"",input$fields_2,
             "\" remapping Complete!\nYour Dataset will be deleted in at most 48 hours, proceed in time.")
    })
    
    output$remapping_status_2 <- renderText({
      remapping_info_2()
    })
    
    finish_mapping_filename <- eventReactive(remapping_status(),{
      remapped_filename()
    })
    
    output$coding_file <- downloadHandler(
      filename = function(){
        html_file %>% str_remove_all(".*/")
      },
      content = function(file){
        file.copy(html_file, file)
      },
      contentType = "txt"
    )
    
    finish_mapping_filename
  })
}
