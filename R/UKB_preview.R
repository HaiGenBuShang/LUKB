library(data.table)
library(DT)
library(tidyverse)

UKB_previewUI <- function(id) {
  tagList(
    hr(),
    waiter::use_waiter(),
    actionButton(NS(id,"preview"),"Preview your dataset"),
    DT::dataTableOutput(NS(id,"preview_tab")),
    hr(),
  )
}

UKB_previewServer <- function(id,preview_UKB_file,field_info) {
  moduleServer(id, function(input, output, session) {
    
    fields <- reactive({field_info()$FieldID})
    
    date_file <- eventReactive(input$preview,{
      paste0("Results/UKB_",fields()[1],"_preview_",format(Sys.time(), "%Y%m%d_%H%M%S"))
    })
      

    dat_preveiew <- eventReactive(date_file(),{
      waiter <- waiter::Waiter$new()
      waiter$show()
      on.exit(waiter$hide())
      
      p <- generate_file(UKB_file = preview_UKB_file(),UKB_field = fields(),date_file = date_file())
      p$wait()
      dat <- fread(paste0(date_file(),".csv")) %>% as_tibble()
    })
    output$preview_tab <- DT::renderDataTable({
      req(dat_preveiew())
      file.remove(paste0(date_file(),".csv"))
      file.remove(paste0(date_file(),".log"))
      dat_preveiew() 
    },options = list(scrollX = TRUE,pageLength = 5))
    
    
  })
}




