data_previewUI <- function(id) {
  tagList(
    waiter::use_waiter(),
    uiOutput(NS(id,"preview")),
    verbatimTextOutput(NS(id,"preview_file")),
    DT::dataTableOutput(NS(id,"preview_tab")),
  )
}

data_previewServer <- function(id,preview_files,preview_text) {
  moduleServer(id, function(input, output, session) {
    
    dat_preveiew <- eventReactive(preview_files(),{
      waiter <- waiter::Waiter$new()
      waiter$show()
      on.exit(waiter$hide())

      dat <- fread(preview_files(),nrows = 100) %>% as_tibble()
    })
    
    output$preview_tab <- DT::renderDataTable({
      req(dat_preveiew())
      dat_preveiew()
    },options = list(scrollX = TRUE,pageLength = 5))
    
    
    output$preview_file <- renderText({
      req(dat_preveiew())
      paste0("The file you are previewing is: ",isolate(preview_files()) %>% str_remove("Results/"))
    })
    
    output$preview <- renderUI({
      req(dat_preveiew())
      h4(preview_text)
    })
    
  })
}
