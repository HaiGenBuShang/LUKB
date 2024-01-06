library(rvest)

Choose_UKB_basketUI <- function(id) {
  tagList(
    selectInput(NS(id,"basket_file"),label = "Choose one UKB dataset",
                # choices = list.files("UKB_data/",pattern = "enc$")),
                choices = finished_dataset(dataset_dir = "UKB_data/")),
    
  )
}

Choose_UKB_basketServer <- function(id,success_info) {
  moduleServer(id, function(input, output, session) {
    
    
    observeEvent(session$clientData,{
      updateSelectInput(session,"basket_file","Choose one UKB dataset",
                        # choices = list.files("UKB_data/",pattern = "enc$"))
                        choices = finished_dataset(dataset_dir = "UKB_data/"))
    })
    
    observeEvent(success_info(),{
      req(success_info)
      if(success_info()==1)
        updateSelectInput(session,"basket_file","Choose one UKB dataset",
                          # choices = list.files("UKB_data/",pattern = "enc$"))
                          choices = finished_dataset(dataset_dir = "UKB_data/"))
    })
    
    UKB_html_table <- reactive({
      req(input$basket_file)
      UKB_summaries <- rvest::read_html(x = paste0("UKB_data/",input$basket_file %>% str_replace_all("\\.enc",".html")))
      
      UKB_summary_tables_full <- (UKB_summaries %>%  html_table())[[2]] %>%
        mutate(FieldID=str_replace_all(UDI,"^([0-9]*)-.*","\\1")) %>%
        mutate(Description_ori=Description) %>% mutate(Description=str_remove_all(Description,"Uses.*")) %>%
        mutate(FieldID=setNames(FieldID,Description))
      
      UKB_summary_tables_short <- UKB_summary_tables_full %>% group_by(FieldID)%>%
        summarise(n=sum(Count),batches=paste0(UDI,collapse = ", ")) %>% mutate(field=names(FieldID))
    })
    
    reactiveValues(basket_file=reactive(input$basket_file),ukb_table=UKB_html_table)
    
  })
}








