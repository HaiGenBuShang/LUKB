modal_sharing_data <- function(session){
  ns <- session$ns
  modalDialog(
    HTML("You can write the data remarks, such as fields/fields ID it contains.<br>"),
    # textInput(ns("remark"),"Data Remarks"),
    textAreaInput(ns("remark"),"Data Remarks",height = "100px"),
    title = "Data Sharing",
    footer = tagList(
      actionButton(ns("cancel"), "I want to go back"),
      actionButton(ns("ok"), "Confirm to share", class = "btn btn-danger"),
    )
  )
}

Data_shareUI <- function(id) {
  tagList(
    actionButton(NS(id,"data_share"),"Share this file?"),
  )
}

Data_shareServer <- function(id,shared_file) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$data_share,{
      showModal(modal_sharing_data(session))
    })
    
    observeEvent(input$cancel, {
      removeModal()
    })
    
    observeEvent(input$ok,{
      showNotification("Data shared!")
      file.copy(shared_file(),
                to = shared_file() %>% str_replace_all("(Results/)(UKB.*)(\\..*)",
                                                       replacement = "Shared_data/\\2\\3"))
      removeModal()
    })
    
    observeEvent(input$ok,{
      write.table(input$remark,
                  file = shared_file() %>% str_replace_all("(Results/)(UKB.*)(\\..*)",replacement = "Shared_data/\\2.remark"),
                  row.names = FALSE,col.names = FALSE,sep = "\n",quote = FALSE)
    })
    
  })
}



