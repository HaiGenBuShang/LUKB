DatasetUI <- function(id) {
  tagList(
    fluidRow(
      column(2, align = "left",Choose_UKB_basketUI(NS(id,"ukb_basket"))),
      column(2, align = "left",downloadHTMLUI(NS(id,"downloadHTML")), style = "margin-top: 25px;")
    ),
    choosefieldsUI(NS(id,"field")),
    GenerateDatasetUI(NS(id,"dataset")),
    hr(),
    DatadownloadUI(NS(id,"download")),
  )
}

DatasetServer <- function(id,authorised_user,auth_res,success_info) {
  moduleServer(id, function(input, output, session) {
    
    basket_info <- Choose_UKB_basketServer("ukb_basket",success_info = success_info)
    downloadHTMLServer("downloadHTML", basket_info = basket_info)
    
    field_info <- choosefieldsServer("field",ukb_table=basket_info$ukb_table,ukb_basket_file=basket_info$basket_file)
    dataset_info <- GenerateDatasetServer("dataset",field_data = field_info,ukb_basket_file=basket_info$basket_file)
    DatadownloadServer("download",auth_user = authorised_user,auth_info = auth_res,
                       file_to_be_downloaded=dataset_info$file_to_be_downloaded)
    reactive(dataset_info$success_info())
  })
}


# ui <- fluidPage(
#   DatasetUI("test")
# )
# server <- function(input,output,session){
#   DatasetServer("test")
# }
# 
# shinyApp(ui,server)