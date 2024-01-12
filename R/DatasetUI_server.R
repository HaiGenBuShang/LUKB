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
    dataset_info <- GenerateDatasetServer("dataset",field_data = field_info,ukb_basket_file=basket_info$basket_file,auth_info = auth_res)
    sharing_info <- DatadownloadServer("download",auth_user = authorised_user,auth_info = auth_res,
                                       success_info=dataset_info$success_info)
    
    reactiveValues(
      success_info=reactive(dataset_info$success_info()),
      sharing_info=reactive(sharing_info())
    )
    
  })
}

