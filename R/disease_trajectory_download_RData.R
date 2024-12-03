download_RData_UI <- function(id,download_label) {
  tagList(
    fluidPage(
      column(width = 12,downloadButton(NS(id,"download_RData"),label = download_label),align = "right")
    ),
  )
}

download_RData_Server <- function(id,RData_file,user,authorised_user) {
  moduleServer(id, function(input, output, session) {
    output$download_RData <- download_RData(file_name = RData_file,user = user,authorised_user = authorised_user)
  })
}

# ui <- fluidPage(
#   download_RData_UI("test")
# )
# 
# server <- function(input, output, session) {
#   download_RData_Server("test",RData_file = reactiveVal("Results/2024_11_30_143711_mtqvqvtdrt_parameters.RData"),
#                         user = reactiveVal("abc"),authorised_user = "abc")
# }
# 
# shinyApp(ui, server)


