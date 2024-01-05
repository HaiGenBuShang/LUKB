DatadownloadUI <- function(id) {
  tagList(
    h5("Please note:\nThe downloads of your account will be recorded.\n"),
    downloadButton(NS(id,"download"),"Download"),
    Data_shareUI(NS(id,"data_sharing")),
  )
}

DatadownloadServer <- function(id,auth_user,auth_info,file_to_be_downloaded) {
  moduleServer(id, function(input, output, session) {
    user <- reactive({
      reactiveValuesToList(auth_info)$user 
    })
    sharing_info <- Data_shareServer("data_sharing",shared_file = file_to_be_downloaded)
    output$download <- download_file(file_name = file_to_be_downloaded,user = user,authorised_user = auth_user)
    sharing_info
  })
}
