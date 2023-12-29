downloadHTMLUI <- function(id) {
  # downloadButton((NS(id, "downloadHTML")), label = ".html file according to your .enc file for view")
  downloadButton((NS(id, "downloadHTML")), label = "All fields in this .enc file")
}


downloadHTMLServer <- function(id, basket_info) {
  moduleServer(id, function(input, output, session) {
    all_html_file <- reactive(paste0("./UKB_data/",
                            basket_info$basket_file() %>% str_replace_all("\\.enc",".html")))

    output$downloadHTML <- downloadHandler(
      filename = function() {
        reactive({basket_info$basket_file() %>% str_replace_all("\\.enc",".html")})()
      },
      content = function(file) {
        file.copy(all_html_file(), file)
      }
    )
    
  })
}
