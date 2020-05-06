
options(shiny.maxRequestSize=30*1024^2)


output$downloadData2 <- downloadHandler(
  filename <- function() {
    paste("example_data", "zip", sep=".")
  },
  
  content <- function(file) {
    file.copy("example_data/example_data.zip", file)
  },
  contentType = "application/zip"
)





