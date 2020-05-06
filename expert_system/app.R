library(shiny)
library(shinythemes)
library(tidyverse)
library(reshape2)
library(impute)
library(pcaMethods)
library(FactoMineR)
library(pheatmap)
library(plyr)
library(BatchQC)
library(factoextra)
library(ber)
library(e1071) #SVM
library(sva) #combat
library(limma)
library(gridExtra)
library(mice)
library(VIM)
library(vsn)
library(ropls)

source(file.path("tools", "tools.R"),  local = TRUE)
source(file.path("tools", "batch_correct.R"),  local = TRUE)
source(file.path("tools", "EigenMS.R"),  local = TRUE)
source(file.path("tools", "sample_normalization.R"),  local = TRUE)
source(file.path("tools", "transformation_and_scaling.R"),  local = TRUE)
source(file.path("tools", "multivariateanalysis.R"),  local = TRUE)

ui <- navbarPage(
  title = "Expert Analysis System for Metabolomics Data",
  # include the UI for each tab
  source(file.path("ui", "Ututorial.R"),  local = TRUE)$value,
  source(file.path("ui", "Umissingvalue.R"),  local = TRUE)$value,
  source(file.path("ui", "Ubatcheffect.R"),  local = TRUE)$value,
  source(file.path("ui", "Udataquality.R"),  local = TRUE)$value,
  source(file.path("ui", "Unormalization.R"),  local = TRUE)$value,
  source(file.path("ui", "Utransformation.R"),  local = TRUE)$value,
  source(file.path("ui", "Uunivariate.R"),  local = TRUE)$value,
  source(file.path("ui", "Umultivariate.R"),  local = TRUE)$value
)

server <- function(input, output, session) {
  # Include the logic (server) for each tab
  source(file.path("server", "Stutotrial.R"),  local = TRUE)$value
  source(file.path("server", "Smissingvalue.R"),  local = TRUE)$value
  source(file.path("server", "Sbatcheffect.R"),  local = TRUE)$value
  source(file.path("server", "Sdataquality.R"),  local = TRUE)$value
  source(file.path("server", "Snormalization.R"),  local = TRUE)$value
  source(file.path("server", "Stransformation.R"),  local = TRUE)$value
  source(file.path("server", "SunivariateAnalysis.R"),  local = TRUE)$value
  source(file.path("server", "SmultivariateAnalysis.R"),  local = TRUE)$value
}

shinyApp(ui = ui, server = server)


