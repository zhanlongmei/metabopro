tabPanel("3.DataQuality", fluid = TRUE,
  fluidPage(theme = shinytheme("cerulean"),
    fluidRow(
      h2("How to assess data quality?"),
      p("This two figures represents the percent of missing.
        If your missing data composes less than 5% of the total datasets.
        It means that you have a relative low missing ratio"),
      h2("Data Quality"),
      column(4,
             h4("PCA analysis"),
             plotOutput("Quality_pca") 
      ),
      column(4,
             h4("CV distribution"),
             plotOutput("Quality_CV")  
      ),
      column(4,
             h4("Correlation between qc samples"),
             plotOutput("Quality_correlation") 
      )
      
      
      
    )
  )
)



