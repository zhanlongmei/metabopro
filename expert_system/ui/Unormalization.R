tabPanel("4.Normalization", fluid = TRUE,
  tabsetPanel(
#---------------tabPanel 4.1----------------------------------------
    tabPanel("4.1 The comparibility among samples", fluid = TRUE,
      fluidPage(theme = shinytheme("cerulean"),
        fluidRow(
          h2("a. Are your samples comparabl to each other?"),
          p("This two figures represents the percent of missing.
            If your missing data composes less than 5% of the total datasets.
            It means that you have a relative low missing ratio"),
          h2("Boxplot of all the samples:"),
          plotOutput("Box_normalization_none")
        )
    )
  ),
#---------------tabPanel 4.2----------------------------------------
    tabPanel("4.2 Removal and evaluation to sample normalization", fluid = TRUE,
      fluidPage(theme = shinytheme("cerulean"),
        fluidRow(
          column(6,
                 h2("Sample normalization"),
                 checkboxGroupInput("normalizationApproach", "Choose sample normalization methods:",
                                    c("SUM" = "SUM",
                                      "PQN" = "PQN",
                                      "VSN" = "VSN",
                                      "Quantile" = "Quantile")),
                 actionButton("normalizationConduct", "Apply these methods", class = "btn-primary")
          ),
          column(6,
                 h2("Choose the best result to download"),
                 radioButtons("bestnormalization", "Choose the best normalization result:",
                              c("None" = "None",
                                "SUM" = "SUM",
                                "PQN" = "PQN",
                                "VSN" = "VSN",
                                "Quantile" = "Quantile")),
                 downloadLink('normalizationDownloa', 'Download')
          ),
          hr(),
          h2("Evaluation to sample normalization results"),
          h5("boxplot plot"),
          radioButtons("sample_normalized_method", "Choose a normalization approach to view the result:",
                       c("None" = "None",
                         "SUM" = "SUM",
                         "PQN" = "PQN",
                         "VSN" = "VSN",
                         "Quantile" = "Quantile")),
          plotOutput("normalization_evaluation_boxplot"),
          
          column(4,
                 h5("Classification accuracy"),
                 tableOutput("nor_classification_accuracy")
          ),
          column(4,
                 h5("CV of the biological groups"),
                 plotOutput("nor_group_cv")
          ),
          column(4,
                 h5("Between group variance/within group variance"),
                 tableOutput("nor_variance")
          )
          
          
        )
      )
    )
#------------end of tabpanel 4.2------------------------------------
  )
)



