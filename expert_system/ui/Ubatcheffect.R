tabPanel("2.Batch effects", fluid = TRUE,
tabsetPanel(
tabPanel("2.1 Visualization of batch effects", fluid = TRUE,
         fluidPage(theme = shinytheme("cerulean"),
         fluidRow(
           h2("Noise filtering"),
           sliderInput("noisecutoff", "set a cut off of noise filter",
                       min = 0, max = 1, value = 0.8
           ),
           h2("Visualization of batch effects:"),
           h4("Between batch effect"),
           p("This two figures represents the percent of missing.
             If your missing data composes less than 5% of the total datasets.
             It means that you have a relative low missing ratio"),
           column(6,
                  h5("Box plot of sample intensity distribution"),
                  plotOutput("RawBox")
           ),
           column(6,
                  h5("PCA plot (coloured by batch)"),
                  plotOutput("batch_pca")
           ),
           h4("Within batch effects"),
           p("The outliers would have a dramatic effect on the imputation results. It's necessary to findout the outlier samples before perform imputation"),
           plotOutput("raw_within_batch")
           )
         )
),
tabPanel("2.2 Removal and evaluation to batch effects", fluid = TRUE,
         fluidPage(theme = shinytheme("cerulean"),
         fluidRow(
           column(6,
                  h2("Batch effects Removal"),
                  checkboxGroupInput("bm", "Choose batch correct methods:",
                                     c("BatchRatio" = "BatchRatio",
                                       "QCRSC" = "QCRSC",
                                       "SVR" = "SVR",
                                       "Combat" = "Combat",
                                       "Limma" = "Limma",
                                       "SVA" = "SVA",
                                       "Ber" = "Ber",
                                       "Eigenms (This may take a long time)" = "Eigenms")),
                  actionButton("batchcorrect", "Apply these methods", class = "btn-primary")
           ),
           column(6,
                  h2("Choose the best result to download"),
                  radioButtons("dw", "Choose the best corrected result:",
                                     c("BatchRatio" = "BatchRatio",
                                       "QCRSC" = "QCRSC",
                                       "SVR" = "SVR",
                                       "Combat" = "Combat",
                                       "Limma" = "Limma",
                                       "SVA" = "SVA",
                                       "Ber" = "Ber",
                                       "Eigenms" = "Eigenms")),
                  downloadLink('batchdownloadDatas', 'Download')
           ),
           hr(),
           
           h2("Evaluation to batch effects removal"),
           h4("Overall"),
           plotOutput("overall"),
           h4("Between batch removal"),
           column(6,
                  h5("batch p value"),
                  plotOutput("batch_pvalue")
           ),
           column(6,
                  h5("pca coloured by batch"),
                  plotOutput("batch_correct_pca")
           ),
           h4("Within batch removal"),
           selectInput("withinbatchmethod", "Choose a method:",
                       c("BatchRatio" = "BatchRatio",
                         "QCRSC" = "QCRSC",
                         "SVR" = "SVR",
                         "Combat" = "Combat",
                         "Limma" = "Limma",
                         "SVA" = "SVA",
                         "Ber" = "Ber",
                         "Eigenms" = "Eigenms",
                         "None"="None")),
           column(4,
                  plotOutput("within_single_heatmap_none")
           ),
           column(4,
                  plotOutput("within_single_heatmap")
           ),
           column(4,
                  plotOutput("within_all")
           ),
           h4("Influence on biological variance"),
           column(6,
                  h5("class p value"),
                  plotOutput("class_pvalue")
           ),
           column(6,
                  h5("pca coloured by class"),
                  plotOutput("class_correct_pca")
           )
         )
         )
)

))



