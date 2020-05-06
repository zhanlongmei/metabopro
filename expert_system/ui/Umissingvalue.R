tabPanel("1.Missing values", fluid = TRUE,
tabsetPanel(
tabPanel("1.1 Visualizaiton", fluid = TRUE,
         fluidPage(theme = shinytheme("cerulean"),
                   fluidRow(
                     h2("File upload"),
                     p("Uplaod your data or try our example data. An example data is avaiable at Tutorial panel. 
                       An instruction about data preparing is also provided."),
                     column(6,
                            fileInput("peakfile", "Choose Sample-by-feature matrix",
                                      accept = c(
                                        "text/csv",
                                        "text/comma-separated-values,text/plain",
                                        ".csv")
                            )
                     ),
                     column(6,
                            fileInput("listfile", "Choose sample list File",
                                      accept = c(
                                        "text/txt",
                                        "text/comma-separated-values,text/plain",
                                        ".txt")
                            )
                     )
                     ),
                   hr(),
                   fluidRow(
                     h3("Miss percent"),
                     column(6,
                            p("the percent of MS features that contianing missing values"),
                            plotOutput("mzmisspercent")
                     ),
                     column(6,
                            p("The percent of missing values on the whole dataset level"),
                            plotOutput("totalmisspercent")
                     ),
                     hr(),
                     h3("missing value distribution"),
                     column(6,
                            p("the distribution of missing value on the whole dataset"),
                            plotOutput("missmatrix")
                     ),
                     column(6,
                            p("missing ratio in each sample"),
                            plotOutput("sample_miss_percent")
                     ),
                     h2("Filtering based on \"80% rules\""),
                     p("Please set the minimum missing ratio for the QC and study samples. 
                       Features from QC sampples that have a higher missing ratio than the threshould will be removed.
                       Features from all study sample that have a higher missing ratio than the threshould will be removed."),
                     
                     column(6,
                            sliderInput("qcpercentst", "Percent of missing in QC sample",
                                        min = 0, max = 1, value = 0.5
                            )
                     ),
                     column(6,
                            sliderInput("samplepercentst", "Percent of missing in study sample",
                                        min = 0, max = 1, value = 0.5
                            )
                     )
                     
                     
                     )
          )
         
),
tabPanel("1.2 Imputation to TM and LIM", fluid = TRUE,
         fluidPage(theme = shinytheme("cerulean"),
                   fluidRow(
                     h2("Introduction:"),
                     h4("TM"),
                     p("Due to the experimental design, some metabolites or chemicals are likely only detectable in one group but completely undetectable in the other.
                       If a MS feature is completely missed in one group but is detected in the other group with 50% detection rate at least, it is classified into TM. 
                       Such missing value is filled with random values with extreme low intensity ranging from 0 to 1."),
                     h4("LIM"),
                     p("If the abundance of a specific feature is similar to the detection limit, the proportion of missing would be relatively large, and the observed value would be relatively samll.
                       A MS feature that appears < 80% of samples from a specific group and its abundance ranks < 0.1 lower quantile of the whole dataset is considered as LIM.
                       LIM could be imputed with randomly selected vlaues from a Gaussian distribution, the mean and standard deviation of which is estimated from the the MS features ranked < 0.1 lower quantile."),
                     hr(),
                     h3("Recognition and imputation to TM"),
                     p("In the figure below, the observed value is marked white, the TM is marked red and the LIM/UM is marked blue "),
                     plotOutput("totalgroupmissedmz"),
                     textOutput("total_nu"),
                     hr(),
                     h3("Recognition and imputation to LIM"),
                     p("The cutoffs used to determine the LIM can be modified:"),
                     p("please specifiy the missing ratio and the largest observed value. For a specific feature from specific biological group, if it contain more missing value than the threshold and the largeset observed value is smaller than the cutoff, it would be marked as low abundance induced missing"),
                     column(6,
                            sliderInput("misspercenthandling", "Percent of missing in each group",
                                        min = 0, max = 1, value = 0.2
                            )
                     ),
                     column(6,
                            sliderInput("lowquantile", "set a low intensity limit",
                                        min = 0, max = 0.2, value = 0.1
                            )
                     ),
                     p("In the figure below, the LIM is marked red, the UM is marked in white and the intensity of the observed value is represent by colour range"),
                     plotOutput("lowintensitymissing"),
                     textOutput("small_nu"),
                     h3("Missing value distribution before and after TM & LIM processing"),
                     p("The following figure shows the distribution of remaining missing value"),
                     column(6,
                            p("Before processing TM and LIM"),
                            plotOutput("missmatrixbefore")
                     ),
                     column(6,
                            p("After processing TM and LIM"),
                            plotOutput("missmatrix_low_fliter")
                     )
                     
                     )

           )
            
),
tabPanel("1.3 Imputation and evaluation to UM", fluid = TRUE,
         fluidPage(theme = shinytheme("cerulean"),
                   fluidRow(
                     h2("Introduction"),
                     p("Since the cause of the UM is unclear, the best imputation for the UM should be determined 
                       based on an evaluation of multiple imputed results. In SIM, three kinds of imputations are 
                       avaiable for UM imputation: (1) Local and (2) Global structure based imputation and (3) machine learning based imputation."),
                     p("The local structure based imputation replace the missing value based on 
                       the expression profiles of several other features with similar intensity profiles in the same 
                       dataset.This strategy ,in general, make the assumption that the features are regulated dependently,
                       and the highly correlated profiles are observed with coregulated features.
                       K nearest neighbors (KNN) and local least-squares (LLS) are two most often used methods:"),
                     p("The global-structure-based imputation methods apply dimension reduction techniques to decompose the
                       data matrix and then iteratively reconstruct the missing values.
                       Bayesian PCA (BPCA), Probabilistic PCA (PPCA) and SVD are most often used methods:"),
                     hr(),
                     h2("Imputation to UM"),
                     column(6,
                            #h2("Batch effects Removal"),
                            checkboxGroupInput("missbm", "Choose imputation methods:",
                                               c("kNN" = "kNN",
                                                 "LLS" = "LLS",
                                                 "bPCA" = "bPCA",
                                                 "pPCA" = "pPCA",
                                                 "SVD" = "SVD",
                                                 "MissForestImp" = "MissForestImp")),
                            actionButton("Imputation", "Apply these methods", class = "btn-primary")
                     ),
                     column(6,
                            #h2("5. Choose the best result to download"),
                            radioButtons("missdw", "Choose the best result to download:",
                                         c("kNN" = "kNN",
                                           "LLS" = "LLS",
                                           "bPCA" = "bPCA",
                                           "pPCA" = "pPCA",
                                           "SVD" = "SVD",
                                           "MissForestImp" = "MissForestImp")),
                            downloadLink('missdownloadDatas', 'Download')
                     )
                   ),
                   p("The imputation would take a long time to complete (especially the MissForest), which depends on the amount of your data, please wait. and do not clikc one button many times"),
                   h2("Evaluation to UM imputation"),
                   column(6,
                          tableOutput("umiatable")
                   ),
                   column(6,
                          plotOutput("umia")
                   )
                   
                   
                     )
         
         )
)
)