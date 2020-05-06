tabPanel("6.Univariate analysis", fluid = TRUE,
    fluidPage(theme = shinytheme("cerulean"),
        fluidRow(
          h2("Parameter setting"),
          column(6,
                 h5("Please select two biological groups"),
                 uiOutput("selgroup")
          ),
          column(6,
                 h5("Is your design paired or unpaired?"),
                 radioButtons("pair_select", "Is your design paired or unpaired?",
                              c("Unpaired" = "Unpaired",
                                "Paired" = "Paired"))
          ),

          h6("Note: If your study design is paired, you will have to upload the paired information"),
          conditionalPanel(
            condition = "input.pair_select == 'Paired'",
            fileInput("pairfile", "Choose pair information File",
                      accept = c(
                        "text/txt",
                        "text/comma-separated-values,text/plain",
                        ".txt"))
          ),
          hr(),
          column(6,
                 h5("Parameter or Noneparameter testing?"),
                 radioButtons("par_nonpar_test", "Choose the significant testing:",
                              c("T-test" = "T-test",
                                "Wilcox" = "Wilcox"))
          ),
          column(6,
                 h5("Select FDR correction strategy"),
                 radioButtons("fdr_selection", "Choose the FDR method:",
                              c("None" = "None",
                                "BH" = "BH"))
          ),
          hr(),
          column(6,
                 h5("Set the cutoff for foldchange"),
                 sliderInput("foldchange_cutoff", "set a cut off of foldchange",
                             min = 0, max = 5, value = 1.5,step = 0.1)
          ),
          column(6,
                 h5("Set the cutoff for significance"),
                 sliderInput("significance_cutoff", "set a cut off of significance",
                             min = 0, max = 0.1, value = 0.05)
          ),
          hr(),
          h5("Download the unvariate analysis results"),
          downloadButton("downloadData", label = "Download"),
          hr(),
          h2("Volcano plot"),
          plotOutput("Volcano_plot")
        )
    )
)




