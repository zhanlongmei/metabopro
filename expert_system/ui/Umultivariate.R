tabPanel("7.Multivariate analysis", fluid = TRUE,
  tabsetPanel(
#---------------tabPanel 4.1----------------------------------------
    tabPanel("7.1 PCA/Clustering", fluid = TRUE,
      fluidPage(theme = shinytheme("cerulean"),
        fluidRow(
          column(6,
                 h2("Select samples"),
                 uiOutput("sel_pca_group")
          ),
          column(6,
                 h2("Clustering analysis"),
                 plotOutput("multivariate_clustering")
          ),
          
          h2("PCA analysis"),
          column(6,
                 h5("PCA score plot:"),
                 plotOutput("PCA_score")
          ),
          column(6,
                 h5("PCA loading plot:"),
                 plotOutput("PCA_loading")
          )
        )
      )
  ),
#---------------tabPanel 4.2----------------------------------------
    tabPanel("7.2 PLS-DA analysis", fluid = TRUE,
      fluidPage(theme = shinytheme("cerulean"),
        fluidRow(
          h2("Introduction to PLS-DA analysis"),
          p("This two figures represents the percent of missing.
            If your missing data composes less than 5% of the total datasets.
            It means that you have a relative low missing ratio"),
          h2("PLS-DA analysis"),
          h2("Select samples"),
          uiOutput("sel_plsda_group"),
          column(6,
                 h5("PLSDA score plot"),
                 plotOutput("plsda_score")
          ),
          column(6,
                 h5("PLSDA loading plot"),
                 plotOutput("plsda_loading")
          ),
          column(4,
                 h5("PLSDA S-plot"),
                 plotOutput("plsda_s_plot")
          ),
          column(4,
                 h5("PLSDA model"),
                 plotOutput("plsda_model_r2_q2")
          ),
          column(4,
                 h5("PLSDA permuattion"),
                 plotOutput("plsda_permutation")
          ))

      )
    )

  )
)



