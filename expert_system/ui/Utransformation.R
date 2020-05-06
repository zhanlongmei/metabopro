tabPanel("5.Transformation&Scaling", fluid = TRUE,
    fluidPage(theme = shinytheme("cerulean"),
      fluidRow(
        h2("How to conduct transformation and scaling?"),
        p("This two figures represents the percent of missing.
          If your missing data composes less than 5% of the total datasets.
          It means that you have a relative low missing ratio"),
        h2("Transformation and Scaling:"),
        column(6,
          h4("Transformation approaches:"),
          radioButtons("transformation_method", "Choose transformation methods:",
                           c("None" = "None",
                             "Log" = "Log",
                             "Cubic" = "Cubic",
                             "Power" = "Power"))
          #actionButton("transformation_act", "Apply the selected methods", class = "btn-primary")
        ),
        column(6,
          h4("Scaling approaches:"),
          radioButtons("scaling_method", "Choose scaling methods:",
                             c("None" = "None",
                               "Auto" = "Auto",
                               "Pareto" = "Pareto",
                               "Level" = "Level",
                               "Range" = "Range",
                               "Vast" = "Vast"))
          #actionButton("scaling_act", "Apply the selected scaling method", class = "btn-primary")
        ),
        h2("Evaluation to transformation and scaling"),
        h5("Normality and heterosedasticity"),
        tableOutput("evaluation_scaling_normality"),
        column(3,
               h5("P value distribution"),
               plotOutput("evaluation_scaling_pvalue")
        ),
        column(3,
               h5("PCA score plot"),
               plotOutput("evaluation_scaling_pcascore")
        ),
        column(3,
               h5("PCA loading plot"),
               plotOutput("evaluation_scaling_pcaloading")
        ),
        column(3,
               h5("correlation between loading and intensity"),
               plotOutput("evaluation_loading_intensity")
        )
    )
  )
)


