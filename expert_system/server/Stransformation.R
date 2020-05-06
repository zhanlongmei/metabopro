#------------------transformation and scaling -----------------------------
transformed_data <- reactive({
  data <- best_normalized()
  nc <- input$transformation_method
  data <- Transformation(data,nc)
  return(data)
})

scaled_data <- reactive({
  data <- transformed_data()
  nc <- input$scaling_method
  data <- Scaling(data,nc)
  return(data)
})
#------------------evaluation---------------------
output$evaluation_scaling_normality <- renderTable({
  data <- scaled_data()
  return(normality_test(data))
})
#----------p value distribution
output$evaluation_scaling_pvalue <- renderPlot({
  data <- scaled_data()
  scaling_class_p_anova(data)
})
#--------pca
scaling_pca <- reactive({
  data <- scaled_data()
  pcaresult <- single_pca_scaling(data)
  return(pcaresult)
})
#-------pca score
output$evaluation_scaling_pcascore <- renderPlot({
  pca_plot <- scaling_pca()
  pca_plot$score
})
#-----pca loading
output$evaluation_scaling_pcaloading <- renderPlot({
  pca_plot <- scaling_pca()
  pca_plot$loading
})
#------loading intensity correlation
output$evaluation_loading_intensity <- renderPlot({
  pca_plot <- scaling_pca()
  pca_plot$corr
})