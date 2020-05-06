#scaled_data <- reactive({
#  return(readRDS("/Users/chaohuyecao/OneDrive - Københavns Universitet/phD/Expert analysis system/script/single model/data.RDS")
#)
#})
#-----------------------group UI-----------------------------
#pca group
output$sel_pca_group <- renderUI({
  list <- scaled_data()
  classchoose <- unique(as.character(list$class))
  selectizeInput(# Replace with what you want to have in sidebarPanel
    'inputId' = "pca_classsel"
    , 'label' = "Please select class labels:"
    , 'choices' = classchoose
    , 'selected' = ""  # pick first column in dataset as names
    , multiple = TRUE
  )
})
#plsda group
output$sel_plsda_group <- renderUI({
  list <- scaled_data()
  classchoose <- unique(as.character(list$class))
  selectizeInput(# Replace with what you want to have in sidebarPanel
    'inputId' = "plsda_classsel"
    , 'label' = "Please select class labels:"
    , 'choices' = classchoose
    , 'selected' = ""  # pick first column in dataset as names
    , multiple = TRUE
    , options = list(maxItems = 2)
  )
})
#----------------------PCA analysis----------------------
multi_pca_result <- reactive({
  data <- scaled_data()
  #saveRDS(data,"/Users/chaohuyecao/OneDrive - Københavns Universitet/phD/Expert analysis system/script/single model/data.RDS")
  cl <- input$pca_classsel
  data <- data[which(data$class %in% cl),]
  pcaresult <- single_pca_scaling(data)
  return(pcaresult)
})
#pca score
output$PCA_score <- renderPlot({
  pca_plot <- multi_pca_result()
  pca_plot$score
})
#pca loading
output$PCA_loading <- renderPlot({
  pca_plot <- multi_pca_result()
  pca_plot$loading
})
#--------------------heatmap--------------------------------
output$multivariate_clustering <- renderPlot({
  data <- scaled_data()
  cl <- input$pca_classsel
  data <- data[which(data$class %in% cl),]
  row.names(data) <- data$sample
  annotation_col = data.frame(Class = factor(data$class))
  rownames(annotation_col) = data$sample
  d <- t(data[,-c(1:5)])
  pheatmap(d,scale = "none",annotation_col = annotation_col, cluster_cols = T,cluster_rows = T,show_colnames=F,show_rownames = F)
})
#-----------------------PLSDA analysis-------------------
multi_plsda_result <- reactive({
  data <- scaled_data()
  cl <- input$plsda_classsel
  data <- data[which(data$class %in% cl),]
  row.names(data) <- data$sample
  plsda_result <- opls(as.matrix(data[,-c(1:5)]), as.character(data$class),
                       plotL=F,printL=F,log10L = FALSE)
  return(plsda_result)
})
#score
output$plsda_score <- renderPlot({
  x <- multi_plsda_result()
  d <- scaled_data()
  score <- as.data.frame(x@scoreMN)
  score$sample <- row.names(score)
  ns <- merge(d[,c(1:5)],score,by.x = "sample",by.y = "sample")
  p <- ggplot(ns,aes(p1,p2,colour=class))+
    geom_point()+
    stat_ellipse()+
    xlab(paste("PC1"," (",sprintf("%.2f%%",x@modelDF$R2X[1]*100),") ",sep=""))+
    ylab(paste("PC2"," (",sprintf("%.2f%%",x@modelDF$R2X[2]*100),") ",sep=""))+
    theme_bw()+
    theme(
      legend.position="top",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank())
  return(p)
})
#loading
output$plsda_loading <- renderPlot({
  mo <- multi_plsda_result()
  data <- scaled_data()
  plsda_loading_plot(mo,data)
})
#splot
output$plsda_s_plot <- renderPlot({
  mo <- multi_plsda_result()
  plsda_s_plot(mo)
})
#plsda_permutation
output$plsda_permutation <- renderPlot({
  mo <- multi_plsda_result()
  plsda_permetation(mo)
})
#plsda_model_r2_q2
output$plsda_model_r2_q2 <- renderPlot({
  mo <- multi_plsda_result()
  plsda_r2y_q2(mo)
})

#-----------------------OPLSDA analysis-------------------
multi_oplsda_result <- reactive({
  data <- scaled_data()
  cl <- input$plsda_classsel
  data <- data[which(data$class %in% cl),]
  row.names(data) <- data$sample
  oplsda_result <- opls(as.matrix(data[,-c(1:5)]), as.character(data$class),
                        predI = 2, orthoI = 1,plotL=F,printL=F,log10L = FALSE)
  return(oplsda_result)
})
#score
output$oplsda_score <- renderPlot({
  mo <- multi_oplsda_result()
  data <- scaled_data()
  plsda_score_plot(mo,data)
})
#loading
output$oplsda_loading <- renderPlot({
  mo <- multi_oplsda_result()
  data <- scaled_data()
  plsda_loading_plot(mo,data)
})
#splot
output$oplsda_s_plot <- renderPlot({
  mo <- multi_oplsda_result()
  plsda_s_plot(mo)
})
#plsda_permutation
output$oplsda_permutation <- renderPlot({
  mo <- multi_oplsda_result()
  plsda_permetation(mo)
})
#plsda_model_r2_q2
output$oplsda_model_r2_q2 <- renderPlot({
  mo <- multi_oplsda_result()
  plsda_r2y_q2(mo)
})
#------------------------Random Forest-------------------

#------------------------LESSO---------------------------