#-----------------------group UI-----------------------------
output$selgroup <- renderUI({
  list <- transformed_data()
  list <- list[(list$class != "QC"),]
  classchoose <- unique(as.character(list$class))
  selectizeInput(# Replace with what you want to have in sidebarPanel
    'inputId' = "classsel"
    , 'label' = "Please select two class labels:"
    , 'choices' = classchoose
    , 'selected' = ""  # pick first column in dataset as names
    , multiple = TRUE
    , options = list(maxItems = 2)
  )
})
#-----------------------univariate analysis-----------------------------
univariate_result <- reactive({
  #----------------parameters-----------------
  #compare group
  validate(
    need(length(input$classsel)==2, "Please choose two biological groups")
  )
  g <- input$classsel
  g1 <- g[1]
  g2 <- g[2]
  #dataset
  data <- transformed_data()
  data <- subset(data,class == g1 | class == g2)
  #-------------------analysis--------------
  if(input$pair_select == "Paired"){
    validate(need(input$pairfile != "", "Please upload list file"))
    inFile3 <- input$pairfile
    if (is.null(inFile3))
      return(NULL)
    pairinfor <- read.delim(inFile3$datapath,header = T,sep="\t")
    pairinfor <- subset(pairinfor,class == g1 | class == g2)
    validate(need(nrow(data) == nrow(pairinfor), "The length of the pair information is not the same to the samples informaiton"))
    data <- merge(pairinfor,data,by.x = "sample",by.y = "sample")
    data <- data[order(data$pair),]
    c1 <- which(data$class == g1)
    c2 <- which(data$class == g2)
    fold <- apply(data[,-c(1:6)],2,function(x){return(mean(x[c1]/x[c2]))})
    wilcox <- apply(data[,-c(1:6)],2,function(x){return(wilcox.test(x[c1],x[c2],paired=TRUE)$p.value)})
    t_test <- apply(data[,-c(1:6)],2,function(x){return(t.test(x[c1],x[c2],paired=TRUE)$p.value)})
  }else{
    c1 <- which(data$class == g1)
    c2 <- which(data$class == g2)
    fold <- apply(data[,-c(1:5)],2,function(x){return(mean(x[c1])/mean(x[c2]))})
    wilcox <- apply(data[,-c(1:5)],2,function(x){return(wilcox.test(x[c1],x[c2],paired=FALSE)$p.value)})
    t_test <- apply(data[,-c(1:5)],2,function(x){return(t.test(x[c1],x[c2],paired=FALSE)$p.value)})
  }
  res <- data.frame(foldchange=fold,Wilcox=wilcox,Ttest=t_test)
  return(res)
})
#------------------------Volcano plot-----------------------------------
output$Volcano_plot <- renderPlot({
  #univariate result
  univariate_res <- univariate_result()
  #parameter or nonparameter
  test_method <- input$par_nonpar_test
  if(test_method == "T-test"){
    univariate_res <- univariate_res[,c(1,3)]
  }else{
    univariate_res <- univariate_res[,c(1,2)]
  }
  #fdr
  fdr_method <- input$fdr_selection
  if(fdr_method=="None"){
    univariate_res$fdr <- univariate_res[,2]
    yl <- "-log10 (P-value)"
  }else if(fdr_method=="BH"){
    univariate_res$fdr <- p.adjust(univariate_res[,2],"BH")
    yl <- "-log10 (BH corrected P-value)"
  }else if(fdr_method=="bonferroni"){
    univariate_res$fdr <- p.adjust(univariate_res[,2],"bonferroni")
    yl <- "-log10 (Bonferroni corrected P-value)"
  }
  #foldchange cutoff
  fold_cut <- input$foldchange_cutoff
  #sig cutoff
  sig_cut <- input$significance_cutoff
  univariate_res$mark <- ifelse(((univariate_res$foldchange>fold_cut | univariate_res$foldchange < 1/fold_cut) & univariate_res$fdr < sig_cut),"sig","normal")
  univariate_res %>% 
    ggplot(aes(log2(foldchange),-log10(fdr),colour=mark))+
    geom_point() +
    theme_bw() +
    ylab(yl)
})
#------------------------Download---------------------------------------