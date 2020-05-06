#-------------------pca-----------------------
output$Quality_pca <- renderPlot({
  x <- batchdwf()
  p <- logTransform(x)[,-c(1:5)] #p[c(1:5),c(1:5)]
  row.names(p) <- x$sample
  
  pcamodel <- PCA(p,scale.unit = FALSE,graph = FALSE)
  eig.val <- pcamodel$eig[,2]
  scores <- pcamodel$ind$coord[,c(1,2)]      # Contributions to the PCs
  
  pca_class_plot <- scores %>%
    as.data.frame() %>%
    mutate(sample=row.names(scores)) %>%
    merge(x[,c(1:5)],by.x = "sample",by.y = "sample") %>%
    ggplot(aes(Dim.1,Dim.2,color=class))+geom_point()+
    stat_ellipse()+
    xlab(paste("PC1"," (",sprintf("%.2f%%",eig.val[1]),") ",sep=""))+
    ylab(paste("PC2"," (",sprintf("%.2f%%",eig.val[2]),") ",sep=""))+
    theme_bw()+
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank())+
    theme(legend.position = "top")
  #saveRDS(pca_class_plot,paste(r,"qc_pca.RDS",sep = "/"))
  pca_class_plot
})

#------------------CV distribution------------
output$Quality_CV <- renderPlot({
  x <- batchdwf()
  x <- cbind(x[,c(1:5)],apply(x[,-c(1:5)],2,function(a){as.numeric(as.character(a))}))
  acv <- sapply(unique(x$class),function(a){
    #a <- "QC"
    xa <- subset(x,class == a)
    cv <- apply(xa[,-c(1:5)],2,function(b){sd(b)/mean(b)})
    return(cv)
  },simplify = T) %>% as.data.frame() %>%
    gather(key="Group",value="CV") %>%
    ggplot(aes(CV,colour=Group))+stat_ecdf()+
    ylab("Percentage")+
    theme_bw()+
    theme(legend.position = "top")
  #saveRDS(acv,paste(r,"qc_cv.RDS",sep = "/"))
  acv
})
#-----------------QC correlation--------------
output$Quality_correlation <- renderPlot({
  x <- batchdwf()
  row.names(x) <- x$sample
  x <- x[which(x$class=="QC"),]
  annotation_col = data.frame(
    Batch = factor(x$batch)
  )
  rownames(annotation_col) = x$sample
  d <- t(x[,-c(1:5)])
  cd <- cor(log(d))
  #print(cd)
  #print(annotation_col)
  #pheatmap(cd,scale="none",cluster_cols = T,cluster_rows = T,show_colnames=F,show_rownames = F)
  pheatmap(cd,scale="none",annotation_col = annotation_col, cluster_cols = T,cluster_rows = T,show_colnames=F,show_rownames = F)
})
