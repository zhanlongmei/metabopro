#-------------log transformation-------------
Transformation=function(x,method){
  #x <- ad_limma
  row.names(x) <- x$sample
  y <- x[,-c(1:5)]
  y[y<0] <- NA
  y <- impute.knn(as.matrix(y))$data
  if(method == "Log"){
    min.val <- min(abs(y[y!=0 & !is.na(y)]))/10;
    y2 <- cbind(x[,c(1:5)],log2((y + sqrt(y^2 + min.val^2))/2))
    row.names(y2) <- y2$sample
    return(y2)
  } else if(method == "Cubic"){
    min.val <- min(abs(y[y!=0 & !is.na(y)]))/10;
    y[is.na(y) | y <=0] <- min.val
    norm.data <- cbind(x[,c(1:5)],abs(y)^(1/3))
    row.names(norm.data) <- norm.data$sample
    return(norm.data)
  }else if(method == "Power"){
    res <- apply(y, 1, function(a) sqrt(a) - mean(sqrt(a)))
    res <- as.data.frame(t(res))
    res$sample <- row.names(res)
    res <- left_join(x[,c(1:5)],res,by="sample")
    return(res)
  }else{
    return(x)
  }
}

Scaling <- function(x,method){
  #x <- cube_combat
  rData <- t(x[,-c(1:5)])
  rData[rData<0] <- NA
  rData <- impute.knn(rData)$data
  if(method == "Auto"){
    ## for each row - metabolite
    res <- apply(rData, 1, function(x) (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE))
  }else if(method == "Range"){
    res <- apply(rData, 1, function(x) (x - mean(x, na.rm = TRUE))/(range(x)[2] -  range(x)[1]))
  }else if(method == "Pareto"){
    res <- apply(rData, 1, function(x) (x - mean(x, na.rm = TRUE))/sqrt(sd(x,na.rm = TRUE)))
  }else if(method == "Vast"){
    res <- apply(rData, 1, function(x) mean(x, na.rm = TRUE) * (x - mean(x, na.rm = TRUE))/(sd(x, na.rm = TRUE)^2))
  }else if(method == "Level"){
    res <- apply(rData, 1, function(x) (x - mean(x, na.rm = TRUE))/mean(x,na.rm = TRUE))
  }else{
    res <- apply(rData, 1, function(x) {x})
  }
  
  res <- as.data.frame(res)
  res$sample <- row.names(res)
  res <- left_join(x[,c(1:5)],res,by="sample")
  return(res)
}

#----------evaluation----------------------
#------p value dsitribution
scaling_class_p_anova <- function(x){
  #x <- ratiod
  x <- subset(x,class != "QC")
  x$class <- as.factor(x$class)
  p_dis <- apply(x[,-c(1:5)],2,function(a){
    return(summary(aov(va ~ cl,data=data.frame(va=a,cl=x$class)))[[1]][["Pr(>F)"]][1])
  })
  return(data.frame(p_value=p_dis) %>%
           ggplot(aes(p_value))+geom_histogram() +
           theme_bw())
}
#------pca
single_pca_scaling <- function(d){
  #d <- data
  row.names(d) <- d$sample
  p <- d[,-c(1:5)] #p[c(1:5),c(1:5)]
  row.names(p) <- d$sample
  #---------mean intensity-----------
  mi <- apply(d[,-c(1:5)],2,mean)
  #---------pca-----------------------
  pcamodel <- PCA(p,scale.unit = FALSE,graph = FALSE)
  eig.val <- pcamodel$eig[,2]
  loadings <- pcamodel$var$coord[,c(1,2)]        # Contributions to the PCs
  scores <- pcamodel$ind$coord[,c(1,2)]      # Contributions to the PCs
  d$batch <- as.factor(d$batch)
  d$class <- as.factor(d$class)
  pca_class_plot <- scores %>%
    as.data.frame() %>%
    mutate(sample=row.names(scores)) %>%
    merge(d[,c(1:5)],by.x = "sample",by.y = "sample") %>%
    ggplot(aes(Dim.1,Dim.2,color=class))+geom_point()+
    stat_ellipse()+
    xlab(paste("PC1"," (",sprintf("%.2f%%",eig.val[1]),") ",sep=""))+
    ylab(paste("PC2"," (",sprintf("%.2f%%",eig.val[2]),") ",sep=""))+
    theme_bw()+
    theme(
      legend.position="top",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank())
  pca_loading_plot <- loadings %>%
    as.data.frame() %>%
    ggplot(aes(Dim.1,Dim.2))+geom_point()+
    stat_ellipse()+
    xlab(paste("PC1"," (",sprintf("%.2f%%",eig.val[1]),") ",sep=""))+
    ylab(paste("PC2"," (",sprintf("%.2f%%",eig.val[2]),") ",sep=""))+
    theme_bw()
  #--------------cbind-------------------------------
  load_cor <- as.data.frame(cbind(loadings,mi)) %>%
    ggplot(aes(abs(Dim.1),mi))+geom_point()+
    theme_bw()+
    xlab("Loadings")+ylab("Mean intensity")
  return(list(score=pca_class_plot,loading=pca_loading_plot,corr=load_cor))
}
#------normality test
normality_test <- function(x){
  #x <- ratiod
  bn <- apply(x[,-c(1:5)],2,function(a){
    #a <- x[,6]
    qq <- aggregate(a,by=list(x$class),function(b){shapiro.test(b)$p.value})
    qx <- qq$x
    names(qx) <- qq$Group.1
    return(qx)
  })
  b <- apply(bn,1,function(d){sum(d>0.05)/length(d)})
  return(data.frame(class=names(b),ratio_of_features_normally_distributed=b))
}
