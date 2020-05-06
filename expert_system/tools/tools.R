#------------------------common---------------------------
ratio_count <- function(x){
  sum(is.na(x))/length(x)
}
rnorm_fixed = function( n,mu, sigma) {
  #mu <- mean_l
  #sigma <- sd_l
  #n <- 20
  x = rnorm(n)  # from standard normal distribution
  x = sigma * x / sd(x)  # scale to desired SD
  x = x - mean(x) + mu  # center around desired mean
  x[which(x<0)] <- abs(x[which(x<0)])
  return(x)
}
logTransform=function(x){
  #x <- ad_limma
  y <- x[,-c(1:5)]
  y[y<0] <- NA
  y <- impute.knn(as.matrix(y))$data
  
  min.val <- min(abs(y[y!=0 & !is.na(y)]))/10;
  y2 <- cbind(x[,c(1:5)],log2((y + sqrt(y^2 + min.val^2))/2))
  row.names(y2) <- y2$sample
  return(y2)
}
pca_plot <- function(data,titlein){
  #get the data
  impdata <- as.matrix(data[,-c(1,2,3,4,5)])
  row.names(impdata) <- data$sample
  print(impdata[c(1:5),c(1:5)])
  diagnosis <- as.numeric(as.factor(data$class))
  #pca analysis
  imp.pr <- prcomp(impdata, scale = TRUE, center = TRUE)
  plotda <- cbind(as.data.frame(imp.pr$x[, c(1, 2)]),data$class)
  names(plotda)[3] <- "sgroup"
  ggplot(plotda,aes(PC1,PC2))+
    geom_point(aes(colour=sgroup))+
    stat_ellipse(aes(x=PC1, y=PC2,color=sgroup),type = "norm")+
    labs(title=titlein)
}
#----------------------imputation-----------------------------
kNN <- function(datas){
  # datas <- data
  data <- datas[,-c(1,2,3,4,5)]
  data2 <- impute.knn(as.matrix(data))
  data3 <- data2$data
  data3[data3<0] <- 0.01
  data <- cbind(datas[,c(1,2,3,4,5)],data3)
  return(data)
}
imp_min <- function(x){
  #x <- pl_f
  data <- x[,-c(1,2,3,4,5)]
  samll_value <- as.numeric(data[data<quantile(data,probs = 0.01,na.rm = T)])
  m <- mean(samll_value[!is.na(samll_value)])
  s <- sd(samll_value[!is.na(samll_value)])
  nn <- sum(is.na(data))
  data[is.na(data)] <- rnorm_fixed(nn,m,s)
  imputed <- cbind(x[,c(1,2,3,4,5)],data)
  return(imputed)
}
pPCA <- function(datas){
  # data <- cdnal
  data <- datas[,-c(1,2,3,4,5)]
  row.names(data) <- datas$sample
  data2 <- t(data)
  pc <- pca(data2,nPcs=2,method="ppca")
  imputed <- completeObs(pc)
  imputed[imputed<0] <- rnorm_fixed(sum(imputed<0),mean(imputed),mean(imputed))
  data <- cbind(datas[,c(1,2,3,4,5)],as.data.frame(t(imputed)))
  return(data)
}
MissForestImp <- function(datas){
  data <- datas[,-c(1,2,3,4,5)]
  data2 <- missForest(data)$ximp
  data2[data2<0] <- 0.01
  data <- cbind(datas[,c(1,2,3,4,5)],data2)
  return(data)
}
bPCA <- function(datas){
  data <- datas[,-c(1,2,3,4,5)]
  row.names(data) <- datas$sample
  data2 <- t(data)
  pc <- pca(data2,nPcs=2,method="bpca")
  imputed <- completeObs(pc)
  imputed[imputed<0] <- 0.01
  data <- cbind(datas[,c(1,2,3,4,5)],as.data.frame(t(imputed)))
  return(data)
}
SVD <- function(datas){
  data <- datas[,-c(1,2,3,4,5)]
  row.names(data) <- datas$sample
  data2 <- t(data)
  pc <- pca(data2,nPcs=2,method="svdImpute")
  imputed <- completeObs(pc)
  imputed[imputed<0] <- 0.01
  data <- cbind(datas[,c(1,2,3,4,5)],as.data.frame(t(imputed)))
  return(data)
}
LLS <- function(datas){
  data <- datas[,-c(1,2,3,4,5)]
  data1 <- t(data)
  set.seed(5)
  result <- llsImpute(data1, k = 10, correlation="pearson", allVariables=TRUE)
  imputed <- completeObs(result)
  imputed[imputed<0] <- 0.01
  data2 <- cbind(datas[,c(1,2,3,4,5)],as.data.frame(t(imputed)))
  return(data2)
}
Mindet <- function(datas){
  #datas <- cdnaf
  data <- datas[,-c(1,2,3,4,5)]
  data2 <- impute.MinDet(t(data))
  data <- cbind(datas[,c(1,2,3,4,5)],t(data2))
  return(data)
}
#-----------------------noise filtering-------------------------
noisefilter <- function(d,cv_cutoff){
  #d <- pl_c
  #cv_cutoff <- 0.4
  dqc <- subset(d,class == "QC")
  batch <- factor(unique(d$batch))
  batch_cv <- lapply(batch,function(x){
    #x <- 1
    m <- filter(dqc,batch == x)[,-c(1:5)]
    mcv <- apply(m,2,function(k){sd(k)/mean(k)})
    return(data.frame(batch=x,cv=mcv))
  })
  batch_cv <- do.call("rbind",batch_cv)
  noise_mz <- unique(row.names(batch_cv[which(batch_cv$cv > cv_cutoff),]))
  #------------------计算qc与样本偏离严重的离子----------------------
  batch_far <- lapply(batch,function(x){
    qc <- which(d$class == "QC" & d$batch == x)
    sa <- which(d$class != "QC" & d$batch == x)
    si <- apply(d[,-c(1:5)],2,function(a){
      #a <- d[,6]
      f1 <- mean(a[qc])/mean(a[sa])
      p1 <- t.test(log(a[qc]),log(a[sa]))$p.value
      o <- c(f1,p1)
      names(o) <- c("f1","p1")
      return(o)
    }) %>%
      t() %>%
      as.data.frame() 
    s1 <- which((si$f1>1.5 |si$f1 <0.5) & si$p1 <0.05)
    return(row.names(si[s1,]))
  })
  noise_mz <- union(noise_mz,do.call("c",batch_far))
  return(d[,!(names(d) %in% noise_mz)])
}
#----------------Check the existence of batch effect --------
RawBox_plot <- function(x){
  #x <- data
  melt(x,id=c("sample","batch","class","order","mark")) %>%
    filter(class=="QC") %>%
    ggplot(aes(factor(order),log(value),colour=factor(batch)))+
    geom_boxplot() +
    theme_bw()+
    theme(
      legend.position="top",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank())
}
batch_pca_plot <- function(d){
  #d <- pl_c
  row.names(d) <- d$sample
  l <- d[,c(1:5)]
  #---------------pca
  x2 <- d
  p <- logTransform(x2)[,-c(1:5)] #p[c(1:5),c(1:5)]
  row.names(p) <- x2$sample
  pcamodel <- PCA(p,scale.unit = FALSE,graph = FALSE)
  eig.val <- pcamodel$eig[,2]
  scores <- pcamodel$ind$coord[,c(1,2)]      # Contributions to the PCs
  l$batch <- as.factor(l$batch)
  pca_batch_plot <- scores %>%
    as.data.frame() %>%
    mutate(sample=row.names(scores)) %>%
    merge(l,by.x = "sample",by.y = "sample") %>%
    ggplot(aes(Dim.1,Dim.2,color=batch))+geom_point()+
    stat_ellipse()+
    xlab(paste("PC1"," (",sprintf("%.2f%%",eig.val[1]),") ",sep=""))+
    ylab(paste("PC2"," (",sprintf("%.2f%%",eig.val[2]),") ",sep=""))+
    theme_bw()+
    theme(
      legend.position="top",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank())
  return(pca_batch_plot)
}
raw_within_batch_heatmap <- function(d,t){
  #d <- pl_c
  b <- unique(d$batch)[1]
  cv <- 0.3
  co <- 0.6
  df <- subset(d,class=="QC" & batch==b)
  mzcv <- apply(df[,-c(1:5)],2,function(k){sd(k)/mean(k)})
  mzcv <- mzcv[mzcv<cv]
  df <- cbind(df[,c(1:5)],df[,names(df) %in% names(mzcv)])
  cof <- apply(df[,-c(1:5)],2,function(k){cor(df$order,k)})
  o <- c(length(cof),sum(abs(cof)>co),sum(abs(cof)>co)/length(cof))
  names(o) <- c("stable mz","high cor mz","ratio")
  mz <- names(cof[abs(cof)>co])
  
  d <- d[order(d$order),] %>% filter(class=="QC" & batch==b)
  d <- cbind(d[,c(1:5)],d[,names(d) %in% mz])
  xr <- t(d[,-c(1:5)]) %>% as.data.frame()
  names(xr) <- d$order #p[c(1:5),c(1:5)]
  annotation_col = data.frame(
    Batch = factor(d$batch)
  )
  rownames(annotation_col) = d$order
  pheatmap(xr,border_color=NA, cluster_cols = F,cluster_rows = T,show_colnames=T,show_rownames = F,scale="row",main=t)
}
#----------------between batch effect------------
batch_p <- function(ll){
  n <- ll$name  #n <- "combat"
  d <- ll$data  #d <- ratiod
  d <- cbind(d[,c(1:5)],apply(d[,-c(1:5)],2,function(a){as.numeric(as.character(a))}))
  #p value of batch
  dp <- subset(d,class == unique(d$class)[1])
  p_cal <- function(x){
    #x <- dp$M101T91
    if(n =="eigenms"){
      x <- as.numeric(as.character(x))
    }else{
      x <- log(as.numeric(as.character(x)))
    }
    p_value <- anova(lm(x~dp$batch))$"Pr(>F)"[1]
    return(p_value)
  }
  pc <- data.frame(
    mark=n,
    p_value=apply(dp[,-c(1:5)],2,p_cal))
  pc$mz <- row.names(pc)
  return(pc)
}
single_compare_pca <- function(ll){
  x <- ll$name    #x <- "none"
  d <- ll$data    #d <- inputl$eigenms$data
  row.names(d) <- d$sample
  l <- d[,c(1:5)]
  #---------------pca
  x2 <- d
  #x2 <- scaling(x2,"pareto")
  if(x=="eigenms"){
    p <- x2[,-c(1:5)] #p[c(1:5),c(1:5)]
  }else{
    p <- logTransform(x2)[,-c(1:5)] #p[c(1:5),c(1:5)]
  }
  row.names(p) <- x2$sample
  
  pcamodel <- PCA(p,scale.unit = FALSE,graph = FALSE)
  eig.val <- pcamodel$eig[,2]
  loadings <- pcamodel$var$coord[,c(1,2)]        # Contributions to the PCs
  scores <- pcamodel$ind$coord[,c(1,2)]      # Contributions to the PCs
  l$batch <- as.factor(l$batch)
  l$class <- as.factor(l$class)
  pca_batch_plot <- scores %>%
    as.data.frame() %>%
    mutate(sample=row.names(scores)) %>%
    merge(l,by.x = "sample",by.y = "sample") %>%
    ggplot(aes(Dim.1,Dim.2,color=batch))+geom_point()+
    stat_ellipse()+
    labs(title = x)+
    theme_bw()+
    theme(
      legend.position="null",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank())+
    theme(axis.title.x = element_blank())+
    theme(axis.title.y = element_blank())
  pca_class_plot <- scores %>%
    as.data.frame() %>%
    mutate(sample=row.names(scores)) %>%
    merge(l,by.x = "sample",by.y = "sample") %>%
    ggplot(aes(Dim.1,Dim.2,color=class))+
    stat_ellipse()+
    geom_point()+
    labs(title = x)+
    theme_bw()+
    theme(
      legend.position="null",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank())+
    theme(axis.title.x = element_blank())+
    theme(axis.title.y = element_blank())
  scores <- as.data.frame(scores) %>%
    mutate(sample=row.names(scores)) %>%
    merge(l,by.x="sample",by.y = "sample")
  return(list(batch=pca_batch_plot,class=pca_class_plot,score=scores,pcs=eig.val))
}
class_p <- function(ll){
  n <- ll$name    #x <- "combat"
  d <- ll$data    #d <- combatd
  d <- cbind(d[,c(1:5)],apply(d[,-c(1:5)],2,function(a){as.numeric(as.character(a))}))
  l <- d[,c(1:5)]
  #p value of batch
  dp <- subset(d,class !="QC")
  p_cal <- function(x){
    #x <- dp[,6]
    if(n=="eigenms"){
      x <- as.numeric(as.character(x))
    }else{
      x <- log(as.numeric(as.character(x)))
    }
    p_value <- anova(lm(x~dp$class))$"Pr(>F)"[1]
    return(p_value)
  }
  pc <- data.frame(
    mark=n,
    p_value=apply(dp[,-c(1:5)],2,p_cal))
  return(pc)
}
single_overlap <- function(k){
  #k <- ou_pca$none
  nsc <- k$score
  pcs <- k$pcs
  
  dmin1 <- aggregate(nsc$Dim.1,by=list(nsc$batch),min)
  dmax1 <- aggregate(nsc$Dim.1,by=list(nsc$batch),max)
  d_min_min1 <- min(dmin1$x)
  d_min_max1 <- max(dmin1$x)
  d_max_min1 <- min(dmax1$x)
  d_max_max1 <- max(dmax1$x)
  if(d_min_max1 > d_max_min1){
    pc1 <- 0
  }else{
    pc1 <- (d_max_min1 - d_min_max1)/(d_max_max1 - d_min_min1)
  }
  dmin2 <- aggregate(nsc$Dim.2,by=list(nsc$batch),min)
  dmax2 <- aggregate(nsc$Dim.2,by=list(nsc$batch),max)
  d_min_min2 <- min(dmin2$x)
  d_min_max2 <- max(dmin2$x)
  d_max_min2 <- min(dmax2$x)
  d_max_max2 <- max(dmax2$x)
  if(d_min_max2 > d_max_min2){
    pc2 <- 0
  }else{
    pc2 <- (d_max_min2 - d_min_max2)/(d_max_max2 - d_min_min2)
  }
  return((pc1*pcs[1]+pc2*pcs[2])/(pcs[1]+pcs[2]))
}
#-----------------within batch effect------------
single_data_within_ratio <- function(mm){
  x <- mm$data
  batch <- as.character(unique(x$batch))
  single_cl_b <- function(d,b,cv,co){
    #d <- pl_c
    #b <- 1
    #cv <- 0.3
    #co <- 0.6
    df <- subset(d,class=="QC" & batch==b)
    cof <- apply(df[,-c(1:5)],2,function(k){cor(df$order,k)})
    return(sum(abs(cof[!is.na(cof)])>co)/length(cof))
  }
  return(mean(sapply(batch,function(k){single_cl_b(x,k,0.3,0.6)})))
}
