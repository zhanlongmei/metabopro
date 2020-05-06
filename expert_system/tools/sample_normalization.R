#-----------------------noise filtering-------------------------
cvfilter_after_sample_normalization <- function(d){
  cv_cutoff <- 0.3
  dqc <- subset(d,class == "QC")
  mcv <- apply(dqc[,-c(1:5)],2,function(k){sd(k)/mean(k)})
  noise_mz <- names(mcv[mcv > cv_cutoff])
  return(d[,!(names(d) %in% noise_mz)])
}

#----------------------sample normalization approaches--------------------
SUM <- function(x){
  #x <- pl_c
  row.names(x) <- x$sample
  gm <- mean(apply(x[,-c(1:5)],1,function(x){sum(x,na.rm=TRUE)}))
  sd <- apply(as.matrix(x[,-c(1:5)]),1,function(x){x/sum(x)}) *gm
  sd <- as.data.frame(t(sd)) %>% mutate(sample=colnames(sd))
  sd <- left_join(x[,c(1:5)],sd,by="sample")
  row.names(sd) <- sd$sample
  return(cvfilter_after_sample_normalization(sd))
}

Quantile <- function(x){
  #x <- pl_c
  row.names(x) <- x$sample
  dat <- t(x[,-c(1:5)])
  sd <- preprocessCore::normalize.quantiles(dat)
  row.names(sd) <- row.names(dat)
  colnames(sd) <- colnames(dat)
  sd <- as.data.frame(t(sd)) %>% mutate(sample=colnames(sd))
  sd <- left_join(x[,c(1:5)],sd,by="sample")
  row.names(sd) <- sd$sample
  return(cvfilter_after_sample_normalization(sd))
}

PQN <- function(x){
  #x <- pl_c
  row.names(x) <- x$sample
  dat <- t(x[,-c(1:5)])
  sd <- apply(dat,2,function(y){y/sum(y,na.rm=TRUE)})
  
  nsd <- t(apply(sd,1,function(y){y/median(y,na.rm=TRUE)}))
  coe <- apply(nsd, 2, median, na.rm = TRUE)
  
  for(i in 1:ncol(sd)){
    sd[,i] <- sd[,i]/coe[i]
  }
  sd <- sd * mean(apply(dat,2,function(x){sum(x,na.rm=TRUE)}))
  
  sd <- as.data.frame(t(sd)) %>% mutate(sample=colnames(sd))
  sd <- left_join(x[,c(1:5)],sd,by="sample")
  row.names(sd) <- sd$sample
  return(cvfilter_after_sample_normalization(sd))
}

VSN <- function(x){
  #x <- pl_c
  row.names(x) <- x$sample
  dat <- t(x[,-c(1:5)])
  
  fdata <- ExpressionSet(assayData=dat)
  fit<-vsn2(fdata)
  fitdata = predict(fit, newdata=fdata)  ## apply fit
  nd<-exprs(fitdata)
  #sd <- nd
  sd <- 2^nd
  
  sd <- as.data.frame(t(sd)) %>% mutate(sample=colnames(sd))
  sd <- left_join(x[,c(1:5)],sd,by="sample")
  row.names(sd) <- sd$sample
  return(cvfilter_after_sample_normalization(sd))
}
#--------------------evaluation-----------------------------------------
#---------boxplot
normalization_single_boxplot <- function(x){
  p <- melt(x,id=c("sample","batch","class","order","mark")) %>%
    filter(class != "QC") %>%
    ggplot(aes(factor(order),log(value),colour=factor(class)))+
    geom_boxplot() +
    theme_bw()+
    theme(
      legend.position="top",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank())
  return(p)
}
normalization_all_boxplot <- function(sl){
  n <- sl$name    #x <- "combat"
  d <- sl$data    #d <- combatd
  b <- normalization_single_boxplot(d)
  b <- b + labs(title = n)
  return(b)
}
#----------evaluation classification accuracy
normalization_single_ca <- function(sl){
  #get the data
  data <- sl$data
  impdata <- as.matrix(data[,-c(1,2,3,4,5)])
  row.names(impdata) <- data$sample
  diagnosis <- as.numeric(as.factor(data$class))
  #pca analysis
  imp.pr <- prcomp(impdata, scale = TRUE, center = TRUE)
  imp.pcs <- imp.pr$x[,1:10]
  imp.pcst <- cbind(imp.pcs, diagnosis)
  lda_ca <- function(x){
    #train and test dataset
    #N <- nrow(imp.pcst)
    N <- nrow(x)
    rvec <- runif(N)
    imp.pcst.train <- x[rvec < 0.75,]
    imp.pcst.test <- x[rvec >= 0.75,]
    nrow(imp.pcst.test)
    #lda analysis
    imp.pcst.train.df <- as.data.frame(imp.pcst.train)
    imp.lda <- lda(diagnosis ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data = imp.pcst.train.df)
    imp.pcst.test.df <- as.data.frame(imp.pcst.test)
    imp.lda.predict <- predict(imp.lda, newdata = imp.pcst.test.df)
    pred <- cbind(imp.pcst.test.df,imp.lda.predict$class)
    accuracy <- sum(pred$diagnosis == pred$`imp.lda.predict$class`)/nrow(pred)
  }
  total_acc <- NULL
  for(i in c(1:200)){
    tryCatch({ac1 <- lda_ca(imp.pcst)
    total_acc <- c(total_acc,ac1)
    },
    error=function(e){cat("error",conditionMessage(e),"\n\n")},
    finally={print("yes")})
    
  }
  return(round(mean(total_acc),2))
}
#-------------------------mean CV
normalization_cv_single <- function(sl){
  #x <- pl_c
  x <- sl$data
  n <- sl$name
  cv <- sapply(unique(x$class),function(a){
    apply(subset(x,class==a)[,-c(1:5)],2,function(b){sd(b)/mean(b)})
  },simplify = T)
  return(data.frame(method=n,class=unique(x$class),CV=apply(cv,2,mean)))
}
#----------between group variance/within group variance
singl_inter_vs_intra_group <- function(sl){
  #d <- pl_c
  d <- sl$data
  d <- cbind(d[,c(1:5)],apply(d[,-c(1:5)],2,function(a){as.numeric(as.character(a))}))
  d <- subset(d,class != "QC")
  inter_intra <- apply(d[,-c(1:5)],2,function(a){
    da <- data.frame(
      re=a,
      tr=as.factor(d$class))
    a.aov <- aov(re~tr,data = da)
    return(summary(a.aov)[[1]]$'F value'[1])
  })
  return(mean(inter_intra))
}
