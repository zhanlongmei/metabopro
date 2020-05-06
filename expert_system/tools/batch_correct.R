#=======================1 batch ratio======================================================
BatchRatio <- function(x){
  #x <- pl_c
  x <- x[order(x$order),]
  info <- x[,c(1:5)]
  info$batch <- as.factor(info$batch)
  data <- as.matrix(x[,-c(1:5)])
  nbatch <- length(unique(info$batch))
  #claculate of batch ratio
  batch_ratio_cal <- function(b){
    mb <- aggregate(b,by=list(batch=info$batch),mean)$x/mean(b)
    return(mb)
  }
  batch_ratio <- apply(data, 2, batch_ratio_cal)
  batch_ratio <- cbind(matrix(1:nbatch,nrow=nbatch,dimnames=list(c(1:nbatch),c("batch"))),batch_ratio)
  batch_ratio <- merge(matrix(c(info$batch,info$order),ncol = 2,dimnames = list(NULL,c("batch","order"))),batch_ratio,by.x="batch",by.y="batch")
  batch_ratio <- batch_ratio[order(batch_ratio[,2]),]
  data <- data/batch_ratio[,-c(1,2)]
  data <- cbind(info,data)
  #data <- cvfilter_after_sample_normalization(data)
  return(data)
}

#=======================2 loess correction=================================================
QCRSC <- function(x){
  #x <- pl_c
  #batch correction
  x <- BatchRatio(x)
  x <- x[order(x$order),]
  #loess correction
  data <- as.matrix(x[,-c(1:5)])
  qcid<-which(x$class == "QC")
  y <- as.numeric(as.character(x$order))
  for(i in 1:dim(data)[2]){
    loe<-loess(data[qcid,i]~qcid,span=0.3,degree=2)
    yf<-predict(loe,y)
    #data[,i]<-data[,i]-yf+median(data[,i])
    data[,i]<-data[,i]/yf*median(data[,i])
  }
  #impute missing value
  #data[data<0] <- NA
  #data <- imp_min(data)
  ld <- cbind(x[,c(1:5)],data)
  return(kNN(ld))
}

#=======================3 SVR correction===================================================
SVR <- function(x){
  #x <- pl_c
  x$batch <- as.factor(x$batch)
  all_order <- x$order
  nbatch <- length(unique(x$batch))
  #---batch ratio correction 
  lx <- melt(x,id=c("sample","batch","class","order","mark"))
  names(lx)[c(6,7)] <- c("mz","raw_value")
  lx$mzb <- as.factor(paste(lx$batch,lx$mz,sep="_"))
  mx <- subset(lx,class=="QC")
  if(nbatch > 1){
    total_mean <- aggregate(mx$raw_value,by=list(mz=mx$mz),mean)
    names(total_mean)[2] <- "total_mean"
    batch_mean <- aggregate(mx$raw_value,by=list(mzb=mx$mzb),mean)
    names(batch_mean)[2] <- "batch_mean"
    lx <- merge(lx,total_mean,by.x = "mz",by.y = "mz")
    lx <- merge(lx,batch_mean,by.x = "mzb",by.y = "mzb")
    lx$batch_ratio_correct_value <- lx$raw_value/lx$batch_mean*lx$total_mean
  }else(
    lx$batch_ratio_correct_value <- lx$raw_value
  )
  #---SVR correction fit
  lx2 <- lx[,c(2,5,6,11)]
  kk <- ddply(lx2, .(mz), function(sub){
    #sub <- lx2[lx2$mz=="100.0041508",]
    sl <- svm(sub$order[sub$class=="QC"],sub$batch_ratio_correct_value[sub$class=="QC"])
    predict_value <- e1071:::predict.svm(sl,data.frame(y=all_order))
    data.frame(
      mz=unique(sub$mz),
      order=all_order,
      predicted_value=predict_value
    )
  })
  lx$mz_order <- paste(lx$order,lx$mz,sep = "_")
  kk$mz_order <- paste(kk$order,kk$mz,sep="_")
  lxk <- merge(lx,kk[,c(3,4)],by.x = "mz_order",by.y = "mz_order")
  #---SVR correction predict
  md <- aggregate(lxk$batch_ratio_correct_value,by=list(mz=lxk$mz),median)
  names(md)[2] <- "mz_media"
  lxk <- merge(lxk,md,by.x = "mz",by.y = "mz")
  lxk$corrected_vlaue <- lxk$batch_ratio_correct_value-lxk$predicted_value+lxk$mz_media
  lxk <- lxk[,c(1,4,5,6,7,8,15)]
  lxk$corrected_vlaue[lxk$corrected_vlaue < 0] <- NA
  lxkc <- dcast(lxk,sample+batch+class+order+mark ~ mz)
  row.names(lxkc) <- lxkc$sample 
  #lxkc <- imp_min(cbind(lxkc[,c(1:5)],lxkc[,-c(1:5)]))
  return(kNN(lxkc))
}

#=======================4 combat correction================================================
Combat <- function(x){
  #x <- pl_c
  l <- x[,c(1:5)]
  d <- x[,-c(1:5)] %>%
    t() 
  #matrix prepare
  mod = model.matrix(~1, data=l)
  batch = factor(l$batch)
  #combat correction
  combat_d = ComBat(dat=d, batch=batch, mod=mod, par.prior=TRUE, prior.plots=FALSE) %>% t()
  combat_d[combat_d<0] <- NA
  combat_d <- combat_d %>%
    as.data.frame() 
   # mutate(sample=as.factor(row.names(combat_d)))
  #combine with sample information and missing value impute
  combat_d3 <- cbind(l,combat_d)
  row.names(combat_d3) <- combat_d3$sample
  combat_d3 <- kNN(combat_d3)
  return(combat_d3)
}

#=======================5 limma correction=================================================
Limma <- function(x){
  #x <- pl_c
  d <- x[,-c(1:5)] %>%
    t()
  batch <- x$batch
  d2 <- removeBatchEffect(d,batch)
  d2[d2<0] <- NA
  d2 <- as.data.frame(t(d2)) %>% mutate(sample=colnames(d2))
  d2 <- cbind(x[,c(1:5)],d2)
  row.names(d2) <- d2$sample
  d2 <- kNN(d2)
  return(kNN(d2))
}

#=======================6 eigenms correction===============================================
Eigenms <- function(x){
  #x <- pl_c
  m_logInts = t(x[,-c(1:5)])
  m_logInts = log2(m_logInts)
  gr1 = as.factor(x$class)# similar to S1, S2, ... in biological data 
  #gr2 = as.factor(x$batch)
  #grps2 = data.frame(gr1,gr2)
  m_prot.info <- data.frame(
    pepID=names(x)[-c(1:5)],
    protID=names(x)[-c(1:5)]
  )
  m_ints_eig2 = eig_norm1(m=m_logInts,treatment=gr1,prot.info=m_prot.info)
  m_ints_norm2 = eig_norm2(rv=m_ints_eig2)
  m_ints_norm2$norm_m[m_ints_norm2$norm_m<0] <- NA
  eo <- cbind(x[,1:5],as.data.frame(t(m_ints_norm2$norm_m)))
  #eo <- imp_min(eo)
  return(kNN(eo))
}

#=======================7 SVA correction===================================================
SVA <- function(x){
  #x <- pl_c
  batch <- x$batch
  condition <- x$class
  pdata <- data.frame(batch,condition)
  modmatrix=model.matrix(~as.factor(condition),data=pdata)
  sva.object <- batchQC_sva(t(x[,-c(1:5)]),mod=modmatrix)
  ad <- batchQC_svregress_adjusted(t(x[,-c(1:5)]),modmatrix, sva.object)
  ad[ad<0] <- NA
  so <- cbind(x[,1:5],as.data.frame(t(ad)))
  so <- kNN(so)
  return(so)
}

#=======================8 ber correction===================================================
Ber <- function(x){
  #x <- pl_c
  Y <- as.matrix(x[,-c(1:5)])
  class <- data.frame(class=x$class)
  batch <- as.factor(x$batch)
  #ber correction and missing value imputation
  YEadj<-ber_bg(Y,batch,class)
  YEadj[YEadj<0] <- NA
  bo <- cbind(x[,1:5],as.data.frame(YEadj))
  #bo <- imp_min(bo)
  return(kNN(bo))
}




