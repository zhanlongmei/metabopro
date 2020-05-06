plsda_score_plot <- function(x,d){
  #x <- oplsda_result
  #d <- l
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
}

plsda_loading_plot <- function(x,d){
  #x <- oplsda_result
  #d <- data
  loading <- x@loadingMN
  p <- ggplot(as.data.frame(loading),aes(p1,p2))+
    geom_point()+
    stat_ellipse()+
    xlab(paste("PC1"," (",sprintf("%.2f%%",x@modelDF$R2X[1]*100),") ",sep=""))+
    ylab(paste("PC2"," (",sprintf("%.2f%%",x@modelDF$R2X[2]*100),") ",sep=""))+
    theme_bw()+
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank())
  return(p)
}

plsda_r2y_q2 <- function(x){
  #x <- oplsda_result
  model <- x@modelDF
  model <- model[c(1:3),c(4,6)]
  names(model) <- c("R2Y","Q2")
  model$PC <- paste("PC",seq(1:3),sep="")
  p <- melt(model,id="PC") %>%
    ggplot(aes(PC,value,fill=variable))+
    geom_col(position = "dodge")+
    theme_bw()
  return(p)
}

plsda_permetation <- function(x){
  permut <- as.data.frame(x@suppLs$permMN[,c(1,2,3,7)])
  names(permut) <- c("R2X","R2Y","Q2","Cor")
  permut$mark <- "Random_model"
  permut$mark[1] <- "Target_model"
  p <- melt(permut,id=c("R2X","mark","Cor")) %>%
    ggplot(aes(Cor,value,colour=variable))+
    geom_point()+
    geom_smooth(method = lm,se=F)+
    theme_bw()+
    theme(legend.position = "top")
    
  return(p)
}

plsda_s_plot <- function(x){
  #x axis: loading on pc1
  #y axis:correlation between intensity and score on pc1
  #colour:vip >1 or not
  p <- data.frame(loading_pc1 = x@loadingMN[,1],
             correlation_pc1_intensity= apply(data[,-c(1:5)],2,function(a){return(cor(log(a),x@scoreMN[,1]))}),
             vip=ifelse(x@vipVn >1,"VIP>1","VIP<1")) %>%
    ggplot(aes(loading_pc1,correlation_pc1_intensity,colour=vip))+geom_point()
  return(p)
}


