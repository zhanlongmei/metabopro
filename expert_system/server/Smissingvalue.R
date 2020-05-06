options(shiny.maxRequestSize=30*1024^2)
#--------------------------data upload----------------------------
peakData <- reactive({
  validate(
    need(input$peakfile != "", "Please upload peak file")
  )
  inFile1 <- input$peakfile
  if (is.null(inFile1))
    return(NULL)
  read_csv(inFile1$datapath)
  #read_csv(inFile1$datapath,n_max = 500)
})

listData<- reactive({
  validate(
    need(input$listfile != "", "Please upload list file")
  )
  inFile2 <- input$listfile
  if (is.null(inFile2))
    return(NULL)
  read.delim(inFile2$datapath,header = T,sep="\t")
})

mergedf <- reactive({
  peak <- peakData()
  list <- listData()
  list$mark <- "sample"
  list$mark[list$class=="QC"] <- "QC"
  peak[peak==0] <- NA
  peakt <- as.data.frame(t(peak[,-c(1,2)]))
  names(peakt) <- peak$mz
  peakt$sample <- row.names(peakt)
  inner_join(list,peakt,by="sample")
})
#--------------------------visualization------------------------
output$mzmisspercent <- renderPlot({
  data <- mergedf()
  mz_miss <- table(apply(data[,-c(1,2,3,4,5)],2,function(x){any(is.na(x))})) %>%
    as.data.frame()
  mz_miss$Var1 <- c("no_miss","has_miss")
  pie(mz_miss$Freq,mz_miss$Var1,main = "mz contain missing", col = rainbow(length(mz_miss$Freq)))
})

output$totalmisspercent <- renderPlot({
  data <- mergedf()
  miss_compose <- data.frame(
    group = c("no_miss","has_miss"),
    Freq = c(sum(!(is.na(data[,-c(1,2,3,4,5)]))),sum(is.na(data[,-c(1,2,3,4,5)])))
  )
  pie(miss_compose$Freq,miss_compose$group,main = "total missing data percent", col = rainbow(length(miss_compose$Freq)))
})

output$sample_miss_percent <- renderPlot({
  data <- mergedf()
  list <- listData()
  sample_name <- data$sample
  data <- as.data.frame(t(data[,-c(1,2,3,4,5)]))
  names(data) <- sample_name
  md_pattern <- md.pattern(data)
  #missing percent in each sample boxplot
  sample_miss_number <- md_pattern[nrow(md_pattern),][-ncol(md_pattern)]/nrow(data)
  sample_miss_number_plot <- data.frame(
    sample=names(sample_miss_number),
    percents=sample_miss_number
  ) 
  inner_join(list[,c(1,3)],sample_miss_number_plot,by="sample") %>%
    ggplot(aes(class,percents))+
    geom_boxplot()+
    geom_jitter(width = 0.2)
})

output$missmatrix <- renderPlot({
  data <- mergedf()
  list <- listData()
  sample_name <- data$sample
  data <- as.data.frame(t(data[,-c(1,2,3,4,5)]))
  names(data) <- sample_name
  annotation_col = as.data.frame(list[,3,drop=F])
  row.names(annotation_col) <- list$sample
  sorted_name <- row.names(annotation_col[order(annotation_col$class),1,drop=F])
  #mz_sel <- which(apply(data,1,function(x){any(is.na(x))})=="TRUE")
  sorted_order_matrixplot <- sapply(sorted_name,function(x){which(colnames(data)==x)})
  #matrixplot(data[mz_sel,sorted_order_matrixplot], interactive = F)
  matrixplot(data[,sorted_order_matrixplot], interactive = F)
})
#----------------------------missing value filtering--------------
datafiltered <- reactive({
  data <- mergedf()
  qc_ratio <- input$qcpercentst
  sam_ratio <- input$samplepercentst
  miss_ratio_by_type <- aggregate(data[,-c(1,2,3,4,5)],by=list(data$mark),ratio_count)   #miss_ratio_by_type[,c(1:10)]
  miss_ratio_by_type2 <- as.data.frame(t(miss_ratio_by_type[,-1]))
  names(miss_ratio_by_type2) <- miss_ratio_by_type$Group.1             #head(miss_ratio_by_type2)
  mz_filter <- row.names(subset(miss_ratio_by_type2,QC >= qc_ratio | sample >= sam_ratio))   #length(mz_filter)
  data_flitered <- data[,!(names(data) %in% mz_filter)]
  return(data_flitered)
})

#======================================total group missing plotling=======================================================================
output$totalgroupmissedmz <- renderPlot({
  data <- datafiltered()
  miss_ratio_by_class <- aggregate(data[,-c(1,2,3,4,5)],by=list(data$class),ratio_count)  #miss_ratio_by_class[,c(1:15)]
  total_group_missed_mz <- miss_ratio_by_class %>%     #head(total_group_missed_mz)
    melt(id=c("Group.1")) %>%
    filter(value == 1)
  # print(head(total_group_missed_mz))
  validate(
    need(nrow(total_group_missed_mz) >0, "No total group missed features")
  )
  data2 <- as.matrix(data[,-c(1,2,3,4,5)])
  row.names(data2) <- data$sample
  
  napostion <- is.na(data2)
  data2[napostion] <- 3
  data2[!napostion] <- 2
  for(i in c(1:nrow(total_group_missed_mz))){
    #i <- 1
    nn <- length(data2[grep(total_group_missed_mz$Group.1[i],data$class),grep(total_group_missed_mz$variable[i],colnames(data2))])
    data2[grep(total_group_missed_mz$Group.1[i],data$class),grep(total_group_missed_mz$variable[i],colnames(data2))] <- 1
  }
  data2 <- as.data.frame(data2)
  data2 <- data2[,!(apply(data2,2,function(x){all(x==2)}))]
  if(ncol(data2)>300){
    data2 <- data2[,c(1:300)]
  }
  list <- listData()
  k<-melt(cbind(sample=rownames(data2), data2),id="sample")
  kk<-merge(list,k,by.x = "sample",by.y = "sample")
  ggplot(kk, 
         aes(x = variable, y = sample, fill = factor(value))) + 
    geom_tile()+
    theme(axis.text.x = element_blank())+
    theme(axis.ticks.x = element_blank())+
    scale_y_discrete(breaks=kk$sample, labels = kk$class)+
    scale_fill_manual(values=c("red", "white", "blue"), 
                      breaks=c("1", "2", "3"),
                      labels=c("TM", "", "LIM/UM"))+
    theme(legend.position="top")+
    guides(fill=guide_legend(title=NULL))
  
})
#======================================total group missing handling=======================================================================
totalImputed <- reactive({
  data <- datafiltered()
  
  all_value <- as.numeric(as.matrix(data[,-c(1,2,3,4,5)]))
  low_value <- quantile(all_value,0.01,na.rm = T)
  all_low <- all_value[which(all_value<low_value)]
  mean_l <- mean(all_low,na.rm = T)
  sd_l <- sd(all_low,na.rm = T)
  
  miss_ratio_by_class <- aggregate(data[,-c(1,2,3,4,5)],by=list(data$class),ratio_count)  #miss_ratio_by_class[,c(1:15)]
  total_group_missed_mz <- miss_ratio_by_class %>%     #head(total_group_missed_mz)
    melt(id=c("Group.1")) %>%
    filter(value == 1)
  
  if(nrow(total_group_missed_mz) == 0){
    validate(
      need(nrow(total_group_missed_mz) >0, "No total group missed features"),
      return(data)
    )
  }else{
    data2 <- as.matrix(data[,-c(1,2,3,4,5)])
    for(i in c(1:nrow(total_group_missed_mz))){
      #i <- 1
      nn <- length(data2[grep(total_group_missed_mz$Group.1[i],data$class),grep(total_group_missed_mz$variable[i],colnames(data2))])
      #data2[grep(total_group_missed_mz$Group.1[i],data$class),grep(total_group_missed_mz$variable[i],colnames(data2))] <- rnorm_fixed(nn,mean_l,sd_l)
      data2[grep(total_group_missed_mz$Group.1[i],data$class),grep(total_group_missed_mz$variable[i],colnames(data2))] <- abs(rnorm(nn))
    }
    data <- cbind(data[,c(1,2,3,4,5)],data2) #data[c(1:5),c(1:10)]
    #print(sum(is.na(data)))
    return(data)
  }
})

output$total_nu <- renderText({
  data1 <- datafiltered()
  data2 <- totalImputed()
  total_nu <- sum(is.na(data1))-sum(is.na(data2))
  paste("A total of ",total_nu,"total group missing has been replaced with small value!") 
})
#======================================low abundance induced missing plotling=======================================================================
output$lowintensitymissing <- renderPlot({
  data <- totalImputed()
  value_ratio <- input$lowquantile
  miss_ratio <- input$misspercenthandling
  list <- listData()
  miss_ratio_by_class <- aggregate(data[,-c(1,2,3,4,5)],by=list(data$class),ratio_count) #miss_ratio_by_class[,c(1:15)]
  mean_intensity_by_class <- aggregate(data[,-c(1,2,3,4,5)],by=list(data$class),function(x){mean(as.numeric(as.character(x)),na.rm = T)}) #mean_intensity_by_class[,c(1:15)]
  a <- melt(miss_ratio_by_class,id=c("Group.1"))
  names(a) <- c("group","mz","ratio")
  a$gmz <- paste(a$group,a$mz,sep="_")
  b <- melt(mean_intensity_by_class,id=c("Group.1"))
  names(b) <- c("group","mz","intensity")
  b$gmz <- paste(b$group,b$mz,sep="_")
  ab <- merge(a,b,by.x = "gmz",by.y = "gmz")  #head(ab)
  #----------------
  all_value <- as.numeric(as.matrix(data[,-c(1,2,3,4,5)]))
  low_value <- quantile(all_value,value_ratio,na.rm = T)
  low_mz <- subset(ab,ratio>=miss_ratio & intensity<=low_value)  #head(low_mz)
  validate(
    need(nrow(low_mz) >0, "No low intensity induced missing identified!")
  )
  #----------------
  data2 <- as.matrix(data[,-c(1,2,3,4,5)])
  row.names(data2) <- data$sample
  data4 <- data2
  data5 <- data2
  napo <- is.na(data2)
  
  data4[!(is.na(data4))] <- NA
  for(i in c(1:nrow(low_mz))){
    class_mark <- grep(low_mz$group.x[i],data$class)
    mz_mark <- grep(low_mz$mz.x[i],colnames(data2))
    k<-which(is.na(data2[class_mark,mz_mark]))
    nn <- length(k)
    data4[class_mark[k],mz_mark]<- 1
  }
  data5 <- data5[,apply(data5,2,function(x){any(is.na(x))})]
  data5 <- as.data.frame(data5)
  data6 <- melt(cbind(sample=rownames(data5), data5))
  data6 <- data6[!(is.na(data6$value)),]
  
  data4 <- as.data.frame(data4)
  data8 <- melt(cbind(sample=rownames(data4), data4))
  data8 <- filter(data8,value==1)
  #if(length(unique(data8$variable))>150){
  #data8 <- data8[(data8$variable %in% unique(data8$variable)[c(1:150)]),]
  #data6 <- data6[(data6$variable %in% unique(data8$variable)),]
  
  #}
  fel <- length(unique(data6$variable))
  if(fel>50){
    fe <- unique(data6$variable)[c(1:50)]
  }else{
    fe <- unique(data6$variable)
  }
  data8 <- data8[(data8$variable %in% fe),]
  data6 <- data6[(data6$variable %in% fe),]
  data6<-merge(list,data6,by.x = "sample",by.y = "sample")
  
  ggplot(data6,aes(variable,sample,fill=sqrt(sqrt(value))))+
    geom_tile()+scale_fill_gradientn(colours = terrain.colors(10),name="intensity")+
    geom_tile(aes(variable,sample),data=data8,fill="red")+
    theme(axis.text.x = element_blank())+
    theme(axis.ticks.x = element_blank())+
    scale_y_discrete(breaks=data6$sample, labels = data6$class)#+
  #theme(legend.position="top")
  
})
#======================================low abundance induced missing handling=======================================================================
minimunImputed <- reactive({
  data <- totalImputed()
  value_ratio <- input$lowquantile
  miss_ratio <- input$misspercenthandling
  miss_ratio_by_class <- aggregate(data[,-c(1,2,3,4,5)],by=list(data$class),ratio_count) #miss_ratio_by_class[,c(1:15)]
  mean_intensity_by_class <- aggregate(data[,-c(1,2,3,4,5)],by=list(data$class),function(x){mean(as.numeric(as.character(x)),na.rm = T)}) #mean_intensity_by_class[,c(1:15)]
  a <- melt(miss_ratio_by_class,id=c("Group.1"))
  names(a) <- c("group","mz","ratio")
  a$gmz <- paste(a$group,a$mz,sep="_")
  b <- melt(mean_intensity_by_class,id=c("Group.1"))
  names(b) <- c("group","mz","intensity")
  b$gmz <- paste(b$group,b$mz,sep="_")
  ab <- merge(a,b,by.x = "gmz",by.y = "gmz")  #head(ab)
  #----------------
  all_value <- as.numeric(as.matrix(data[,-c(1,2,3,4,5)]))
  low_value <- quantile(all_value,value_ratio,na.rm = T)
  all_low <- all_value[which(all_value<low_value)]
  mean_l <- mean(all_low,na.rm = T)
  sd_l <- sd(all_low,na.rm = T)
  low_mz <- subset(ab,ratio>=miss_ratio & intensity<=low_value)  #head(low_mz)
  
  if(nrow(low_mz) == 0){
    validate(
      need(nrow(low_mz) >0, "No low intensity induced missing identified!"),
      return(data)
    )
  }else{
    data2 <- as.matrix(data[,-c(1,2,3,4,5)])
    for(i in c(1:nrow(low_mz))){
      #i <- 1
      class_mark <- grep(low_mz$group.x[i],data$class)
      mz_mark <- grep(low_mz$mz.x[i],colnames(data2))
      k<-which(is.na(data2[class_mark,mz_mark]))
      nn <- length(k)
      data2[class_mark[k],mz_mark]<- rnorm_fixed(nn,mean_l,sd_l)
      # print(data2[class_mark[k],mz_mark])
    }
    data <- cbind(data[,c(1,2,3,4,5)],data2) #data[c(1:5),c(1:10)]
    return(data)
  }
})

output$small_nu <- renderText({
  data1 <- totalImputed()
  data2 <- minimunImputed()
  s_nu <- sum(is.na(data1))-sum(is.na(data2))
  paste("A total of ",s_nu,"low abundance induced missing has been replaced with small value!") 
})

output$missmatrixbefore <- renderPlot({
  data <- datafiltered()
  list <- listData()
  #saveRDS(data,paste(r,"data_flitered.RDS",sep = "/"))
  sample_name <- data$sample
  data <- as.data.frame(t(data[,-c(1,2,3,4,5)]))
  names(data) <- sample_name
  annotation_col = as.data.frame(list[,3,drop=F])
  row.names(annotation_col) <- list$sample
  sorted_name <- row.names(annotation_col[order(annotation_col$class),1,drop=F])
  #mz_sel <- which(apply(data,1,function(x){any(is.na(x))})=="TRUE")
  sorted_order_matrixplot <- sapply(sorted_name,function(x){which(colnames(data)==x)})
  #matrixplot(data[mz_sel,sorted_order_matrixplot], interactive = F)
  matrixplot(data[,sorted_order_matrixplot], interactive = F)
})

output$missmatrix_low_fliter <- renderPlot({
  data <- minimunImputed()
  #saveRDS(data,paste(r,"mini_impute.RDS",sep = "/"))
  list <- listData()
  sample_name <- data$sample
  data <- as.data.frame(t(data[,-c(1,2,3,4,5)]))
  names(data) <- sample_name
  annotation_col = as.data.frame(list[,3,drop=F])
  row.names(annotation_col) <- list$sample
  sorted_name <- row.names(annotation_col[order(annotation_col$class),1,drop=F])
  #mz_sel <- which(apply(data,1,function(x){any(is.na(x))})=="TRUE")
  sorted_order_matrixplot <- sapply(sorted_name,function(x){which(colnames(data)==x)})
  #matrixplot(data[mz_sel,sorted_order_matrixplot], interactive = F)
  matrixplot(data[,sorted_order_matrixplot], interactive = F)
})

#------------------imputation-------------------------
umimputed <- eventReactive(input$Imputation, {
  data <- minimunImputed()
  m <- input$missbm
  #m <- c("BatchRatio","QCRSC")
  n <- sapply(m,function(x){
    #x <- "kNN"
    k <- eval(parse(text=paste(x,"(data)",sep = "")))
    return(list(name=x,data=k))
  },simplify = F)
  #saveRDS(n,"/Users/chaohuyecao/OneDrive - Københavns Universitet/phD/Expert analysis system/script/debug/imputed.RDS")
  return(n)
})

#------------------file download-------------------------
dwf <- reactive({
  f <- input$missdw
  ll <- umimputed()
  data <- eval(parse(text=paste("ll$",f,"$data",sep = "")))
  print(data[1:5,1:10])
  return(data)
})
output$missdownloadDatas <- downloadHandler(
  filename = function() {
    paste("Imputed", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.table(dwf()[,-5], file,row.names = F,col.names = T)
  }
)
#------------------IA-------------------------------------
ia <- reactive({
  imputations <- umimputed()
  val <- function(ll){
    d <- ll$data
    na <- ll$name
    cdnaf <- minimunImputed()
    gr <- unique(cdnaf$class[which(cdnaf$class != "QC")])[1]
    #gr <- "AI"
    sa <- apply(cdnaf[which(cdnaf$class==gr),-c(1:5)],2,function(x){sum(is.na(x))/length(x)})
    cong <- names(sa[sa>0 & sa <1])
    ori <- cdnaf[which(cdnaf$class==gr),which(names(cdnaf) %in% cong)] %>% melt()
    ori$label <- ifelse(is.na(ori$value),"imputed","observed")
    imp <- d[which(d$class==gr),which(names(d) %in% cong)] %>%melt()
    imp <- cbind(ori[,3,drop=F],imp)
    ou <- NULL
    for(i in unique(imp$variable)){
      #i <- imp1$variable[5]
      a <- subset(imp,variable==i)
      im <- which(a$label=="imputed")
      ob <- which(a$label=="observed")
      m <- mean(a$value[im])/mean(a$value[ob])
      o <- data.frame(m=m)
      row.names(o) <- i
      ou <- rbind(ou,o)
    }
    ou$n <- na
    return(ou)
  }
  ias <- sapply(imputations,val,simplify = F)
  iast <- do.call("rbind",ias)
  return(iast)
})

output$umia <- renderPlot({
  dd <- ia()
  iap <- ggplot(dd,aes(n,log2(m)))+geom_boxplot()+
    theme_bw()+
    labs(x=NULL,y="-log2(IAs)")
  #saveRDS(iap,paste(r,"ia.RDS",sep = "/"))
  return(iap)
})

output$umiatable <- renderTable({
  dd <- ia()
  print(head(dd))
  k <- aggregate(log2(dd$m),by=list(approach=as.factor(dd$n)),mean)
  names(k)[2] <- "IA"
  return(k)
})



