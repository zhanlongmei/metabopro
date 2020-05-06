#------------------noise filtering-----------------------------
cleandata <- reactive({
  data <- dwf()
  nc <- input$noisecutoff
  data <- noisefilter(data,nc)
  return(data)
})
#------------------between batch variation---------------------
output$RawBox <- renderPlot({
  data <- cleandata()
  #saveRDS(RawBox_plot(data),paste(r,"batch_box.RDS",sep = "/"))
  RawBox_plot(data)
})
output$batch_pca <- renderPlot({
  data <- cleandata()
  batch_pca_plot(data)
})
#------------------within batch variation---------------------
output$raw_within_batch <- renderPlot({
  data <- cleandata()
  raw_within_batch_heatmap(data,"None")
})

batchcorrected <- eventReactive(input$batchcorrect, {
  data <- cleandata()
  m <- input$bm
  #m <- c("BatchRatio","QCRSC")
  n <- sapply(m,function(x){
    #x <- "BatchRatio"
    k <- eval(parse(text=paste(x,"(data)",sep = "")))
    return(list(name=x,data=k))
  },simplify = F)
  n$None$name <- "None"
  n$None$data <- data
  #saveRDS(n,paste(r,"batch_corrected.RDS",sep = "/"))
  return(n)
})
com_pca <- reactive({
  ll <- batchcorrected()
  #ou_pca <- sapply(n,single_compare_pca,simplify = FALSE)
  return(sapply(ll,single_compare_pca,simplify = FALSE))
})
batch_pvalue_matrix <- reactive({
  ll <- batchcorrected()
  ou <- sapply(ll,batch_p,simplify = FALSE) 
  p_matrix <- do.call("rbind",ou)   #rbind all the elements in the list of ou
  return(p_matrix)
})
#------------------p value distribution----------------
output$batch_pvalue <- renderPlot({
  p_matrix <- batch_pvalue_matrix()
  p_dis <- ggplot(p_matrix, aes(x=p_value)) + 
    geom_histogram(color="black", fill="white")+
    theme_bw()+
    theme(panel.grid =element_blank())+
    #scale_x_continuous(breaks=seq(0, 1, 0.05)) +
    facet_wrap(vars(mark), nrow = 3)+
    theme(axis.text.x = element_text(size = 5, vjust = 0.7, hjust = 0.7, angle = 45))
  p_dis
})
output$class_pvalue <- renderPlot({
  ll <- batchcorrected()
  ou_cp <- sapply(ll,class_p,simplify = FALSE)
  p_dis_class <- do.call("rbind",ou_cp) %>%
    ggplot(aes(x=p_value)) + 
    geom_histogram(color="black", fill="white")+
    theme_bw()+
    theme(panel.grid =element_blank())+
    #scale_x_continuous(breaks=seq(0, 1, 0.05)) +
    facet_grid(cols = vars(mark))+
    labs(title = "p_value distribution grouped in class")
  p_dis_class
})
#------------------PCA---------------------------------
output$batch_correct_pca <- renderPlot({
  ou_pca <- com_pca()
  m <- names(ou_pca)
  cmd <- paste("grid.arrange(",paste(paste("ou_pca$",m,"$batch",sep=""),collapse = ","),",nrow=3)",sep = "")
  eval(parse(text=cmd))
})
output$class_correct_pca <- renderPlot({
  ou_pca <- com_pca()
  m <- names(ou_pca)
  cmd <- paste("grid.arrange(",paste(paste("ou_pca$",m,"$class",sep=""),collapse = ","),",nrow=3)",sep = "")
  eval(parse(text=cmd))
})
#------------------overall-----------------------------
output$overall <- renderPlot({
  p_matrix <- batch_pvalue_matrix()
  #batch p value ratio
  pl_c <- cleandata()
  np <- unclass(table(subset(p_matrix,p_value<0.05)$mark))
  chd_serum <- data.frame(method=names(np),ratio=np/(ncol(pl_c)-5))
  #pca batch distance
  dis_pca <- com_pca()
  eval(parse(text=paste("batch_distance=c(",
                        paste(paste("single_overlap(dis_pca$",names(dis_pca),")",sep=""),collapse = ","),
                        ")",sep = "")
  ))
  batch_effect <- data.frame(
    method=names(dis_pca),
    batch_distance=batch_distance
  ) %>%
    mutate(within=within_all_ratio()) %>%
    merge(chd_serum,by.x = "method",by.y = "method") %>%
    ggplot(aes(batch_distance,ratio,label=method))+theme_bw()+
    #geom_label()
    geom_label(aes(fill=within))+
    scale_fill_distiller(palette = "Spectral")
  #saveRDS(batch_effect,paste(r,"batch_overall.RDS",sep = "/"))
  batch_effect
  
})
#------------------within batch------------------------
output$within_single_heatmap_none <- renderPlot({
  ll <- batchcorrected()
  d <- ll$None$data
  raw_within_batch_heatmap(d,"None")
})
output$within_single_heatmap <- renderPlot({
  m <- input$withinbatchmethod
  ll <- batchcorrected()
  eval(parse(text=paste("d <- ll$",m,"$data")))
  eval(parse(text=paste("t <- ll$",m,"$name")))
  raw_within_batch_heatmap(d,t)
})
within_all_ratio <- reactive({
  pl_c <- cleandata()
  ll <- batchcorrected()
  ou <- sapply(ll,single_data_within_ratio,simplify = T)
  return(ou)
})
output$within_all <- renderPlot({
  r <- within_all_ratio()
  data.frame(method=names(r),value=r) %>%
    ggplot(aes(method,value))+
    geom_point()+
    theme_bw()+
    theme(panel.grid =element_blank())+
    labs(x = "", y = "Ratio of within batch effects")
})
#------------------file download-------------------------
batchdwf <- reactive({
  f <- input$dw
  ll <- batchcorrected()
  data <- eval(parse(text=paste("ll$",f,"$data",sep = "")))
  return(data)
})
output$batchdownloadDatas <- downloadHandler(
  filename = function() {
    paste("batch_corrected", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(batchdwf()[,-5], file,row.names = F,col.names = T)
  }
)
#----------------------------------------------------------