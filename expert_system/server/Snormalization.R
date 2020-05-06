#------------------check the compariability of the samples---------------------
output$Box_normalization_none <- renderPlot({
  d <- batchdwf()
  #saveRDS(normalization_single_boxplot(d),paste(r,"nor_none.RDS",sep = "/"))
  normalization_single_boxplot(d)
})
#-------------------sample normalization------------------------
samplenormalized <- eventReactive(input$normalizationConduct, {
  data <- batchdwf()
  #saveRDS(data,"/Users/chaohuyecao/OneDrive - Københavns Universitet/phD/Expert analysis system/script/debug/batchcorrected.RDS")
  m <- input$normalizationApproach
  #m <- c("SUM","PQN","Quantile","VSN")
  n <- sapply(m,function(x){
    #x <- "BatchRatio"
    k <- eval(parse(text=paste(x,"(data)",sep = "")))
    return(list(name=x,data=k))
  },simplify = F)
  n$None$name <- "None"
  n$None$data <- data
  #saveRDS(n,paste(r,"normalized_list.RDS",sep = "/"))
  return(n)
})
#-------------------evaluation to sample normalization------------
#output$normalization_evaluation_boxplot <- renderPlot({
#  ll <- samplenormalized()
#  ou_cp <- sapply(ll,normalization_all_boxplot,simplify = FALSE)
#  m <- names(ou_cp)
#  cmd <- paste("grid.arrange(",paste(paste("ou_cp$",m,sep=""),collapse = ","),",nrow=2)",sep = "")
#  eval(parse(text=cmd))
#})

output$normalization_evaluation_boxplot <- renderPlot({
  ll <- samplenormalized()
  m <- input$sample_normalized_method
  eval(parse(text=paste("d <- ll$",m,"$data")))
  normalization_single_boxplot(d)
})

output$nor_classification_accuracy <- renderTable({
  ll <- samplenormalized()
  ou_cp <- sapply(ll,normalization_single_ca,simplify = TRUE)
  #saveRDS(data.frame(method=names(ou_cp),classification_accuracy=ou_cp),paste(r,"ia.txt",sep = "/"))
  return(data.frame(method=names(ou_cp),classification_accuracy=ou_cp))
})

output$nor_group_cv <- renderPlot({
  ll <- samplenormalized()
  ou_cp <- sapply(ll,normalization_cv_single,simplify = FALSE)
  ou_a <- do.call("rbind",ou_cp) %>%
    ggplot(aes(method,CV,fill=class))+geom_col(position = "dodge")
  #saveRDS(ou_a,paste(r,"nor_cv.RDS",sep = "/"))
  return(ou_a)
})

output$nor_variance <- renderTable({
  ll <- samplenormalized()
  ou_cp <- sapply(ll,singl_inter_vs_intra_group,simplify = TRUE)
  return(data.frame(method=names(ou_cp),between_vs_within=ou_cp))
})
#------------------file download-------------------------
best_normalized <- reactive({
  f <- input$bestnormalization
  ll <- samplenormalized()
  data <- eval(parse(text=paste("ll$",f,"$data",sep = "")))
  return(data)
})
output$normalizationDownloa <- downloadHandler(
  filename = function() {
    paste("sample_normalized", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(best_normalized()[,-5], file,row.names = F,col.names = T)
  }
)