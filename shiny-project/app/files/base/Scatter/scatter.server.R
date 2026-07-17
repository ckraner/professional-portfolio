#################### SCATTER SERVER ########################
#################### BY CHRIS KRANER #######################
############## NORTHERN ILLINOIS UNIVERSITY ################
######################### 9/2017 ###########################
######################## V: 1.0 ############################
############################################################

#### Variable buttons ####
#source(scatyreactivetemp,local = T)[1]
#source(scatxreactivetemp,local=T)[1]  

observe({
  # x.catch=tryCatch(grep(paste("^",input$scat1,"$",sep=""),colnames(reducedDF)),error=function(e) e, warning=function(w) w)
  # y.catch=tryCatch(grep(paste("^",input$scat2,"$",sep=""),colnames(reducedDF)),error=function(e) e, warning=function(w) w)
  myx1=grep(paste("^",input$scat1,"$",sep=""),colnames(reducedDF))
  myy1=grep(paste("^",input$scat2,"$",sep=""),colnames(reducedDF))
  
  if(length(myx1)>0 && length(myy1)>0){

  
  id.grep=grep(paste("^",colid,"$",sep=""),colnames(reducedDF))
  
  isolate(updateTextInput(session=session,inputId = "xtitle",value=if({myx1-id.grep}>0){mylabels[myx1-id.grep]}else{""}))
  isolate(updateTextInput(session=session,inputId = "ytitle",value=if({myy1-id.grep}>0){mylabels[myy1-id.grep]}else{""}))
  isolate(updateTextInput(session=session,inputId = "scatcapt",value=capture.output(cat("Histogram of","by"))))
  }
})
#### SCATTER PLOTS ####

#### Don't know why, but scatx() and y not returning unquoted values

output$scatter=renderPlot({
  
  # myx=scatx()
  # myy=scaty()

  myx1=grep(paste("^",input$scat1,"$",sep=""),colnames(reducedDF))
  myy1=grep(paste("^",input$scat2,"$",sep=""),colnames(reducedDF))
  
  if(length(myx1)>0 && length(myy1)>0){
  
  plot(reducedDF[[myx1]], reducedDF[[myy1]],main=input$scatcapt,xlab=input$xtitle,ylab=input$ytitle)
    
  }else{
    # #### Silly plot from https://github.com/Selbosh/ggChernoff
    # ggplot(ufos) +
    #   aes(x, y, fill = colour, size = size) +
    #   geom_chernoff(smile = -1, aes(brow = brow)) +
    #   geom_chernoff(data = cannon) +
    #   geom_tile(data = bunkers, width = 1) +
    #   geom_tile(data = data.frame(x = 0, y = 3, colour = 'white', size = 2), width = .1) +
    #   scale_fill_identity() +
    #   scale_size_identity() +
    #   ggtitle("Plot from Selbosh/ggChernoff") +
    #   theme_void() +
    #   theme(plot.background = element_rect(fill = 'black'),
    #         legend.position = 'none')
    
    plot(c(1,1,1,1),c(2,2,2,2))
  }
  
})

#### Shapiro, etc. tests ####

output$shapiro_kul=renderUI({
  
  mytest.thang=input$tests_1
  
  if(mytest.thang){
    
    myy1=grep(paste("^",input$scat2,"$",sep=""),colnames(reducedDF))
    x=shapiro.test(reducedDF[[myy1]])
    y=ks.test(reducedDF[[myy1]],"pnorm")

    my.x=cbind(x$method,x$statistic,x$p.value)
    my.x=as.data.frame(my.x)
    my.y=c(y$method,y$statistic,y$p.value)
    my.y[1]=substr(my.y[1],11,nchar(my.y[1]))
    names(my.y)=c("V1","V2","V3")

    my.x=dplyr::bind_rows(my.x,my.y)

    my.x$V2=as.numeric(my.x$V2)
    my.x$V3=as.numeric(my.x$V3)
    
    options(pixie_interactive = F)
    my.dusted.x=pixiedust::dust(my.x)%>%
      sprinkle(cols="V2",round=2)%>%
      sprinkle(cols="V3",fn=quote(pvalString(value,digits=3,format="default")))%>%
      sprinkle_colnames("","Statistic","p-value")%>%
      sprinkle_print_method("html")
    
    my.dusted.x=print(my.dusted.x,quote=F)[1]
    
    return(HTML(my.dusted.x))
  
  }else{
    
    p(" ")
    
  }
  
})

#### LEAVE THIS EXTRA LINE BELOW! ####
