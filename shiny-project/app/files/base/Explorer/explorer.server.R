#################### EXPLORER SERVER #######################
#################### BY CHRIS KRANER #######################
############## NORTHERN ILLINOIS UNIVERSITY ################
######################### 9/2017 ###########################
######################### V: 1.0 ###########################
############################################################

#### Variable Buttons ####

source(varreactivetemp,local = T)  



#### MCAR AND MAHALINOBIS TESTS ####

#### Don't need to be updated, so outside anything with output
#### Round Mahalinobis test to two decimals and attach to original dataframe

### MCAR Very finnicky

# try=tryCatch(BaylorEdPsych::LittleMCAR(reducedDF2),error=function(e) e, warning=function(w) w)
# 
# if(typeof(try[[1]])=="character"){
# 
#   long=NULL
# 
# }else{
#   
#   long=BaylorEdPsych::LittleMCAR(reducedDF2)
# 
# }
# 
long=NULL
#### Correlations Output ####

output$correlations=renderUI({
  
  cov.test=cov(scale(na.omit.DF))
  my.length=ncol(cov.test)
  squar.var=2
  
  #### Make only show lower half
  
  for(i in 1:{my.length-1}){
    
    cov.test[i,squar.var:my.length]=NA
    squar.var=squar.var+1
    
  }
  
  length.cov=dim(cov.test)[1]
  
  options(pixie_interactive = F)
  cor.table=pixiedust::dust(cov.test)%>%
    sprinkle(cols=2:{length.cov+1},round=3)%>%
    sprinkle_print_method("html")%>%
    sprinkle(cols=1:{length.cov+1},rows=length.cov,border="bottom")%>%
    sprinkle(cols=1,rows=1:length.cov,border=c("left","right"))%>%
    sprinkle(cols={length.cov+1},rows=1:length.cov,border="right")%>%
    sprinkle(cols=1:{length.cov+1},rows=1,border=c("top","bottom"),part="head")%>%
    sprinkle(cols=1,rows=1,border=c("left","right"),part="head")%>%
    sprinkle(cols={length.cov+1},rows=1,border="right",part="head")%>%
    sprinkle(cols=1:{length.cov+1},pad=10)%>%
    sprinkle(cols=1,border="right")%>%
    sprinkle_na_string(na_string="")%>%
    sprinkle(rows=1,halign="center",part="head")%>%
    sprinkle_width(cols=1,width=50,width_units="pt")%>%
    sprinkle(rows=1,rotate_degree=-90,height=60,part="head")%>%
    sprinkle(cols=1,bold=T)
  
  
  that.cor2=print(cor.table,quote=F)[1]
  #writeLines(capture.output(cat("<html><head></head><body>",that.cor2,"</body></html>")),that.cor)

  #return(tags$iframe(src="www/pearson.html",height={45*length.cov+120},width="100%",seamless=T))
  return(div(style="overflow-x: auto",HTML(that.cor2)))
  
})



#### PSYCH DESCRIBE TABLE ####

output$mySummary=renderUI({
  varreact2=varreact()
  
  psy.desc=with(reducedDF,psych::describe(varreact2))
  psy.desc1=tidyr::gather(psy.desc,"X1")
  psy.desc1$value=round(psy.desc1$value,3)
  psy.desc1=psy.desc1[-1,]
  
  options(pixie_interactive=F)
  psy.dust=pixiedust::dust(psy.desc1)%>%
    sprinkle(cols="value",round=3)%>%
    sprinkle_colnames(X1="",value="Value")%>%
    sprinkle(cols="X1",
             replace=c("n","Mean","Standard Deviation","Median","Trimmed Mean","MAD",
                       "Min","Max","Range","Skew","Kurtosis","Standard Error"))%>%
    sprinkle_print_method("html")
  
  psy.desc=print(psy.dust,quote=F)[1]

  return(HTML(psy.desc))
  
})

output$allSummary=renderPrint(
  psych::describe(reducedDF)
)
#### VARIABLE STAT TABLES ####
#### Histogram with normal, QQ Plot, Box Plot with Outliers, Multigraph (Hist, QQ Plot, normal Box Plot)

output$myPlot=renderPlot({
  varreact2=varreact()
  
  #### get column numbers for ID and variable sreducedDFtion ####
  
  mynumba=grep(input$variable,colnames(reducedDF))
  mynumbacase=grep(colid,colnames(reducedDF))
  
  
  #### zz.statexplorer ####
  
  myList=stats.explor.r::zz.statexplorer(reducedDF,varreact2,mynumba,colid,mynumbacase)
  
  
  
  #### Only have to get it sent to the viewer to tie it in.
  
  switch(input$gtype,
         "BoxPlot"=myList[4],
         "Histogram"=myList[2],
         "QQPlot"=myList[3],
         "All"=myList[5])
})



#### LITTLES MCAR TEST OUTPUT ####
#### Output for test for Missing Completely at random (MCAR)
#### Need to turn into full HTML


output$lilMCAR=renderPrint({
  
  if(is.null(long)){
  
    mymsg="I'm sorry, there is a problem with your data for this test."
    cat(mymsg)
  
  }else{
    
    cat("X^2")
    cat(" ( ")
    cat(long$df)
    cat(" ) =  ")
    cat(long$chi.square)
    cat(", p= ")
    
    if(long$p.value<.001){
      
      cat("<.001")
      
    }else{
      
      long$p.value=round(long$p.value,3)
      cat(long$p.value)
      
    }
    
  }
  
})


#### Shapiro Wilk and Smironov Tests ####

output$xplorer.shapiro_kul=renderUI({
  
  myy1=grep(paste("^",input$variable,"$",sep=""),colnames(reducedDF))
  
  x=shapiro.test(reducedDF[[myy1]])
  y=ks.test(reducedDF[[myy1]],"pnorm")

  my.x=cbind("Shapiro-Wilk",x$statistic,x$p.value)
  my.x=as.data.frame(my.x)
  my.x$V1=as.character(my.x$V1)
  my.x$V2=as.numeric(as.character(my.x$V2))
  my.x$V3=as.numeric(as.character(my.x$V3))
  
  my.y=cbind("Kologorov-Smirnov",y$statistic,y$p.value)
  my.y=as.data.frame(my.y)
  my.y$V1=as.character(my.y$V1)
  my.y$V2=as.numeric(as.character(my.y$V2))
  my.y$V3=as.numeric(as.character(my.y$V3))
  
  my.x=dplyr::bind_rows(my.x,my.y)
  
  options(pixie_interactive = F)
  my.dusted.x=pixiedust::dust(my.x)%>%
    sprinkle(cols="V2",rows=1:2,round=2)%>%
    sprinkle(cols="V3",rows=1:2,fn=quote(pvalString(value,digits=3,format="default")))%>%
    sprinkle_colnames("","Statistic","p-value")%>%
    sprinkle_print_method("html")

  my.dusted.x=print(my.dusted.x,quote=F)[1]
  
  return(HTML(my.dusted.x))

})

#### LEAVE THIS EXTRA LINE BELOW! ####
