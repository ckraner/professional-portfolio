##################### LINEAR SERVER ########################
#################### BY CHRIS KRANER #######################
############## NORTHERN ILLINOIS UNIVERSITY ################
######################### 9/2017 ###########################
######################## V: 1.0 ############################
############################################################


#### Inits
baseformula=paste(myvars[1],independencesquig,1)
mypremodel=lm(formula = baseformula, data=reducedDF)
mymodel=lm(formula = baseformula, data=reducedDF)
my.mlm.formula=NULL


#### MLM AND REGRESSION
observeEvent(input$MLMmodalSave,{
  shiny.env=environment()
  mycom=capture.output(cat(input$MLMmodelName,"<<- mymodel"))
  eval(parse(text=mycom))
  removeModal()
})

observe({
  mlm.options=input$mlmOptions
  shinyjs::hide("mlm.dw")
  if(length(mlm.options)>0){
  for(i in 1:{length(mlm.options)}){
    if(mlm.options[i]=="Durbin-Watson"){
      shinyjs::show("mlm.dw")
    }
  }
  }
})

observe({
  
  mlm.dv=input$mlm1
  mlm.iv=input$mlmvars
  
  my.mlm.formula=paste(mlm.dv,independencesquig)
  
  for (i in 1:length(mlm.iv)){
    
    if (i>1){
      
      addvarr=paste(plusand,mlm.iv[i])
      my.mlm.formula=paste(my.mlm.formula,addvarr)
      
    }else{
      
      my.mlm.formula=paste(my.mlm.formula,mlm.iv[i])
      
    }
    
  }
  
  my.mlm.formula=my.mlm.formula
  updateTextInput(session,"mlm.formula",value=my.mlm.formula)
  
})


my.mlm.formula=eventReactive(input$enter_input,{
  
  sreducedDFtedvariables=input$mlm.formula
  
})

output$DWtest=renderUI({
  
  my.mlm.formula=my.mlm.formula()
  
  my.DW.test=lmtest::dwtest(formula=eval(parse(text=my.mlm.formula)),data=reducedDF)

  part1=paste("DW = ",round(as.numeric(my.DW.test[1]),3),", p-value = ",pixiedust::pvalString(my.DW.test[[4]]))
  part2=paste("Alternative H0: ",my.DW.test$alternative)
  
  return(HTML(paste(part1,part2,sep="<br />")))
  
})

#### MLM STUFF ####
output$model=renderUI({
  
  independentvar=input$mlm1
  my.mlm.formula=my.mlm.formula()
  
  mypremodel<<-mymodel
  mymodel<<-lm(formula = my.mlm.formula, data=reducedDF)

  #### Residuals ####
  
  myresid=as.list(resid(mymodel))
  myresid=as.data.frame(myresid)
  myresid=tidyr::gather(myresid)
  myresid$value=as.numeric(myresid$value)
  
  for(i in 1:length(myresid$key)){
    
    myresid$key[i]=substring(myresid$key[i],2)
    
  }
  

  #### Predicted ####
  
  mypredict=as.list(predict(mymodel))
  mypredict=as.data.frame(mypredict)
  mypredict=tidyr::gather(mypredict)
  mypredict$value=as.numeric(mypredict$value)
  
  for(i in 1:length(mypredict$key)){
    
    mypredict$key[i]=substring(mypredict$key[i],2)
    
  }
  
  
  myresid$key=as.numeric(myresid$key)
  colnames(myresid)=c("rows","lm_resid")
  mypredict$key=as.numeric(mypredict$key)
  colnames(mypredict)=c("rows","lm_pred")
  
  
  
  #### Combine it back into the data frame ####
  
  if(residual.flag==0){
    
    reducedDF<<-dplyr::full_join(myresid,reducedDF,by="rows")
    reducedDF<<-dplyr::full_join(mypredict,reducedDF,by="rows")
    residual.flag<<-1
  }else{

    mlm.drops=c("lm_pred","lm_resid")
    reducedDF<<-reducedDF[,!names(reducedDF)%in%mlm.drops]
    reducedDF<<-dplyr::full_join(myresid,reducedDF,by="rows")
    reducedDF<<-dplyr::full_join(mypredict,reducedDF,by="rows")
    
  }
  
  
  # 
  # #### Make actual table ####
  my.dust.print=quick.lavaan::quick.reg(mymodel,reducedDF,pix.int=F)
  
  
  return(HTML(my.dust.print))
  
})


#### ANOVA UPDATE MODEL ####

output$anova=renderUI({
  
  independentvar=input$mlm1
  independentvar2=my.mlm.formula()
  
  
  mt.mlm.anova.catch=tryCatch(anova(mypremodel,mymodel),error=function(e) e)
  
  if(!is.null(dim(mt.mlm.anova.catch))){
    
    myanova=anova(mypremodel,mymodel)
    
    
    mycaption=attr(myanova,"heading")[2]
    mycaption=sub("\n","<br />",mycaption)
    
    if(typeof(myanova$RSS[1])=="double"){
      
      myanova$RSS[1]=round(myanova$RSS[1],2)
      
    }
    
    myanova$models=c(1,2)
    myanova=myanova[c(7,1,2,3,4,5,6)]
    my.caption.table=as.data.frame(matrix(ncol=7,nrow=1))
    my.caption.table[1]=mycaption
    
    options(pixie_interactive = F)
    my.dusted.anova=pixiedust::dust(myanova)%>%
      sprinkle_print_method("html")%>%
      sprinkle(cols="RSS",rows=2,round=2)%>%
      sprinkle(cols=c("Sum of Sq","F"),rows=1:2,round=3)%>%
      sprinkle(rows=1,replace=c("1",myanova$Res.Df[1],myanova$RSS[1],"","","",""))%>%
      sprinkle(cols="Pr(>F)",fn=quote(pvalString(value,digits=3,format="default")))
    
    my.dusted.anova=pixiedust::redust(my.dusted.anova,my.caption.table,part="foot")%>%
      sprinkle(rows=1,replace=c(mycaption,"","","","","",""),part="foot")%>%
      sprinkle(merge=T,halign="center",part="foot")%>%
      sprinkle(cols=1:7,pad=10)%>%
      sprinkle(halign="center",part="head")%>%
      sprinkle_colnames("Model","Resid <br /> df","Resid <br /> Sums of Sq","df","Sums of <br /> Squares","F-Value","Pr(>F)")%>%
      sprinkle(rows=2,cols=1:7,border="bottom")%>%
      sprinkle(rows=1:2,cols=1:7,border=c("left","right"))%>%
      sprinkle(rows=1:2,cols=7,border="right")%>%
      sprinkle(rows=1,cols=1:7,border=c("top","bottom"),part="head")%>%
      sprinkle(rows=1,cols=1:7,border=c("left","right"),part="head")%>%
      sprinkle(rows=1,cols=7,border="right",part="head")%>%
      sprinkle_width(cols=1,rows=1:2,width=50,width_units="pt")%>%
      # sprinkle_width(cols=1,rows=1,width=60,width_units="pt",part="head")%>%
      sprinkle_width(cols=2,rows=1:2,width=60,width_units="pt")%>%
      # sprinkle_width(cols=2,rows=1,width=68,width_units="pt",part="head")%>%
      sprinkle_width(cols=3,width=80,width_units="pt")%>%
      sprinkle_width(cols=4,width=30,width_units="pt")%>%
      sprinkle_width(cols=5,width=58,width_units="pt")%>%
      sprinkle_width(cols=6,width=50,width_units="pt")

    my.dusted.anova=print(my.dusted.anova,quote=F)[1]

    return(HTML(my.dusted.anova))
    
  }else{
    
    p("This did not work. Sorry.")
    
  }
  
})

#### LEAVE THIS EXTRA LINE BELOW! ####
