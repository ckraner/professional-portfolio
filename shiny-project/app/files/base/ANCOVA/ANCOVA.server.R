##################### ANCOVA SERVER ########################
#################### BY CHRIS KRANER #######################
############## NORTHERN ILLINOIS UNIVERSITY ################
######################### 9/2017 ###########################
######################### V: 1.0 ###########################
############################################################

#### ANCOVA STUFF ####
my.anova.formula=NULL

anova.residual.flag=0
if(length(myfactors)>0){
  
  ancova.name.temp=1
  
  while(ancova.name.temp<{length(myfactors)+1}){
    
    ancova.name.grep=grep(substr(myfactors[ancova.name.temp],1,{nchar(myfactors[ancova.name.temp])-2}),my.reduced.vars)
    
    if(ancova.name.grep>0){
      my.reduced.vars=my.reduced.vars[-ancova.name.grep]
      my.reduced.labels=my.reduced.labels[-ancova.name.grep]
      ancova.name.temp=ancova.name.temp+1
    }else{
      ancova.name.temp=ancova.name.temp+1
    }}
  
  baseformula2=paste(my.reduced.vars[1],independencesquig,1)
  mypremodel.ancova=lm(formula = baseformula2, data=reducedDF)
  
}


observe({
  ancova.options=input$ANCOVAOptions
  shinyjs::hide("ANCOVAdw")
  shinyjs::hide("ANCOVAlevene")
  if(length(ancova.options)>0){
    for(i in 1:{length(ancova.options)}){
      if(ancova.options[i]=="Durbin-Watson"){
        shinyjs::show("ANCOVAdw")
      }
      if(ancova.options[i]=="Levene's Test"){
        shinyjs::show("ANCOVAlevene")
      }
    }
  }
})


#### Text input magic ####
observe({
  anova.dv=input$ancova.dv.select
  anova.cov=input$ancova.cov.check
  anova.fac=input$ancova.factor.check
  ancova.thang=c(anova.fac,anova.cov)
  my.anova.formula=paste(anova.dv,independencesquig)
  
  for (i in 1:length(ancova.thang))
  {
    if (i>1){
      addvarr=paste(plusand,ancova.thang[i])
      my.anova.formula=paste(my.anova.formula,addvarr)
    }
    else{
      my.anova.formula=paste(my.anova.formula,ancova.thang[i])
    }
    
  }
  
  updateTextInput(session,"anova.formula",value=my.anova.formula)
})


#### Enter button ####
myformula2 = eventReactive(input$enter_input2,{
  sreducedDFtedvariables2=input$anova.formula
})

#### Output Durbin Watson Stuff ####
output$anova.DWtest=renderUI({
  my.mlm.formula=myformula2()
  
  my.DW.test=lmtest::dwtest(formula=eval(parse(text=my.mlm.formula)),data=reducedDF)
  
  part1=paste("DW = ",round(as.numeric(my.DW.test[1]),3),", p-value = ",pixiedust::pvalString(my.DW.test[[4]]))
  part2=paste("Alternative H0: ",my.DW.test$alternative)
  
  return(HTML(paste(part1,part2,sep="<br />")))
})

#### Output Levene's Test ####
output$anova.levene=renderUI({
  full.anova.formula=myformula2()
  anova.dv=isolate(input$ancova.dv.select)
  anova.fac=isolate(input$ancova.factor.check)
  
  #### Levene's test only allows factors.
  #### In addition, it's only one term connected by *
  my.anova.formula3=paste(anova.dv,independencesquig)
  
  for (i in 1:length(anova.fac)){
    if (i>1){
      addvarr=paste(interactand,anova.fac[i],sep="")
      my.anova.formula3=paste(my.anova.formula3,addvarr,sep="")
    }
    else{
      my.anova.formula3=paste(my.anova.formula3,anova.fac[i])
    }
  }
  
  my.anova.levene=car::leveneTest(eval(parse(text=my.anova.formula3)),data=reducedDF)
  
  my.anova.levene$name=c("group","")
  my.anova.levene=my.anova.levene[,c(4,1,2,3)]
  
  
  options(pixie_interactive = F,pixie_na_string="")
  my.dust2=pixiedust::dust(my.anova.levene)%>%
    sprinkle(cols=c("F value"),round=2)%>%
    sprinkle(cols="Pr(>F)",fn=quote(pvalString(value,digits=3,format="default")))%>%
    sprinkle_colnames(" ","df","F-Value","Pr(>F)")%>%
    sprinkle_print_method("html")%>%
    sprinkle(cols=c(2,3,4),pad=5)%>%
    sprinkle(rows=1:2,cols=1:4,
             border="right",border_color="black")%>%
    sprinkle(rows=1:2,cols=1,
             border="left",border_color="black")%>%
    sprinkle(rows=1,cols=1:4,
             border=c("top","bottom","left","right"),border_color="black",part="head")%>%
    sprinkle(rows=2,cols=1:4,
             border="bottom",border_color="black")%>%
    sprinkle(cols=1:4,rows=1,halign="center",part="head")%>%
    sprinkle_width(cols=1,width=45,width_units="pt")%>%
    sprinkle_width(cols=2,width=35,width_units="pt")%>%
    sprinkle(cols=2,halign="right")%>%
    sprinkle_width(cols=3,width=62,width_units="pt")%>%
    sprinkle_width(cols=4,width=62,width_units="pt")%>%
    sprinkle_na_string()
  
  
  my.dust.print2=print(my.dust2,quote=F)[1]  
  
  return(HTML(my.dust.print2))
})


#### ACTUAL ANALYSIS STUFF ####
output$model2=renderUI({
  
  independentvar=isolate(input$ancova.dv.select)
  my.factor=isolate(input$ancova.factor.check)
  myformula=myformula2()
  
  mypremodel.ancova<<-my.ancova.model
  my.ancova.model<<-lm(formula = myformula, data=reducedDF)
  
  
  #### Residuals ####
  myresid=as.list(resid(my.ancova.model))
  myresid=as.data.frame(myresid)
  
  #### Have to attach back by row number, not colid
  myresid=tidyr::gather(myresid)
  myresid$value=as.numeric(myresid$value)
  
  for(i in 1:length(myresid$key)){
    myresid$key[i]=substring(myresid$key[i],2)
  }
  
  myresid$key=as.numeric(myresid$key)
  colnames(myresid)=c("rows","anova_resid")
  
  
  #### Predicted ####
  mypredict=as.list(predict(my.ancova.model))
  mypredict=as.data.frame(mypredict)
  
  #### Have to attach back by row number, not colid
  mypredict=tidyr::gather(mypredict)
  mypredict$value=as.numeric(mypredict$value)
  
  for(i in 1:length(mypredict$key)){
    mypredict$key[i]=substring(mypredict$key[i],2)
  }
  
  mypredict$key=as.numeric(mypredict$key)
  colnames(mypredict)=c("rows","anova_pred")
  
  #### ATTACH RESIDUALS ####
  
  #### Was a real pain, so to update just removed rows and reattached
  
  if(anova.residual.flag==0){
    
    reducedDF<<-dplyr::full_join(myresid,reducedDF,by="rows")
    reducedDF<<-dplyr::full_join(mypredict,reducedDF,by="rows")
    anova.residual.flag<<-1
    
  }else{
    
    anova.drops=c("anova_pred","anova_resid")
    reducedDF<<-reducedDF[,!names(reducedDF)%in%anova.drops]
    reducedDF<<-dplyr::full_join(myresid,reducedDF,by="rows")
    reducedDF<<-dplyr::full_join(mypredict,reducedDF,by="rows")
    
  }
  
  
  
  #### Get all the information for the table ####
  my.dust.print=quick.lavaan::quick.reg(my.ancova.model,reducedDF,my.factor = my.factor,pix.int = F)
  
  return(HTML(my.dust.print))
  
}
)


#### ANOVA UPDATE MODEL ####

output$anova2=renderUI({
  independentvar=input$ancova.dv.select
  independentvar2=myformula2()
  
  #### ERROR CATCH ####
  mt.mlm.anova.catch2=tryCatch(anova(mypremodel.ancova,my.ancova.model), error=function(e) e)
  
  if(!is.null(dim(mt.mlm.anova.catch2))){
    myanova=anova(mypremodel.ancova,my.ancova.model)
    
    

    mycaption=attr(myanova,"heading")[2]
    mycaption=sub("\n","<br />",mycaption)
    
    if(typeof(myanova$RSS[1])=="double"){
    
      myanova$RSS[1]=round(myanova$RSS[1],2)
    
    }
    if(dim(myanova)[1]>1){
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
      sprinkle_colnames("Model","Resid <br /> df","Resid <br /> Sums of Sq","df",
                        "Sums of <br /> Squares","F-Value","Pr(>F)")%>%
      sprinkle(rows=2,cols=1:7,border="bottom")%>%
      sprinkle(rows=1:2,cols=1:7,border=c("left","right"))%>%
      sprinkle(rows=1:2,cols=7,border="right")%>%
      sprinkle(rows=1,cols=1:7,border=c("top","bottom"),part="head")%>%
      sprinkle(rows=1,cols=1:7,border=c("left","right"),part="head")%>%
      sprinkle(rows=1,cols=7,border="right",part="head")%>%
      sprinkle_width(cols=1,rows=1:2,width=50,width_units="pt")%>%
      sprinkle_width(cols=2,rows=1:2,width=60,width_units="pt")%>%
      sprinkle_width(cols=3,width=80,width_units="pt")%>%
      sprinkle_width(cols=4,width=30,width_units="pt")%>%
      sprinkle_width(cols=5,width=58,width_units="pt")%>%
      sprinkle_width(cols=6,width=50,width_units="pt")


    my.dusted.anova=print(my.dusted.anova,quote=F)[1]
    
    return(HTML(my.dusted.anova))
    }else{
      p("This did not work. Sorry.")
    }
  }else{
    
    p("This did not work. Sorry.")
    
  }
})

#### LEAVE THIS EXTRA LINE BELOW! ####
