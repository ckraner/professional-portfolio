##################### glm SERVER ########################
#################### BY CHRIS KRANER #######################
############## NORTHERN ILLINOIS UNIVERSITY ################
######################### 9/2017 ###########################
######################### V: 1.0 ###########################
############################################################

#### glm STUFF ####
my.glm.formula=NULL
my.glm.model=NULL
glm.residual.flag=0
# if(length(myfactors)>0){
#   
#   glm.name.temp=1
#   
#   while(glm.name.temp<{length(myfactors)+1}){
#     
#     glm.name.grep=grep(substr(myfactors[glm.name.temp],1,{nchar(myfactors[glm.name.temp])-2}),my.reduced.vars)
#     
#     if(glm.name.grep>0){
#       my.reduced.vars=my.reduced.vars[-glm.name.grep]
#       my.reduced.labels=my.reduced.labels[-glm.name.grep]
#       glm.name.temp=glm.name.temp+1
#     }else{
#       glm.name.temp=glm.name.temp+1
#     }}
#   
#   baseformula2=paste(my.reduced.vars[1],independencesquig,1)
#   mypremodel.glm=glm(formula = baseformula2, data=reducedDF)
#   
# }


observe({
  glm.options=input$glmOptions
  shinyjs::hide("glmdw")
  shinyjs::hide("glmlevene")
  if(length(glm.options)>0){
    for(i in 1:{length(glm.options)}){
      if(glm.options[i]=="Durbin-Watson"){
        shinyjs::show("glmdw")
      }
      if(glm.options[i]=="Levene's Test"){
        shinyjs::show("glmlevene")
      }
    }
  }
})


#### Text input magic ####
observe({
  glm.dv=input$glm.dv.select
  glm.cov=input$glm.cov.check
  glm.fac=input$glm.factor.check
  glm.thang=c(glm.fac,glm.cov)
  my.glm.formula=paste(glm.dv,independencesquig)
  
  for (i in 1:length(glm.thang))
  {
    if (i>1){
      addvarr=paste(plusand,glm.thang[i])
      my.glm.formula=paste(my.glm.formula,addvarr)
    }
    else{
      my.glm.formula=paste(my.glm.formula,glm.thang[i])
    }
    
  }
  
  updateTextInput(session,"glm.formula",value=my.glm.formula)
})


#### Enter button ####
myformula2 = eventReactive(input$glm_input,{
  sreducedDFtedvariables2=input$glm.formula
})

#### Output Durbin Watson Stuff ####
output$glm.DWtest=renderUI({
  my.mlm.formula=myformula2()
  
  my.DW.test=lmtest::dwtest(formula=eval(parse(text=my.mlm.formula)),data=reducedDF)
  
  part1=paste("DW = ",round(as.numeric(my.DW.test[1]),3),", p-value = ",pixiedust::pvalString(my.DW.test[[4]]))
  part2=paste("Alternative H0: ",my.DW.test$alternative)
  
  return(HTML(paste(part1,part2,sep="<br />")))
})

#### Output Levene's Test ####
output$glm.levene=renderUI({
  full.glm.formula=myformula2()
  glm.dv=isolate(input$glm.dv.select)
  glm.fac=isolate(input$glm.factor.check)
  
  #### Levene's test only allows factors.
  #### In addition, it's only one term connected by *
  my.glm.formula3=paste(glm.dv,independencesquig)
  
  for (i in 1:length(glm.fac)){
    if (i>1){
      addvarr=paste(interactand,glm.fac[i],sep="")
      my.glm.formula3=paste(my.glm.formula3,addvarr,sep="")
    }
    else{
      my.glm.formula3=paste(my.glm.formula3,glm.fac[i])
    }
  }
  
  my.glm.levene=car::leveneTest(eval(parse(text=my.glm.formula3)),data=reducedDF)
  
  my.glm.levene$name=c("group","")
  my.glm.levene=my.glm.levene[,c(4,1,2,3)]
  
  
  options(pixie_interactive = F,pixie_na_string="")
  my.dust2=pixiedust::dust(my.glm.levene)%>%
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
output$glm.model=renderUI({
  
  independentvar=isolate(input$glm.dv.select)
  my.factor=isolate(input$glm.factor.check)
  myformula=myformula2()
  my.ord.grep=grep(independentvar,names(reducedDF))
  mypremodel.glm<<-my.glm.model
  if(is.ordered(reducedDF[[my.ord.grep]])){
    my.glm.model<<-ordinal::clm(formula=myformula,data=reducedDF,link="logit")
    print(summary(my.glm.model))
    my.dust.print=quick.lavaan::quick.reg(my.glm.model,reducedDF,my.factor = my.factor,pix.int = F,type="ord")
  }else{
  my.glm.model<<-glm(formula = myformula, data=reducedDF,family=binomial)
  my.dust.print=quick.lavaan::quick.reg(my.glm.model,reducedDF,my.factor = my.factor,pix.int = F,type="glm")
  }
  
  
  
  return(HTML(my.dust.print))
  
}
)


#### glm UPDATE MODEL ####

output$glm2=renderUI({
  independentvar=input$glm.dv.select
  independentvar2=myformula2()
  
  #### ERROR CATCH ####
  mt.mlm.glm.catch2=tryCatch(glm(mypremodel.glm,my.glm.model), error=function(e) e)
  
  if(!is.null(dim(mt.mlm.glm.catch2))){
    myglm=glm(mypremodel.glm,my.glm.model)
    
    

    mycaption=attr(myglm,"heading")[2]
    mycaption=sub("\n","<br />",mycaption)
    
    if(typeof(myglm$RSS[1])=="double"){
    
      myglm$RSS[1]=round(myglm$RSS[1],2)
    
    }
    if(dim(myglm)[1]>1){
    myglm$models=c(1,2)
    myglm=myglm[c(7,1,2,3,4,5,6)]

    my.caption.table=as.data.frame(matrix(ncol=7,nrow=1))
    my.caption.table[1]=mycaption
    
    options(pixie_interactive = F)
    my.dusted.glm=pixiedust::dust(myglm)%>%
      sprinkle_print_method("html")%>%
      sprinkle(cols="RSS",rows=2,round=2)%>%
      sprinkle(cols=c("Sum of Sq","F"),rows=1:2,round=3)%>%
      sprinkle(rows=1,replace=c("1",myglm$Res.Df[1],myglm$RSS[1],"","","",""))%>%
      sprinkle(cols="Pr(>F)",fn=quote(pvalString(value,digits=3,format="default")))
    
    my.dusted.glm=pixiedust::redust(my.dusted.glm,my.caption.table,part="foot")%>%
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


    my.dusted.glm=print(my.dusted.glm,quote=F)[1]
    
    return(HTML(my.dusted.glm))
    }else{
      p("This did not work. Sorry.")
    }
  }else{
    
    p("This did not work. Sorry.")
    
  }
})

#### LEAVE THIS EXTRA LINE BELOW! ####
