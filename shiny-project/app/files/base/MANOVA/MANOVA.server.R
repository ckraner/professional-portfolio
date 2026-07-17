##################### MANOVA SERVER ########################
#################### BY CHRIS KRANER #######################
############## NORTHERN ILLINOIS UNIVERSITY ################
######################## 10/2017 ###########################
######################## V: 1.0 ############################
############################################################

# my.dv.mancova.blah=cbind(reducedDF[[1]],reducedDF[[2]])
# my.manova=manova(data=reducedDF,my.dv.mancova.blah~reducedDF[[3]])
#### Text input magic ####
observe({
  manova.dv=input$manova.dv
  manova.cov=input$manova.iv
  manova.fac=input$manova.factors
  mancova.thang=c(manova.fac,manova.cov)
  #my.manova.formula=paste(manova.dv,independencesquig)
  for(i in 1:length(manova.dv)){
    if(i==1){
      my.manova.formula=paste(manova.dv[i])
    }else{
      my.manova.formula=paste(my.manova.formula,plusand,manova.dv[i])
    }
  }
  my.manova.formula=paste(my.manova.formula,independencesquig)
  for (i in 1:length(mancova.thang))
  {
    if (i>1){
      addvarr=paste(plusand,mancova.thang[i])
      my.manova.formula=paste(my.manova.formula,addvarr)
    }
    else{
      my.manova.formula=paste(my.manova.formula,mancova.thang[i])
    }
    
  }
  
  updateTextInput(session,"manova.formula",value=my.manova.formula)
})


my.iv.formula=eventReactive(input$enter_input_manova,{
  
  my.factors=input$manova.formula
  
})


output$manova.model=renderUI({
  
  #my.dv.list=isolate(input$manova.dv)
  my.formula=my.iv.formula()
  #my.iv.list=isolate(c(input$manova.factors,input$manova.iv))

  my.piece=strsplit(my.formula,"~")
  my.first.piece=strsplit(my.piece[[1]][1],"\\+")
  my.dv.list=stringi::stri_trim_both(my.first.piece[[1]])
  my.second.piece=strsplit(my.piece[[1]][2],"\\+")
  my.iv.list=stringi::stri_trim_both(my.second.piece[[1]])
  #### Make DV ####
  
  my.dv.combined=NULL
  
  for(j in 1:length(my.dv.list)){
    
    manova.grep=grep(paste("^",my.dv.list[j],"$",sep=""),names(reducedDF))
    my.dv.combined=cbind(my.dv.combined,reducedDF[[manova.grep]])
    
  }
  
  colnames(my.dv.combined)=my.dv.list
  
  #### Make Equation ####

  myformula=paste("my.dv.combined",independencesquig)
  
  for (i in 1:length(my.iv.list)){
    
    if (i>1){
      
      addvarr=paste(plusand,my.iv.list[i])
      myformula=paste(myformula,addvarr)
      
    }else{
      
      myformula=paste(myformula,my.iv.list[i])
      
    }
    
  }
  
  my.manova<<-manova(data=reducedDF,eval(parse(text=myformula)))

   
  #summary(my.manova,test="Wilks")
  #car::Anova(my.manova,type=3,test="Pillai")

  #### Make table ####
  my.manova.pixie=quick.lavaan::quick.reg(my.manova,type="manova", pix.int = F)
  
  return(HTML(my.manova.pixie))
  
})

output$phia=renderUI({
  
  my.factors=isolate(input$manova.factors)
  
  #my.manova2-my.manova
  x3=capture.output(car::Anova(my.manova,type=3,test="Wilks"))
  my.phia.print=as.data.frame(matrix(ncol=8,nrow=1))
  my.lengths=NULL
  this.table.var=1
  
  while(this.table.var<{length(my.factors)+1}){
    
    my.phia=phia::testInteractions(my.manova,fixed=my.factors[this.table.var])
    my.phia$names=attr(my.phia,"row.names")
    my.phia=my.phia[c("names","Df","test stat","approx F","num Df","den Df","Pr(>F)")]
    attr(my.phia,"class")=attr(my.phia,"class")[-1]
    my.lengths=c(my.lengths,nrow(my.phia))
    this.stuff=c(my.factors[this.table.var],NA)
    
    if(my.lengths[this.table.var]>2){
      
      for(i in 1:{my.lengths[this.table.var]-2}){
        
        this.stuff=c(this.stuff,NA)
        
      }
      
    }
    
    my.phia=cbind(this.stuff,my.phia)

    if(this.table.var==1){
      
      my.phia.print=my.phia
      
    }else{
      
      my.phia.print=rbind(my.phia.print,my.phia)
      
    }
    
    this.table.var=this.table.var+1
    my.phia.print
    
  }
  
  rownames(my.phia.print)=NULL
  phia.length=dim(my.phia.print)[1]
  
  options(pixie_interactive = F)
  my.phia.pixie=pixiedust::dust(my.phia.print)%>%
    sprinkle_print_method("html")%>%
    sprinkle(cols="Pr(>F)",fn=quote(pvalString(value,digits=3,format="default")))%>%
    sprinkle(cols="test stat",round=3)%>%
    sprinkle(cols="approx F",round=3)%>%
    sprinkle_colnames("","Levels","df","Pillai <br /> Statistic","approx <br /> F-value","num <br /> df","den <br /> df","Pr(>F)")%>%
    sprinkle(cols=1:8,rows={sum(my.lengths)},border="bottom")%>%
    sprinkle(cols=1:8,pad=10)%>%
    sprinkle(cols=1,rows=1:{sum(my.lengths)},border="left")%>%
    sprinkle(cols=3:8,rows=1:{sum(my.lengths)},border=c("right","left"))%>%
    sprinkle(cols=1:8,rows=1,border=c("top","bottom"),part="head")%>%
    sprinkle(cols=1,rows=1,border="left",part="head")%>%
    sprinkle(cols=3:8,rows=1,border=c("right","left"),part="head")%>%
    sprinkle_na_string(na_string="")%>%
    sprinkle_width(cols=1,rows=1:2,width=70,width_units="pt")%>%
    sprinkle_width(cols=2,rows=1:2,width=70,width_units="pt")%>%
    sprinkle_width(cols=3,width=30,width_units="pt")%>%
    sprinkle_width(cols=4,width=60,width_units="pt")%>%
    sprinkle_width(cols=5,width=50,width_units="pt")%>%
    sprinkle_width(cols=6,width=50,width_units="pt")%>%
    sprinkle_width(cols=7,width=50,width_units="pt")%>%
    sprinkle_width(cols=8,width=70,width_units="pt")%>%
    sprinkle(rows=1,halign="center",part="head")
  
  my.phia.pixie=print(my.phia.pixie,quote=F)[1]
  
  return(HTML(my.phia.pixie))
  
})

#### LEAVE THIS EXTRA LINE BELOW! ####
