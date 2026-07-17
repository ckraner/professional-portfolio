############# VARIABLE SELECTION INTERFACES ################
#################### BY CHRIS KRANER #######################
############## NORTHERN ILLINOIS UNIVERSITY ################
######################### 9/2017 ###########################
######################### V: 1.0 ###########################
############################################################

#### This is really the heart and soul.
#### Makes the shiny buttons and server code with all variables.


#### EXPLORER ####
varreactivetemp=writetotemp_server(mylabels,myvars,c(2,2),"varreact","input$variable","reducedDF")
if(!too.large){
variableradio=writetotemp_ui("radio",mylabels,myvars,c(1,2),"variable","Variable:",T)
}else{
  variableradio=writetotemp_ui("radio",myvars,myvars,c(1,2),"variable","Variable:",T)
}

#### SCATTERPLOTS ####
scatyreactivetemp=writetotemp_server(mylabels,myvars,c(2,2),"scaty","input$scat1","reducedDF")
scatxreactivetemp=writetotemp_server(mylabels,myvars,c(2,2),"scatx","input$scat2","reducedDF")
scat1radio=writetotemp_ui("select",regressionlabels,regressionvars,c(1,2),"scat1","X-Variable:",F)
scat2radio=writetotemp_ui("select",regressionlabels,regressionvars,c(1,2),"scat2","Y-Variable:",F)


#### LINEAR MODELING ####
mlm1select=writetotemp_ui("select",mylabels,myvars,c(1,2),"mlm1","Dependent Variable:",F)
mlmvarscheckbox=writetotemp_ui("check",mylabels,myvars,c(1,2),"mlmvars","Independent Variable(s)",F)


#### ANCOVA ####
if(length(myfactors)>0){
ancova.dv=writetotemp_ui("select",my.reduced.labels,my.reduced.vars,c(1,2),"ancova.dv.select","Dependent Variable:",F)
ancova.factor=writetotemp_ui("check",myfactorlabels,myfactors,c(1,2),"ancova.factor.check","Factor(s)",F)
ancova.cov=writetotemp_ui("check",my.reduced.labels,my.reduced.vars,c(1,2),"ancova.cov.check","Covariate(s)",F)
}


#### MANCOVA ####
if(length(myfactors)>0){
  if(!too.large){
mancova.dv.check=writetotemp_ui("check",my.reduced.labels,my.reduced.vars,c(1,2),"manova.dv","Dependent Variables:",F)
mancova.factor.check=writetotemp_ui("check",myfactorlabels,myfactors,c(1,2),"manova.factors","Factor(s)",F)
mancova.cov.check=writetotemp_ui("check",my.reduced.labels,my.reduced.vars,c(1,2),"manova.iv","Covariate(s)",F)
  }else{
    mancova.dv.check=writetotemp_ui("check",my.reduced.vars,my.reduced.vars,c(1,2),"manova.dv","Dependent Variables:",F)
    mancova.factor.check=writetotemp_ui("check",myfactors,myfactors,c(1,2),"manova.factors","Factor(s)",F)
    mancova.cov.check=writetotemp_ui("check",my.reduced.vars,my.reduced.vars,c(1,2),"manova.iv","Covariate(s)",F)
}
}



#### GLM ####
if(length(myfactors)>0){
  glm.dv=writetotemp_ui("select",my.log.labels,my.log.vars,c(1,2),"glm.dv.select","Dependent Variable:",F)
  glm.factor=writetotemp_ui("check",myfactorlabels,myfactors,c(1,2),"glm.factor.check","Factor(s)",F)
  glm.cov=writetotemp_ui("check",my.reduced.labels,my.reduced.vars,c(1,2),"glm.cov.check","Covariate(s)",F) 
}

#### LEAVE THIS EXTRA LINE BELOW! ####
