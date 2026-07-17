################### MANOVA SIDE PANEL ######################
#################### BY CHRIS KRANER #######################
############## NORTHERN ILLINOIS UNIVERSITY ################
######################## 10/2017 ###########################
######################## V: 1.0 ############################
############################################################

sidebarPanel(
  
  if(length(myfactors)>0){
    
    source(mancova.dv.check,local=T)[1]},
  
  if(length(myfactors)>0){
    
    source(mancova.factor.check,local=T)[1]},
  
  if(length(myfactors)>0){
    
    source(mancova.cov.check,local=T)[1]},
  
  div(id='goright3', class="simpleDiv",align="right",
      actionButton("enter_input_manova", "Enter"))

)

#### LEAVE THIS EXTRA LINE BELOW! ####
