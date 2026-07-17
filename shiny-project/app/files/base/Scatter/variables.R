################## SCATTER SIDE PANEL ######################
#################### BY CHRIS KRANER #######################
############## NORTHERN ILLINOIS UNIVERSITY ################
######################### 9/2017 ###########################
######################## V: 1.0 ############################
############################################################

box(
  title = "Options", solidHeader = TRUE,
  collapsible = TRUE,width=NULL,collapsed=F,
  
  textInput("scatcapt","Caption", "Histogram",width="100%"),
  
  span(column(6,textInput("xtitle","x-Axis","X-Value")),
       column(6,textInput("ytitle","y-Axis","Y-Value"))),
  
  span(column(6,
  source(scat1radio, local=T)[1]),
  column(6,
         source(scat2radio, local=T)[1]))
  
)

#### LEAVE THIS EXTRA LINE BELOW! ####
