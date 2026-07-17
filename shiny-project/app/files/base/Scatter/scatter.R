################## SCATTER MAIN PANEL ######################
#################### BY CHRIS KRANER #######################
############## NORTHERN ILLINOIS UNIVERSITY ################
######################### 9/2017 ###########################
######################## V: 1.0 ############################
############################################################

box(
  title = "Plot", solidHeader = TRUE,
  collapsible = TRUE,width=NULL,collapsed=F,
  
  jqui_resizabled(plotOutput("scatter",width="600px"))

)

#### LEAVE THIS EXTRA LINE BELOW! ####
