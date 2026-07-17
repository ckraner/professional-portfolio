#################### MLR SIDE PANEL ########################
#################### BY CHRIS KRANER #######################
############## NORTHERN ILLINOIS UNIVERSITY ################
######################### 9/2017 ###########################
######################## V: 1.0 ############################
############################################################

box(
  title = "Formula", solidHeader = TRUE,
  collapsible =F,width=NULL,
  tagAppendAttributes(shiny::textInput("mlm.formula",""),`data-proxy-click` = "enter_input"),
  source(mlm1select, local=T)[1],
  fluidRow(width=12,box(title="Variables",solidHeader=T,collapsible=T,width=NULL,
      tags$div(align="left",class="multicol32",
               source(mlmvarscheckbox, local=T)[1]))),
  div(id='goright', class="simpleDiv",align="right",
      actionButton("enter_input", "Enter"))
)

#### LEAVE THIS EXTRA LINE BELOW! ####
