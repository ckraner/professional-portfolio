################## EXPLORER MAIN PANEL #####################
#################### BY CHRIS KRANER #######################
############## NORTHERN ILLINOIS UNIVERSITY ################
######################### 9/2017 ###########################
######################### V: 1.0 ###########################
############################################################

box(
  title = "Plots", solidHeader = TRUE,
  collapsed = F,collapsible = T,width=NULL,
  
  
    radioButtons("gtype","Graph Type:",list("Histogram"
                                            ,"QQPlot"
                                            ,"BoxPlot"
                                            ,"All"), selected="All", inline=T),
    jqui_resizabled(plotOutput("myPlot",width="400px"))
  
)

#### LEAVE THIS EXTRA LINE BELOW! ####
