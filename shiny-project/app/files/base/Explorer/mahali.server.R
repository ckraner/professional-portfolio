################ MAHALI AND TABLE SERVER ###################
#################### BY CHRIS KRANER #######################
############## NORTHERN ILLINOIS UNIVERSITY ################
######################## 10/2017 ###########################
######################## V: 1.0 ############################
############################################################

tryMahali=tryCatch(Explorer::mahali(reducedDF2),error=function(e) e, warning=function(w) w)

if(typeof(tryMahali[[1]])=="character"){

  output$mahaliGraph=renderPlot({})
  output$mahali=renderPrint({
    mymsg="I'm sorry, there is a problem with your data for this test."
    cat(mymsg)
  })

}else{

  mahliman=Explorer::mahali(reducedDF2)
  mahliman=round(mahliman,2)
  reducedDF$mahli=mahliman
  #### MAHALINOBIS PLOTS ####

  output$mahaliGraph=renderPlot({

    Explorer::mahali(reducedDF2)
    myGraph=grid::grid.grab()
    myGraph
    myGraph=recordPlot()

  })

  output$mahali=renderPrint({

    psych::describe(mahliman)

  })

}




#### TABLE WITH MAHALINOBIS DISTANCES ####

output$mahaliTable=DT::renderDataTable({

  update.test=input$table_update

  if(update.test){

    reducedDF

  }

},fillContainer=T)

#### Experimental Table

# output$editTable=renderRHandsontable({
#
#   #reducedDF=Electric2
#   reducedDF2=cbind(reducedDF[[1]],logical=rep(TRUE,dim(reducedDF)[1]))
#   for(i in 2:dim(reducedDF)[2]){
#     reducedDF2=cbind(reducedDF2,reducedDF[[i]])
#   }
#   reducedDF2=as.data.frame(reducedDF2,stringsAsFactors=F)
#   reducedDF2$logical=as.logical(reducedDF2$logical)
#   my.hot=rhandsontable(reducedDF2)%>%
#     hot_cols(columnSorting = T)
#   #reducedDF2=hot_to_r(my.hot)
#   #reducedDF<<-reducedDF2
# })


#### LEAVE THIS EXTRA LINE BELOW! ####
