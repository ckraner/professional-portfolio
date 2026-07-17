############## INSTALLER FOR STATS EXPLOR.R ################
#################### BY CHRIS KRANER #######################
############## NORTHERN ILLINOIS UNIVERSITY ################
######################### 9/2017 ###########################
############################################################



#' Install Directories
#' 
#' Create UI code to be ran by the program.
#' @keywords Explore
#' @export
#' @examples 
#' intal_stats_explor.r
#' 

install_stats_explor.r=function(){
  
  myf="stats.explor.r.files"
  explor.r.directory=file.path(.libPaths(),myf)
  
  #### Check if directory exists, create if not
  if(dir.exists(file.path(.libPaths(),myf))){
       print("Directory already exists.")
  } else{
    prevdir=getwd()
    setwd(file.path(.libPaths()))
    dir.create(myf)
    setwd(file.path(.libPaths(),myf))
    dir.create("base")
    dir.create("live")
    setwd(prevdir)
  }

  ##### PATHS ####
  base.path=file.path(explor.r.directory,"base")
  live.path=file.path(explor.r.directory,"live")

  
  #### Footer   ####
  file.create(file.path(base.path,"footer.R"),showWarnings = F)
  footer.file=file.path(base.path,"footer.R")
  test=capture.output(cat("fluidRow(column(12,
                          p(\"Created by Chris Kraner\",align=\"center\")))"))
  writeLines(test,footer.file[1])

  #### required packages ####
  file.create(file.path(live.path,"packages.R"),showWarnings = F)
  live.file=file.path(live.path,"packages.R")
  test=capture.output(cat("require(shiny)
                          require(Explorer)
                          require(ggplot2)
                          require(dplyr)
                          require(psych)
                          require(BaylorEdPsych)
                          require(stats.explor.r.utils)"))
  writeLines(test,live.file[1])
  
  #### Database Operations ####
  file.create(file.path(live.path,"databases.R"),showWarnings = F)
  databases.file=file.path(live.path,"databases.R")
  test=capture.output(cat("allvars=c(myvars,colid)
                          reducedDF=myDF[allvars]
                          reducedDF2=reducedDF[myvars]
                          reducedDF.resid=na.omit(reducedDF)"))
  writeLines(test,databases.file[1])
  
  #### Initializations ####
  file.create(file.path(live.path,"init.R"),showWarnings = F)
  init.file=file.path(live.path,"init.R")
  test=capture.output(cat("independencesquig=\"~\"
baseformula=paste(myvars[1],independencesquig)
baseformula=paste(baseformula,myvars[1])
mypremodel=lm(formula = baseformula, data=reducedDF)
mymodel=lm(formula = baseformula, data=reducedDF)

myothervars=c(\"res1\",\"predict1\")
myotherlabels=c(\"Residuals\",\"Predicted Values\")
regressionlabels=c(mylabels,myotherlabels)
regressionvars=c(myvars,myothervars)"))
  writeLines(test,init.file[1])
  
  #### Reactives ####
  file.create(file.path(live.path,"react.R"),showWarnings = F)
  react.file=file.path(live.path,"react.R")
  test=capture.output(cat("#### SERVER SIDE
varreactivetemp=writetotemp_server(mylabels,myvars,c(2,2),\"varreact\",\"input$variable\",\"myDF\")
                          scatyreactivetemp=writetotemp_server(mylabels,myvars,c(2,2),\"scaty\",\"input$scat1\",\"myDF\")
                          scatxreactivetemp=writetotemp_server(mylabels,myvars,c(2,2),\"scatx\",\"input$scat2\",\"myDF\")
                          regressiontemp=writetotemp_server(regressionlabels,regressionvars,c(2,2),\"yresid\",\"input$residy\",\"myDF\")
                          regressiontemp2=writetotemp_server(regressionlabels,regressionvars,c(2,2),\"xresid\",\"input$residx\",\"myDF\")
                          #### UI SIDE
                          variableradio=writetotemp_ui(\"radio\",mylabels,myvars,c(1,2),\"variable\",\"Variable:\",T)
                          mlm1select=writetotemp_ui(\"select\",mylabels,myvars,c(1,2),\"mlm1\",\"Dependent Variable:\",F)
                          mlmvarscheckbox=writetotemp_ui(\"check\",mylabels,myvars,c(1,2),\"mlmvars\",\"Independent Variable(s)\",F)
                          scat1radio=writetotemp_ui(\"radio\",mylabels,myvars,c(1,2),\"scat1\",\"Variable:\",F)
                          scat2radio=writetotemp_ui(\"radio\",mylabels,myvars,c(1,2),\"scat2\",\"Variable:\",T)
                          resyradio=writetotemp_ui(\"radio\",regressionlabels,regressionvars,c(1,2),\"residy\",\"Residual\",F)
                          resxradio=writetotemp_ui(\"radio\",mylabels,myvars,c(1,2),\"residx\",\"Residual\",T)"))
  writeLines(test,react.file[1])
  
  
  #### MLM TAB ####
  
  file.create(file.path(base.path,"MLMtab.ui.sidepanel.R"),showWarnings = F)            
  mlm.ui.sidepanel=file.path(base.path,"MLMtab.ui.sidepanel.R")
  MLM.sidepanel=capture.output(cat("sidebarPanel(
                         source(mlm1select, local=T)[1],
                         source(mlmvarscheckbox, local=T)[1],
                         actionButton(\"enter_input\", \"Enter\")
                         )"))
  writeLines(MLM.sidepanel,mlm.ui.sidepanel[1])
  
  file.create(file.path(base.path,"MLMtab.ui.mainpanel.R"),showWarnings = F)            
  mlm.ui.mainpanel=file.path(base.path,"MLMtab.ui.mainpanel.R")
  MLM.mainpanel=capture.output(cat("mainPanel(
                                   verbatimTextOutput(\"anova\"),
                                   verbatimTextOutput(\"model\")
                                   )"))
  writeLines(MLM.mainpanel,mlm.ui.mainpanel[1])
  

  #### Scatterplots ####
  
  file.create(file.path(base.path,"scatter.ui.mainpanel.R"),showWarnings = F)            
  scatter.ui.mainpanel=file.path(base.path,"scatter.ui.mainpanel.R")
  scatter.mainpanel=capture.output(cat("mainPanel(
                                    source(scat2radio, local=T)[1],
                                       plotOutput(\"scatter\")
  )"))
  writeLines(scatter.mainpanel,scatter.ui.mainpanel[1])
  
  file.create(file.path(base.path,"scatter.ui.sidepanel.R"),showWarnings = F)            
  scatter.ui.sidepanel=file.path(base.path,"scatter.ui.sidepanel.R")
  scatter.sidepanel=capture.output(cat("sidebarPanel(
                                   
                                       textInput(\"scatcapt\",\"Caption\", \"Histogram\"),
                                       verbatimTextOutput(\"scatcapt2\"),
                                       textInput(\"ytitle\",\"y-Axis\",\"Value\"),
                                       verbatimTextOutput(\"ytitle2\"),
                                       textInput(\"xtitle\",\"x-Axis\",\"Value\"),
                                       verbatimTextOutput(\"xtitle2\"),
                                       
                                       source(scat1radio, local=T)[1]
                                       
                                       
                                       
  )"))
  writeLines(scatter.sidepanel,scatter.ui.sidepanel[1])
  
  #### Explorer ####
  #### EXPLORER SERVER
  
  file.create(file.path(base.path,"explorer.server.R"),showWarnings = F)            
  explorer.server.file=file.path(base.path,"explorer.server.R")
  explorer.server=capture.output(cat("#### MCAR AND MAHALINOBIS TESTS ####
  #### Don't need to be updated, so outside anything with output
                         #### Round Mahalinobis test to two decimals and attach to original dataframe
                         
                         
                         long=BaylorEdPsych::LittleMCAR(reducedDF2)
                         # BaylorEdPsych::LittleMCAR(reducedDF2)
                         # long$df
                         
                         source(varreactivetemp,local = T)    
                         
                         
                         #### PSYCH DESCRIBE TABLE ####
                         
                         output$mySummary=renderPrint({
                         varreact2=varreact()
                         
                         z=with(myDF,psych::describe(varreact2))
                         #print(xtable(z),type = \"html\")
                         z1=tidyr::gather(z,\"X1\")
                         z1$value=round(z1$value,input$decimal)
                         z1
                         #### statexplorer ####
                         #myList=Explorer::statexplorer(reducedDF,varreact2,mynumba,colid,mynumbacase)
                         #myList[1]
                         
                         })
                         
                         #### VARIABLE STAT TABLES ####
                         #### Histogram with normal, QQ Plot, Box Plot with Outliers, Multigraph (Hist, QQ Plot, normal Box Plot)
                         
                         output$myPlot=renderPlot({
                         varreact2=varreact()
                         
                         #### get column numbers for ID and variable sreducedDFtion ####
                         
                         mynumba=grep(input$variable,colnames(reducedDF))
                         mynumbacase=grep(colid,colnames(reducedDF))
                         
                         
                         #### statexplorer ####
                         
                         myList=Explorer::statexplorer(reducedDF,varreact2,mynumba,colid,mynumbacase)
                         
                         
                         
                         #### Only have to get it sent to the viewer to tie it in.
                         
                         switch(input$gtype,
                         \"BoxPlot\"=myList[4],
                         \"Histogram\"=myList[2],
                         \"QQPlot\"=myList[3],
                         \"All\"=myList[5])
                         })
                         
                         
                         
                         #### LITTLES MCAR TEST OUTPUT ####
                         #### Output for test for Missing Completely at random (MCAR)
                         #### Need to turn into full HTML
                         
                         
                         output$lilMCAR=renderPrint({
                         
                         cat(\"X^2\")
                         cat(\" ( \")
                                 cat(long$df)
                                 cat(\" ) =  \")
                         cat(long$chi.square)
                         cat(\", p= \")
                         cat(long$p.value)
                         })"))
  writeLines(explorer.server,explorer.server.file[1])
  
  
  
  #### Mahali
  #### Server
  mahali.server.file=file.path(base.path,"mahali.server.R")
  output1=capture.output(cat("mahliman=Explorer::mahali(reducedDF2)
  mahlimanr=round(mahliman, 2)
                          reducedDF$mahli=mahlimanr
                          
                          
                          #### MAHALINOBIS PLOTS ####
                          
                          
                          output$mahaliGraph=renderPlot({
                          Explorer::mahali(reducedDF2)
                          myGraph=grid::grid.grab()
                          myGraph
                          myGraph=recordPlot()
                          })
                          
                          
                          #### TABLE WITH MAHALINOBIS DISTANCES ####
                          
                          
                          output$mahaliTable=renderDataTable({
                          reducedDF
                          # rhandsontable(reducedDF)%>%
                          #   hot_col(\"factor_allow\", allowInvalid = TRUE)
                          })"))
  writeLines(output1,mahali.server.file[1])
  
  
  #### Scatter Plots
  scatter.server.file=file.path(base.path,"scatter.server.R")
  test=capture.output(cat("  source(scatyreactivetemp,local = T)[1]
  
  
  source(scatxreactivetemp,local=T)[1]  
  
  
  #### SCATTER PLOTS ####
  #### Don't know why, but scatx() and y not returning unquoted values
  
  output$scatter=renderPlot({
    myx=scatx()
    myy=scaty()
    
    myx1=grep(input$scat1,colnames(reducedDF))
    myy1=grep(input$scat2,colnames(reducedDF))
    
    plot(reducedDF[[myx1]], reducedDF[[myy1]],main=input$scatcapt,xlab=input$xtitle,ylab=input$ytitle)
    
  })"))
  writeLines(test,scatter.server.file[1])
  

  
  #### Little's MCAR and psych describe
  file.create(file.path(base.path,"explorer.ui.sidebar.R"),showWarnings = F)
  explorer.ui.sidebar=file.path(base.path,"explorer.ui.sidebar.R")
  explorer.ui.sidebar.output=capture.output(cat("sidebarPanel(
                                                #### psych:describe ####
                                                
                                                h4(print(\"Basic Descriptives - from psych::describe\")),
                                                #### Variable Select ####
                                                verbatimTextOutput(\"mySummary\"),
                                                sliderInput(\"decimal\",\"Num of Decimals:\",
                                                min=1,max=6,
                                                value=2,step=1),
                                                
                                                
                                                br(),br(),
                                                
                                                
                                                h4(print(\"Little's Test for MCAR\")),
                                                br(),
                                                textOutput(\"lilMCAR\"))"))
  writeLines(explorer.ui.sidebar.output,explorer.ui.sidebar[1])
  
  
  #### Variable Select And Plots
  file.create(file.path(base.path,"explorer_ui.mainpanel.R"),showWarnings = F)
  explorer.ui.mainpanel=file.path(base.path,"explorer.ui.mainpanel.R")
  explorer.ui.mainpanel.output=capture.output(cat("mainPanel(
                                                source(variableradio, local=T)[1],
                                                #### Plots ####
                                                h4(print(\"Plots\")),
                                                radioButtons(\"gtype\",\"Graph Type:\",list(\"Histogram\"
                                                ,\"QQPlot\"
                                                ,\"BoxPlot\"
                                                ,\"All\"), selected=\"All\", inline=T),
                                                plotOutput(\"myPlot\"))"))
  writeLines(explorer.ui.mainpanel.output,explorer.ui.mainpanel[1])
  #footer=
  #### MLM STUFF ####
  file.create(file.path(base.path,"MLM.server.R"),showWarnings = F)
  mlm.server.file=file.path(base.path,"MLM.server.R")
  mlm.server.file.output=capture.output(cat("#### MLM AND REGRESSION
source(regressiontemp,local=T)[1]
                                            source(regressiontemp2,local=T)[1]
                                            myformula = eventReactive(input$enter_input,{
                                            #independentvar=input$mlm1
                                            sreducedDFtedvariables=input$mlmvars
                                            })
                                            
                                            #### MLM STUFF ####
                                            output$model=renderPrint({
                                            independentvar=input$mlm1
                                            sreducedDFtedvariables=myformula()
                                            #independentvar=\"ddepre\"
                                            independencesquig=\"~\"
                                            plusand=\"+\"
                                            myformula=paste(independentvar,independencesquig)
                                            
                                            
                                            
                                            #sreducedDFtedvariables=myvars
                                            
                                            
                                            for (i in 1:length(sreducedDFtedvariables))
                                            {
                                            #print(i)
                                            #ifelse((i!=1),addvar=paste(plusand,sreducedDFtedvariables[i]),addvar=sreducedDFtedvariables[i])
                                            if (i>=1){
                                            addvarr=paste(plusand,sreducedDFtedvariables[i])
                                            myformula=paste(myformula,addvarr)
                                            }
                                            else{
                                            #addvar=paste(independencesquig,sreducedDFtedvariables[1])
                                            myformula=paste(myformula,independentvar)
                                            }
                                            
                                            }
                                            # if(i<3){
                                            #   input$mypremodel=lm(forumula=myformula,data=reducedDF)
                                            # }
                                            library(dplyr)
                                            library(tibble)
                                            
                                            print(myformula)
                                            mymodel<<-lm(formula = myformula, data=reducedDF)
                                            
                                            #mymodel=lm(EDUYR~DBP58,data=Electric)
                                            #reducedDF.resid=Electric
                                            #typeof(myDF$CASEID)
                                            #typeof(mypredict$CASEID)
                                            #colid=\"CASEID\"
                                            #  names(myDF)
                                            #resid1=resid(mymodel)
                                            #  myresid=resid(mymodel)
                                            # 
                                            #  #Electric$caseid
                                            #  
                                            #  #typeof(myres2)
                                            #  #rownames(myres2)
                                            #  myres2=as.list(myresid)
                                            #  myres3=as.data.frame(myres2)
                                            #  myresid=myres3
                                            #  #myres3=tibble::rownames_to_column(myres3,\"colid\")
                                            #  # mycolumnnames=names(myres3)
                                            #  # mycolumnnames=sub('.','',mycolumnnames)
                                            #  # mycolumnnames=mycolumnnames
                                            #  
                                            #  #colnames(myresid)=\"res1\"
                                            #  #myresid$res1
                                            #  #Divorce$age
                                            #  #names(myresid)
                                            #  #myresid=add_rownames(myresid,\"colid\")
                                            #  #names(mypredict)
                                            # # myresid=tibble::rownames_to_column(myresid,colid)
                                            #  myresid[[1]]=as.numeric(myresid[[1]])
                                            #  #reducedDF.resid<<-dplyr::inner_join(myresid,reducedDF.resid,by=colid)
                                            #  myresid=tidyr::gather(myresid)
                                            #  reducedDF.resid$res1<<-myresid$value
                                            #  #typeof(myresid[)
                                            #  #myresid[[1]]
                                            #  
                                            #  mypredict=as.list(predict(mymodel))
                                            #  mypredict=as.data.frame(mypredict)
                                            #  #names(mypredict)
                                            # #colnames(mypredict)=\"predict1\"
                                            #  #mypredict=tibble::rownames_to_column(mypredict,colid)
                                            #  mypredict[[1]]=as.numeric(mypredict[[1]])
                                            #  mypredict=tidyr::gather(mypredict)
                                            #  reducedDF.resid$predict1<<-mypredict$value
                                            #  #reducedDF.resid<<-dplyr::inner_join(reducedDF.resid,mypredict)
                                            
                                            # 
                                            # reducedDF.resid$residuals<<-as.data.frame(resid(mymodel))
                                            # reducedDF.resid$predicted<<-as.data.frame(predict(mymodel))
                                            # #ouput$anova
                                            
                                            
                                            print(summary(mymodel))
                                            #print(dim(reducedDF.resid))
                                            #print(dim(myresidDF))
                                            #return(mymodel)
                                            
                                            }
                                            )
                                            
                                            #plot(myDF[[1]],myDF[[3]])
                                            #### RESIDUALS ####
                                            output$residplot=renderPlot({
                                            
                                            myx=xresid()
                                            myy=yresid()
                                            
                                            myxnum=grep(input$residx,colnames(reducedDF.resid))
                                            myynum=grep(input$residy,colnames(reducedDF.resid))
                                            # library(ggplot2)
                                            # x1=print(colid,quote=F)
                                            # x2=print(input$mlm1,quote=F)
                                            plot(reducedDF.resid[[myxnum]],reducedDF.resid[[myynum]])
                                            # myplot=grid::grid.grab()
                                            # myplot
                                            # myplot=recordPlot()
                                            # return(myplot)
                                            
                                            })
                                            #### ANOVA UPDATE MODEL ####
                                            
                                            output$anova=renderPrint({
                                            independentvar=input$mlm1
                                            independentvar2=myformula()
                                            
                                            myanova=anova(mypremodel,mymodel)
                                            print(myanova)
                                            mypremodel<<-mymodel
                                            # mypremodel=mymodel
                                            #   myanova=anova(output$mypremodel,mymodel)
                                            # 
                                            #     output$model()
                                            #     print(summary(myanova))
                                            })"))
  writeLines(mlm.server.file.output,mlm.server.file[1])
  
  #### Original NavBar Page ####
  file.create(file.path(live.path,"navPage.R"),showWarnings = F)
  live.navpage=file.path(live.path,"navPage.R")
  live.navpage.output=capture.output(cat("
navbarPage(\"Stats Explor.R\",
                                         
                                         #### Summary TAB ####
                                         
                                         # source(file.path(base.path,\"explorer.page.R\")[1],local = T)[1],
                                         tabPanel(\"Summary\",
                                         source(file.path(base.path,\"explorer.ui.sidebar.R\")[1],local=T)[1],
                                         source(file.path(base.path,\"explorer.ui.mainpanel.R\")[1],local=T)[1]
                                         ),
                                         
                                         
                                         
                                         ##### 2ND TAB ####
                                         tabPanel(\"MLM\",
                                         titlePanel(\"Multiple Linear Model\"),
                                         source(file.path(base.path,\"MLMtab.ui.sidepanel.R\")[1],local=T)[1],
                                         source(file.path(base.path,\"MLMtab.ui.mainpanel.R\")[1],local=T)[1]
                                         ),
                                         
                                         
                                         tabPanel(\"ScatterPlots\",
                                         #### Change to checkboxes
                                         
                                         tabsetPanel(
                                         tabPanel(\"Variables\",
                                         titlePanel(\"Scatter Plots for Variables\"),
                                         source(file.path(base.path,\"scatter.ui.sidepanel.R\")[1],local=T)[1],
                                         source(file.path(base.path,\"scatter.ui.mainpanel.R\")[1],local=T)[1]
                                         
                                         ),
                                         
                                         tabPanel(\"Residuals\",
                                         titlePanel(\"Scatter Plots for Residuals\"),
                                         sidebarPanel(
                                         source(resyradio, local=T)[1]
                                         ),
                                         mainPanel(
                                         plotOutput(\"residplot\"),
                                         source(resxradio,local=T)[1]
                                         )
                                         )
                                         )
                                         ),
                                         
                                         tabPanel(\"Advanced\",
                                         
                                         
                                         tabsetPanel(
                                         tabPanel(\"Graph\",
                                         h4(print(\"Mahalanobis Distances\")),
                                         
                                         verbatimTextOutput(\"mahali\"),
                                         plotOutput(\"mahaliGraph\")
                                         # hr(),
                                         # source(footer[1],local=T)[1]
                                         ),
                                         
                                         
                                         tabPanel(\"Table\",
                                         
                                         dataTableOutput(\"mahaliTable\")
                                         # hr(),
                                         # source(footer[1],local=T)[1]
                                         )
                                         
                                         )
                                         )
  )
                                         "))
  writeLines(live.navpage.output,live.navpage[1])
  
  #### Original Server
  file.create(file.path(live.path,"myserver.R"),showWarnings = F)
  my.server=file.path(live.path,"myserver.R")
  my.server.output=capture.output(cat("  source(file.path(base.path,\"explorer.server.R\")[1],local = T)[1]
  source(file.path(base.path,\"mahali.server.R\")[1],local = T)[1]
                                      source(file.path(base.path,\"scatter.server.R\")[1],local = T)[1]  
                                      source(file.path(base.path,\"MLM.server.R\")[1],local=T)[1]"))
  writeLines(my.server.output,my.server[1])
}

#' Write UI Info to TEMP
#' 
#' Create UI code to be ran by the program.
#' @param inputType Type, i.e. "radio","select","check"
#' @param mylabels labels
#' @param myvars Variable to be analyzed
#' @param order List Number
#' @param inputVariable name of column ID variable
#' @param desclabel Text seen next to UI device
#' @param inlinetest Inline?
#' @return Directory to file created
#' @keywords Explore
#' @export
#' @examples 
#' writetotemp_ui()
#' 
writetotemp_ui = function(inputType,mylabels,myvars,order,inputVariable,desclabel,inlinetest)
{
  
  if(order[1]==2){
    order[1]=3
  }
  if(order[2]==2){
    order[2]=3
  }
  
  mytempfile=tempfile()
  
  mylabels2=mapply(c,mylabels, "=" ,myvars)
  
  beginning=switch(inputType,
                   "radio"="radioButtons(",
                   "select"="selectInput(",
                   "check"="checkboxGroupInput(")
  
  j=1      
  writeLines(c(capture.output(cat(beginning,
                                  "\"",inputVariable,"\",",
                                  "\"",desclabel,"\",",
                                  "c(",sep="")),
               capture.output(while(j < dim(mylabels2)[2]){
                 cat("\"",mylabels2[order[1],j],"\"",mylabels2[2,j],"\"",mylabels2[order[2],j],"\"",",","\n",sep="")
                 j=j+1
               }),
               capture.output(cat("\"",mylabels2[order[1],j],"\"",mylabels2[2,j],"\"",mylabels2[order[2],j],"\"",")",sep="")),
               {if(inlinetest){capture.output(cat(",inline=T"))}},
               capture.output(cat(")"))
  ),
  
  mytempfile)
  
  return(mytempfile)
}


#' Write SERVER Info to TEMP
#' 
#' Create SERVER code to be ran by the program.
#' @param mylabels labels
#' @param myvars Variable to be analyzed
#' @param order List Number
#' @param inputVariable name of column ID variable
#' @param desclabel Text seen next to UI device
#' @return Directory to file created
#' @keywords Explore
#' @export
#' @examples 
#' writetotemp_server(mylabels,myvars,c(2,2),"varreact","input$variable","myDF")
#' 
writetotemp_server = function(mylabels,myvars,order,varreact,inputVariable,myDF)
{
  
  if(order[1]==2){
    order[1]=3
  }
  if(order[2]==2){
    order[2]=3
  }
  
  mytempfile=tempfile()
  
  mylabels2=mapply(c,mylabels, "=" ,myvars)
  
  # scaty<-reactive({
  #   with(myDF, switch(input$scat1,
  #                         "eduyr"=eduyr,
  #                         "dbp58"=dbp58,
  #                         "chol58"=chol58,
  #                         "ht58"=ht58,
  #                         "wt58"=wt58))
  # })
  
  j=1      
  writeLines(c(capture.output(cat(varreact,
                                  "=reactive({with(",
                                  myDF,
                                  ", switch(",
                                  inputVariable,
                                  ",", sep="")),
               capture.output(while(j < dim(mylabels2)[2]){
                 cat("\"",mylabels2[order[2],j],"\"",mylabels2[2,j],mylabels2[order[2],j],",","\n",sep="")
                 j=j+1
               }),
               capture.output(cat("\"",mylabels2[order[2],j],"\"",mylabels2[2,j],mylabels2[order[2],j],"))})",sep=""))
  ),
  
  mytempfile)
  
  return(mytempfile)
}
