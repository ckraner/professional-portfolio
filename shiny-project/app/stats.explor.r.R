### SERVER BACKGROUND FOR DATA ANALYSIS FOR HLM PROGRAM ####
#################### BY CHRIS KRANER #######################
############## NORTHERN ILLINOIS UNIVERSITY ################
######################### 9/2017 ###########################
############################################################

#' MLM And Data Screen Explorer
#'
#' Just do it.It's Shiny.
#'
#' @param myDF Dataframe. Only needed piece if used label.explor.r
#' @param omit Optional. How to omit data? Default is pairwise. Currently supported is listwise
#' @param df.drop Optional. Should results of models be dropped into the global data frame (i.e. to use outside of stats.explor.r)
#' @param mylabels Optional. labels for variables
#' @param myvars Optional. Variable to be analyzed
#' @param myfactors Optional. Factors
#' @param myfactorlabels Optional. Factor labels
#' @param colid Optional. name of column ID variable
#' @param dashboard Optional. Use ShinyDashboard?
#' @return Data frame used. May have added columns, such as residuals or Mahalinobis distances
#' @keywords Explore
#' @export
#' @examples
#' #### Use in combination with label.explor.r
#' stats.explor.r()

stats.explor.r=function(myDF,omit="pairwise",colid=mycolid,myvars=foundvars,mylabels=foundlabels,myfactors=foundfactors,myfactorlabels=foundfactorlabels,myordered=foundordered,myorderedlabels=foundorderedlabels,df.drop=F, dashboard=T, dashTitle="stats.explor.R",thelayout="default",the.display="normal",the.browser=getOption("shiny.launch.browser", interactive()),too.large=F)
{
  library(shiny)
  library(shinydashboard)
  library(shinyjs)

  #### Try and find arguments ####
  foundlabels=attr(myDF,"selected.labels")
  foundvars=attr(myDF,"selected")
  foundfactors=attr(myDF,"factors")
  foundfactorlabels=attr(myDF,"factors.labels")
  foundordered=attr(myDF,"ordered")
  foundorderedlabels=attr(myDF,"ordered.labels")
  mycolid=attr(myDF,"caseid")


  #### SET PATHS ####
  files.path=file.path(.libPaths(),"stats.explor.r.files",thelayout)
  base.path=file.path(files.path,"base")
  live.path=file.path(files.path,"live")

  #### Add www path
  addResourcePath('www',directoryPath = file.path(files.path,"www/")[1])

  #### LOAD REQUIRED PACKAGES ####
  source(file.path(live.path,"packages.R")[1],local=T)[1]


  #### Data Frame Maintenance ####
  source(file.path(live.path,"databases.R")[1],local=T)[1]


  #### Initializations ####
  source(file.path(live.path,"init.R")[1],local=T)[1]


  #### MAKE ALL THE SHINY STUFF ####
  source(file.path(live.path,"react.R")[1],local=T)[1]




  #### Parse ui and server files ####
  #### The only reason for this is to allow www folder so can
  #### use iframes.
  if(dashboard==T){
    #### Make header for Dashboard ####
    myheader=readLines(file.path(live.path,"dashHead.R")[1])
    myheader2=eval(parse(text=myheader))
    #system("R -e \"shiny::runApp(list(ui=dashboardPage(myheader2,dashboardSidebar(source(file.path(live.path,\"dashSide.R\")[1],local=T)[1]),dashboardBody(useShinyjs(),source(file.path(live.path,\"navPage.R\")[1],local=T)[1],hr(),hr(),fluidRow(column(12,p(\"Interface created by Chris Kraner.\",align=\"center\"))),fluidRow(column(12,p(\"This program uses many others' R packages. Please see Credits for a list.\",align=\"center\"))))),server = function(input, output, session) {source(file.path(live.path,\"myserver.R\")[1],local=T)[1];session$onSessionEnded(function() {stopApp(reducedDF)})}),launch.browser = the.browser, display.mode = the.display)\"")
    post.shiny.df=runApp(list(ui=dashboardPage(myheader2,
                                               dashboardSidebar(
                                                 source(file.path(live.path,"dashSide.R")[1],local=T)[1]),
                                               dashboardBody(useShinyjs(),
                                                             tags$head(tags$link(rel="stylesheet",type="text/css",href="www/custom.css"),
                                                                       tags$script(src="www/custom.js")),
                                                             source(file.path(live.path,"navPage.R")[1],local=T)[1],
                                                             hr(),
                                                             hr(),
                                                             fluidRow(column(12,p("Interface created by Chris Kraner.",align="center"))),
                                                             fluidRow(column(12,p("This program uses many others' R packages. Please see Credits for a list.",align="center"))))
    ),
    server = function(input, output, session) {
      source(file.path(live.path,"myserver.R")[1],local=T)[1]

      session$onSessionEnded(function() {
        stopApp(reducedDF)
      })
    }),launch.browser = the.browser, display.mode = the.display)

  }else{

    post.shiny.df=runApp(list(ui=shinyUI(
      fluidPage(
        source(file.path(live.path,"navPage.R")[1],local=T)[1],
        hr(),
        hr(),
        fluidRow(column(12,p("Interface created by Chris Kraner.",align="center"))),
        fluidRow(column(12,p("This program uses many others' R packages. Please see Credits for a list.",align="center"))))
    ),

    server = function(input, output, session) {
      source(file.path(live.path,"myserver.R")[1],local=T)[1]

      session$onSessionEnded(function() {
        stopApp(reducedDF)
      })
    }))
  }

  #### TO RUN APP ####
  #shiny::shinyApp(ui,server)
  #runApp(this.function)
  #return(this.function)

  source(file.path(live.path,"post.R")[1],local=T)[1]

  return(post.shiny.df)
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

#' Layout List Helper
#'
#' Display list of available layouts for Stats.Explor.R
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
explor.r.layouts=function(){
  files.path=file.path(.libPaths(),"stats.explor.r.files")
  my.layouts=list.dirs(path=files.path,full.names=F,recursive = F)


  my.layouts=my.layouts[-1]
  my.layouts
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

#' Basic Data Screening Explorer
#'
#' This function allows you to quickly ex[;pre a variable in your data set.
#' @param mydata Dataframe
#' @param myVar Variable to be analyzed
#' @param mynumba List Number for myVar
#' @param colID name of column ID variable
#' @param mynumbacase List number for colID
#' @return list containing results of psych describe as well as histogram with normal, QQ Plot, Box Plot, and all 3 in one graph
#' @keywords Explore
#' @export
#' @examples
#' zz.statexplorer()

zz.statexplorer=function(mydata,myVar,mynumba,colID,mynumbacase){

  myVar=as.matrix(myVar)
  #### Calculate stats from psych::describe ####

  myStat = with(mydata, psych::describe(myVar))
  #print(myStat)




  #### histogram and QQ plot ####

  {
    layout(matrix(c(1),1,1,byrow=T))
    {
      with( mydata, hist(myVar, prob=TRUE)) ; with(mydata, rug(myVar))
      curve(dnorm(x,mean=myStat$mean,sd=myStat$sd),col="darkblue",add=TRUE)
      myGraph=grid::grid.grab()
      myGraph
      myGraph=recordPlot()

      with(mydata, qqnorm(myVar))
      with(mydata, qqline (myVar, col=2))
      qqplot1=grid::grid.grab()
      qqplot1
      qqplot1=recordPlot()
    }
  }



  #### multi plot with simple box plot ####
  layout(matrix(c(1,1,2,3),2,2,byrow=T))
  {
    with(mydata, hist(myVar, prob=TRUE)) ; with(mydata, rug(myVar))
    curve(dnorm(x,mean=myStat$mean,sd=myStat$sd),col="darkblue",add=TRUE)
    with(mydata, qqnorm(myVar))
    with(mydata, qqline (myVar, col=2))
    with(mydata, boxplot(myVar))
  }
  multigr=grid::grid.grab()
  multigr
  multigr=recordPlot()



  #### box plot with outliers named ####

  myBox=extremeboxplot(mydata,mynumba,mynumbacase)



  mylist=list(myStat,myGraph,qqplot1,myBox,multigr)
  return(mylist)

}







#' Box Plot with outliers
#'
#' Box plot with outliers
#' @param mydata Dataframe
#' @param mynumba List Number for myVar
#' @param mynumbacase List number for colID
#' @return list containing results of psych describe as well as histogram with normal, QQ Plot, Box Plot, and all 3 in one graph
#' @keywords Explore
#' @export
#' @examples
#' extremeboxplot()

extremeboxplot=function(mydata,mynumba,mynumbacase){

  #### Does not play well with other plots...
  #### Make each ggplot in seperate function?
  ####
  ####


  library(ggplot2)
  library(dplyr)

  #### Remove NA values for selected variable and then create ####

  mydf=mydata[mynumbacase]
  mydf$var=mydata[[mynumba]]
  mydf=mydf[complete.cases(mydf$var),]

  #### Copy value IF outlier, else NA and add to df ####

  df=mydf %>%
    mutate(outlier = ifelse(is_myoutlier(mydf$var), mydf$var, as.numeric(NA)))

  #### Make boxplot ####
  #### NOTE: Need to do it this way and print(mybox) it.

  mybox=ggplot(data=df, aes(x = 1, y = df$var)) +
    geom_boxplot() +
    geom_text(aes(label = df$outlier), na.rm = TRUE, hjust = -0.3)
  print(mybox)


  return(mybox)
}


#' Mahalinobis Distances
#'
#' This function calculates the Mahalinobis distances and creates density plots
#'
#'
#' @param myData Dataset
#'
#'
#' @return Distances as list, base plots
#'
#' @keywords Mahali
#' @export
#' @examples
#' mahali()

mahali=function(myData){

  #### Function credit R-manual example ####
  ####
  ####
  #### A point that has a greater Mahalanobis distance from the rest of the sample population of points
  #### is said to have higher leverage since it has a greater influence on the slope or coefficients of
  #### the regression equation.


  #### Covariance matrix and mahalanobis distance ####


  Sx=cov(myData,use="complete.obs")
  D2=mahalanobis(myData,colMeans(myData, na.rm=T),Sx)



  #### Density plot and QQ plot ####

  D2red=D2[!is.na(D2)]
  par(mfrow=c(1,2))
  plot(density(D2red, bw = 0.5),
       main="Squared Mahalanobis distances, \n n=100, p=3") ; rug(D2)
  qqplot(qchisq(ppoints(100), df = 3), D2red,
         main = expression("Q-Q plot of Mahalanobis \n" * ~D^2 *
                             " vs. quantiles of" * ~ chi[3]^2))
  abline(0, 1, col = 'gray')


  return(D2)
}

#' Outlier Test
#'
#' Blah
#' @param x X
#' @return logical
#' @keywords Blah
#' @export
#' @examples
#' is_myoutlier()
#'
is_myoutlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}


