

box(
  title = "Formula", solidHeader = TRUE,
  collapsible =F,width=NULL,
  shiny::textInput("anova.formula","Formula"),
  source(ancova.dv,local=T)[1],
  if(length(myfactors)>0){
  fluidRow(width=12,box(title="Factors",solidHeader = T,collapsible = T,width=NULL,
                        tags$div(align="left",class="multicol32",
                                 source(ancova.factor,local=T)[1])),
    box(title="Covariates",solidHeader=T,collapsible=T,width=NULL,tags$div(align="left",class="multicol32",
                                                                           source(ancova.cov,local=T)[1]))
  )},
  div(id='goright2', class="simpleDiv",align="right",
      actionButton("enter_input2", "Enter"))
)

#### LEAVE THIS EXTRA LINE BELOW! ####
