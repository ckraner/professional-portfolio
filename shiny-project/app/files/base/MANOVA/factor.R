box(
  title = "Formula", solidHeader = TRUE,
  collapsible =F,width=NULL,
  shiny::textInput("manova.formula","Formula"),
  if(length(myfactors)>0){
    fluidRow(width=12,box(title="Dependent Variables",solidHeader = T,collapsible = T,width=NULL,
                          tags$div(align="left",class="multicol3",
                                   source(mancova.dv.check,local=T)[1])))},
  if(length(myfactors)>0){
    fluidRow(width=12,box(title="Factors",solidHeader = T,collapsible = T,width=NULL,
                          tags$div(align="left",class="multicol3",
                                   source(mancova.factor.check,local=T)[1])))},
  if(length(myfactors)>0){
    fluidRow(width=12,box(title="Covariates",solidHeader = T,collapsible = T,width=NULL,
                          tags$div(align="left",class="multicol3",
                                   source(mancova.cov.check,local=T)[1]))
    )},
  div(id='goright2', class="simpleDiv",align="right",
      actionButton("enter_input_manova", "Enter"))
)

#### LEAVE THIS EXTRA LINE BELOW! ####
