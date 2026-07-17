box(
  title = "Formula", solidHeader = TRUE,
  collapsible =F,width=NULL,
  shiny::textInput("glm.formula","Formula"),
  source(glm.dv,local=T)[1],
  if(length(myfactors)>0){
    fluidRow(width=12,box(title="Factors",solidHeader = T,collapsible = T,width=NULL,
                          tags$div(align="left",class="multicol32",
                                   source(glm.factor,local=T)[1])),
             box(title="Covariates",solidHeader=T,collapsible=T,width=NULL,tags$div(align="left",class="multicol32",
                                                                                    source(glm.cov,local=T)[1]))
    )},
  div(id='goright2', class="simpleDiv",align="right",
      actionButton("glm_input", "Enter"))
)

#### LEAVE THIS EXTRA LINE BELOW! ####
