shinyjs::hidden(div(id="mlm.dw",
box(
  title = "Durbin Watson", solidHeader = TRUE,
  collapsible = TRUE,width=NULL,collapsed=T,
  htmlOutput("DWtest")
)
))

#### LEAVE THIS EXTRA LINE BELOW! ####
