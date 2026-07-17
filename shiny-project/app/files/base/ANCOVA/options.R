box(
  title = "Options", solidHeader = TRUE,
  collapsible =T,width=NULL,collapsed = T,
  checkboxGroupInput("ANCOVAOptions","Show:",choices = c("Durbin-Watson","Levene's Test"))
)

#### LEAVE THIS EXTRA LINE BELOW! ####
