box(
  title = "Options", solidHeader = TRUE,
  collapsible =T,width=NULL,collapsed = T,
  checkboxGroupInput("mlmOptions","Show:",choices = c("Durbin-Watson","Shapiro-Wilk")),
  checkboxInput("mlm.VIF","Variable Inflation Factor (VIF)"),
  checkboxInput("mlm.etasq","Eta Squared")
)

#### LEAVE THIS EXTRA LINE BELOW! ####
