modalDialog(
  p("These items will be saved to your environment upon closing the application:"),
  fluidRow(div(column(width=2,div(class="save-check",checkboxInput("MLMmodel","Model"))),
           column(width=10,textInput("MLMmodelName",label ="",value="my.lm")))),
  
            footer = tagList(modalButton("Dismiss"),actionButton("MLMmodalSave","Save")),easyClose = T)

#### LEAVE THIS EXTRA LINE BELOW! ####
