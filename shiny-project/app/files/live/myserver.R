source(file.path(base.path,"Explorer","explorer.server.R")[1],local=T)[1];
source(file.path(base.path,"Explorer","mahali.server.R")[1],local=T)[1];
source(file.path(base.path,"Scatter","scatter.server.R")[1],local=T)[1]; 
source(file.path(base.path,"MLM","MLM.server.R")[1],local=T)[1];
source(file.path(base.path,"ANCOVA","ANCOVA.server.R")[1],local=T)[1];
source(file.path(base.path,"MANOVA","MANOVA.server.R")[1],local=T)[1];
source(file.path(base.path,"credits.R")[1],local=T)[1];
source(file.path(base.path,"GLM","GLM.server.R")[1],local=T);



#### Save Button ####
saveModal = function(field=F){
  save.path=file.path(base.path,input$sidebarmenu,"save.R")[1]
  if(file.exists(save.path)){
    source(save.path,local=T)[1];
  }else{
  modalDialog(p("This page does not support save....yet!"),p(print(input$sidebarmenu)),
              footer = tagList(modalButton("Cancel"),actionButton("modalSave","Save")))
  }
}

observeEvent(input$save,{
  showModal(saveModal())
})



#### Table Button ####
observeEvent(input$table,{
  showModal(tableModal())
})

tableModal=function(field=F){
  modalDialog(DT::dataTableOutput("theTable",height="550px"),size="l",easyClose = T)
}

output$theTable=DT::renderDataTable({
    if(input$table){
    reducedDF
    }
  },fillContainer=T)

#### Command Button ####

observeEvent(input$run.that,{
  showModal(runModal())
})

runModal=function(field=F){
  modalDialog(fluidRow(column(8,textInput("run.text","Command to run")),
              column(4,div(class="run-button",actionButton("run.go","Run")))),
              fluidRow(width=12,verbatimTextOutput("run.result")),
    easyClose = T)
}

observeEvent(input$run.go,{
  print("ok")
  my.cmd<<-isolate(input$run.text)
  
})

output$run.result=renderPrint({
  if(input$run.go){
  if(length(my.cmd)>0){
  my.result=eval(parse(text=my.cmd))
  my.result
  # cat(my.result)
  }}
})

#### LEAVE THIS EXTRA LINE BELOW! ####
