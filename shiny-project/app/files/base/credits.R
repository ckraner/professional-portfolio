#################### CREDITS SERVER ########################
#################### BY CHRIS KRANER #######################
############## NORTHERN ILLINOIS UNIVERSITY ################
######################### 9/2017 ###########################
######################### V: 1.0 ###########################
############################################################


output$credits=renderUI({
  # files.path=file.path(.libPaths(),"stats.explor.r.files")
  # live.path=file.path(files.path,"live")
  
  # mywidth=getOption("width")
  # options(width=5000)
  
  
  #credits=capture.output(print(source(file.path(live.path,"packages.R")[1],local=T,echo=T)[1])[1])
  
  # xxxxx=length(credits)
  # yyyyy=xxxxx-5
  # yy22=yyyyy
  # while(yy22<{xxxxx+1}){
  #   credits=credits[-yyyyy]
  #   yy22=yy22+1
  # }

  credits=as.data.frame(credits)

 credits=lapply(credits[[1]],stringr::str_split, pattern=">")
 credits=as.data.frame(credits)
 credits=tidyr::gather(credits,key="key",value="value")


 options(pixie_interactive = F,pixie_na_string="",pixiedust_print_method="html")
  my.dust.credits=pixiedust::dust(credits$value)%>%
    sprinkle_colnames("")


  my.dust.print.credits=print(my.dust.credits,quote=F)[1]

  #options(width=mywidth)

  return(HTML(my.dust.print.credits))
  #return(credits)
  })

#### LEAVE THIS EXTRA LINE BELOW! ####
