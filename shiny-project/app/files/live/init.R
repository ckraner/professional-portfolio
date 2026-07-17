###################### PANEL INITS #########################
#################### BY CHRIS KRANER #######################
############## NORTHERN ILLINOIS UNIVERSITY ################
######################### 9/2017 ###########################
######################### V: 1.0 ###########################
############################################################


my.cmd=NULL

my.credits=file(file.path(live.path,"packages.R")[1])
credits=readLines(my.credits)
close(my.credits)
#### Should models go to the global environment? ####
if(df.drop==F){
  
  my.ancova.model=NULL
  my.manova=NULL
}


#### INIT LM ####
independencesquig="~"
plusand="+"
interactand="*"
my.reduced.vars=myvars
my.reduced.labels=mylabels
#### Explorer tab ####
that.cor=file(file.path(files.path,"www","pearson.html")[1])

#### Residuals ####
myothervars=c("lm_resid","lm_pred","anova_resid","anova_pred")
myotherlabels=c("LM Residuals","LM Predicted Values","AN(C)OVA Residuals","AN(C)OVA Predicted Values")
regressionlabels=c(mylabels,myotherlabels)
regressionvars=c(myvars,myothervars)
residual.flag=0

#### GLM stuff ####
if(!is.null(myfactors)){
  if(!is.null(myordered)){
    my.log.vars=c(myfactors,myordered)
    my.log.labels=c(myfactorlabels,myorderedlabels)
  }else{
    my.log.vars=myfactors
    my.log.labels=myfactorlabels
  }
}

#### LEAVE THIS EXTRA LINE BELOW! ####
