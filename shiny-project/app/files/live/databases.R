####################### DATABASES ##########################
#################### BY CHRIS KRANER #######################
############## NORTHERN ILLINOIS UNIVERSITY ################
######################### 9/2017 ###########################
######################### V: 1.0 ###########################
############################################################


allvars=c(colid,myvars,myfactors,myordered)

reducedDF=myDF[allvars]

#### Omit options ####
if(omit=="listwise"){
  reducedDF=reducedDF[complete.cases(reducedDF)]
}

#### Make row names into column ####
reducedDF=tibble::rownames_to_column(reducedDF,"rows")
reducedDF$rows=as.numeric(reducedDF$rows)


#### Other Databases ####
reducedDF2=reducedDF[myvars]
na.omit.DF=na.omit(reducedDF2)


#### Silly UFO Plot from Selbosh gitHub page
# cannon <- data.frame(x = 0, y = 0, colour = 'white', size = 20)
# bunkers <- data.frame(x = seq(-4, 4, l = 4), y = 2, colour = 'green', size = 1)
# ufos <- data.frame(x = rep(seq(-6, 6, length.out = 12), 5),
#                    y = rep(6:10, each = 12), size = 10,
#                    colour = c('cyan', 'yellow', 'magenta')[
#                      c(rep(1:3,4), rep(c(2,3,1),4), rep(c(3,1,2),4), rep(1:3,4), rep(c(2,3,1),4))
#                      ],
#                    brow = rnorm(60))

#### LEAVE THIS EXTRA LINE BELOW! ####
