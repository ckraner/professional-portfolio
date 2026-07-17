################### DASHBOARD SIDEBAR ######################
#################### BY CHRIS KRANER #######################
############## NORTHERN ILLINOIS UNIVERSITY ################
######################## 11/2017 ###########################
######################### V: 1.0 ###########################
############################################################

sidebarMenu(id="sidebarmenu",
  menuItem("Explore",startExpanded=T,
           menuSubItem("Univariate",tabName = "Explore"),
           menuSubItem("Multivariate",tabName = "ExploreMore"),
           menuSubItem("ScatterPlots",tabName = "scatter")),
  menuItem("Regression",startExpanded = T,
           menuSubItem("Linear Model",tabName = "MLM"),
           menuSubItem("AN(C)OVA",tabName = "ANCOVA"),
           menuSubItem("GLM",tabName="GLM")),
  menuItem("Multivariate",startExpanded=T,
           menuSubItem("MAN(C)OVA",tabName = "MANOVA")),
  menuItem("Advanced",tabName = "advanced"),
  menuItem("Credits",tabName = "credits")
)

#### LEAVE THIS EXTRA LINE BELOW! ####
