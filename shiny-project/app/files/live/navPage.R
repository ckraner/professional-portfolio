tabItems(tabItem(tabName = "Explore",
                   fluidRow(span(width=12,
                                 source(file.path(base.path,"Explorer","variable_selector.R")[1],local=T)[1])),
                   fluidRow(column(width=5,
                                   source(file.path(base.path,"Explorer","psych.R")[1],local=T)[1]
                                   #source(file.path(base.path,"Explorer","shapiro.R")[1],local=T)[1]
                                   ),
                            column(width=7,
                                   source(file.path(base.path,"Explorer","plots.R")[1],local=T)[1]
                                   )
                            )
           ),

         tabItem(tabName="ExploreMore",
                 #source(file.path(base.path,"Explorer","mcar.R")[1],local=T)[1],
                 source(file.path(base.path,"Explorer","bigpsych.R")[1],local=T)[1],
                 source(file.path(base.path,"Explorer","pearson.R")[1],local=T)[1],
                 source(file.path(base.path,"Explorer","mahali.R")[1],local=T)[1]
                 ),



           tabItem(tabName = "MLM",
                    titlePanel("Linear Model"),
                   fluidRow(span(width=12,
                                 source(file.path(base.path,"MLM","formula.R")[1],local=T)[1])),
                   fluidRow(                            column(width=4,
                                                               source(file.path(base.path,"MLM","options.R")[1],local=T)[1],
                                                               source(file.path(base.path,"MLM","dw.R")[1],local=T)[1]),
                                                        column(width=8,
                                   jqui_sortabled(div(
                                     source(file.path(base.path,"MLM","anova.R")[1],local=T)[1],
                                   source(file.path(base.path,"MLM","table.R")[1],local=T)[1]))))
           ),
           tabItem(tabName = "ANCOVA",
                    titlePanel("ANOVA, ANCOVA"),
                   if(length(myfactors)>0){fluidRow(span(width=12,
                                                         source(file.path(base.path,"ANCOVA","formula.R")[1],local=T)[1]))}else{p("You have no variables in your data frame.")},
                   if(length(myfactors)>0){fluidRow(                            column(width=4,
                                                               source(file.path(base.path,"ANCOVA","options.R")[1],local=T)[1],
                                                               source(file.path(base.path,"ANCOVA","dw.R")[1],local=T)[1],
                                                               source(file.path(base.path,"ANCOVA","levene.R")[1],local=T)[1]
                                                               ),
                                                        column(width=8,
                                                               jqui_sortabled(div(
                                                                 source(file.path(base.path,"ANCOVA","anova.R")[1],local=T)[1],
                                                               source(file.path(base.path,"ANCOVA","table.R")[1],local=T)[1])))
           )}),

           tabItem(tabName = "MANOVA",
                    titlePanel("MANOVA, MANCOVA"),
                   if(length(myfactors)>0){fluidRow(span(width=12,
                                                         source(file.path(base.path,"MANOVA","factor.R")[1],local=T)[1]))}else{p("You have no variables in your data frame.")},
                   if(length(myfactors)>0){fluidRow(span(width=12,
                                                         source(file.path(base.path,"MANOVA","table.R")[1],local=T)[1]))},
                   if(length(myfactors)>0){fluidRow(span(width=12),
                                                    source(file.path(base.path,"MANOVA","phia.R")[1],local=T)[1])}
           ),

         tabItem(tabName="GLM",
                 titlePanel("Binary and Ordinal Logistic Regression"),
                 if(length(myfactors)>0){fluidRow(span(width=12,
                                                       source(file.path(base.path,"GLM","glm_formula.R")[1],local=T)[1]))}else{p("You have no variables in your data frame.")},
                 if(length(myfactors)>0){fluidRow(span(width=12,
                                                       source(file.path(base.path,"GLM","table.R")[1],local=T)[1]))}),

           tabItem(tabName = "scatter",
                    titlePanel("Scatter Plots for Variables"),
                    source(file.path(base.path,"Scatter","variables.R")[1],local=T)[1],
                    source(file.path(base.path,"Scatter","scatter.R")[1],local=T)[1]
           ),

           tabItem(tabName = "advanced",
                    h2("Table"),
                    div(align="right",actionButton("table_update","Update")),
                   DT::dataTableOutput("mahaliTable",height="500px")
           ),

           tabItem(tabName = "credits",
                    h2("Thanks to..."),
                    p("Coming Soon..."),
                    h2("Packages used"),
                    div(align="left",uiOutput("credits"))
           )
)

#### LEAVE THIS EXTRA LINE BELOW! ####
