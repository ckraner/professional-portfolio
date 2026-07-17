shinyjs::hidden(div(id="ANCOVAlevene",
                    box(title = "Levene's Test",solidHeader = T,
                        collapsible = T,collapsed = T,width=NULL,
                        htmlOutput("anova.levene")
                    )
))

#### LEAVE THIS EXTRA LINE BELOW! ####
