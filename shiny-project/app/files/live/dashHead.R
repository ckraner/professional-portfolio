tags$header(class = "main-header",
            tags$title("stats.Explor.R"),
            span(class = "logo", "stats.explor.R"),
tags$nav(class = "navbar navbar-static-top", role = "navigation",
# Embed hidden icon so that we get the font-awesome dependency
span(shiny::icon("bars"), style = "display:none;"),
# Sidebar toggle button
a(href="#", class="sidebar-toggle", `data-toggle`="offcanvas",
role="button",
span(class="sr-only", "Toggle navigation")
),
div(class ="navbar-custom-menu",
tags$ul(class = "nav navbar-nav",
span(actionButton("run.that","Command",class="sidebar-toggle2",icon=icon("terminal")),
actionButton("table","Table",class="sidebar-toggle2",icon=icon("table")),
actionButton("save"," Save",class="sidebar-toggle2",icon=icon("floppy-o")))
)
)
)
)

#### LEAVE THIS EXTRA LINE BELOW! ####
