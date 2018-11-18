# Define UI for application that draws a histogram
dashboardPage(
  skin = "green",
  title = "theia2r: an R toolbox to find, download and preprocess Sentinel-2 data",
  
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(
    # logo
    div(
      style = "text-align:center;padding-top:17px;padding-bottom:30px;",
      a(
        href='https://forestys.github.io/futuressence',
        target = "_blank",
        uiOutput("img_logo")
      )
    ),
    
    sidebarMenu(
      menuItem("Product selection", tabName = "tab_steps", icon = icon("image"))
    )),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(plotOutput("plot1", height = 250)),
      
      box(
        title = "Controls",
        sliderInput("slider", "Number of observations:", 1, 100, 50)
      )
    )
  )
)