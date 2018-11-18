# Define UI for application that draws a histogram
dashboardPage(
  skin = "green",
  title = "Future Essence : an R toolbox to find, download and preprocess forest data",

  dashboardHeader(title = "Forestys"),
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
      menuItem(i18n$t("Calc zone"), tabName = "tab_steps", icon = icon("image"))
    )),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      column(
        width=6,
        # Buttons to load the extent with modal dialogs
        strong(i18n$t("Specify the extent:\u2000")),
        span(
          div(style="padding-top:5px;padding-bottom:10px;",
              actionButton(
                "button_extent_bbox",
                label = i18n$t("\u2000Specify a bounding box"),
                width = 200,
                icon=icon("object-group")
              ),
              actionButton(
                "button_extent_vectfile",
                label =  i18n$t("\u2000Load a vector file"),
                width = 200,
                icon=icon("upload")
              ),
              actionButton(
                "button_extent_draw",
                label = i18n$t("\u2000Draw it on the map"),
                width = 200,
                icon=icon("paint-brush")
              ),
              actionButton(
                "button_refresh_map",
                label = i18n$t("\u2000Reload the extent on map"),
                width = 200,
                icon=icon("retweet")
              )) # end of div
        ) # end of span
      ) # end of column
    ), # end of fluidrow
    fluidRow(
      box(
        title = i18n$t("Map"),
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        # Map
        leafletOutput("view_map", height=600, width="100%")
      )
    )
  )
)
