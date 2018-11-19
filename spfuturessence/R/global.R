# add libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny,shinythemes,shinydashboard,shinyjs,shinyWidgets,leaflet,ggvis,ggrepel,dplyr,RColorBrewer,raster,gstat,rgdal,Cairo,ggmap,ggplot2,
               reticulate,tools,leaflet.extras,pool,RPostgreSQL,devtools,mapedit,shiny.i18n)
pacman::p_load_gh('hadley/tidyverse','tidyverse/ggplot2','tidyverse/dplyr','r-spatial/sf','jrowen/rhandsontable','forestys/futuressence')

# internationalize
i18n <- Translator$new(translation_json_path = system.file("translations/translation.json", package="spfuturessence"))
i18n$set_translation_language("fr")


