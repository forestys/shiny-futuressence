## server
function(input, output, session) {

  # link to www directory and objects
  addResourcePath("www", system.file("www", package="spfuturessence"))
  output$img_logo<-renderUI(
    img(src='www/img/logo_forestys.png',height='100',width='200')
  )
  # initialise rv
  rv <- reactiveValues()

  # initialise buffer
  buffer <- 10

  #-- Function to update the map and the list of tiles --#
  # it returns TRUE if the input extent source was correctly read, FALSE elsewhere.
  # argument extent_source determines which source to be used:
  # "bbox", "vectfile", "draw" from selection buttons, "imported" from parameter;
  # in this case, the argument "custom_source" is the source to be passed.
  update_extent <- function(extent_source, custom_source=NA) {

    # 1. Define rv$extent
    if (extent_source == "bbox") {
      # Bbox mode #
      # check that the polygon is valid
      if (attr(rv$bbox_polygon, "valid")) {
        rv$extent <- rv$bbox_polygon
        attr(rv$extent, "new") <- TRUE
      } else {
        return(FALSE)
      }

    } else if (extent_source == "vectfile") {
      # Vectfile mode #
      # check that the polygon is valid
      if (attr(rv$vectfile_polygon, "valid")) {
        rv$extent <- rv$vectfile_polygon
        attr(rv$extent, "new") <- TRUE
      } else {
        return(FALSE)
      }

    } else if (extent_source == "draw") {
      # Drawn mode #
      # namespace for extent selection
      sel_drawn <- if (!is.null(rv$extent_edits()$finished)) {
        x <- rv$extent_edits()$finished
        attr(x, "valid") <- TRUE
        attr(x, "new") <- TRUE
        x
      } else {
        x <- st_polygon(); attr(x, "valid") <- FALSE; x
      }

      if (!attr(sel_drawn, "valid")) {
        return(FALSE)
      }

      rv$extent <- sel_drawn

    } else if (extent_source == "imported") {
      # Imported from parameters #
      sel_imported_extent <- if (is.null(custom_source) | anyNA(custom_source)) {
        x <- st_polygon(); attr(x, "valid") <- FALSE; x
      } else {
        x <- st_read(custom_source, quiet=TRUE) %>%
          st_transform(4326)
        attr(x, "valid") <- TRUE
        attr(x, "new") <- TRUE
        x
      }

      if (!attr(sel_imported_extent, "valid")) {
        return(FALSE)
      }

      rv$extent <- sel_imported_extent

    } else {

      # For any other value of extent_source, use the existing rv$extent

      if (is.null(rv$extent)) {
        return(FALSE)
      } else if (!attr(rv$extent, "valid")) {
        return(FALSE)
      } else {
        attr(rv$extent, "new") <- FALSE
      }

    }


    # 2. Update the list of overlapping tiles and the tiles on the map
    if(length(rv$extent) > 0) {

      # buffer
      rv$extent <- rv$extent %>% st_transform(crs = 2154) %>% st_buffer(buffer)

      # reset and update the map
      react_map(base_map())
      rv$extent_ll <- st_transform(rv$extent, 4326)
      leafletProxy("view_map") %>%
        clearShapes() %>%
        fitBounds(
          lng1 = min(st_coordinates(rv$extent_ll)[,"X"]),
          lat1 = min(st_coordinates(rv$extent_ll)[,"Y"]),
          lng2 = max(st_coordinates(rv$extent_ll)[,"X"]),
          lat2 = max(st_coordinates(rv$extent_ll)[,"Y"])
        ) %>%
        # add extent
        addPolygons(data = rv$extent,
                    group = "Extent",
                    # label = ~tile_id,
                    # labelOptions = labelOptions(noHide = TRUE, direction = "auto"),
                    fill = TRUE,
                    fillColor = "green",
                    fillOpacity = .3,
                    stroke = TRUE,
                    weight = 3,
                    color = "darkgreen") #%>%
    }

    return(TRUE)

  }

  #-- Create the map (once) --#
  base_map <- function() {
    leaflet() %>%

      # add tiles
      addTiles(group = "OpenStreetMap") %>%
      # addTiles(paste0("https://{s}.tile.thunderforest.com/outdoors/{z}/{x}/{y}.png",
      #                 if (!is.na(thunderforest_api)) {paste0("?apikey=",thunderforest_api)}),
      #          group = "OpenStreetMap Outdoors") %>%
      # addProviderTiles(providers$OpenTopoMap, group = "OpenTopoMap") %>%
      addTiles("https://{s}.tile.opentopomap.org/{z}/{x}/{y}.png",
               group = "OpenTopoMap") %>%
      # addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>%
      addTiles("https://cartodb-basemaps-{s}.global.ssl.fastly.net/light_all/{z}/{x}/{y}.png",
               group = "CartoDB") %>%
      # addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addTiles("https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
               group = "Satellite") %>%
      # addProviderTiles(providers$CartoDB.PositronOnlyLabels, group = "Dark names") %>%
      addTiles("https://cartodb-basemaps-{s}.global.ssl.fastly.net/light_only_labels/{z}/{x}/{y}.png",
               group = "Light names") %>%
      # addProviderTiles(providers$CartoDB.DarkMatterOnlyLabels, group = "Dark names") %>%
      addTiles("https://cartodb-basemaps-{s}.global.ssl.fastly.net/dark_only_labels/{z}/{x}/{y}.png",
               group = "Dark names") %>%
      # addTiles(paste0("https://{s}.tile.thunderforest.com/spinal-map/{z}/{x}/{y}.png",
      #                 if (!is.na(thunderforest_api)) {paste0("?apikey=",thunderforest_api)}),
      #          group = "Metal or death") %>%

      # view and controls
      addLayersControl(
        baseGroups = c("OpenStreetMap", "OpenTopoMap", "CartoDB", "Satellite"),
        overlayGroups = c("Light names","Dark names","S2 tiles","Extent"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup(c("Light names","Dark names"))
  }

  # create a new map for principal view
  react_map <- reactiveVal({base_map()})
  output$view_map <- renderLeaflet({react_map()})

  #######- Bbox mode -#######

  # message for bboxproj
  output$bboxproj_message <- renderUI({
    bboxproj_validated <- tryCatch(
      st_crs2(input$bboxproj),
      error = function(e) {st_crs(NA)}
    )$proj4string
    if (input$bboxproj=="") {
      rv$bboxproj <- NA
      ""
    } else if (is.na(bboxproj_validated)) {
      rv$bboxproj <- NA
      span(style="color:red",
           "Insert a valid projection (EPSG code).")
    } else {
      rv$bboxproj <- bboxproj_validated
      # span(style="color:darkgreen", "\u2714") # check
      div(strong("Selected projection:"),
          br(),
          bboxproj_validated,
          style="color:darkgreen")
    }
  })

  # create a new map (to be shown in modal dialog)
  react_map_bbox <- reactiveVal(base_map())
  output$view_map_bbox <- renderLeaflet({react_map_bbox()})

  # Open modal dialog to edit bbox
  observeEvent(input$button_extent_bbox, {
    showModal(load_extent_bbox())
  })

  # update the map dinamically
  observeEvent(c(
    input$bbox_xmin, input$bbox_xmax,
    input$bbox_ymin, input$bbox_ymax,
    rv$bboxproj
  ), {

    # Check that the bounding box is valid
    if (!anyNA(c(input$bbox_xmin, input$bbox_xmax,
                 input$bbox_ymin, input$bbox_ymax)) &
        !(is.null(rv$bboxproj) || is.na(rv$bboxproj))) {
      if (input$bbox_xmin != input$bbox_xmax &
          input$bbox_ymin != input$bbox_ymax) {
        # create the polygon
        rv$bbox_polygon <- st_as_sfc(
          st_bbox(
            c("xmin" = input$bbox_xmin,
              "ymin" = input$bbox_ymin,
              "xmax" = input$bbox_xmax,
              "ymax" = input$bbox_ymax),
            crs = rv$bboxproj
          )
        ) %>% st_transform(4326)
        attr(rv$bbox_polygon, "valid") <- TRUE
      } else {
        rv$bbox_polygon <- st_polygon()
        attr(rv$bbox_polygon, "valid") <- FALSE
      }
    } else {
      rv$bbox_polygon <- st_polygon()
      attr(rv$bbox_polygon, "valid") <- FALSE
    }

    # if bbox is valid, update the map
    if (attr(rv$bbox_polygon, "valid")) {
      rv$bbox_ll <- st_bbox(st_transform(rv$bbox_polygon, 4326))
      leafletProxy("view_map_bbox") %>%
        clearShapes() %>%
        fitBounds(
          lng1 = as.numeric(rv$bbox_ll$xmin-(rv$bbox_ll$xmax-rv$bbox_ll$xmin)/3),
          lat1 = as.numeric(rv$bbox_ll$ymin-(rv$bbox_ll$ymax-rv$bbox_ll$ymin)/3),
          lng2 = as.numeric(rv$bbox_ll$xmax+(rv$bbox_ll$xmax-rv$bbox_ll$xmin)/3),
          lat2 = as.numeric(rv$bbox_ll$ymax+(rv$bbox_ll$ymax-rv$bbox_ll$ymin)/3)
        ) %>%
        addPolygons(data = rv$bbox_polygon,
                    group = "Extent",
                    # label = ~tile_id,
                    # labelOptions = labelOptions(noHide = TRUE, direction = "auto"),
                    fill = TRUE,
                    fillColor = "green",
                    fillOpacity = .3,
                    stroke = TRUE,
                    weight = 3,
                    color = "darkgreen") #%>%
    } else {
      # if bbox is not valid, reset the map
      react_map_bbox(base_map())
    }

  })

  # use bbox
  observeEvent(input$save_extent_bbox, {
    # Add a progress bar while update_extent is running
    withProgress(message = 'Creating the extent...', value = 0, {
      bbox_valid <- update_extent(extent_source="bbox")
      if (bbox_valid) {
        removeModal()
      } else {
        sendSweetAlert(
          session,
          title = "Invalid bounding box",
          text = paste(
            "Please insert a valid bounding box."
          ),
          type = "error",
          btn_labels = "Ok"
        )
      }
      # Fake progress
      for (i in 1:10) {incProgress(1/10); Sys.sleep(0.1)}
    })
  })

  #############- Vector file mode -###############""""

  # if
  observeEvent(input$path_vectfile_sel, {
    uploaded_exts <- gsub("^.+\\.(.+)$","\\1",input$path_vectfile_sel$name)
    # checks
    if (length(unique(gsub("\\..+$","",input$path_vectfile_sel$name))) > 1) {
      # if more than one vector were chosen, give an alert and do not use the file
      sendSweetAlert(
        session,
        title = "Invalid vector",
        text = paste(
          "Please select a single vector",
          "(multiple selection is allowed only for shapefiles)."
        ),
        type = "error",
        btn_labels = "Ok"
      )
      rv$vectfile_path <- ""
    } else if (length(uploaded_exts) == 1 && !uploaded_exts %in% c("shp","shx","dbf","prj")) {
      # if a single file was chosen and it is not a shapefile, use it
      rv$vectfile_path <- input$path_vectfile_sel$datapath
    } else if (anyNA(match(c("shp","shx","dbf","prj"),uploaded_exts))) {
      # if a shapefile was chosen but some files are missing, do not use it
      sendSweetAlert(
        session,
        title = "Incomplete shapefile",
        text = paste(
          "Please select all the files of the shapefile",
          "(at most .shp, .shx, .prj, .dbf)."
        ),
        type = "error",
        btn_labels = "Ok"
      )
      rv$vectfile_path <- ""
    } else {
      # if a shapefile was chosen and all the files are present,
      # rename the uploaded files in order to have the same filename and use them
      path_vectfile_sel_new_datapath <- file.path(
        dirname(input$path_vectfile_sel$datapath), input$path_vectfile_sel$name
      )
      for(i in seq_len(nrow(input$path_vectfile_sel))) {
        file.rename(input$path_vectfile_sel$datapath[i], path_vectfile_sel_new_datapath[i])
      }
      rv$vectfile_path <- path_vectfile_sel_new_datapath[
        input$path_vectfile_sel$type=="application/x-esri-shape"
        ]
    }
  })

  # create a new map (to be shown in modal dialog)
  react_map_vectfile <- reactiveVal(base_map())
  output$view_map_vectfile <- renderLeaflet({react_map_vectfile()})

  # Open modal dialog to load the vector file
  observeEvent(input$button_extent_vectfile, {
    rv$vectfile_path <- ""
    showModal(load_extent_vectfile())
  })

  # load the vector on the map
  observeEvent(rv$vectfile_path, {

    # Check that the vector is valid
    rv$vectfile_polygon <- tryCatch(
      {
        x <- st_read(rv$vectfile_path, quiet=TRUE) %>%
          st_transform(4326)
        attr(x, "valid") <- TRUE
        attr(x, "new") <- TRUE
        x
      },
      error = function(e) {x <- st_polygon(); attr(x, "valid") <- FALSE; x}
    )

    if(attr(rv$vectfile_polygon, "valid")) {
      # if the vector is valid, update the map
      rv$vectfile_polygon_ll <- st_transform(rv$vectfile_polygon, 4326)
      leafletProxy("view_map_vectfile") %>%
        clearShapes() %>%
        fitBounds(
          lng1 = min(st_coordinates(rv$vectfile_polygon_ll)[,"X"]),
          lat1 = min(st_coordinates(rv$vectfile_polygon_ll)[,"Y"]),
          lng2 = max(st_coordinates(rv$vectfile_polygon_ll)[,"X"]),
          lat2 = max(st_coordinates(rv$vectfile_polygon_ll)[,"Y"])
        ) %>%
        addPolygons(data = rv$vectfile_polygon,
                    group = "Extent",
                    # label = ~tile_id,
                    # labelOptions = labelOptions(noHide = TRUE, direction = "auto"),
                    fill = TRUE,
                    fillColor = "green",
                    fillOpacity = .3,
                    stroke = TRUE,
                    weight = 3,
                    color = "darkgreen") #%>%
    } else {
      # if the vector is not valid, reset the map
      react_map_vectfile(base_map())
    }

  })

  # use bbox
  observeEvent(input$save_extent_vectfile, {
    withProgress(message = 'Creating the extent...', value = 0, {
      vectfile_valid <- update_extent(extent_source="vectfile")
      if (vectfile_valid) {
        removeModal()
      } else {
        sendSweetAlert(
          session,
          title = "Please specify a valid vector file.",
          text = NULL,
          type = "error",
          btn_labels = "Ok"
        )
      }
      for (i in 1:10) {incProgress(1/10); Sys.sleep(0.1)}
    })
  })


  ###################- Draw mode -######################"

  # Open modal dialog to edit bbox
  observeEvent(input$button_extent_draw, {

    # create a new namespace every time the button is pushed,
    # in order not to make mess between modules
    extent_ns_name <- paste0("editor_",sample(1E9,1))
    extent_ns <- NS(extent_ns_name)
    rv$extent_edits <- callModule(editModPoly, extent_ns_name, base_map())

    # show the modal dialog
    showModal(load_extent_draw(extent_ns_name))

  })

  # use bbox
  observeEvent(input$save_extent_draw, {
    withProgress(message = 'Creating the extent...', value = 0, {
      drawn_valid <- update_extent(extent_source="draw")
      if (drawn_valid) {
        removeModal()
      } else {
        sendSweetAlert(
          session,
          title = "Please draw a valid extent.",
          text = NULL,
          type = "error",
          btn_labels = "Ok"
        )
      }
      for (i in 1:10) {incProgress(1/10); Sys.sleep(0.1)}
    })

  })


  #- Refresh the map if required -#
  observeEvent(input$button_refresh_map, {
    withProgress(message = 'Refreshing the map...', value = 0, {
      update_extent(extent_source="fake")
      for (i in 1:10) {incProgress(1/10); Sys.sleep(0.1)}
    })
  })

  ######"## end of extent module ############"


}
