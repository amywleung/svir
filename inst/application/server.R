library(shiny)
library(rgdal)
library(rpostgis)
library(RPostgreSQL)
library(sp)
library(leaflet)
library(rgeos)
library(viridis)
library(stringr)
library(dplyr)
library(DT)
library(leaflet.extras)
library(plotly)

function(input, output) {
  # create a PostgreSQL instance and create one connection
  drv <- dbDriver("PostgreSQL")

  # open the connection with credentials
  con <- dbConnect(
    drv,
    user = "postgres",
    password = "gisde2018",
    host = "localhost",
    port = 5432,
    dbname = "svir"
  )

  # create reactive upload file function to store data
  uploadShpfile <- reactive({
    if (!is.null(input$shp)) {
      shpDF <- input$shp
      pwd <- getwd()
      updir <- dirname(shpDF$datapath[1])
      setwd(updir)
      for (i in 1:nrow(shpDF)) {
        file.rename(shpDF$datapath[i], shpDF$name[i])
      }
      shpName <- shpDF$name[grep(x = shpDF$name, pattern = "*.shp")]
      shpPath <- paste(updir, shpName, sep = "/")
      setwd(pwd)
      shpFile <- readOGR(shpPath)
      shpFile <-
        spTransform(
          shpFile,
          CRS(
            "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
          )
        )

      # write shp to PG table & create sp index
      writeOGR(
        shpFile,
        dsn = c(
          "PG:user = 'postgres' password = 'gisde2018' dbname = 'svir' host = 'localhost'"
        ),
        layer = "userext",
        overwrite_layer = TRUE,
        driver = "PostgreSQL"
      )

      # find intersections of user input and states and return geom
      res <-
        pgGetGeom(
          con,
          query = sprintf(
            "SELECT public.svi2014_us.*, public.userext.wkb_geometry
            FROM public.svi2014_us, public.userext
            WHERE ST_Intersects(public.svi2014_us.geom, public.userext.wkb_geometry);"
          )
          )
      svi <- svi_calc(con, res)  # calculate SVI for user input AOI
      return(svi)

    } else {
      return()
    }
  })  # end uploadshpfile reactive

  # create output$map
  output$map <- renderLeaflet({
    leaflet(width = "100%", height = "100%") %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addMiniMap(tiles = providers$CartoDB.Positron,
                 toggleDisplay = T) %>%
      addFullscreenControl(position = "topleft") %>%
      setView(-98.6106479, 39.8123024, zoom = 4)
  })  # end output$map

  # observe when user uploads file
  observeEvent(input$shp, if (!is.null(uploadShpfile())) {
    observe({
      if (input$select == 1) {
        mapStyle(
          map = "map",
          data = uploadShpfile(),
          rpl = "rpl_themes",
          flag = "f_total"
        )
      }
      else if (input$select == 2) {
        mapStyle(
          map = "map",
          data = uploadShpfile(),
          rpl = "rpl_theme1",
          flag = "f_theme1"
        )
      }
      else if (input$select == 3) {
        mapStyle(
          map = "map",
          data = uploadShpfile(),
          rpl = "rpl_theme2",
          flag = "f_theme2"
        )
      }
      else if (input$select == 4) {
        mapStyle(
          map = "map",
          data = uploadShpfile(),
          rpl = "rpl_theme3",
          flag = "f_theme3"
        )
      }
      else if (input$select == 5) {
        mapStyle(
          map = "map",
          data = uploadShpfile(),
          rpl = "rpl_theme4",
          flag = "f_theme4"
        )
      }
    })
  })

  # end observeevent
  datasetInput <- reactive({
    switch(
      input$fileType,
      ".shp" = uploadShpfile(),
      ".csv" = slot(uploadShpfile(), "data")
    )
  })

  observeEvent(input$shp, {
    if (!is.null(input$shp)) {
      output$down = downloadHandler(
        filename = function() {
          if (input$fileType == ".shp") {
            paste("regional_svi_dl.zip")
          }
          else{
            paste("regional_svi_dl.csv")
          }
        },
        content = function(file) {
          direct <- tempdir()
          setwd(direct)
          if (input$fileType == ".shp") {
            writeOGR(
              uploadShpfile(),
              dsn = direct,
              layer = "2014svi_us",
              driver = "ESRI Shapefile",
              overwrite_layer = TRUE
            )
            zip(zipfile = file,
                files = Sys.glob(paste("2014svi_us.*")))
          }

          else{
            write.csv(slot(uploadShpfile(), "data"),
                      file,
                      sep = ",",
                      row.names = FALSE)
          }
        },
        contentType = "application/zip"
      )
    }
  })

  observeEvent(input$shp, {
    output$table = DT::renderDataTable(
      slot(uploadShpfile(), "data")[, input$show_vars, drop = FALSE],
      options = list(
        lengthMenu = c(10, 20, 30, 40, 50, 75, 100),
        pageLength = 5
      ),
      filter = 'top'
    )
  })

  # intialize empty reactive values list
  rv <- reactiveValues(clickedShape = NULL)

  # observe map clicks when a poly is clicked
  observeEvent(input$map_shape_click, {
    rv$clickedShape <- input$map_shape_click
    if (!is.null(rv$clickedShape)) {
      id <- rv$clickedShape$id  # get census tract id from click event
      bg <- makeDash(shp = uploadShpfile(), uid = id)
      bp <- makeBox(shp = uploadShpfile(), uid = id)
      output$dash <- renderPlotly(bg)  # create plotly bar graph
      output$boxplot <- renderPlotly(bp)  # create plotly boxplot
    }
  })

  # clear rv list when clicking off the shape
  observeEvent(input$map_click, {
    rv$clickedShape <- NULL
  })
}
