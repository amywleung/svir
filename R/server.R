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
      setView(-98.6106479, 39.8123024, zoom = 4)
  })  # end output$map

  # observe when user uploads file
  observeEvent(input$shp, if (!is.null(uploadShpfile())) {
    dat <-
      slot(uploadShpfile(), "data")  # create data and bbox slot objects
    bb <- slot(uploadShpfile(), "bbox")
    observe({
      if (input$select == 1) {
        mapStyle(
          map = "map",
          data = dat,
          shp = uploadShpfile(),
          rpl = "rpl_themes",
          flag = "f_total",
          bbox = bb
        )
      } else {
        lapply(1:4, function(n) {
          mapStyle(
            map = "map",
            data = dat,
            shp = uploadShpfile(),
            rpl = paste0("rpl_theme", n),
            flag = paste0("f_theme", n),
            bbox = bb
          )
        })
      }
    })
    # end observeevent
  })

}
