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

# Define server logic
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

  # create upload file function
  uploadShpfile <- reactive({
    if (!is.null(input$shp)) {
      shpDF <- input$shp
      prevWD <- getwd()
      updir <- dirname(shpDF$datapath[1])
      setwd(updir)
      for (i in 1:nrow(shpDF)) {
        file.rename(shpDF$datapath[i], shpDF$name[i])
      }
      shpName <- shpDF$name[grep(x = shpDF$name, pattern = "*.shp")]
      shpPath <- paste(updir, shpName, sep = "/")
      setwd(prevWD)
      shpFile <- readOGR(shpPath)
      shpFile <-
        spTransform(
          shpFile,
          CRS(
            "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
          )
        )
      writeOGR(
        shpFile,
        dsn = c("PG:user = 'postgres' dbname = 'svir' host = 'localhost'"),
        # write shp to PG table & create sp index
        layer = "userext",
        overwrite_layer = TRUE,
        driver = "PostgreSQL"
      )

      res <-
        pgGetGeom(
          # find intersections of user input and states and return geom
          con,
          query = sprintf(
            "SELECT public.svi2014_us.*, public.userext.wkb_geometry
            FROM public.svi2014_us, public.userext
            WHERE ST_Intersects(public.svi2014_us.geom, public.userext.wkb_geometry);"
          )
          )
      svi <- svi_calc(con, res)
      return(svi)

    } else {
      return()
    }
  })

  # add leaflet map
  output$map <- renderLeaflet({
    leaflet(width = "100%", height = "100%") %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      setView(-98.6106479, 39.8123024, zoom = 4)
  })

  # add polygons
  observeEvent(input$shp, {
    if (!is.null(uploadShpfile())) {

      # Write spdf back to db
      pgInsert(
        con,
        name = c("public", "regsvir"),
        geom = "geom",
        data.obj = uploadShpfile(),
        overwrite = TRUE
      )
      inExt <- slot(uploadShpfile(), "bbox")
      pal <- colorNumeric(palette = "viridis",
                          domain = slot(uploadShpfile(), "data")$rpl_themes)
      leafletProxy("map") %>%
        addPolygons(
          data = uploadShpfile(),
          stroke = FALSE,
          smoothFactor = 0,
          fillOpacity = 0.4,
          color = ~ pal(slot(uploadShpfile(), "data")$rpl_themes)
        ) %>%
        addLegend(
          "bottomright",
          pal = pal,
          values = slot(uploadShpfile(), "data")$rpl_themes,
          title = "SVI",
          opacity = 1
        ) %>%
        fitBounds(
          lng1 = inExt[1],
          lat1 = inExt[2],
          lng2 = inExt[3],
          lat2 = inExt[4]
        )
    }
  })

  # # disconnect db connection
  # dbDisconnect(con)
  # dbUnloadDriver(drv)
}
