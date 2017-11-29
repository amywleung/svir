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
        dsn = c("PG:user = 'postgres' dbname = 'svir' password = 'gisde2018' host = 'localhost'"),
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
      dat <- slot(uploadShpfile(), "data")
      bb <- slot(uploadShpfile(), "bbox")
      nndat <- dat$rpl_themes[!dat$rpl_themes == -999]
      pal <- colorNumeric(palette = "viridis",
                          domain = nndat)
      labs <-
        sprintf("<strong>%s</strong>%s<br/><strong>%s</strong>%g<br/><strong>%s</strong>%i",
                "Location: ", dat$location[!dat$rpl_themes == -999],
                "Overall SVI: ", round(nndat, digits = 2), "Total Flags: ",
                dat$f_total[!dat$rpl_themes == -999]) %>%
        lapply(HTML)
      leafletProxy("map") %>%
        addPolygons(
          data = uploadShpfile(),
          popup = ~ paste("Overall SVI: ",
                          str_extract(round(
                            nndat, digits = 2
                          ), "^([^,]*)")),
          smoothFactor = 0,
          fillColor = ~ pal(nndat),
          fillOpacity = 0.7,
          color = "white",
          weight = 1,
          label = labs,
          labelOptions = labelOptions(
            textsize = "12px",
            style = list(padding = "3px 5px",
            opacity = 0.7)
          ),
          dashArray = "3",
          highlightOptions = highlightOptions(
            color = "white",
            weight = 4,
            fillOpacity = 0.7,
            bringToFront = TRUE
          )
        ) %>%
        addLegend(
          "bottomright",
          pal = pal,
          values = nndat,
          title = "Overall SVI",
          opacity = 1
        ) %>%
        fitBounds(
          lng1 = bb[1],
          lat1 = bb[2],
          lng2 = bb[3],
          lat2 = bb[4]
        )

      # Write spdf back to db
      pgInsert(
        con,
        name = c("public", "regsvir"),
        geom = "geom",
        data.obj = uploadShpfile(),
        overwrite = TRUE
      )
    }
  })

  datasetInput <- reactive({
    switch(
      input$fileType,
      ".shp" = uploadShpfile(),
      ".csv" = slot(uploadShpfile(), "data")
    )
  })

  observeEvent(input$shp, {
    if(!is.null(input$shp)) {
      output$down = downloadHandler(
        filename = function(){
          if(input$fileType == ".shp"){
            paste("regional_svi_dl.zip")
          }
          else{
            paste("regional_svi_dl.csv")
          }
        },
        content = function(file){
          direct <- tempdir()
          setwd(direct)
          if(input$fileType == ".shp"){
           writeOGR(uploadShpfile(), dsn = direct, layer = "2014svi_us", driver = "ESRI Shapefile", overwrite_layer = TRUE)
           zip(zipfile = file, files = Sys.glob(paste("2014svi_us.*")))
          }

          else{
            write.csv(slot(uploadShpfile(), "data"), file, sep = ",", row.names = FALSE)
          }
        },
        contentType = "application/zip"
      )
    }
  })

  observeEvent(input$shp, {
    output$table = DT::renderDataTable(slot(uploadShpfile(), "data"))
  }
  )

}

# disconnect db connection
# dbDisconnect(con)
# dbUnloadDriver(drv)

