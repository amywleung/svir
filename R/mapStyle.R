mapStyle <- function(map, data, shp, bbox, rpl, flag) {

    nondat <- data[[rpl]][!data[[rpl]] == -999]  # subset out tracts with no SVIs
    pal <- colorNumeric(palette = "viridis",
                        domain = nondat)
    # format labels for reactive map labels
    labs <-
      sprintf(
        "<strong>%s</strong>%s<br/><strong>%s</strong>%g<br/><strong>%s</strong>%i",
        "Location: ",
        data$location[!data[[rpl]] == -999],
        "SVI: ",
        round(nondat, digits = 2),
        "Total Flags: ",
        data[[flag]][!data[[rpl]] == -999]
      ) %>%
      lapply(HTML)

    # initialize leaflet map for all tabs and add polys
    leafletProxy(map) %>%
      clearShapes() %>%  # clear shapes with each upload
      clearControls() %>%  # clear controls with each upload
      addPolygons(
        data = shp,
        smoothFactor = 0,
        fillColor = ~ pal(nondat),
        fillOpacity = 0.7,
        color = "white",
        weight = 1,
        label = labs,
        labelOptions = labelOptions(
        # additional label formatting
          textsize = "12px",
          style = list(padding = "3px 5px",
                       opacity = 0.7)
        ),
        dashArray = "3",
        highlightOptions = highlightOptions(
          # highlight polygons that cursor is over
          color = "white",
          weight = 4,
          fillOpacity = 0.7,
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        # add legend
        "bottomleft",
        pal = pal,
        values = nondat,
        title = "SVI",
        opacity = 1
      ) %>%
      fitBounds(
        # upon user upload zoom to uploaded file extent
        lng1 = bbox[1],
        lat1 = bbox[2],
        lng2 = bbox[3],
        lat2 = bbox[4]
      )
}