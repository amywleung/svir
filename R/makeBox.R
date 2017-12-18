#' makeBox
#'
#' Creates and styles plotly box plots using leaflet user click events. These
#' boxplots visualize the spread of data for each of the thematic domains
#' and adds a point indicating where a selected census tract falls within
#' the nationwide dataset. This point updates dynamically based on click
#' events.
#'
#' @param shp The SVI shapefile from the CDC website
#' @param uid The click event polygon id
#'
#' @return A styled group of boxplots using the SVI shapefile \code{shp}
#' and click event polygon id \code{uid}
#'
#' @examples
#' makeBox(shp = uploadShpFile(), uid = 62878)
#' makeBox(shp = uploadShpFile(), uid = event$id)
#'
#' @export

makeBox <- function(shp, uid) {
  dat <- slot(shp, "data")
  b1 <- "Overall"
  b2 <- "Socioeconomic Status"
  b3 <- "Household Composition & Disability"
  b4 <- "Minority Status & Language"
  b5 <- "Housing & Transportation"
  o1vals <- dat[["rpl_themes_US"]][!dat[["rpl_themes_US"]] == -999]
  o2vals <- dat[["rpl_theme1_US"]][!dat[["rpl_theme1_US"]] == -999]
  o3vals <- dat[["rpl_theme2_US"]][!dat[["rpl_theme2_US"]] == -999]
  o4vals <- dat[["rpl_theme3_US"]][!dat[["rpl_theme3_US"]] == -999]
  o5vals <- dat[["rpl_theme4_US"]][!dat[["rpl_theme4_US"]] == -999]
  b1vals <- dat[["rpl_themes"]][(dat$gid == uid) &
                                  (!dat[["rpl_themes"]] == -999)]
  b2vals <- dat[["rpl_theme1"]][(dat$gid == uid) &
                                  (!dat[["rpl_theme1"]] == -999)]
  b3vals <- dat[["rpl_theme2"]][(dat$gid == uid) &
                                  (!dat[["rpl_theme2"]] == -999)]
  b4vals <- dat[["rpl_theme3"]][(dat$gid == uid) &
                                  (!dat[["rpl_theme3"]] == -999)]
  b5vals <- dat[["rpl_theme4"]][(dat$gid == uid) &
                                  (!dat[["rpl_theme4"]] == -999)]
  p1 <-
    plot_ly(
      dat,
      y = ~ o1vals,
      hoverinfo = "y",
      type = "box",
      x = ~ "Overall",
      color = I("#8023ea"),
      showlegend = FALSE
    ) %>%
    add_markers(y = ~ b1vals, marker = list(size = 10,
                                            color = "#c6fffd",
                                            line = list(color = "#85fcf8",
                                                        width = 2))) %>%
    layout(margin = list(b = 230), font = list(color = "white"),
           yaxis = list(title = "Percentile Percentage"),
           xaxis = list(tickangle = 90)
    )
  p2 <-
    plot_ly(
      dat,
      y = ~ o2vals,
      hoverinfo = "y",
      type = "box",
      x = ~ "Socioeconomic Status",
      color = I("#1e5e8c"),
      showlegend = FALSE
    ) %>%
    add_markers(y = ~ b2vals, marker = list(size = 10,
                                            color = "#c6fffd",
                                            line = list(color = "#85fcf8",
                                                        width = 2))) %>%
    layout(margin = list(b = 230), font = list(color = "white"), xaxis = list(tickangle = 90))
  p3 <-
    plot_ly(
      dat,
      y = ~ o3vals,
      hoverinfo = "y",
      type = "box",
      x = ~ "Household Composition & Disability",
      color = I("#bf6415"),
      showlegend = FALSE
    ) %>%
    add_markers(y = ~ b3vals, marker = list(size = 10,
                                            color = "#c6fffd",
                                            line = list(color = "#85fcf8",
                                                        width = 2))) %>%
    layout(margin = list(b = 230), font = list(color = "white"), xaxis = list(tickangle = 90))
  p4 <-
    plot_ly(
      dat,
      y = ~ o4vals,
      hoverinfo = "y",
      type = "box",
      x = ~ "Minority Status & Language",
      color = I("#287c2b"),
      showlegend = FALSE
    ) %>%
    add_markers(y = ~ b4vals, marker = list(size = 10,
                                            color = "#c6fffd",
                                            line = list(color = "#85fcf8",
                                                        width = 2))) %>%
    layout(margin = list(b = 230), font = list(color = "white"), xaxis = list(tickangle = 90))
  p5 <-
    plot_ly(
      dat,
      y = ~ o5vals,
      hoverinfo = "y",
      type = "box",
      x = ~ "Housing & Transportation",
      color = I("#a22527"),
      showlegend = FALSE
    ) %>%
    add_markers(y = ~ b5vals, marker = list(size = 10,
                                            color = "#c6fffd",
                                            line = list(color = "#85fcf8",
                                                        width = 2))) %>%
    layout(margin = list(b = 230), font = list(color = "white"), xaxis = list(tickangle = 90),
           legend = list(x = 0.1, y = 0.9), plot_bgcolor = "transparent",
           paper_bgcolor = "transparent")
  bp <- subplot(p1, p2, p3, p4, p5, shareY = TRUE)
}
