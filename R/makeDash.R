#' makeDash
#'
#' Creates and styles plotly dashboard using leaflet user click events. This
#' dashboard visualizes each of the indicator variables that comprise a thematic
#' domain, and updates dynamically based on click events.
#'
#' @param shp The SVI shapefile from the CDC website
#' @param uid The click event polygon id
#'
#' @return A styled dashboard using the SVI shapefile \code{shp}
#' and click event polygon id \code{uid}
#'
#' @examples
#' makeDash(shp = uploadShpFile(), uid = 62878)
#' makeDash(shp = uploadShpFile(), uid = event$id)
#'
#' @export

makeDash <- function(shp, uid) {
  dat <- slot(shp, "data")
  t1 <- c("POV", "UNEMP", "PCI", "NOHSDP")
  t2 <- c("AGE65", "AGE17", "DISABL", "SNGPNT")
  t3 <- c("MINRTY", "LIMENG")
  t4 <- c("MUNIT", "MOBILE", "CROWD", "NOVEH", "GROUPQ")
  t1vals <-
    c(dat[["epl_pov"]][(dat$gid == uid) &
                         (!dat[["rpl_themes"]] == -999)], dat[["epl_unemp"]][(dat$gid == uid) &
                                                                               (!dat[["rpl_themes"]] == -999)],
      dat[["epl_pci"]][(dat$gid == uid) &
                         (!dat[["rpl_themes"]] == -999)], dat[["epl_nohsdp"]][(dat$gid == uid) &
                                                                                (!dat[["rpl_themes"]] == -999)])
  t2vals <-
    c(dat[["epl_age65"]][(dat$gid == uid) &
                           (!dat[["rpl_themes"]] == -999)], dat[["epl_age17"]][(dat$gid == uid) &
                                                                                 (!dat[["rpl_themes"]] == -999)],
      dat[["epl_disabl"]][(dat$gid == uid) &
                            (!dat[["rpl_themes"]] == -999)], dat[["epl_sngpnt"]][(dat$gid == uid) &
                                                                                   (!dat[["rpl_themes"]] == -999)])
  t3vals <-
    c(dat[["epl_minrty"]][(dat$gid == uid) &
                            (!dat[["rpl_themes"]] == -999)], dat[["epl_limeng"]][(dat$gid == uid) &
                                                                                   (!dat[["rpl_themes"]] == -999)])
  t4vals <-
    c(dat[["epl_munit"]][(dat$gid == uid) &
                           (!dat[["rpl_themes"]] == -999)], dat[["epl_mobile"]][(dat$gid == uid) &
                                                                                  (!dat[["rpl_themes"]] == -999)],
      dat[["epl_crowd"]][(dat$gid == uid) &
                           (!dat[["rpl_themes"]] == -999)], dat[["epl_noveh"]][(dat$gid == uid) &
                                                                                 (!dat[["rpl_themes"]] == -999)],
      dat[["epl_groupq"]][(dat$gid == uid) &
                            (!dat[["rpl_themes"]] == -999)])
  p1 <-
    plot_ly(
      dat,
      x = ~ t1,
      y = ~ t1vals,
      type = "bar",
      name = "Socioeconomic Status",
      hoverinfo = "y",
      opacity = 0.7
    ) %>%
    layout(
      yaxis = list(title = "Percentile Percentage"),
      xaxis = list(tickangle = 45),
      font = list(color = "white"), margin = list(b = 105)
    )
  p2 <-
    plot_ly(
      dat,
      x = ~ t2,
      y = ~ t2vals,
      type = "bar",
      name = "Household Composition & Disability",
      hoverinfo = "y",
      opacity = 0.7
    ) %>%
    layout(xaxis = list(tickangle = 45),
           font = list(color = "white"), margin = list(b = 105))
  p3 <-
    plot_ly(
      dat,
      x = ~ t3,
      y = ~ t3vals,
      type = "bar",
      name = "Minority Status & Language",
      hoverinfo = "y",
      opacity = 0.7
    ) %>%
    layout(xaxis = list(tickangle = 45),
           font = list(color = "white"), margin = list(b = 105))
  p4 <-
    plot_ly(
      dat,
      x = ~ t4,
      y = ~ t4vals,
      type = "bar",
      name = "Housing & Transportation",
      hoverinfo = "y",
      opacity = 0.7
    ) %>%
    layout(xaxis = list(tickangle = 45),
           font = list(color = "white"), margin = list(b = 105), plot_bgcolor = "transparent",
           paper_bgcolor = "transparent")

  subp <- subplot(p1, p2, p3, p4, shareY = TRUE)
}
