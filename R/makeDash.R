makeDash <- function() {
dat <- slot(uploadShpfile(), "data")
t1 <- c("POV", "UNEMP", "PCI", "NOHSDP")
t2 <- c("AGE65", "AGE17", "DISABL", "SNGPNT")
t3 <- c("MINRTY", "LIMENG")
t4 <- c("MUNIT", "MOBILE", "CROWD", "NOVEH", "GROUPQ")
t1vals <-
  c(dat[["epl_pov"]][(dat$gid == 62913) &
                       (!dat[["rpl_themes"]] == -999)], dat[["epl_unemp"]][(dat$gid == 62913) &
                                                                             (!dat[["rpl_themes"]] == -999)],
    dat[["epl_pci"]][(dat$gid == 62913) &
                       (!dat[["rpl_themes"]] == -999)], dat[["epl_nohsdp"]][(dat$gid == 62913) &
                                                                              (!dat[["rpl_themes"]] == -999)])
t2vals <-
  c(dat[["epl_age65"]][(dat$gid == 62913) &
                         (!dat[["rpl_themes"]] == -999)], dat[["epl_age17"]][(dat$gid == 62913) &
                                                                               (!dat[["rpl_themes"]] == -999)],
    dat[["epl_disabl"]][(dat$gid == 62913) &
                          (!dat[["rpl_themes"]] == -999)], dat[["epl_sngpnt"]][(dat$gid == 62913) &
                                                                                 (!dat[["rpl_themes"]] == -999)])
t3vals <-
  c(dat[["epl_minrty"]][(dat$gid == 62913) &
                          (!dat[["rpl_themes"]] == -999)], dat[["epl_limeng"]][(dat$gid == 62913) &
                                                                                 (!dat[["rpl_themes"]] == -999)])
t4vals <-
  c(dat[["epl_munit"]][(dat$gid == 62913) &
                         (!dat[["rpl_themes"]] == -999)], dat[["epl_mobile"]][(dat$gid == 62913) &
                                                                                (!dat[["rpl_themes"]] == -999)],
    dat[["epl_crowd"]][(dat$gid == 62913) &
                         (!dat[["rpl_themes"]] == -999)], dat[["epl_noveh"]][(dat$gid == 62913) &
                                                                               (!dat[["rpl_themes"]] == -999)],
    dat[["epl_groupq"]][(dat$gid == 62913) &
                          (!dat[["rpl_themes"]] == -999)])
p1 <-
  plot_ly(
    dat,
    x = ~ t1,
    y = ~ t1vals,
    type = "bar",
    name = "Socioeconomic Status",
    hoverinfo = "x"
  ) %>%
  layout(yaxis = list(title = "Percentile Percentage"),
         xaxis = list(tickangle = 45))
p2 <-
  plot_ly(
    dat,
    x = ~ t2,
    y = ~ t2vals,
    type = "bar",
    name = "Household Composition & Disability",
    hoverinfo = "x"
  ) %>%
  layout(xaxis = list(tickangle = 45))
p3 <-
  plot_ly(
    dat,
    x = ~ t3,
    y = ~ t3vals,
    type = "bar",
    name = "Minority Status & Language",
    hoverinfo = "x"
  ) %>%
  layout(xaxis = list(tickangle = 45))
p4 <-
  plot_ly(
    dat,
    x = ~ t4,
    y = ~ t4vals,
    type = "bar",
    name = "Housing & Transportation",
    hoverinfo = "x"
  ) %>%
  layout(xaxis = list(tickangle = 45))
subp <- subplot(p1, p2, p3, p4, shareY = TRUE)
output$dash <- renderPlotly(subp)
}
