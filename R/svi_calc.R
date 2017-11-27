#' svi_calc
#'
#' Calculates the CDC/ATSDR formulated Social Vulnerability Index (SVI) using ACS 2010-2014 5-year
#' estimates. User must specify a PostGIS database connection and the ST_Intersects() result as
#' inputs.
#'
#' @param con A PostGIS database connection
#' @param res1 The result from an ST_Intersects() of PostGIS spatial tables.
#'
#' @return A SpatialPolygons Data Frame object resulting from PostgreSQL connection \code{con}
#' and the resulting object from PostGIS function ST_Intersects() \code{res1}
#'
#' @example
#' svi_calc(con, res1)
#'
#' @export

svi_calc <- function(con, res1) {

  # Assign vector of column names
  clmNames <- names(res1)
  clmNames <- clmNames[!clmNames == 'wkb_geometry']
  res1 <- res1[clmNames]



  # delete any row with 0 value in totpop row

  nPop_res <- res1[(res1$e_totpop=="0"),]
  res1 <- res1[!(res1$e_totpop=="0"),]

  # Note on column prefix meanings:
  #   'e_'    - indicates raw number estimate
  #   'ep_'   - indicates percentage of variable estimate (e.g. ep_crowd = percentage of occupied housing
  #             units with more people than rooms estimate)
  #   'epl_'  - indicates percentile rank of percentage of variable estimate

  # Find estimate column names, and percentile rank column names
  ep_nms  <- grep('ep_', clmNames, value=TRUE)
  ep_nms  <- ep_nms[-16]
  epl_nms <- grep('epl_', clmNames, value=TRUE)

  # Create variable containing just the stems of the variable names
  colNeut <- sapply(strsplit(epl_nms, split='_', fixed=TRUE), function(x) (x[2]))

  # Remove 'pci' or income variable, as it must be ranked in an inverse manner to all other variables
  ep_nms  <- ep_nms[!ep_nms == 'ep_pci']
  epl_nms <- epl_nms[!epl_nms == 'epl_pci']

  # Create a variable containing the names of the flag columns
  f_nms   <- paste("f_", colNeut, sep="")

  # Calculate rank value of 15 tier 1 variables (except pci) ("ep_" columns), assign values to "epl" columns
  for(i in 1:length(ep_nms)){
    res1[[paste(epl_nms[i])]] <- (rank(res1[[paste(ep_nms[i])]], ties.method = "max")-1)/(length(res1$gid)-1)
  }

  # Calculate rank value of 'pci'
  res1$epl_pci <- (rank(res1$ep_pci * -1, ties.method = "max")-1)/(length(res1$ep_pov)-1)

  # Add 'pci' variable back into the list of variables
  ep_nms  <- c(ep_nms,'ep_pci')
  epl_nms <- c(epl_nms, 'epl_pci')

  # Flag values for 15 variables
  for(i in 1:length(f_nms)){
    slot(res1, "data")[f_nms[i]][slot(res1, "data")[epl_nms[i]] >= .9] <- 1
    slot(res1, "data")[f_nms[i]][slot(res1, "data")[epl_nms[i]] < .9]  <- 0
  }

  # identify variable names that contribute to each of the 4 thematic domains
  epl_nms_split <- split(epl_nms, rep(1:4, c(4,4,2,5)))
  names(epl_nms_split) <- c('theme1','theme2','theme3','theme4')

  # identify the thematic domain columns where the sum of the 15 variable values are stored, remove overall theme
  # column
  spl_nms <- grep('spl_', clmNames, value=TRUE)
  spl_nms <- spl_nms[! spl_nms %in% "spl_themes"]

  rpl_nms <- grep ('rpl_', clmNames, value=TRUE)
  rpl_nms <- rpl_nms[! rpl_nms %in% "rpl_themes"]

  # Sum 15 variable percentile ranks for each theme to `theme#_epl` column
  for(i in 1:4){
    res1[[paste(spl_nms[i])]] <- apply(slot(res1, "data")[epl_nms_split[[i]]], 1, sum)
  }

  # Calculate rank value of 4 tier 2 thematid domains ("spl_" columns), assign values to "rpl_" columns
  for(i in 1:length(spl_nms)){
    res1[[paste(rpl_nms[i])]] <- (rank(res1[[paste(spl_nms[i])]], ties.method = "max")-1)/(length(res1$gid)-1)
  }

  f_theme_nms <- gsub("spl_", "f_", spl_nms)

  # Flag values for 4 thematic domains
  for(i in 1:length(f_theme_nms)){
    slot(res1, "data")[f_theme_nms[i]][slot(res1, "data")[rpl_nms[i]] >= .9] <- 1
    slot(res1, "data")[f_theme_nms[i]][slot(res1, "data")[rpl_nms[i]] < .9]  <- 0
  }

  # Sum 4 thematic domains for overall SVI values to `spl_themes` column
  res1$spl_themes <- apply(slot(res1, "data")[c("spl_theme1", "spl_theme2","spl_theme3", "spl_theme4")], 1, sum)

  # Calculate rank value of overall SVI (`spl_themes`), assign values to `rpl_themes`
  res1$rpl_themes <- (rank(res1$spl_themes, ties.method = "max")-1)/(length(res1$gid)-1)

  # Sum Flags to calculate total number of flags at each census tract
  res1$f_total <- apply(slot(res1, "data")[c(f_theme_nms, f_nms)], 1, sum)

  # res1$f_total
  # slot(res1, "data")[c(f_theme_nms, f_nms)]
  #
  #
  # f_nms
  # f_theme_nms

  # Add the non-populated census tracts back into the main spatialpolygonsdataframe
  res1 <- rbind(res1, nPop_res)
}
