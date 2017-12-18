#' svi_calc
#'
#' Calculates the CDC/ATSDR formulated Social Vulnerability Index (SVI) using ACS 2010-2014 5-year
#' estimates. User must specify a PostGIS database connection and the ST_Intersects() result as
#' inputs.
#'
#' @param con A PostGIS database connection
#' @param res The result from an ST_Intersects() of PostGIS spatial tables
#'
#' @return A SpatialPolygons Data Frame object resulting from PostgreSQL connection \code{con}
#' and the resulting object from PostGIS function ST_Intersects() \code{res}
#'
#' @example
#' svi_calc(con, res)
#'
#' @export

svi_calc <- function(con, res) {

  res$rpl_theme1_US <- res$rpl_theme1
  res$rpl_theme2_US <- res$rpl_theme2
  res$rpl_theme3_US <- res$rpl_theme3
  res$rpl_theme4_US <- res$rpl_theme4
  res$rpl_themes_US <- res$rpl_themes

  # Assign vector of column names
  clmNames <- names(res)
  clmNames <- clmNames[!clmNames == 'wkb_geometry']
  res <- res[clmNames]

  # delete any row with 0 value in totpop row

  nPop_res <- res[(res$e_totpop=="0"),]
  res <- res[!(res$e_totpop=="0"),]

  slot(res, "data")[slot(res, "data") == -999] <- NA

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
    res[[paste(epl_nms[i])]] <- na.omit((rank(res[[paste(ep_nms[i])]], ties.method = "max")-1)/(length(res$gid)-1))
  }

  # Calculate rank value of 'pci'
  res$epl_pci <- (rank(res$ep_pci * -1, ties.method = "max")-1)/(length(res$ep_pov)-1)

  # Add 'pci' variable back into the list of variables
  ep_nms  <- c(ep_nms,'ep_pci')
  epl_nms <- c(epl_nms, 'epl_pci')

  # Flag values for 15 variables
  for(i in 1:length(f_nms)){
    slot(res, "data")[f_nms[i]][slot(res, "data")[epl_nms[i]] >= .9] <- 1
    slot(res, "data")[f_nms[i]][slot(res, "data")[epl_nms[i]] < .9]  <- 0
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
    res[[paste(spl_nms[i])]] <- rowSums(slot(res, "data")[,epl_nms_split[[i]]])
  }

  # Calculate rank value of 4 tier 2 thematid domains ("spl_" columns), assign values to "rpl_" columns
  for(i in 1:length(spl_nms)){
    res[[paste(rpl_nms[i])]] <- na.omit((rank(res[[paste(spl_nms[i])]], ties.method = "max")-1)/(length(res$gid)-1))
  }

  f_theme_nms <- gsub("spl_", "f_", spl_nms)

  # Flag values for 4 thematic domains
  for(i in 1:length(f_theme_nms)){
    slot(res, "data")[f_theme_nms[i]][slot(res, "data")[rpl_nms[i]] >= .9] <- 1
    slot(res, "data")[f_theme_nms[i]][slot(res, "data")[rpl_nms[i]] < .9]  <- 0
  }

  # Sum 4 thematic domains for overall SVI values to `spl_themes` column
  res$spl_themes <- rowSums(slot(res, "data")[,c("spl_theme1", "spl_theme2","spl_theme3", "spl_theme4")])

  # Calculate rank value of overall SVI (`spl_themes`), assign values to `rpl_themes`
  res$rpl_themes <- na.omit((rank(res$spl_themes, ties.method = "max")-1)/(length(res$gid)-1))

  # Sum Flags to calculate total number of flags at each census tract
  res$f_total <- rowSums(slot(res, "data")[,c(f_theme_nms, f_nms)])

  slot(res, "data")[is.na(slot(res, "data"))] <- -999

  # Add the non-populated census tracts back into the main spatialpolygonsdataframe
  res <- rbind(res, nPop_res)
}
