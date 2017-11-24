library(rgdal)
library(sp)
library(rpostgis)
library(stringr)
library(dplyr)

# set Postgres driver
drv <- dbDriver("PostgreSQL")
# open the connection with credentials
con <- dbConnect(drv, user="postgres", password="gisde2018",
                 host="localhost", port=5432,dbname="svir")

# Read in data
res <- pgGetGeom(con, name = 'svi2014_ct')


nPop_res <- res[which(res$ep_pci == -999), ]
res <- res[-c(which(res$ep_pci == -999)), ]


# Assign vector of column names
clmNames <- names(res)

# Note on column prefix meanings:
#   'e_'    - indicates raw number estimate
#   'ep_'   - indicates percentage of variable estimate (e.g. ep_crowd = percentage of occupied housing
#             units with more people than rooms estimate)
#   'epl_'  - indicates percentile rank of percentage of variable estimate

# Find estimate column names, and percentile rank column names
ep_nms <- grep('ep_', clmNames, value=TRUE)
epl_nms <- grep('epl_', clmNames, value=TRUE)

colNeut <- sapply(strsplit(epl_nms, split='_', fixed=TRUE), function(x) (x[2]))
f_nms <- paste("f_", colNeut, sep="")


# Calculate rank value of 15 tier 1 variables ("e_" columns), assign values to "ep" columns
for(i in 1:length(ep_nms)){
  res[[paste(epl_nms[i])]] <- (rank(res[[paste(ep_nms[i])]], ties.method = "max")-1)/(length(res$gid)-1)
}

# Flag values for 15 variables
for(i in 1:length(f_nms)){
  slot(res, "data")[f_nms[i]][slot(res, "data")[epl_nms[i]] >= .9] <- 1
  slot(res, "data")[f_nms[i]][slot(res, "data")[epl_nms[i]] < .9]  <- 0
}


epl_nms_split <- split(epl_nms, rep(1:4, c(4,4,2,5)))
names(epl_nms_split) <- c('theme1','theme2','theme3','theme4')


spl_nms <- grep('spl_', clmNames, value=TRUE)
spl_nms <- spl_nms[! spl_nms %in% "spl_themes"]

rpl_nms <-grep ('rpl_', clmNames, value=TRUE)
rpl_nms <- rpl_nms[! rpl_nms %in% "rpl_themes"]

theme1_epl <- epl_nms_split[[1]]
theme2_epl <- epl_nms_split[[2]]
theme3_epl <- epl_nms_split[[3]]
theme4_epl <- epl_nms_split[[4]]

themes <- c(theme1_epl, theme2_epl, theme3_epl, theme4_epl)

# Sum 15 variable percentile ranks for each theme to `theme#_epl` column
for(i in 1:length(theme1_epl)){
  res[[paste(spl_nms[i])]] <- apply(slot(res, "data")[themes[i]], 1, sum)
}







