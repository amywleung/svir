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
ep_nms  <- grep('ep_', clmNames, value=TRUE)
ep_nms <- ep_nms[-16]
epl_nms <- grep('epl_', clmNames, value=TRUE)


# Create variable containing just the stems of the variable names
colNeut <- sapply(strsplit(epl_nms, split='_', fixed=TRUE), function(x) (x[2]))


# Create a variable containing the names of the flag columns
f_nms   <- paste("f_", colNeut, sep="")


# Calculate rank value of 15 tier 1 variables ("ep_" columns), assign values to "epl" columns
for(i in 1:length(ep_nms)){
  res[[paste(epl_nms[i])]] <- (rank(res[[paste(ep_nms[i])]], ties.method = "max")-1)/(length(res$gid)-1)
}
length(epl_nms)
length(ep_nms)

testNms <- names(res)

testNms
clmNames



testNms[!testNms %in% clmNames]



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

rpl_nms <-grep ('rpl_', clmNames, value=TRUE)
rpl_nms <- rpl_nms[! rpl_nms %in% "rpl_themes"]


# Sum 15 variable percentile ranks for each theme to `theme#_epl` column
for(i in 1:length(theme1_epl)){
  res[[paste(spl_nms[i])]] <- apply(slot(res, "data")[epl_nms_split[[i]]], 1, sum)
}

# Calculate rank value of 4 tier 2 thematid domains ("spl_" columns), assign values to "rpl_" columns

for(i in 1:length(spl_nms)){
  res[[paste(rpl_nms[i])]] <- (rank(res[[paste(spl_nms[i])]], ties.method = "max")-1)/(length(res$gid)-1)
}


f_theme_nms <- gsub("spl_", "f_", spl_nms)

# Flag values for 4 thematic domains
for(i in 1:length(f_theme_nms)){
  slot(res, "data")[f_theme_nms[i]][slot(res, "data")[rpl_nms[i]] >= .9] <- 1
  slot(res, "data")[f_theme_nms[i]][slot(res, "data")[rpl_nms[i]] < .9]  <- 0
}


# Sum 4 thematic domains for overall SVI values to `spl_themes` column
res$spl_themes <- apply(slot(res, "data")[c("spl_theme1", "spl_theme2","spl_theme3", "spl_theme4")], 1, sum)

# Calculate rank value of overall SVI (`spl_themes`), assign values to `rpl_themes`

res$rpl_themes <- (rank(res$spl_themes, ties.method = "max")-1)/(length(res$gid)-1)


# Sum Flags to calculate total number of flags at each census tract
res$f_total <- apply(slot(res, "data")[f_theme_nms, f_nms], 1, sum)


all_flags <- c(f_theme_nms, f_nms)
res$f_total <- apply(slot(res, "data")[all_flags], 1, sum)


# Add the non-populated census tracts back into the main spatialpolygonsdataframe

res <- rbind(res, nPop_res)


