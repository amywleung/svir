library(shiny)
library(leaflet)
library(shinythemes)

# Set limit for file upload
options(shiny.maxRequestSize = 50*1024^2)

# Define UI for file upload
navbarPage(theme = shinytheme("slate"),
  "CDC's Social Vulnerability Index Map",
  fluid = TRUE,
  tabPanel(
    "Overall SVI",
    sidebarLayout(
      # sidebar panel with file upload
      sidebarPanel(
        fileInput(inputId="shp", label="Upload Shapefile", multiple=TRUE)#, # must upload all 6 shapefile extensions (.dbf, .prj, .sbn, .sbx, .shp, .shx)
        #downloadOutput("downloadData", "Download")
        ),

      # plot shapefile on leaflet map
      mainPanel(
        leafletOutput("map")
      )
    )
  ),
  tabPanel(
    "Socioeconomic Status"
  ),
  tabPanel(
    "Household Composition & Disability"
  ),
  tabPanel(
    "Minority Status & Language"
  ),
  tabPanel(
    "Housing & Transportation"
  )
)
