library(shiny)
library(leaflet)
library(shinythemes)

# Set limit for file upload
options(shiny.maxRequestSize = 50 * 1024 ^ 2)

# Define UI for file upload
navbarPage(theme = shinytheme("slate"),
           title = "Center for Disease Control and Prevention: Social Vulnerability Index",
           tabPanel("SVI Maps",
                    sidebarLayout(
                      sidebarPanel(
                        # create select input
                        selectInput(
                          "select",
                          label = h3("Select SVI Domain"),
                          choices = list(
                            "Overall SVI" = 1,
                            "Socioeconomic Status" = 2,
                            "Household Composition & Disability" = 3,
                            "Minority Status & Language" = 4,
                            "Housing & Transportation" = 5
                          ),
                          selected = 1
                        ),

                        # create file upload
                        fileInput(
                          inputId = "shp",
                          label = "Upload Shapefile",
                          multiple = TRUE
                        )
                      ),
                      # plot shapefile on leaflet map
                      mainPanel(leafletOutput("map"))
                    ))
             # tabPanel("Data Table")
           #            leafletOutput("map2")),
           #   tabPanel("Household Composition & Disability",
           #            leafletOutput("map3")),
           #   tabPanel("Minority Status & Language",
           #            leafletOutput("map4")),
           #   tabPanel("Housing & Transportation",
           #            leafletOutput("map5"))
           )
