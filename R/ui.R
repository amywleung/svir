library(shiny)
library(leaflet)
library(shinythemes)
library(DT)

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
        fileInput(inputId="shp", label="Upload Shapefile", multiple=TRUE), # must upload all 6 shapefile extensions (.dbf, .prj, .sbn, .sbx, .shp, .shx)
        radioButtons(inputId = "fileType", label = "Select file type for download", choices = list(".shp", ".csv")),
        downloadButton(outputId = "down", label = "Download File")
        ),

      # plot shapefile on leaflet map
      mainPanel(
        leafletOutput("map")
      )
    )
  ),
  tabPanel(
    "Tabular SVI",
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput("show_vars", "Columns in SVI:",
                           selected = c('rpl_theme1', 'rpl_theme2', 'rpl_theme3',
                                        'rpl_theme4', 'rpl_themes', "f_pov", "f_unemp",
                                        "f_pci", "f_nohsdp", "f_age65", "f_age17",
                                        "f_disabl", "f_sngpnt", "f_minrty", "f_limeng",
                                        "f_munit", "f_mobile", "f_crowd", "f_noveh", "f_groupq"),
                                      c('rpl_theme1', 'rpl_theme2', 'rpl_theme3',
                                        'rpl_theme4', 'rpl_themes', "f_pov", "f_unemp",
                                        "f_pci", "f_nohsdp", "f_age65", "f_age17",
                                        "f_disabl", "f_sngpnt", "f_minrty", "f_limeng",
                                        "f_munit", "f_mobile", "f_crowd", "f_noveh", "f_groupq")
                          )
        ),
      mainPanel(
        dataTableOutput("table")
        )
      )
    )
  )
