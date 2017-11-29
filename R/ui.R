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
                        ),
                        radioButtons(inputId = "fileType", label = "Select file type for download",
                                     choices = list(".shp", ".csv")),
                        downloadButton(outputId = "down", label = "Download File")


                      ),
                      # plot shapefile on leaflet map
                      mainPanel(leafletOutput("map"))
                    )),
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
             # tabPanel("Data Table")
           #            leafletOutput("map2")),
           #   tabPanel("Household Composition & Disability",
           #            leafletOutput("map3")),
           #   tabPanel("Minority Status & Language",
           #            leafletOutput("map4")),
           #   tabPanel("Housing & Transportation",
           #            leafletOutput("map5"))
  )
)
