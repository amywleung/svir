library(shiny)
library(leaflet)
library(shinythemes)
library(plotly)

# Set limit for file upload
options(shiny.maxRequestSize = 50 * 1024 ^ 2)

# Define UI for file upload
navbarPage(theme = shinytheme("slate"),
           title = "Centers for Disease Control and Prevention: Social Vulnerability Index",
           tabPanel("Interactive Map",
                    sidebarLayout(
                      sidebarPanel(
                        # create select input to view different thematic domain maps
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
                        downloadButton(outputId = "down", label = "Download File"),
                        h3(""),
                        h3(""),
                        plotlyOutput("boxplot")
                      ),
                      # plot shapefile on leaflet map
                      mainPanel(leafletOutput("map"),
                                h4(""),
                                plotlyOutput("dash"))
                    )),
           tabPanel(
             "Data Table",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   "selectDT",
                   label = h3("Select DataTable"),
                   choices = list(
                     "SVI Value Columns" = 1,
                     "SVI Flag Columns" = 2
                   ),
                   selected = 1
                 ),

                 checkboxGroupInput("show_vals", h4("SVI Values:"),
                                    selected = c('rpl_themes', 'rpl_theme1', 'rpl_theme2', 'rpl_theme3',
                                                 'rpl_theme4'),
                                    choiceValues = c('rpl_themes', 'rpl_theme1', 'rpl_theme2', 'rpl_theme3',
                                                     'rpl_theme4', "epl_pov", "epl_unemp",
                                                     "epl_pci", "epl_nohsdp", "epl_age65", "epl_age17",
                                                     "epl_disabl", "epl_sngpnt", "epl_minrty", "epl_limeng",
                                                     "epl_munit", "epl_mobile", "epl_crowd", "epl_noveh", "epl_groupq"),
                                    choiceNames = c("Overall SVI", "Socio-Economic Theme", "Household Composition & Disability Theme",
                                                    "Minority Status & Language Theme", "Housing & Transportation Theme", "Below Poverty",
                                                    "Unemployed", "Income", "No High School Diploma", "Aged 65+", "Aged 17 or Younger",
                                                    "Civilian with Disability", "Single-Parent Household", " Percent Minority", 'Speak English "Less than Well"',
                                                    "Multi-Unit Structures", "Mobile Homes", "Crowding", "No Vehicle", "Group Quarters"),
                                    inline = TRUE
                 ),
                 checkboxGroupInput("show_flags", h4("SVI Flags:"),
                                    selected = c("f_themes", "f_theme1", "f_theme2", "f_theme3", "f_theme4"),
                                    choiceValues = c("f_total", "f_theme1", "f_theme2", "f_theme3", "f_theme4",
                                                     "f_pov", "f_unemp", "f_pci", "f_nohsdp", "f_age65", "f_age17",
                                                     "f_disabl", "f_sngpnt", "f_minrty", "f_limeng",
                                                     "f_munit", "f_mobile", "f_crowd", "f_noveh", "f_groupq"),
                                    choiceNames = c("Total Flags", "Theme 1 Flag", "Theme 2 Flag", "Theme 3 Flag", "Theme 4 Flag", "Below Poverty",
                                                    "Unemployed", "Income", "No High School Diploma", "Aged 65+", "Aged 17 or Younger",
                                                    "Civilian with Disability", "Single-Parent Household", " Percent Minority", 'Speak English "Less than Well"',
                                                    "Multi-Unit Structures", "Mobile Homes", "Crowding", "No Vehicle", "Group Quarters"),
                                    inline = TRUE
                 )
               ),
               mainPanel(
                 column(div(dataTableOutput("table"), style="color:#000000"), width = 4)
               )
             )
           )
)
