# This is a Shiny web application for the arcMS package.
#' You can run the application by clicking
# the 'Run App' button in Rstudio.
#

# Define UI for application
ui <- bs4Dash::dashboardPage(skin = "blue",
                    bs4Dash::dashboardHeader(title = "Parquet MS data converter", titleWidth = 280),
                    bs4Dash::dashboardSidebar(
                       width = 280,
                      bs4Dash::sidebarMenu(
                          id = "inTab",
                          bs4Dash::menuItem("Connect to UNIFI API", tabName = "unificonnect", icon = icon("plug")),
                          bs4Dash::menuItem("Navigate", tabName = "navigate_folders", icon = icon("list"))
                          )
                      ),

                    ## Body content
                    bs4Dash::dashboardBody(
                        shinyjs::useShinyjs(),
                        bs4Dash::tabItems(
                          # tab-content: Connect to UNIFI API to retrieve MS spectrum of markers
                          bs4Dash::tabItem(tabName = "unificonnect",
                              shiny::fluidRow(
                                  bs4Dash::box(width = 12,
                                          title = "Connect to UNIFI API",
                                          status = "primary",
                                          p("To connect to the UNIFI API, the API must first be installed in UNIFI. Create a client ID in the UNIFI API with the following parameters :"),
                                          tags$ul(
                                            tags$li("Client ID: resourceownerclient"),
                                            tags$li("OAuth 2.0 Flow: ResourceOwner"),
                                            tags$li("Response type: token"),
                                            tags$li("Scope: unifi"),
                                            tags$li("State: 1234")
                                          ),
                                          p("Please enter the IP address of the UNIFI computer or server :"),
                                          shiny::textInput("serverurl", label = "Server URL", value = "http://localhost:50034/unifi/v1", placeholder = "http://localhost:50034/unifi/v1"),
                                          shiny::textInput("authorizationurl", label = "Authorization URL", value = "http://localhost:50333/identity/connect/token", placeholder = "http://localhost:50333/identity/connect/token"),
                                          shiny::textInput("unifiuser", label = "User name", value = "administrator", placeholder = "administrator"),
                                          shiny::textInput("unifipwd", label = "Password", value = "administrator", placeholder = "administrator"),
                                          p("Clicking on the following button will open a connection window (Waters UNIFI Identity Server), please enter your credentials to access UNIFI."),
                                          shiny::actionButton("connecttounifi", "Connect to UNIFI API"),
                                          br(), br(),
                                          bs4Dash::infoBoxOutput("connectionInfobox")
                                        )
                    ) #end fluidrow
                ), #end tab
                    bs4Dash::tabItem(tabName = "navigate_folders",
                        shiny::fluidRow(
                                        bs4Dash::box(
                                          title = "Folders",
                                          width = 12,
                                          status = "primary",
                                          collapsible = TRUE,

                                          p(strong("Click on the following button to load folders available in UNIFI")), p("Once the list of folders is displayed, select a folder."),
                                          shiny::actionButton("loadfolders", "Load Folders"),
                                          jsTreeR::jstreeOutput("jstreefolders"),
                                          shiny::htmlOutput("folders")
                                        ),
                                        bs4Dash::box(
                                          title = "Select an Analysis",
                                          width = 12,
                                          status = "primary",
                                          collapsible = TRUE,

                                        DT::dataTableOutput("analysis_datatable")
                                        ),
                                        bs4Dash::box(
                                          title = "Samples",
                                          width = 12,
                                          status = "primary",
                                          collapsible = TRUE,
                                          DT::dataTableOutput("samples_datatable")
                                      ),
                                      bs4Dash::box(
                                        title = "Collect and convert data",
                                        width = 12,
                                        status = "primary",
                                        collapsible = TRUE,
                                        shiny::fluidRow(
                                            column(width = 3,
                                        shiny::selectInput("fileFormat", label = "Choose a file format",
                                                    choices = list("Apache Parquet" = 1, "HDF5" = 2),
                                                    selected = 1)
                                                )
                                                ),
                                            shiny::fluidRow(
                                            column(width = 2,
                                                shinyFiles::shinyDirButton("selected_dir", "Select where to save files", "Select the directory where converted files will be saved")
                                            ),
                                            column(width = 3,
                                                p("Selected download path:"),
                                                align = "center"
                                            ),
                                            column(width = 3,
                                                shiny::verbatimTextOutput("selected_dir", placeholder = TRUE),
                                                align = "center"
                                            )
                                        ),
                                        shiny::checkboxInput(inputId = "overwrite", label = "Overwrite files if already present in selection directory?", value = FALSE),
                                        shiny::actionButton(inputId = "convert_one", label = "Convert one sample selected in the Samples table"),
                                        shiny::actionButton(inputId = "convert_all", label = "Convert all samples from the selected Analysis"),
                                        shiny::textOutput("conversion_end_message")
                                      )
                                  ) # end fluidrow
                          ) # end tab
								) #end tabItems
  ) #end dashboardBody
                    ) #end dashboardPage
