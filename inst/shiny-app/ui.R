# This is a Shiny web application for the parquetMS package.
#' You can run the application by clicking
# the 'Run App' button in Rstudio.
#

# Define UI for application
ui <- shinydashboard::dashboardPage(skin = "blue",
                    shinydashboard::dashboardHeader(title = "Parquet MS data converter", titleWidth = 280),
                    shinydashboard::dashboardSidebar(
                       width = 280,
                      shinydashboard::sidebarMenu(
                          id = "inTab",
                          shinydashboard::menuItem("Connect to UNIFI API", tabName = "unificonnect", icon = icon("plug")),
                          shinydashboard::menuItem("Navigate", tabName = "navigate_folders", icon = icon("list"))
                          )
                      ),

                    ## Body content
                    shinydashboard::dashboardBody(
                        shinyjs::useShinyjs(),
                        shinydashboard::tabItems(
                          # tab-content: Connect to UNIFI API to retrieve MS spectrum of markers
                          shinydashboard::tabItem(tabName = "unificonnect",
                              shiny::fluidRow(
                                  shinydashboard::box(width = 12,
                                          title = "Connect to UNIFI API to retrieve MS spectra of markers",
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
                                          p("Clicking on the following button will open a connection window (Waters UNIFI Identity Server), please enter your credentials to access Unifi."),
                                          shiny::actionButton("connecttounifi", "Connect to UNIFI API"),
                                          br(), br(),
                                          shinydashboard::infoBoxOutput("connectionInfobox")
                                        )
                    ) #end fluidrow
                ), #end tab
                    shinydashboard::tabItem(tabName = "navigate_folders",
                        shiny::fluidRow(
                                        shinydashboard::box(
                                          title = "Folders",
                                          width = 12,
                                          status = "primary",
                                          collapsible = TRUE,

                                          p(strong("Click on the following button to load folders available in Unifi")), p("Once the list of folders is displayed, select a folder."),
                                          shiny::actionButton("loadfolders", "Load Folders"),
                                          jsTreeR::jstreeOutput("jstreefolders"),
                                          shiny::htmlOutput("folders")
                                        ),
                                        shinydashboard::box(
                                          title = "Select an Analysis",
                                          width = 12,
                                          status = "primary",
                                          collapsible = TRUE,

                                        DT::dataTableOutput("analysis_datatable")
                                        ),
                                        shinydashboard::box(
                                          title = "Samples",
                                          width = 12,
                                          status = "primary",
                                          collapsible = TRUE,
                                          DT::dataTableOutput("samples_datatable")
                                      ),
                                      shinydashboard::box(
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
                                        shiny::actionButton(inputId = "convert_one", label = "Convert one sample selected in the Samples table"),
                                        shiny::actionButton(inputId = "convert_all", label = "Convert all samples from the selected Analysis"),
                                        shiny::textOutput("conversion_end_message")
                                      )
                                  ) # end fluidrow
                          ) # end tab
								) #end tabItems
  ) #end dashboardBody
                    ) #end dashboardPage
