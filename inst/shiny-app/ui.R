# This is a Shiny web application for the parquetMS package.
#' You can run the application by clicking
# the 'Run App' button in Rstudio.
#

# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "Parquet MS data converter", titleWidth = 280),
                    dashboardSidebar(
                       width = 280,
                      sidebarMenu(
                          id = "inTab",
                          menuItem("Connect to UNIFI API", tabName = "unificonnect", icon = icon("plug")),
                          menuItem("Navigate", tabName = "navigate_folders", icon = icon("list"))
                          )
                      ),

                    ## Body content
                    dashboardBody(
                      tabItems(
                          # tab-content: Connect to UNIFI API to retrieve MS spectrum of markers
                          tabItem(tabName = "unificonnect",
                              fluidRow(
                                  box(width = 12,
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
                                          textInput("serverurl", label = "Server URL", value = "http://localhost:50034/unifi/v1", placeholder = "http://localhost:50034/unifi/v1"),
                                          textInput("authorizationurl", label = "Authorization URL", value = "http://localhost:50333/identity/connect/token", placeholder = "http://localhost:50333/identity/connect/token"),
                                          textInput("unifiuser", label = "User name", value = "administrator", placeholder = "administrator"),
                                          textInput("unifipwd", label = "Password", value = "administrator", placeholder = "administrator"),
                                          p("Clicking on the following button will open a connection window (Waters UNIFI Identity Server), please enter your credentials to access Unifi."),
                                          actionButton("connecttounifi", "Connect to UNIFI API"),
                                          br(), br(),
                                          infoBoxOutput("connectionInfobox")
                                        )
                    ) #end fluidrow
                ), #end tab
                    tabItem(tabName = "navigate_folders",
                        fluidRow(
                                        box(
                                          title = "Folders",
                                          width = 12,
                                          status = "primary",
                                          collapsible = TRUE,

                                          p(strong("Click on the following button to load folders available in Unifi")), p("Once the list of folders is displayed, select a folder."),
                                          actionButton("loadfolders", "Load Folders"),
                                          jstreeOutput("jstreefolders"),
                                          htmlOutput("folders")
                                        ),
                                        box(
                                          title = "Select an Analysis",
                                          width = 12,
                                          status = "primary",
                                          collapsible = TRUE,

                                        dataTableOutput("analysis_datatable")
                                        ),
                                        box(
                                          title = "Samples",
                                          width = 12,
                                          status = "primary",
                                          collapsible = TRUE,
                                          dataTableOutput("samples_datatable")
                                      ),
                                      box(
                                        title = "Collect and convert data",
                                        width = 12,
                                        status = "primary",
                                        collapsible = TRUE,
                                        fluidRow(
                                            column(width = 3,
                                        selectInput("fileFormat", label = "Choose a file format",
                                                    choices = list("Apache Parquet" = 1, "HDF5" = 2),
                                                    selected = 1)
                                                )
                                                ),
                                        actionButton(inputId = "convert_one", label = "Convert one sample selected in the Samples table"),
                                        actionButton(inputId = "convert_all", label = "Convert all samples from the selected Analysis"),
                                        textOutput("conversion_end_message")
                                      )
                                  ) # end fluidrow
                          ) # end tab
								) #end tabItems
  ) #end dashboardBody
                    ) #end dashboardPage
