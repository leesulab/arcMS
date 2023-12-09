#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com/
#

library(httr)
library(js)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyjqui)
library(shinycssloaders)
library(shinyWidgets)
library(plotly)
library(DT)
library(sortable)
library(dplyr)
library(dtplyr)
library(data.table)
library(parquetMS)
#
# library(RProtoBuf)
# source("decodeVarint.R")
# source("all_functions.R")
# library(promises)
# library(future)
# library("arrow")


# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "red",
                    dashboardHeader(title = "Identification App"),
                    dashboardSidebar(
                      sidebarMenu(
                        # menuItem("Input your MzML files", tabName = "inputmzml", icon = icon("file-import")),
                        menuItem("Analysis ID", tabName = "analysisID", icon = icon("file-import"))

                      )
                    ),
                    ## Body content
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "analysisID",
                                fluidRow(
                                  box(
                                    title = "Connexion to API",
                                    width = 12,
                                    #Color of the head of the box (primary=Blue,success=Green,info=Blue,warning=Orange,danger=Red)
                                    status = "danger",
                                    #solidHeader = TRUE,
                                    collapsible = TRUE,
                                    actionButton(inputId = "connexion", label = "Create connexion"),
                                    textOutput("connexion_status"),

                                  ),
                                  box(
                                    title = "folder",
                                    width = 12,
                                    #Color of the head of the box (primary=Blue,success=Green,info=Blue,warning=Orange,danger=Red)
                                    status = "danger",
                                    #solidHeader = TRUE,
                                    collapsible = TRUE,
                                    actionButton(inputId = "processing_2", label = "Folder")
                                  ),
                                  box(
																		title = "Choose your Folder",
																		width = 12,
																		collapsible = TRUE,
                                    DTOutput("tablefolder")
                                  ),
                                  box(
                                    title = "Analysis ID",
                                    width = 12,
                                    collapsible = TRUE,
                                    DTOutput("tableanalysisId")
                                  ),

                                  box(
                                    title = "Sample",
                                    width = 12,
                                    #Color of the head of the box (primary=Blue,success=Green,info=Blue,warning=Orange,danger=Red)
                                    status = "danger",
                                    #solidHeader = TRUE,
                                    collapsible = TRUE,
                                    DTOutput("tableRow_3")

                                  )
                                  ,



                                box(
                                  title = "Collect data",
                                  width = 12,
                                  #Color of the head of the box (primary=Blue,success=Green,info=Blue,warning=Orange,danger=Red)
                                  status = "danger",
                                  #solidHeader = TRUE,
                                  collapsible = TRUE,
                                  textOutput("sampleid_text"),
                                  actionButton(inputId = "collect_one", label = "collect one sample from the selected analysis"),
                                  actionButton(inputId = "collect_all", label = "collect all sample of the selected analysis")

                                )
                                ,
                                ) #End FluidRow
                        ) #End TabItem


								) #end tabItems
                                                                                                                              ) #end dashboardBody
                    ) #end dashboardPage
