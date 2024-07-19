###############################################################################
###############################################################################
###############################################################################
  library(shiny)
  library(shinydashboard)
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(DT)
  library(ggplot2)
  library(viridis)
###############################################################################
# Define UI for application that draws a histogram
  dashboardPage(
    skin = "purple",
    dashboardHeader(
      title = "GISDDrRef"
    ),
    dashboardSidebar(
      sidebarMenu(
      menuItem("Summary", tabName = "Summary", icon = icon("dashboard")),
      menuItem("Epide in China", tabName = "EpideinChina", icon = icon("dashboard")),
      menuItem("Data", tabName = "Data",icon = icon("th")),
      menuItem("About", tabName = "About", icon = icon("cube"))  
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "Summary",
                h2("GISDDrRef"),
                fluidRow(
                  infoBoxOutput("studyBox"),
                  infoBoxOutput("journalBox"),
                  infoBoxOutput("affiliationBox")
                  ),
                fluidRow(
                  infoBoxOutput("EpideBox"),
                  infoBoxOutput("MoleBox"),
                  infoBoxOutput("CliniBox")
                  ),
                fluidRow(
                  box(title = "TOP Affiliations",  
                      background = "purple",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      imageOutput("Plot3", height = 420)),
                  box(title = "TOP Journals",
                      background = "purple",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      imageOutput("Plot4", height = 420))
                  ),
                fluidRow(
                  box(title = "Year of Publish",  
                      background = "purple",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      imageOutput("Plot1", height = 280)),
                  box(title = "Country/area Report",
                      background = "purple",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      imageOutput("Plot2", height = 280))
                  ),
                fluidRow(
                  box(title = "Study period start - end",  
                      background = "purple",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      imageOutput("Plot5", height = 280)),
                  box(title = "Study period length",
                      background = "purple",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      imageOutput("Plot6", height = 280))
                 )
                ),
        tabItem(tabName = "EpideinChina",
                h2("GISDDrRef"),
                fluidRow(
                  infoBoxOutput("studyBoxCN"),
                  infoBoxOutput("journalBoxCN"),
                  infoBoxOutput("affiliationBoxCN")
                ),
                fluidRow(
                  box(title = "省级范围研究发表",  
                      #background = "purple",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      imageOutput("PlotCN1", height = 350)),
                  box(title = "值得关注的研究范围/城市",
                      #background = "purple",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      imageOutput("PlotCN2", height = 350))
                  ),
                fluidRow(
                  box(title = "TOP Journals",  
                      #background = "purple",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      imageOutput("PlotCN3", height = 500)),
                  box(title = "TOP Affiliations",
                      #background = "purple",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      imageOutput("PlotCN4", height = 500))
                )
        ),
        tabItem(tabName = "Data",
                selectInput("Epi_Region", "Choose a region:",
                            c("All",
                              "GMS-China",
                              "SEA",
                              "CNA",
                              "SA",
                              "SASC")),
                selectInput("Epi_Country", "Choose a country/area:",
                          c("All",
                            "China","Vietnam","Thailand","Myanmar",
                            "Brazil")),
                fluidRow(
                  column(width = 12, status = "info", solidHeader = TRUE,
                         box(dataTableOutput("table"), width = NULL))    
                  )
                ),
        tabItem(tabName = "About",
                h2("GISDDrRef"),
                h5(""),
                h5("Database Operation Manager: Xiang Guo"),
                h5(""),
                h5("Address: Southern Medical University"),
                h5(""),
                h5("Phone: 022-61648598")
                )
      )
    )
  )
    
