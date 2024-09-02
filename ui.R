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
      title = "GISDDrRef Dashborad"
    ),
    dashboardSidebar(
      sidebarMenu(
      menuItem("Dashborad - Global", tabName = "Summary", icon = icon("dashboard")),
      menuItem("Dashborad - China", tabName = "EpideinChina", icon = icon("dashboard")),
      #menuItem("Dashborad - Brazil", tabName = "Brazil", icon = icon("dashboard")),
      #menuItem("Dashborad - India", tabName = "India", icon = icon("dashboard")),
      menuItem("Data", tabName = "Data",icon = icon("th")),
      menuItem("About", tabName = "About", icon = icon("cube"))  
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "Summary",
                h2("GISDDrRef"),
                selectInput("Epi_Region", "Choose a region:",
                            c("All","GMS-China","SEA","SASC",
                              "CNA","SA","MED","PHI",
                              "OCE","TWP","SWIO","RAR")),
                selectInput("Epi_Country", "Choose a country/area:",
                            c("All",
                              "China","Vietnam","Thailand","Myanmar","Lao PDR","Myanmar",
                              "Singapore","Malaysia","Indonesia","Philippines","India",
                              "Brazil")),
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
                  box(title = "Year of Publish",  
                      #background = "purple",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      imageOutput("Plot1", height = 280)),
                  box(title = "Country/area Report",
                      #background = "purple",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      imageOutput("Plot2", height = 280))
                  ),
                fluidRow(
                  box(title = "TOP Affiliations",  
                      #background = "purple",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      imageOutput("Plot3", height = 450)),
                  box(title = "TOP Journals",
                      #background = "purple",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      imageOutput("Plot4", height = 450))
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
                  box(title = "Province",  
                      #background = "purple",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      imageOutput("PlotCN1", height = 350)),
                  box(title = "City/Area",
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
        tabItem(tabName = "Brazil",
                h2("GISDDrRef"),
                fluidRow(
                  infoBoxOutput("studyBoxBrazil"),
                  infoBoxOutput("journalBoxBrazil"),
                  infoBoxOutput("affiliationBoxBrazil")
                ),
                fluidRow(
                  box(title = "Province",  
                      #background = "purple",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      imageOutput("PlotBrazil1", height = 350)),
                  box(title = "City/Area",
                      #background = "purple",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      imageOutput("PlotBrazil2", height = 350))
                ),
                fluidRow(
                  box(title = "TOP Journals",  
                      #background = "purple",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      imageOutput("PlotBrazil3", height = 500)),
                  box(title = "TOP Affiliations",
                      #background = "purple",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      imageOutput("PlotBrazil4", height = 500))
                )
        ),
        tabItem(tabName = "India",
                h2("GISDDrRef"),
                fluidRow(
                  infoBoxOutput("studyBoxIndia"),
                  infoBoxOutput("journalBoxIndia"),
                  infoBoxOutput("affiliationBoxIndia")
                ),
                fluidRow(
                  box(title = "Province",  
                      #background = "purple",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      imageOutput("PlotIndia1", height = 350)),
                  box(title = "City/Area",
                      #background = "purple",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      imageOutput("PlotIndia2", height = 350))
                ),
                fluidRow(
                  box(title = "TOP Journals",  
                      #background = "purple",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      imageOutput("PlotIndia3", height = 500)),
                  box(title = "TOP Affiliations",
                      #background = "purple",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      imageOutput("PlotIndia4", height = 500))
                )
        ),
        tabItem(tabName = "Data",
                selectInput("Epi_Region", "Choose a region:",
                            c("All","GMS-China","SEA","SASC",
                              "CNA","SA","MED","PHI",
                              "OCE","TWP","SWIO","RAR")),
                selectInput("Epi_Country", "Choose a country/area:",
                          c("All",
                            "China","Vietnam","Thailand","Myanmar","Lao PDR","Myanmar",
                            "Singapore","Malaysia","Indonesia","Philippines","India",
                            "Brazil")),
                fluidRow(
                  column(width = 12, status = "info", solidHeader = TRUE,
                         box(dataTableOutput("table"), width = NULL))    
                  )
                ),
        tabItem(tabName = "About",
                h3("Introduction"),
                h5("To create a thorough overview of dengue molecular epidemiology research and explore pioneering topics, we systematically retrieved 591 studies for inclusion in our descriptive analysis. This compilation is now accessible through GISDDrRef, an intuitive and use-friendly web application"),
                h5("GISDDrRef features two interactive dashboards, offering an extensive overview of global and Chinese dengue molecular epidemiological research. Each dashboard has been carefully curated to underscore key findings and trends in the field. With this app, we focus on the intricacies of molecular epidemiology, including fragment measurements conducted in each study, the sequencing platform employed, and the accession numbers of sequences reported. For clinical studies, we have provided a concise summary that captures essential aspects: the demographics of target populations, the sources of clinical data, and the sample sizes involved."),
                hr(),
                h3("Contact"),
                a(href = "https://www.smu.edu.cn/rdyjs/info/1003/1355.htm",
                  "Xiang Guo  Institutes of Tropical Medicine, Southern Medical University, China",
                  target = "_blank")
                
        )
      )
    )
  )
    
