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
      menuItem("Affiliations", tabName = "Affiliations", icon = icon("school")),
      menuItem("Journals", tabName = "Journals", icon = icon("book")),
      menuItem("Researchers", tabName = "Authors", icon = icon("at")),
      #menuItem("Time", tabName = "Time", icon = icon("dashboard")),
      menuItem("Explore", tabName = "Explore",icon = icon("th")),
      menuItem("About", tabName = "About", icon = icon("cube"))  
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "Summary",
                #selectInput("Epi_Region", "Choose a region:",
                 #           c("All","GMS-China","SEA","SASC",
                  #            "CNA","SA","MED","PHI",
                   #           "OCE","TWP","SWIO","RAR")),
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
                      imageOutput("Plot3p", height = 480)),
                  box(title = "TOP Journals",
                      #background = "purple",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      imageOutput("Plot4p", height = 480))
                  )
                
                ),
        tabItem(tabName = "EpideinChina",
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
        tabItem(tabName = "Affiliations",
                fluidRow(
                  box(
                    width = 12,
                    title = "Involved Affiliations",  
                      #background = "purple",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      imageOutput("Plot3", height = 1000))
                )
        ),
        tabItem(tabName = "Journals",
                fluidRow(
                  box(
                    width = 12,
                    title = "Involved Journals",
                      #background = "purple",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      imageOutput("Plot4", height = 1200))
                )
        ),
        tabItem(tabName = "Authors",
                  fluidRow(
                    box(
                      width = 12,
                      title = "TOP Researchers",
                      #background = "purple",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      imageOutput("Plot7", height = 1200))
                  )
        ),
        tabItem(tabName = "Time",
                fluidRow(
                  column(12,
                         box(width=12,
                             title = "Study period",
                             solidHeader = TRUE,
                           imageOutput("Plot5",height = 1200)
                           ),
                         box(width=6,
                             title = "Length of study period",
                             solidHeader = TRUE,
                             imageOutput("Plot6",height = 600)
                         )
                  )
                )
          
        ),
        tabItem(tabName = "Explore",
                fluidRow(
                  column(12,
                    column(
                      width = 9, status = "info", solidHeader = TRUE,
                      box(dataTableOutput("table"), width = NULL)
                      ),
                    column(
                      width = 3,
                      box(width = "6 col-md-12",
                        selectInput("Epi_Region", "Choose a region:",
                                    c("All","Worldwide","GMS-China","SEA","SASC",
                                      "CNA","SA","MED","PHI",
                                      "OCE","TWP","SWIO","RAR")),
                        selectInput("Epi_Country", "Choose a country/area:",
                                    c("All",
                                      "China","Vietnam","Thailand","Myanmar","Lao PDR","Myanmar",
                                      "Singapore","Malaysia","Indonesia","Philippines","India",
                                      "Brazil")),
                        radioButtons("Type_Mole","Choose involved molecular epidemic study",
                                     c("All","T","F")),
                        radioButtons("Type_Epide","Choose involved epidemic study",
                                    c("All","T","F")),
                        radioButtons("Type_CasRep","Choose if Case Report article",
                                     c("All","T","F")),
                        radioButtons("Type_Review","Choose if Review article",
                                     c("All","T","F"))
                        )
                      )
                   )
                  )
                ),
        tabItem(tabName = "About",
                h3("Introduction"),
                h4("To create a thorough overview of dengue molecular epidemiology research and explore pioneering topics, we systematically retrieved 591 studies for inclusion in our descriptive analysis. This compilation is now accessible through GISDDrRef, an intuitive and use-friendly web application"),
                h4("GISDDrRef features two interactive dashboards, offering an extensive overview of global and Chinese dengue molecular epidemiological research. Each dashboard has been carefully curated to underscore key findings and trends in the field. With this app, we focus on the intricacies of molecular epidemiology, including fragment measurements conducted in each study, the sequencing platform employed, and the accession numbers of sequences reported. For clinical studies, we have provided a concise summary that captures essential aspects: the demographics of target populations, the sources of clinical data, and the sample sizes involved."),
                hr(),
                h3("Last Update"),
                h4("2025-02-17"),
                hr(),
                h3("Contact"),
                
                a(href = "https://www.smu.edu.cn/rdyjs/info/1003/1355.htm",
                  "Xiang Guo",
                  target = "_blank"),
                h5("School of Public Health, Southern Medical University, China"),
                h5("School of Basic Medical Science, Henan University, China")

        )
      )
    )
  )
    
