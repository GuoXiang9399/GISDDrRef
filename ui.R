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
                  box(title = "TOP Affiliations",  
                      background = "purple",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      imageOutput("Plot3", height = 320)),
                  box(title = "TOP Journals",
                      background = "purple",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      imageOutput("Plot4", height = 320))
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
                  box(title = "值得关注的专业期刊",  
                      #background = "purple",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      imageOutput("PlotCN3", height = 500)),
                  box(title = "值得关注的研究机构",
                      #background = "purple",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      imageOutput("PlotCN4", height = 500))
                )
        ),
        tabItem(tabName = "Data",
                selectInput("Epi_Country", "Choose a country/area:",
                          c("All",
                            "China","Vietnam","Thailand","Myanmar",
                            "Brazil")),
                selectInput("Pub_Affiliation", "Choose a affiliation:",
                            c("All",
                              "Chinese Center for Disease Control and Prevention",
                              "Guangdong Provincial Center for Disease Control and Prevention",
                              "Guangdong Guangzhou Center for Disease Control and Prevention",
                              "Zhejiang Provincial Center for Disease Control and Prevention",
                              "Fujian Provincial Center for Disease Control and Prevention",
                              "Southern Medical University",
                              "Chinese Academy of Medical Sciences & Peking Union Medical College",
                              "Yunnan Institute of Parasitic Diseases")),
                fluidRow(
                  column(width = 12, status = "info", solidHeader = TRUE,
                         box(dataTableOutput("table"), width = NULL)),
                  column(width = 12,
                         box(tabItem(tabName = "downloadData")))     
                  )
                ),
        tabItem(tabName = "About",
                h2("GISDDrRef"),
                h5("Our study has established a reproduceable and comparable global genotyping framework of DENV with contextualizing spatiotemporal epidemiological information before. The defned framework was discriminated with three hierarchical layers of genotype, subgenotype and clade with respective mean pairwise distances 2–6%, 0.8–2%, and ≤0.8%. This framework reveals that the persisting traditional endemic sourcing, the emerging epidemic difusing, and the probably hidden epidemics are the crucial drivers of the rapid global spread of dengue. ")
                )
      )
    )
  )
    
