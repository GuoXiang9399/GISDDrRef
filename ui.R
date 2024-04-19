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
                  infoBox("Published paper", 348, icon = icon("credit-card")),
                  infoBox("Involved Journal", 128, icon = icon("credit-card")),
                  infoBox("Involved Affiliation", 155, icon = icon("credit-card")),
                  infoBoxOutput("progressBox"),
                  infoBoxOutput("HH2")
                  ),
                fluidRow(
                  box(title = "Year of Publish",  
                      background = "purple",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      imageOutput("Plot1", height = 280)),
                  box(title = "Epi_Country",
                      background = "purple",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      imageOutput("Plot2", height = 280))
                  ),
                fluidRow(
                  box(title = "Journal",  
                      background = "purple",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      imageOutput("Plot3", height = 450)),
                  box(title = "Affiliation",
                      background = "purple",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      imageOutput("Plot4", height = 450))
                  )
                ),
        tabItem(tabName = "EpideinChina",
                h2("GISDDrRef"),
                fluidRow(
                  infoBox("Published paper", 348, icon = icon("credit-card")),
                  infoBox("Involved Journal", 128, icon = icon("credit-card")),
                  infoBox("Involved Affiliation", 155, icon = icon("credit-card"))
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
                h2("GISDDrlearn"),
                h5("Our study has established a reproduceable and comparable global genotyping framework of DENV with contextualizing spatiotemporal epidemiological information before. The defned framework was discriminated with three hierarchical layers of genotype, subgenotype and clade with respective mean pairwise distances 2–6%, 0.8–2%, and ≤0.8%. This framework reveals that the persisting traditional endemic sourcing, the emerging epidemic difusing, and the probably hidden epidemics are the crucial drivers of the rapid global spread of dengue. "),
                h5("GISDDrlearn is a easy-used R Shiny app for assigning DENV sequences to serotype, genotype, subgenotype, and clade based on Random Forest method. As one of the import machine learning model, random forest is an ensemble of secision trees, each built using a subset of the training sample. The structural from show a interesting connection between Random Forest and evolution tree.")
                )
      )
    )
  )
    
