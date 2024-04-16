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
                  infoBox("Published paper", 355, icon = icon("credit-card")),
                  infoBox("Journal", 122, icon = icon("credit-card")),
                  infoBox("Research Institutes", 123, icon = icon("credit-card")),
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
                      imageOutput("Plot3", height = 420)),
                  box(title = "Affiliation",
                      background = "purple",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      imageOutput("Plot4", height = 420))
                  )
                ),
        tabItem(tabName = "EpideinChina",
                h2("GISDDrRef"),
                fluidRow(
                  infoBox("Pubed paper", 355, icon = icon("credit-card")),
                  infoBox("Joual", 122, icon = icon("credit-card"))
                  ),
                fluidRow(
                  box(title = "各省相关文章发表年份",  
                      #background = "purple",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      imageOutput("PlotCN1", height = 320)),
                  box(title = "研究城市",
                      #background = "purple",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      imageOutput("PlotCN2", height = 320))
                  ),
                fluidRow(
                  box(title = "发表期刊",  
                      #background = "purple",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      imageOutput("PlotCN3", height = 550)),
                  box(title = "研究机构",
                      #background = "purple",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      imageOutput("PlotCN4", height = 550))
                )
        ),
        tabItem(tabName = "Data",
                selectInput("Epi_Country", "Choose a target:",
                          c("All","China")),
                selectInput("Pub_Journal", "Choose a target:",
                            c("All","Frontiers in Public Health")),
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
    
