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
      menuItem("Data", tabName = "Data",icon = icon("th"), badgeColor = "green"),
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
                      imageOutput("Plot1", height = 250)),
                  box(title = "Epi_Country",
                      background = "purple",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      imageOutput("Plot2", height = 250))
                  ),
                fluidRow(
                  box(title = "Journal",  
                      background = "purple",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      imageOutput("Plot3", height = 380)),
                  box(title = "Affiliation",
                      background = "purple",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      imageOutput("Plot4", height = 380))
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
    