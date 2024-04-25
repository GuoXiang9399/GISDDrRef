###############################################################################
###############################################################################
###############################################################################
#loading packages
  library(shiny)
  library(shinydashboard)
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(maps)
  library(DT)
  library(ggplot2)
  library(viridis)
###############################################################################
#loading files
  data <- read_excel("Data/GISDDrRef.xlsx")
###############################################################################
#data
  data$Epi_Year_star <- as.numeric(data$Epi_Year_star)
  data$Epi_Year_end <- as.numeric(data$Epi_Year_end)
  data <- mutate(data, Epi_period = (Epi_Year_end - Epi_Year_star + 1) )
  data <- group_by(data, Epi_period) %>%
    summarise(Number=n())
  data <- subset(data, Epi_period>1)
  ggplot(data)+
    geom_col(aes(Epi_period,Number),
                   color="black", fill="#9BD5E7", linewidth=0.50)+
    xlab("") +  ylab("Number of involved papers") +  
    scale_x_continuous(breaks=c(seq(0,100,by=10)))+
    scale_y_continuous(expand = c(0,0),breaks=c(seq(0,100,by=1)))+
    theme_classic()+  
    theme(legend.position = "none",
          axis.text.x = element_text(size=9),
          axis.text.y = element_text(size=10))
###############################################################################
#data
  data$Epi_Year_star <- as.numeric(data$Epi_Year_star)
  data$Epi_Year_end <- as.numeric(data$Epi_Year_end)
  data <- unite(data,Epi_Year_star, Epi_Year_end,col="Epi_during",sep="-" )
  data <- group_by(data, Epi_during) %>% summarise(Number= n()) %>%
    separate(Epi_during,into=c("Epi_Year_star", "Epi_Year_end"),
             sep="-",remove=F)
  data <- subset(data, Epi_during!="NA-NA")
  data$Epi_Year_star <- as.numeric(data$Epi_Year_star)
  data$Epi_Year_end <- as.numeric(data$Epi_Year_end)
  data <- mutate(data, Epi_period = (Epi_Year_end - Epi_Year_star + 1) )
  data <- subset(data, Epi_period>1)
  ggplot(data)+
    geom_segment(aes(
      x=Epi_during,xend=Epi_during,yend = Epi_Year_end,y=Epi_Year_star,
      color=Number),linewidth=2.0)+
    xlab("") +  ylab("Study period") +  
    theme_classic()+  
    theme(legend.position = "none",
          axis.text.x = element_text(angle=90,size=9),
          axis.text.y = element_text(size=10))
###############################################################################
#authro
  data <- data[,c("Pub_CorAuthor1",
                        "Pub_CorAuthor2",
                        "Pub_CorAuthor3",
                        "Pub_CorAuthor4",
                        "Pub_CorAuthor5")]
  data <- pivot_longer(data,cols = c(1:5),
                       names_to = "Label",
                       values_to = "Author")
  data <- group_by(data,Author) %>% summarise(Number=n())
  data <- subset(data,Author!="NA")
  
  