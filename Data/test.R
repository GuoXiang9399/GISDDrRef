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
  
  library(dygraphs)
  
  data_summary <- data %>%  
    group_by(Pub_Publish_year) %>%  
    summarise(Number=n(), .groups = 'drop') # 加上.groups = 'drop'以避免警告  
  data_summary <- subset(data_summary, Pub_Publish_year>0)
  dygraph(data_summary , ylab = "Number of involved papers")%>% dyRangeSelector()
  # 使用ggplot2创建柱状图  
  ggplot(data_summary, aes(x = Pub_Publish_year, y = Number)) +  
    geom_col(color="black", fill="#9BD5E7", linewidth=0.50,width=0.7) +  
    xlab("") +  
    ylab("Number of involved papers") +  
    scale_x_continuous(breaks = seq(1900, 2100, by = 2)) +  
    scale_y_continuous(expand = c(0,0),breaks = c(seq(0,2000,by=5)))+
    theme_classic()+  
    theme(legend.position = "none",
          axis.text.x = element_text(angle=30,size=10),
          axis.text.y = element_text(size=10))
  
  https://primalscheme.com/