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
  function(input, output, session) {  
    # data input
    datasetInput <- function() {  
      read_excel("GISDDrRef.xlsx")  
    }  
    filteredData <- reactive({  
      data <- datasetInput()  
      if (input$Epi_Country != "All") {  
        data <- data[data$Epi_Country == input$Epi_Country, ]  
      }  
      if (input$Pub_Affiliation != "All") {  
        data <- data[data$Pub_Affiliation == input$Pub_Affiliation, ]  
      }  
      return(data)  
    })
    # Output the filtered data as a table  
    output$table <- renderDataTable({  
      data <- filteredData()  
      datatable(data, 
                extensions = 'Buttons',
                options = list(
                  dom = 'Bfrtip',  
                  buttons = list(  
                    'copy', 'csv', 'excel', 'pdf'  
                    ),
                  scrollX = TRUE, # 启用水平滚动 
                  scrollCollapse = TRUE, # 仅在需要时才显示滚动条  
                  paging = FALSE,
                  columnDefs = list(     # 设置列的定义  
                    list(visible = FALSE, targets = 0),  # 隐藏第一列（通常是行索引） 
                    list(visible = FALSE, targets = 1),  # 设置第二列的宽度为100像素  
                    list(width = '150px', targets = 2),  # 设置第二列的宽度为100像素 
                    list(width = '150px', targets = 3),  # 设置第二列的宽度为100像素  
                    list(width = '150px', targets = 4),  # 设置第二列的宽度为100像素  
                    list(width = '150px', targets = 5),  # 设置第二列的宽度为100像素  
                    list(width = '150px', targets = 6),  # 设置第二列的宽度为100像素  
                    list(width = '15px', targets = 7),  # 设置第二列的宽度为100像素  
                    list(width = '15px', targets = 8)  )
                  )
                )  
    }, server = FALSE)
    
    #Output the plot  
    output$Plot1 <- renderPlot({  
      data <- datasetInput()  
      data_summary <- data %>%  
        group_by(Pub_Publish_year) %>%  
        summarise(Number=n(), .groups = 'drop') # 加上.groups = 'drop'以避免警告  
      
      # 使用ggplot2创建柱状图  
      ggplot(data_summary, aes(x = Pub_Publish_year, y = Number)) +  
        geom_col(color="black", fill="#9BD5E7", linewidth=0.50,width=0.7) +  
        xlab("") +  
        ylab("Number of papers") +  
        scale_x_continuous(breaks = seq(1900, 2100, by = 2)) +  
        scale_y_continuous(expand = c(0,0))+
        theme_classic()+  
        theme(legend.position = "none",
              axis.text.x = element_text(angle=30,size=9))
    })
    #Output the plot  
    output$Plot2 <- renderPlot({  
      data <- datasetInput()
      data <- group_by(data,Epi_Country) %>%
        summarise(Number=n())
      ggplot(data) +  
        theme_classic() + xlab("") + ylab("Number of paper") +
        geom_col(aes(x = reorder(Epi_Country,-Number),y = Number,
                     fill = Epi_Country),
                 color="black",linewidth=0.50,width=0.7) +  
        scale_y_continuous(expand = c(0,0))+
        theme(legend.position = "none",
              axis.text.x = element_text(angle=30,size=9))
    })
    #Output the plot  
    output$Plot3 <- renderPlot({  
      data <- datasetInput()
      data <- subset(data, Pub_Journal!="NA")
      data <- group_by(data, Pub_Journal) %>%
        summarise(Number=n())
      ggplot(data) +  
        theme_classic() +  
        ylab("")+xlab("Number of paper")+
        geom_col(aes(x = Number,y = reorder(Pub_Journal, Number),
                     fill=Pub_Journal),
                 color="black", linewidth=0.50,width=0.7) +  
        scale_x_continuous(expand = c(0,0),#limits = c(0,10),
                           breaks = c(seq(0,1000,by=1)))+
        theme(legend.position = "none",
              axis.text.x.bottom = element_text(size=8))
    })
    #Output the plot  
    output$Plot4 <- renderPlot({  
      data <- datasetInput()
      data <- subset(data, Pub_Affiliation!="NA")
      data <- group_by(data, Pub_Affiliation) %>%
        summarise(Number=n())
      ggplot(data) +  
        theme_classic() +  
        ylab("")+xlab("Number of paper")+
        geom_col(aes(x = Number,y = reorder(Pub_Affiliation, Number),
                     fill = Pub_Affiliation),
                 color="black",linewidth=0.50,width=0.7) +  
        scale_x_continuous(expand = c(0,0),breaks = c(seq(0,1000,by=2)))+
        theme(legend.position = "none",
              axis.text.y = element_text(size = 6.5),
              axis.text.x.bottom = element_text(size=8))
    })
    #Output the plot  
    output$PlotCN1 <- renderPlot({  
      data <- datasetInput()  
      data <- subset(data, Epi_Country=="China")
      data <- subset(data, Epi_ProvinceCN!="NA")
      data_summary <- data %>%
        unite(Epi_ProvinceCN,Pub_Publish_year, col="Abc",sep="%%%") %>%
        group_by(Abc) %>%  
        summarise(Number=n(), .groups = 'drop') %>% # 加上.groups = 'drop'以避免警告  
        separate(Abc,into=c("Epi_ProvinceCN","Pub_Publish_year"),sep="%%%")
      # 使用ggplot2创建柱状图  
      ggplot(data_summary, aes(x = Pub_Publish_year, y = Epi_ProvinceCN)) +  
        geom_point(aes(size=Number,fill=Epi_ProvinceCN),color="black",shape=21 )+
        xlab("") +  ylab("") +  
        theme_bw()+  
        scale_y_discrete(limits=c(
          "北京","河北",
          "湖北","贵州","安徽","陕西","上海",
          "香港","山东","湖南","江西","江苏",
          "澳门","河南","广西","海南","台湾",
          "福建","云南","浙江","广东","全国范围"
          ))+
        scale_fill_viridis(discrete = T)+
        theme(legend.position = "none",
              axis.text.x = element_text(angle=30,size=10),
              axis.text.y = element_text(size=11))
    })
    #Output the plot  
    output$PlotCN2 <- renderPlot({  
      data <- datasetInput()
      data <- subset(data, Epi_Country=="China")
      data <- subset(data, Epi_CityCN!="NA")
      data <- group_by(data,Epi_CityCN) %>%
        summarise(Number=n())
      ggplot(data) +  
        theme_classic() + xlab("") + ylab("Number of paper") +
        geom_col(aes(x = reorder(Epi_CityCN,-Number),y = Number,
                     fill = Epi_CityCN),
                 color="black",linewidth=0.50,width=0.7) +  
        scale_y_continuous(expand = c(0,0),breaks = c(seq(0,1000,by=3)))+
        scale_fill_viridis(discrete = T)+
        theme(legend.position = "none",
              axis.text.x = element_text(angle=90,size=10))
    })
    #Output the plot  
    output$PlotCN3 <- renderPlot({  
      data <- datasetInput()
      data <- subset(data, Epi_Country=="China")
      data <- subset(data, Pub_Journal!="NA")
      data <- group_by(data, Pub_Journal) %>%
        summarise(Number=n())
      ggplot(data,aes(x = Number,y = reorder(Pub_Journal, Number),
                      fill=Pub_Journal)) +  
        theme_classic() +  
        ylab("")+xlab("Number of paper")+
        geom_segment(aes(yend=reorder(Pub_Journal, Number)),xend=0)+
        geom_point(aes(size=Number),
                   color="black", shape=21,linewidth=0.50,width=0.7) +  
        scale_x_continuous(#expand = c(0,0),#limits = c(0,10),
                           breaks = c(seq(0,1000,by=1)))+
        scale_fill_viridis(discrete = T)+
        theme(legend.position = "none",
              axis.ticks.y = element_blank(),
              axis.text.y = element_text(size = 12),
              axis.text.x.bottom = element_text(size=8))
    })
    #Output the plot  
    output$PlotCN4 <- renderPlot({  
      data <- datasetInput()
      data <- subset(data, Epi_Country=="China")
      data <- subset(data, Pub_AffiliationCN!="NA")
      data <- group_by(data, Pub_AffiliationCN) %>%
        summarise(Number=n())
      ggplot(data) +  
        theme_classic() +  
        ylab("")+xlab("Number of paper")+
        geom_col(aes(x = Number,y = reorder(Pub_AffiliationCN, Number),
                     fill = Pub_AffiliationCN),
                 color="black",linewidth=0.50,width=0.7) +  
        scale_x_continuous(expand = c(0,0),breaks = c(seq(0,1000,by=2)))+
        scale_fill_viridis(discrete = T)+
        theme(legend.position = "none",
              axis.text.y = element_text(size = 12),
              axis.text.x.bottom = element_text(size=8))
    })
    #Download handler for the filtered data  
    output$downloadData <- downloadHandler(  
      filename = function() {  
        paste("filtered_data", ".csv", sep = "")  
      },  
      content = function(file) {  
        write.csv(filteredData(), file, row.names = FALSE)  
    })  
  }
