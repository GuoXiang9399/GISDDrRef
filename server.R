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
  function(input, output, session) {  
    # data input
    datasetInput <- function() {  
      read_excel("GISDDref.xlsx")  
    }  
    filteredData <- reactive({  
      data <- datasetInput()  
      if (input$Epi_Country != "All") {  
        data <- data[data$Epi_Country == input$Epi_Country, ]  
      }  
      if (input$Pub_Journal != "All") {  
        data <- data[data$Pub_Journal == input$Pub_Journal, ]  
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
        group_by(Pub_Year_of_publish) %>%  
        summarise(Number=n(), .groups = 'drop') # 加上.groups = 'drop'以避免警告  
      
      # 使用ggplot2创建柱状图  
      ggplot(data_summary, aes(x = Pub_Year_of_publish, y = Number)) +  
        geom_col(color="black", fill="#9BD5E7", linewidth=0.50,width=0.7) +  
        xlab("") +  
        ylab("Number of papers") +  
        scale_x_continuous(breaks = seq(1900, 2100, by = 2)) +  
        scale_y_continuous(expand = c(0,0))+
        theme_classic()+  
        theme(legend.position = "none",
              axis.text.x = element_text(angle=90))
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
              axis.text.x = element_text(angle=90))
    })
    #Output the plot  
    output$Plot3 <- renderPlot({  
      data <- datasetInput()
      data <- group_by(data, Pub_Journal) %>%
        summarise(Number=n())
      ggplot(data) +  
        theme_classic() +  
        ylab("")+xlab("Number of paper")+
        geom_col(aes(x = Number,y = reorder(Pub_Journal, Number),
                     fill=Pub_Journal),
                 color="black", linewidth=0.50,width=0.7) +  
        scale_x_continuous(expand = c(0,0),breaks = c(seq(0,1000,by=1)))+
        theme(legend.position = "none")
    })
    #Output the plot  
    output$Plot4 <- renderPlot({  
      data <- datasetInput()
      data <- group_by(data, Pub_First_affiliation) %>%
        summarise(Number=n())
      ggplot(data) +  
        theme_classic() +  
        ylab("")+xlab("Number of paper")+
        geom_col(aes(x = Number,y = reorder(Pub_First_affiliation, Number),
                     fill = Pub_First_affiliation),
                 color="black",linewidth=0.50,width=0.7) +  
        scale_x_continuous(expand = c(0,0),breaks = c(seq(0,1000,by=1)))+
        theme(legend.position = "none",
              axis.text.y = element_text(size = 6.5))
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
