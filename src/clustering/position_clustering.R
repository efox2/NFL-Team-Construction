#Position based Clustering
library(shiny)
library(dplyr)

# df =  read.csv("C:/Users/T460S/Desktop/SDM II/Project/data.csv")
df =  read.csv("C:/Users/Evan/Documents/Github/NFL-Team-Construction/clustering/data.csv")

unique_position = list("RE", "CB","HB", "QB", "WR", "LE", "RG",  
              "TE",  "MLB", "LOLB", "DT", "LT", "SS", "C",
              "LG", "FS",  "RT", "ROLB", "K", "FB", "P")

# Filter the data to perform the clustering based on a position 
# position_data = filter(df, Position == 'QB')

vars = list("Player_Name", "Overall_Rating", "Age","Annual_Salary")

#UI for the Clustering
ui = pageWithSidebar(
     headerPanel(' k-means clustering based on a Position'),
     sidebarPanel(selectInput('pos', 'Position', unique_position),
                  selectInput('xcol', 'X Variable', vars),
                  selectInput('ycol', 'Y Variable', vars),
                  numericInput('clusters', 'Cluster count', 3, min = 1, max = 9)),
     mainPanel(plotOutput('plot1')))


# Server
server = function(input, output, session)
  
  {
  # filter position data based on input
  # TODO: THIS IS THE PROBLEM LINE
  position_data = reactive(filter(df, Position == input$pos))
  
  # Combine the selected variables into a new data frame
  selectedData = reactive({position_data[, c(input$xcol, input$ycol)]})
  
  clusters = reactive({kmeans(selectedData(), input$clusters)})
  
  output$plot1 = renderPlot({palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3","#FF7F00", 
                                       "#FFFF33", "#A65628", "#F781BF", "#999999"))
                             par(mar = c(5.1, 4.1, 0, 1))
                             plot(selectedData(),col = clusters()$cluster,pch = 20, cex = 3)
                             points(clusters()$centers, pch = 4, cex = 4, lwd = 4)})
  
  }

shinyApp(ui = ui, server = server)
