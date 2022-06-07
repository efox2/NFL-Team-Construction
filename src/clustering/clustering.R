library(shiny)

# data<- read.csv("C:/Users/T460S/Desktop/SDM II/Project/simplified_dataset_v2.csv")
data =  read.csv("C:/Users/Evan/Documents/Github/NFL-Team-Construction/clustering/simplified_dataset_v2.csv")

head(data,10)
#scaled_data = scale(data[5:7])
#data[5:7] = scaled_data
encode_ordinal <- function(x, order = unique(x)) {
  x=as.numeric(factor(x, levels = order, exclude = NULL))
  x
}

table(data[['Player_Name']], encode_ordinal(data[['Player_Name']]), useNA = "ifany")
data['Player_Name'] = encode_ordinal(data[['Player_Name']])
table(data[['Position']], encode_ordinal(data[['Position']]), useNA = "ifany")
data['Position'] = encode_ordinal(data[['Position']])
data <- subset (data, select = -c(Player_Team_Zone ))
head(data,20)


#write.csv(data,"C:\\Users\\T460s\\Desktop\\data.csv", row.names = FALSE)

vars <- setdiff(names(data),'Team')


#UI for the Clustering
ui <- pageWithSidebar(
  headerPanel(' k-means clustering'),
  sidebarPanel(
    selectInput('xcol', 'X Variable', vars),
    selectInput('ycol', 'Y Variable', vars, selected = vars[[1]]),
    numericInput('clusters', 'Cluster count', 3, min = 1, max = 9)
  ),
  mainPanel(
    plotOutput('plot1')
  )
)


# Server
server <-function(input, output, session) {
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    data[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$plot1 <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
}

shinyApp(ui = ui, server = server)
