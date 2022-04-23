#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)
library(ggplot2)
library(shiny)
library(shinycssloaders)
library(shinydashboard)
library(tidyverse)
library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(shinydashboard)
library(plotly)

source('GeneticAlg.R')
source('GeneticAlg2.R')

# Options for Spinner
options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)

data = read.csv("/Users/amrithasubburayan/Downloads/NFL-Team-Construction-main/Datasets/simplified_dataset_v2.csv")
# Define UI for application that draws a histogram




sidebar <- dashboardSidebar(selectInput("position", "Position", choices= data%>%
                                 select(Position) %>% 
                                 distinct()%>%
                                 arrange(Position)%>%
                                 drop_na()
                                 ),
                 
                 selectInput("team", "Team", choices= data%>%
                                 select(Team) %>% 
                                 distinct()%>%
                                 arrange(Team)%>%
                                 drop_na()
                             ),
                 
                 selectInput("PlayerTeamZone" , "PlayerTeamZone", choices = data %>%
                                 select(PlayerTeamZone) %>%
                                 distinct() %>%
                                 group_by(PlayerTeamZone) %>%
                                 
                                 summarise(count = n()) %>%
                                 arrange(desc(count)) %>%
                                 head(100)
                             ),


                sidebarMenu(
                            menuItem("Summary", tabName = "Summary"),
                            menuItem("Exploratory Data Analysis", tabName = "EDA"),
                            menuItem("Clustering", tabName = "ClusteringofPlayers"),
                            menuItem("Genetic Algorithm 1", tabName = "GeneticAlg1"),
                            menuItem("Genetic Algorithm 2", tabName = "GeneticAlg2"),
                            menuItem("Genetic Algorithm 3", tabName = "GeneticAlg3")
                            )
                )


body <- dashboardBody(
    tabItems(
        tabItem(tabName = "Summary",
                h2("Summary")
            ),
        
        # Dinesh Should Work on this
        
        tabItem(tabName = "ClusteringofPlayers",
                h2("Clustering")
        ),
    
    
        # EDA FOR NFL STATISTICS
    
        tabItem(tabName = "EDA",
                h2("EDA"),
                box(
                    plotOutput(outputId = "plot1"),
                    plotOutput(outputId = "plot2"),
                    plotOutput(outputId = "plot3"),
                    plotOutput(outputId = "plot4"),
                    plotOutput(outputId = "plot5"),
                    plotOutput(outputId = "plot6")
                )
        ),
        
        
        #Genetic Algorithm with No ML function
        
        tabItem(tabName = "GeneticAlg1",
                h2("Genetic Alg1"),
    
                    fluidRow(
                        column(12,
                               withSpinner(dataTableOutput(outputId = "genalg1"),type=4)
                               )
                    )
                
        ),
        
        # Sumedh -- > copy the UI of genealg1
        
        tabItem(tabName = "GeneticAlg2",
                h2("GeneticAlg2"),
                fluidRow(
                    column(12,
                           withSpinner(dataTableOutput(outputId = "genalg2"),type=4)
                    )
                )
        ),
        
        # Sanjay -- > copy the UI of genealg1
        
        tabItem(tabName = "GeneticAlg3",
                h2("GeneticAlg3")
        )
    )
)


ui <- dashboardPage(
        dashboardHeader(title = "NFL STATISTICS"),
        sidebar,
        body 
)
    

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Exploratory Data Analsys
    output$plot1 <- renderPlot({
        data %>%
            filter(
                Position == input$position) %>%
            ggplot(aes(x = OverallRating,
                       y = AnnualSalary, height = 50, width = 50)) +
            geom_point(aes(color = factor(OverallRating))) +
            scale_x_continuous(limits = input$OverallRating) +
            theme_minimal()
    })
    
    output$plot2 <- renderPlot({
        data %>%
            filter(
                Position == input$position) %>%
            ggplot(aes(x = OverallRating,
                       y = Team,  color=OverallRating , height = 20, width = 10)) +
            geom_line() +
            scale_x_continuous(limits = input$OverallRating) +
            theme_minimal()
    })
    
    
    output$plot3 <- renderPlot({
        data %>%
            select(PlayerName, Age, Position) %>%
            group_by(PlayerName, Position) %>%
            summarise(avg_age = mean(Age)) %>%
            arrange(desc(avg_age)) %>%
            head(20) %>%
            ggplot(aes(fct_reorder(PlayerName,avg_age), avg_age, fill = Position)) +
            geom_bar(stat = "identity") +
            coord_flip() +
            xlab(NULL) +
            ylab("Average Age of Player based on Position") +
            ggtitle("The Oldest Players in the NFL") +
            theme(text = element_text(family = "Impact"))
        
    })
    output$plot4 <- renderPlot({
        data %>%
            select(PlayerName, Age, Position) %>%
            group_by(PlayerName, Position) %>%
            summarise(avg_age = mean(Age)) %>%
            arrange(desc(avg_age)) %>%
            tail(20) %>%
            ggplot(aes(fct_rev(fct_reorder(PlayerName,avg_age)), avg_age, fill = Position)) +
            geom_bar(stat = "identity") +
            coord_flip() +
            xlab(NULL) +
            ylab("Average Age of Player of the teams") +
            ggtitle("The Youngest Players in the NFL") 
        
    })
    
    output$plot5 <- renderPlot({
        data %>%
            
            filter(
                Team == input$team) %>%
            
            select(PlayerName, OverallRating, Team) %>%
            group_by(PlayerName, Team) %>%
            summarise(avg_rating = mean(OverallRating)) %>%
            arrange(desc(avg_rating)) %>%
            tail(20) %>%
            ggplot(aes(fct_rev(fct_reorder(PlayerName,avg_rating)), avg_rating, fill = Team)) +
            geom_bar(stat = "identity") +
            coord_flip() +
            xlab(NULL) +
            ylab("Average Rating of Player based on Team") +
            ggtitle(" Players Rating in the NFL") 
        
    })
    
    output$plot6 <- renderPlot({    
        
        data %>% 
            
            filter(
                Position == input$position) %>%
            
            group_by(Team) %>% 
            filter(!is.na(Team)) %>%
            summarise(TotalWages = sum(AnnualSalary, na.rm = T),
                      AverageRating = mean(OverallRating, na.rm = T)) %>%
            mutate(ValueForMoney = TotalWages / AverageRating) %>%
            arrange(desc(ValueForMoney)) %>% tail(n= 20) %>%
            mutate(Over70 = ifelse(AverageRating >= 70, "Yes", "No")) %>%
            ggplot(aes(x= reorder(Team, -TotalWages), y= TotalWages, fill = Over70)) +
            geom_col(colour = "black") +
            geom_text(aes(label = scales::dollar(round(ValueForMoney), prefix = "$"), hjust = 1)) +
            scale_fill_manual(values = c("grey", "green")) +
            scale_y_continuous(labels = scales::dollar_format(prefix = "$")) +
            coord_flip() +
            ggtitle("Extracting Value For Money", subtitle = "The 20 clubs who spend the least amount of wages per overall rating point.\nHighlighted clubs have an average rating over 70") +
            
            theme(legend.position = "none")
    })
    
    # Dinesh -- > Plot for clustering
    
    #Genetic ALgorithm function
    
    output$genalg1 <- renderDataTable({ geneticalg(3,"New England Patriots")})

    # Sumedh -- > Call the function for your gen alg using the structre abv
    
    output$genalg2 <- renderDataTable({ geneticalg2(3,"New England Patriots")})
    
    # Sanjay -- > Call the function for your gen alg using the structre abv
    
    
    
}


#It appears that quarterbacks have the most agility at an older age, whereas the youngets players play a large variety of positions.

# Run the application 
shinyApp(ui = ui, server = server)
