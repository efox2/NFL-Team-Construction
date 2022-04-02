#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#customerID,gender,SeniorCitizen,Partner,Dependents,tenure,PhoneService,MultipleLines,
#InternetService,OnlineSecurity,OnlineBackup,DeviceProtection,TechSupport,StreamingTV,StreamingMovies,
#Contract,PaperlessBilling,PaymentMethod,MonthlyCharges,TotalCharges,Churn
library(shiny)
library(tidyverse)

data = read.csv("Datasets/simplified_dataset_v2.csv")
# Define UI for application that draws a histogram
ui <- fluidPage(selectInput(inputId = "Position",
                            label = "Position:",
                            choices = list(RE = "RE",
                                           CB = "CB",
                                           HB = "HB",
                                           QB = "QB",
                                           WR = "WR",
                                           LE = "LE",
                                           SS = "SS",
                                           TE = "TE",
                                           MLB = "MLB",
                                           LOLB = "LOLB",
                                           DT = "DT",
                                           LT = "LT",
                                           C = "C",
                                           LG = "LG",
                                           RG = "RG",
                                           FS = "FS",
                                           RT = "RT",
                                           ROLB = "ROLB",
                                           K = "K",
                                           FB = "FB",
                                           P = "P")),
    
                selectInput(inputId = "PlayerTeamZone",
                            label = "Playerteamzone:", 
                            choices = list(NFC_West = "NFC West",
                                           AFC_East = "AFC East",
                                           NFC_South = "NFC South",
                                           AFC_West = "AFC West",
                                           NFC_East = "NFC East",
                                           NFC_North = "NFC North",
                                           AFC_North = "AFC North")),
                sliderInput(inputId = "OverallRating",
                            label = "OverallRating:",
                            min = min(data$OverallRating),
                            max = max(data$OverallRating),
                            value = c(min(data$OverallRating),
                                          max(data$OverallRating)), 
                            sep = ""),
                sliderInput(inputId = "AnnualSalary",
                            label = "Annualsalary:",
                            min = min(data$AnnualSalary),
                            max = max(data$AnnualSalary),
                            value = c(min(data$AnnualSalary),
                                      max(data$AnnualSalary)), 
                            sep = ""),
                sliderInput(inputId = "Age",
                            label = "Age:",
                            min = min(data$Age),
                            max = max(data$Age),
                            value = c(min(data$Age),
                                      max(data$Age)), 
                            sep = ""),
                submitButton(text = "Create my plot!"),
                plotOutput(outputId = "nameplot"),
                plotOutput(outputId = "lineplot"),
                plotOutput(outputId = "boxplot")
                
                )


# Define server logic required to draw a histogram
server <- function(input, output) {
    output$nameplot <- renderPlot({
        data %>%
            filter(PlayerTeamZone == input$PlayerTeamZone,
                   Position == input$Position) %>%
            ggplot(aes(x = OverallRating,
                       y = AnnualSalary,  col = 'darkgray', border = 'white', height = 50, width = 50)) +
            geom_point() +
            scale_x_continuous(limits = input$OverallRating) +
            theme_minimal()
    })
    
    output$lineplot <- renderPlot({
        data %>%
            filter(PlayerTeamZone == input$PlayerTeamZone, 
                   Position == input$Position) %>%
            ggplot(aes(x = OverallRating,
                       y = Age,  col = 'darkgray', border = 'white', height = 50, width = 50)) +
            geom_line() +
            scale_x_continuous(limits = input$OverallRating) +
            theme_minimal()
    })
    
    
    output$boxplot <- renderPlot({
        data %>%
            filter(PlayerTeamZone == input$PlayerTeamZone,
                   Position == input$Position) %>%
            ggplot(aes(x = OverallRating,
                       col = 'darkgray', border = 'white', height = 50, width = 50)) +
            geom_histogram() +
            scale_x_continuous(limits = input$OverallRating) +
            theme_minimal()
    })
}

   
# Run the application 
shinyApp(ui = ui, server = server)
