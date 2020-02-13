#install.packages("shiny")
#install.packages("ggplot")
#install.packages("tidyverse")
library(shiny)
library(ggplot2)
library(tidyverse)
source("RFinalProject_ShutimaP/RFinalProject_ShutimaP.R")

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("R for Big Data Final Project: Exploratory Data Analysis of Airbnb Database by Shutima Potivorakun"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 20,
                  max = 40,
                  value = 25),
      
      # Check box for the density estimation
      strong("Density Estimation:"),
      checkboxInput("checkbox", label = "Enable", value = TRUE),
      
      # Select box for the color
      selectInput("select", strong("Color of the Density Function:"), 
                  choices = list("Red" = 1, "Blue" = 2,
                                 "Green" = 3), selected = 1),
      
      # Text for chaning the colors of the scatter plot
      textInput("color", strong("Color of the scatter plot points"), 
                value = "red")
      
    ),
    
    
          
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot"),
      
      # Output: ggplot ----
      plotOutput(outputId = "distPlot2"),
      
      # Output: ggplot ----
      plotOutput(outputId = "distPlot3"),
      
      # Output: ggplot ----
      plotOutput(outputId = "distPlot4"),
      
      # Output: barplot ----
      plotOutput(outputId = "distPlot5"),
      
      # Output: ggplot ----
      plotOutput(outputId = "distPlot6"),
      
      # Output: ggplot ----
      plotOutput(outputId = "distPlot7")
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
    #x    <- faithful$waiting
    #bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    #hist(x, breaks = bins, col = "#75AADB", border = "white",
    #     xlab = "Waiting time to next eruption (in mins)",
    #     main = "Histogram of waiting times", freq = FALSE)
    
    x <- L$price
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Apartment Price",
         main = "Histogram Apartment Price",freq = FALSE)

    
    if (input$checkbox) {
      switch(input$select,
             "1" = lines(density(x), col = 'red', lwd = 2),
             "2" = lines(density(x), col = 'blue', lwd = 2),
             "3" = lines(density(x), col = 'green', lwd = 2))
      
    }
    
    
    
  })
  
  output$distPlot2 <- renderPlot({
    #plot(faithful, type = 'p', pch = 19, col = input$color,
    #     main = "The faithful data")
    p <- L %>%
      group_by(neighbourhood) %>%
      summarize(price = mean(price)) %>%
      ggplot(aes(y = price, x = neighbourhood)) + 
      geom_col() +
      theme(axis.text.x = element_text(size = 8, angle = 90)) +
      ggtitle("Average Apt Price per Neighbourhood")
    p
    
  })
  
  output$distPlot3 <- renderPlot({
    p2 <- tmpRT_new %>%
      group_by(Group.1) %>%
      summarize(x = mean(x)) %>%
      ggplot(aes(y = x, x = Group.1)) + 
      geom_col() +
      theme(axis.text.x = element_text(size = 8, angle = 90)) + ylab("Price") + xlab("Room Type") +
      ggtitle("Average Apt Price per Room Type")
    p2
    
  })
  
  output$distPlot4 <- renderPlot({
    p3 <- tmpBT_new %>%
      group_by(Group.1) %>%
      summarize(x = mean(x)) %>%
      ggplot(aes(y = x, x = Group.1)) + 
      geom_col() +
      theme(axis.text.x = element_text(size = 8, angle = 90)) + ylab("Price") + xlab("Bed Type") +
      ggtitle("Average Apt Price per Bed Type")
    p3
    
  })
  
  output$distPlot5 <- renderPlot({
    count <- table(L$host_total_listings_count)
    countHN <- table(L$host_name)
    barplot(countHN, count, main = "Number of Apt per owner", xlab = "Host Name", ylab = "Host Total Listings count")
    
  })
  
  output$distPlot6 <- renderPlot({
    p4 <- tmpAR_new %>%
      group_by(Group.1) %>%
      summarize(x = mean(x)) %>%
      ggplot(aes(y = x, x = Group.1)) + 
      geom_col() +
      theme(axis.text.x = element_text(size = 8, angle = 90)) + ylab("Price") + xlab("Arrondissement") +
      ggtitle("Average Apt Price per Arrondissement")
    p4
    
  })
  
  output$distPlot7 <- renderPlot({
    p5 <- L %>%
      group_by(zipcode) %>%
      summarize(reviews_per_month = mean(reviews_per_month)) %>%
      ggplot(aes(y = reviews_per_month, x = zipcode)) + 
      geom_col() +
      theme(axis.text.x = element_text(size = 8, angle = 90)) + xlab("Arrondissement") +
      ggtitle("Visit Frequency of the Different Quarters according to Time")
    p5
  })  

  
}


shinyApp(ui = ui, server = server)
