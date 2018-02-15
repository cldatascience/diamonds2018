#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(stats)
library(scales)
library(DT)
library(shinythemes)
library(ggthemes)
data(diamonds)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  theme = shinytheme("united"),
  
  # Application title
  headerPanel(
    "Diamond Price Estimator"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      # Specify the colour
      selectInput("colour", 
                  label = "Colour",
                  choices = list("D","E","F","G","H","I","J"),
                  selected = "G"),
      selectInput("cut", 
                  label = "Cut",
                  choices = list("Fair","Good","Very Good","Premium","Ideal"),
                  selected = "Very Good"),
      selectInput("clarity", 
                  label = "Clarity",
                  choices = list("I1","SI2","SI1",
                                 "VS2","VS1","VVS2",
                                 "VVS1","IF"),
                  selected = "VS1"),
      # Specify the carat weight
      sliderInput("carats", "Carats",
                  min = 0.5, max = 2.0, 
                  value = 1.5, step=0.1),
      h5("For more information about diamond quality factors see:"),
      a(href="https://www.gia.edu/diamond-quality-factor", 
             "Diamond Quality Factors")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput(outputId = "scatterPlot1")),
        tabPanel("Compare price by carats", DT::dataTableOutput(outputId = "pricetable")),
        tabPanel("Attribute descriptions", DT::dataTableOutput(outputId = "infotable"))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$scatterPlot1 <- renderPlot({
    diamonds <- filter(diamonds, color==input$colour) %>%
      filter(cut==input$cut) %>%
      filter(clarity==input$clarity)
    model <- lm(price ~ carat, data=diamonds)
    prediction <- predict(model, data.frame(carat=c(input$carats)))
    ggplot(data=diamonds,aes(carat,price)) +
      geom_point(color="darkgray") + geom_smooth(method="lm") +
      geom_vline(xintercept=input$carats) +
      geom_hline(yintercept=prediction) +
      geom_label(aes(x = input$carats, y = prediction, 
                     label = dollar(prediction)), fill = "white") +
      theme_solarized() + scale_colour_solarized("blue")
  })
  
  output$pricetable <- DT::renderDataTable({
    diamonds <- filter(diamonds, color==input$colour) %>%
      filter(cut==input$cut) %>%
      filter(clarity==input$clarity)
    model <- lm(price ~ carat, data=diamonds)
    carats <- seq(0.5,2,0.1)
    prediction <- predict(model, data.frame(carat=carats))
    summarydata <- data.frame(colour=c(input$colour),cut=c(input$cut),
                              clarity=c(input$clarity),carat=carats,
                              price=prediction)
    DT::datatable(data = summarydata, 
                  options = list(pageLength = 10), 
                  rownames = FALSE)
  })
  
  output$infotable <- DT::renderDataTable({
    infodata <- data.frame(attribute=c("Colour", "Cut", "Clarity","Carats"),
                           highest=c("D","Ideal","VVS2","2"),
                           lowest=c("J","Fair","I1","0.5"))
    DT::datatable(data = infodata, 
                  options = list(pageLength = 10), 
                  rownames = FALSE)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

