library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  theme = "style.css",
  
  # Application title
  titlePanel("German Climate Change App"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      uiOutput("widgets")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot", height = 500)
    )
  )
))