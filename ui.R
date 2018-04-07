
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinythemes)


shinyUI(fluidPage(
  
  theme = shinytheme("cyborg"),
  
  # Application title
  titlePanel("Star Cluster Simulations"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "time",
                  label = c("Select time to display a snapshot", color = "white"),
                  min = 0,
                  max = 18,
                  value = 0),
      selectInput(inputId = "id",
                  label = "Select the ID of the stars escaping from the cluster",
                  choices = c("1797", "3150", "4591", 
                              "5856", "6621", "8011", 
                              "10466", "16750", "19600", 
                              "22770", "23650", "26765", 
                              "26771", "29582", "34497", 
                              "35526", "38199", "46544", 
                              "46586", "47597", "49827", 
                              "50170", "50534", "53000", 
                              "54442", "57698", "57839", 
                              "59117", "59636", "63358"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("distPlot"),
      # textOutput("centroid")
      htmlOutput("centroid"),
      htmlOutput("radius"),
      plotlyOutput("escape")
    )
  )
))
