# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(plotly)
library(dplyr)

# function to load data, find centroids, and plot
for(i in 0:18) {
  
  # load data
  pad_int<-function(n, scale){
    out_string<-paste(10*scale + n, "00", sep='')
    out_string<-substr(out_string, 2, nchar(out_string))
    return(out_string)
  }
  
  infile <- paste0("Data/c_", pad_int(i, 10), ".csv")
  a <- read.csv(infile)
  a$time <- rep(i, nrow(a)) # for combining afterwords
  b <- assign(paste0("c_", pad_int(i, 10)), a)
  
  # finding cluster centroids
  find_cm <- function(snap)
  {
    x_cm <- mean(snap$x)
    y_cm <- mean(snap$y)
    z_cm <- mean(snap$z)
    cm <- c(x_cm, y_cm, z_cm)
    return(cm)
  }
  
  # plotting
  pl <- plot_ly(b, x = ~x, y = ~y, z = ~z, 
                color = ~id, 
                colors = 'YlOrBr', 
                size = 5, sizes = c(1, 1),
                type = 'scatter3d',
                showscale = FALSE,
                showlegend = FALSE) %>%
    
    add_markers(text = ~paste('ID: ', id)) %>%
    
    layout(scene = list(xaxis = list(title = 'x', color = 'white'),
                        yaxis = list(title = 'y', color = 'white'),
                        zaxis = list(title = 'z', color = 'white')),
           paper_bgcolor='#060606',
           showlegend = FALSE,
           legend=list(color = "white")) %>%
    
    add_trace(x = find_cm(b)[1], 
              y = find_cm(b)[2], 
              z = find_cm(b)[3], 
              color = "#33FFFF", 
              name = "Cluster Centroid", 
              mode = 'markers', 
              marker = list(size = 20))
  
  # object to plot stars
  assign(paste0("pl", i), pl)
  
  # object to plot the centroid
  assign(paste0("centroid", i), find_cm(b))
  
  
  # radius of the cluster
  find_reff <- function(snap) {
    cm <- find_cm(snap)
    x <- snap$x - cm[1]
    y <- snap$y - cm[2]
    z <- snap$z - cm[3]
    r2 <- x*x + y*y + z*z
    return(sqrt(median(r2)))
  }
  
  assign(paste0("radius", i), find_reff(b))
  
}

# create a data frame of stars escaping from the cluster
id.esc <- c_0000$id[!(c_0000$id %in% c_1800$id)]

c_global <- rbind(c_0000, c_0100, c_0200, c_0300, c_0400,
                  c_0500, c_0600, c_0700, c_0800, c_0900,
                  c_1000, c_1100, c_1200, c_1300, c_1400, 
                  c_1500, c_1600, c_1600, c_1700, c_1800)

c_esc <- c_global %>%
  filter(id %in% id.esc) %>%
  arrange(id)




# shinyServer
shinyServer(function(input, output) {
  
  # generate a 3D plot based on input$time from ui.R
  plot.data <- reactive({
    switch(paste0("pl", input$time),
           "pl0" = pl0, "pl1" = pl1, "pl2" = pl2,
           "pl3" = pl3, "pl4" = pl4, "pl5" = pl5,
           "pl6" = pl6, "pl7" = pl7, "pl8" = pl8,
           "pl9" = pl9, "pl10" = pl10, "pl11" = pl11,
           "pl12" = pl12, "pl13" = pl13, "pl14" = pl14,
           "pl15" = pl15, "pl16" = pl16, "pl17" = pl17,
           "pl18" = pl18)
  })
  
  # plot a centroid
  plot.centroid <- reactive({
    switch(paste0("centroid", input$time),
           "centroid0" = centroid0, "centroid1" = centroid1, "centroid2" = centroid2,
           "centroid3" = centroid3, "centroid4" = centroid4, "centroid5" = centroid5,
           "centroid6" = centroid6, "centroid7" = centroid7, "centroid8" = centroid8,
           "centroid9" = centroid9, "centroid10" = centroid10, "centroid11" = centroid11,
           "centroid12" = centroid12, "centroid13" = centroid13, "centroid14" = centroid14,
           "centroid15" = centroid15, "centroid16" = centroid16, "centroid17" = centroid17,
           "centroid18" = centroid18)
  })
  
  output$distPlot <- renderPlotly({
    
    plot.data()
    
  })
  
  
  
  
  # print the cordinates of the centroid
  output$centroid <- renderText(
    
    paste("The Centroid is located at",
          paste("x = ", plot.centroid()[1]), 
          paste("y = ", plot.centroid()[2]), 
          paste("z = ", plot.centroid()[3]), 
          sep = '<br/>')
    
  )
  
  # print the radius
  print.radius <- reactive({
    switch(paste0("radius", input$time),
           "radius0" = radius0, "radius1" = radius1, "radius2" = radius2,
           "radius3" = radius3, "radius4" = radius4, "radius5" = radius5,
           "radius6" = radius6, "radius7" = radius7, "radius8" = radius8,
           "radius9" = radius9, "radius10" = radius10, "radius11" = radius11,
           "radius12" = radius12, "radius13" = radius13, "radius14" = radius14,
           "radius15" = radius15, "radius16" = radius16, "radius17" = radius17,
           "radius18" = radius18)
  })
  
  output$radius <- renderText(
    
    paste("The radius of the cluster is",
          paste(round(print.radius(), digits = 4)))
    
  )
  
  # plot the stars escaping from the cluster
  escape.plot <- reactive({
    c <- c_esc %>% 
      filter(id == input$id)
    
    plot_ly(c, x = ~x, y = ~y, z = ~z,
            colors = 'YlOrBr',
            size = 3, sizes = c(3, 3),
            mode = "lines+markers",
            type = 'scatter3d') %>%
      add_markers(text = ~paste('time: ', time)) %>%
      add_trace(x = subset(c, time==0)[1, 1],
                y = subset(c, time==0)[1, 2],
                z = subset(c, time==0)[1, 3],
                color = "red", 
                name = "time = 0",
                mode = "markers",
                marker = list(size = 5)) %>%
      layout(scene = list(xaxis = list(title = 'x', color = 'white'),
                          yaxis = list(title = 'y', color = 'white'),
                          zaxis = list(title = 'z', color = 'white')),
             paper_bgcolor='#060606',
             showlegend = TRUE,
             legend=list(color = "white"))
  })
  
  output$escape <- renderPlotly(
    escape.plot()
  )
  
})
