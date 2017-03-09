# Load the shiny, ggplot2, and dplyr libraries
library("tidyr")
library("maps")
library("shiny")
library("ggplot2")
library("dplyr")
library("plotly")

income.data <- read.csv("data/Income_Employment_2015.csv", stringsAsFactors = FALSE)

# It filter the information about state where user choose.
my.server <- function(input, output) {
  filtered <- reactive({
    new <- filter(long.join, input$state == State)
    print(input$state)
    
    return(new)
  })
  
# This paragraph will just make sure that the region user choose will get some outputs.
  filtered.map <- reactive({
    new.table <- State.join
    if (input$radio.region != "All") {
      new.table <- filter(State.join, Region %in% input$radio.region)
    }
    return(new.table)
  })
  
  
  output$description1 <- renderText({
    
    message <- paste0("This graph shows the level of ", input$labor.type," in 2015 in ",input$radio.region, " part. From lightest color to darkest color,
                      it shows the state which has least percentage of ",input$labor.type, " to state which has largest percentage of ", input$labor.type)
    
    return(message)
  })
  
  output$description2 <- renderText({
    
    message <- paste0("This bar graph shows the percentage of three categories(Employed labor force,
                      Unemployed labor force, Armed labor force) in ",input$state)
    
    return(message)
  })
  
  # This graph shows the level of labor force in 2015 in one region where user choose. From lightest color to darkest color,
  # it shows the state which has least percentage of labor force type to state which has largest percentage of labor force type.
  output$plot1 <- renderPlot({
    
    if (input$labor.type == "Employed labor force") {
      p <- ggplot(data = filtered.map()) +
        geom_polygon(aes(x = long, y = lat, group = group, color = Region, 
                         fill = `Employed labor force`), color = "black") +
        scale_fill_continuous(low = "#2A9B13", high = "#901010")
    } else if (input$labor.type == "Unemployed labor force") {
      p <- ggplot(data = filtered.map()) +
        geom_polygon(aes(x = long, y = lat, group = group, color = Region, 
                         fill = `Unemployed labor force`), color = "black") +
        scale_fill_continuous(low = "#2A9B13", high = "#672A70")
    } else {
      p <- ggplot(data = filtered.map()) +
        geom_polygon(aes(x = long, y = lat, group = group, 
                         fill = `Armed labor force`), color = "black") +
        scale_fill_continuous(low = "#2A9B13", high = "#138A6E")
    }
    
    p <- p + coord_fixed(ratio = 1.0)
    
    return(p)
  })
  
  # This bar graph shows the percentage of three categories(Employed labor force,
  # Unemployed labor force, Armed labor force) in state that user choose.
  output$plot2 <- renderPlot({
    my.plot <- ggplot(data = filtered()) + 
      geom_bar(mapping = aes(x = type.of.force, y = value, fill = type.of.force),
               stat = "identity")
    
    return(my.plot)
    
  })
  
  
}


shinyServer(my.server)