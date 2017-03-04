library("httr")
library("jsonlite")
library("shiny")
library("ggplot2")
library("dplyr")

server <- function(input, output) {
  
  # reactive table that is used in both the table and ggplot
  filtered <- reactive({
    new.table <- filter(joined.one, Region %in% input$checkGroup)
    
    return(new.table)
  })
  
  # The graph, that is based off the reactive data, and inputted color scheme
  output$plot <- renderPlot({
    
    
    p <- ggplot(data = filtered()) +
      geom_point(mapping = aes(x = as.numeric(ESTAB), y = as.numeric(PAYANN), color = Region), size = 3) +
      xlab("Number of Establishments") +
      ylab("Annual Payroll ($1,000)")
    
#    if (input$color.choice == "Primary") {
#      p <- p + scale_color_manual(values = c("#EA2828", "#2158C6", "#C6AC19"))
#    } else if (input$color.choice == "Cool") {
#      p <- p + scale_color_manual(values = c("#2B59C3", "#3CB8C8", "#500C84"))
#    } else {
#      p <- p + scale_color_manual(values = c("#EAD143", "#A41111", "#D96431"))
#    }
    
    return(p)
  })
  
  # The data below the graph that shows the coordinates of the point
  output$info <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
    }
    
    
    paste0("click: ", xy_str(input$plot_click))
  })
  
  output$summary <- renderText({
    
    current.species <- function(number) {
      if (!is.na(input$checkGroup[number])) {
        paste0(input$checkGroup[number], sep = ", ")
      }
    }
    
    
    message <- paste0("This graph shows the petal-length of Irises in relation to the sepal-length,\n", 
                      "colored by species. The current species are: ", current.species(1), current.species(2),
                      current.species(3))
    
    return(message)
  })
  
  output$table.summary <- renderText({
    current.range <- function() {
      if (!is.na(input$sepal.choice)) {
        paste0(round(input$sepal.choice[1], 2), " - ", round(input$sepal.choice[2], 2))
      }
    }
    
    current.species <- function(number) {
      if (!is.na(input$checkGroup[number])) {
        paste0(input$checkGroup[number], sep = ", ")
      }
    }
    
    table.message <- paste0("This table shows the current range of: ", current.range(),
                            " for sepal length. It is also showing the species: ", current.species(1),
                            current.species(2), current.species(3))
    
  })
  
  # Generate an table view of the reactive data
  output$table <- renderTable({
    data.frame(x=filtered())
  })
  
}

# Makes the server
shinyServer(server)