max.total.pov <- max(filtered.poverty$`Total Poverty`)
max.total.18.pov <- max(filtered.poverty$`Total Poverty (Under 18)`)

mean.table <- group_by(ultra.join, Region) %>% 
                summarize("mean.percent" = mean(`Percent Total Poverty`), 
                          "mean.percent.18" = mean(`Percent Total Poverty (Under 18)`))

server <- function(input, output) {
  
  # reactive table that is used in both the table and ggplot
  filtered.map <- reactive({
      new.table <- ultra.join
      if (input$radio.region != "All") {
      new.table <- filter(ultra.join, Region %in% input$radio.region)
      }
      return(new.table)
  })
  
  filtered.graph <- reactive({
    new.chart <- filter(filtered.poverty, State == input$state1 | State == input$state2)
    return(new.chart)
  })
  
  filter.pie <- reactive({
    pie.table <- ultra.join  
    if (input$radio.for.regions != "All") {
      pie.table <- filter(ultra.join, Region %in% input$radio.for.regions)
    }  
    
    pie.table <- group_by(pie.table, State) %>% 
                  summarize("state.percent" = mean(`Percent Total Poverty`),
                           "state.under.18.percent" = mean(`Percent Total Poverty (Under 18)`))
      
    return(pie.table)
  })
  
  # The graph that is based off the reactive region
  output$plot <- renderPlot({
    
    if (input$radio.type == 1) {
      p <- ggplot(data = filtered.map()) +
        geom_polygon(aes(x = long, y = lat, group = group, color = Region, 
                         fill = `Total Poverty`), color = "black") +
        scale_fill_continuous(low = "#F2F2F2", high = "#901010", 
                              breaks = c(500000, 2000000,4000000,6000000),
                              labels = c("500,000", "Two Million", "Four Million", "Six Million"))
    } else if (input$radio.type == 2) {
      p <- ggplot(data = filtered.map()) +
        geom_polygon(aes(x = long, y = lat, group = group, color = Region, 
                         fill = `Percent Total Poverty`), color = "black") +
        scale_fill_continuous(low = "#F2F2F2", high = "#672A70")
    } else if (input$radio.type == 3) {
      p <- ggplot(data = filtered.map()) +
        geom_polygon(aes(x = long, y = lat, group = group, 
                         fill = `Total Poverty (Under 18)`), color = "black") +
        scale_fill_continuous(low = "#F2F2F2", high = "#E6E151",
                              breaks = c(1000000,2000000),
                              labels = c("One Million", "Two Million"))
    } else {
      p <- ggplot(data = filtered.map()) +
        geom_polygon(aes(x = long, y = lat, group = group, 
                         fill = `Percent Total Poverty (Under 18)`), color = "black") +
        scale_fill_continuous(low = "#F2F2F2", high = "#138A6E")
    }
    
    p <- p + coord_fixed(ratio = 1.0) +
             labs(x = "", y = "") +
             theme_minimal() +
             theme(axis.title.x = element_blank(),
                   axis.text.x = element_blank(),
                   axis.ticks.x = element_blank(),
                   axis.title.y = element_blank(),
                   axis.text.y = element_blank(),
                   axis.ticks.y = element_blank())
    
    return(p)
  })
  
  output$summary <- renderText({
    
    message <- paste0("This graph shows total population under the poverty line per state")
    
    return(message)
  })
  
  output$plot2 <- renderPlot({
    if (input$radio == 1) {
      graph <- ggplot(data = filtered.graph()) +
        geom_bar(mapping = aes(x = State, y = `Total Poverty`,
                               fill = `Total Poverty`), stat = "identity")
    } else if (input$radio == 2) {
      graph <- ggplot(data = filtered.graph()) +
        geom_bar(mapping = aes(x = State, y = `Percent Total Poverty`,
                                   fill = `Percent Total Poverty`), stat = "identity")
    } else if (input$radio == 3) {
      graph <- ggplot(data = filtered.graph()) +
        geom_bar(mapping = aes(x = State, y = `Total Poverty (Under 18)`,
                                   fill = `Total Poverty (Under 18)`), stat = "identity")
    } else {
      graph <- ggplot(data = filtered.graph()) +
        geom_bar(mapping = aes(x = State, y = `Percent Total Poverty (Under 18)`,
                                   fill = `Percent Total Poverty (Under 18)`), stat = "identity")
    }
    
    graph <- graph + scale_fill_continuous(low = "#E68669", high = "#185A90",
                                           breaks = c(),
                                           labels = c(""))
    
    return(graph)
  })
  
  output$plot3 <- renderPlot({
    if (input$statistic == 1) {
      my.plot <- ggplot(data = mean.table) +
        geom_bar(mapping = aes(x = Region, y = mean.percent, fill = Region),
                 stat = "identity") +
        labs(y = "Mean Percentage of Population Below Poverty Line")
    } else {
      my.plot <- ggplot(data = mean.table) +
        geom_bar(mapping = aes(x = Region, y = mean.percent.18, fill = Region),
                 stat = "identity") +
        labs(y = paste("Mean", input$statistic)) +
        labs(y = "Mean Percentage of Population Under 18 Below Poverty Line")
    }
    if (input$radio.for.regions == "All") {
      my.plot <- my.plot + scale_fill_manual(values = c("#E3C73C", "#2821A3", 
                                             "#0F7D32", "#D73413", "#932F3E"))
    } else if (input$radio.for.regions == "Midwest") {
      my.plot <- my.plot + scale_fill_manual(values = c("#E3C73C", "#858585", 
                                                        "#858585", "#858585", "#858585"))
    } else if (input$radio.for.regions == "Northeast") {
      my.plot <- my.plot + scale_fill_manual(values = c("#858585", "#2821A3", 
                                                        "#858585", "#858585", "#858585"))
    } else if (input$radio.for.regions == "Northwest") {
      my.plot <- my.plot + scale_fill_manual(values = c("#858585", "#858585", 
                                                        "#0F7D32", "#858585", "#858585"))
    } else if (input$radio.for.regions == "South") {
      my.plot <- my.plot + scale_fill_manual(values = c("#858585", "#858585", 
                                                        "#858585", "#D73413", "#858585"))
    } else {
        my.plot <- my.plot + scale_fill_manual(values = c("#858585", "#858585", 
                                                          "#858585", "#858585", "#932F3E"))
    }
    return(my.plot)
  })
  
  output$pie.plot <- renderPlotly({
    if (input$radio.for.regions == "All") {
      my.pie <- plot_ly(filter.pie(), labels = ~c(""), values = ~c(""), type = 'pie') %>%
        layout(title = '',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    } else {
      if (input$statistic == 1) {
        my.pie <- plot_ly(filter.pie(), labels = ~State, values = ~state.percent, type = 'pie') %>%
          layout(title = 'Percentage Under Poverty Line For Region',
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      } else {
        my.pie <- plot_ly(filter.pie(), labels = ~State, values = ~state.under.18.percent, type = 'pie') %>%
          layout(title = 'Percentage Under Poverty Line (Under 18) For Region',
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      }
      
    }
    return(my.pie)
  })

}

shinyServer(server)