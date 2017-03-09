# Load the shiny, ggplot2, and dplyr libraries
library("tidyr")
library("maps")
library("shiny")
library("ggplot2")
library("dplyr")
library("plotly")

server <- function(input, output) {
    # Tony's Portion
    filtered <- reactive({
        data <- selected %>% 
            filter(`Tax Type` == input$facet)
        
        if (input$state1 != "United States" && input$state2 != "United States") {
            data <- filter(data, State %in% c(input$state1, input$state2))
        }
        
        return (data)
    })
    
    states <- reactive({
        name1 = input$state1
        name2 = input$state2
        
        state1 <- selected %>% 
            filter(State == name1, `Tax Type` != "Total Taxes") %>% 
            mutate(name1 = Amount) %>% 
            select(name1, `Tax Type`)
        
        state2 <- selected %>% 
            filter(State == name2, `Tax Type` != "Total Taxes") %>% 
            mutate(name2 = Amount) %>% 
            select(name2, `Tax Type`)
        
        data <- left_join(state1, state2)
        
        return (data)
    })
    
    output$taxes.map <- renderPlotly({
        p <- plot_geo(data = filtered(), locationmode = 'USA-states') %>%
            add_trace(z = ~Amount, locations = ~Id, 
                      marker = list(colorbar = list(title = "Amount ($)")), colors = 'Blues') %>%
            layout(
                title = paste(input$facet, "Across the United States"),
                geo = list(scope = 'usa')
            )
        
        return (p)
    })
    
    output$state.info <- renderPlotly({
        if (input$state1 != "United States" && input$state2 != "United States") {
            name1 = input$state1
            name2 = input$state2
            
            p <- plot_ly(data = states(), x = ~`Tax Type`, y = ~name1,
                         type = 'bar', name = name1) %>%
                add_trace(y = ~name2, name = name2) %>% 
                layout(
                    title = paste(name1, "vs", name2, "taxes"),
                    xaxis = list(title = "Tax Type (Hover for more details)", showticklabels = FALSE),
                    yaxis = list(title = "Amount ($)"),
                    barmode = 'group')
        } else {
            plotly_empty()
        }
    })
    
    ##########################################################
    
    # Tim's Portion
    # reactive table that is used in both the table and ggplot
    filtered.map <- reactive({
        new.table <- ultra.join
        if (input$radio.region != "All") {
            new.table <- filter(ultra.join, Region %in% input$radio.region)
        }
        return(new.table)
    })
    
    filtered.graph <- reactive({
        new.chart <- filter(filtered.poverty, State == input$state1.tim | State == input$state2.tim)
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
    
    ##########################################################
    
    # Sam's Portion
    filtered.sam <- reactive({
        new <- filter(long.join, input$state == State)
        print(input$state)
        
        return(new)
    })
    
    # This paragraph will just make sure that the region user choose will get some outputs.
    filtered.map.sam <- reactive({
        new.table <- State.join
        if (input$radio.region.sam != "All") {
            new.table <- filter(State.join, Region %in% input$radio.region.sam)
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
    output$plot1.sam <- renderPlot({
        if (input$labor.type == "Employed labor force") {
            p <- ggplot(data = filtered.map.sam()) +
                geom_polygon(aes(x = long, y = lat, group = group, color = Region, 
                                 fill = `Employed labor force`), color = "black") +
                scale_fill_continuous(low = "#2A9B13", high = "#901010")
        } else if (input$labor.type == "Unemployed labor force") {
            p <- ggplot(data = filtered.map.sam()) +
                geom_polygon(aes(x = long, y = lat, group = group, color = Region, 
                                 fill = `Unemployed labor force`), color = "black") +
                scale_fill_continuous(low = "#2A9B13", high = "#672A70")
        } else {
            p <- ggplot(data = filtered.map.sam()) +
                geom_polygon(aes(x = long, y = lat, group = group, 
                                 fill = `Armed labor force`), color = "black") +
                scale_fill_continuous(low = "#FFFFFF", high = "#138A6E")
        }
        
        p <- p + coord_fixed(ratio = 1.0)
        
        return(p)
    })
    
    # This bar graph shows the percentage of three categories(Employed labor force,
    # Unemployed labor force, Armed labor force) in state that user choose.
    output$plot2.sam <- renderPlot({
        my.plot <- ggplot(data = filtered.sam()) + 
            geom_bar(mapping = aes(x = type.of.force, y = value, fill = type.of.force),
                     stat = "identity")
        
        return(my.plot)
    })
}

shinyServer(server)