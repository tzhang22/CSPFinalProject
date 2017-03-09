# Load necessary libraries
library("tidyr")
library("shiny")
library("maps")
library("ggplot2")
library("dplyr")
library("plotly")
library("shinythemes")

# Tony's Portion
# Arranging data set column names
tax.data <- read.csv(file = "data/Taxes_2015.csv", stringsAsFactors = FALSE)
colnames(tax.data) <- tax.data[1,]
tax.data <- tax.data[c(2:1582),]

# Filling in missing state id 
state.id <- read.csv(file = "data/states.csv", stringsAsFactors = FALSE)
colnames(state.id) <- c("Geographic area name", "state.abbrev")
tax.data <- full_join(state.id, tax.data, by = "Geographic area name")

# Selecting and changing variable names to be ready for joining.
selected <- select(tax.data, State = `Geographic area name`, Id = state.abbrev,
                   `Tax Type` = `Meaning of Tax Type`, `Amount ($1,000)`) %>% 
    filter(State != "District of Columbia")

# Changing the amount to numbers so they can be graphed
selected$Amount <- as.numeric(selected$`Amount ($1,000)`) * 1000

# Getting all the unique values for users to select
tax.types <- unique(selected$`Tax Type`)
states <- unique(selected$State)

##########################################################

# Rhea's Portion
income.data <- read.csv("data/Income_Employment_2015.csv", stringsAsFactors = FALSE)

# select total households data set from the whole data set
total.households <-
    select(
        income.data,
        GEO.display.label,
        HC01_VC74,
        HC01_VC75,
        HC01_VC76,
        HC01_VC77,
        HC01_VC78,
        HC01_VC79,
        HC01_VC80,
        HC01_VC81,
        HC01_VC82,
        HC01_VC83,
        HC01_VC84,
        HC01_VC85,
        HC01_VC86
    )
total.households <- total.households[2:53,]

colnames(total.households) <-
    c(
        "Geography",
        "Total households",
        "Less than $10,000",
        "Up to $14,999",
        "Up to $24,999",
        "Up to $34,999",
        "Up to $49,999",
        "Up to $74,999",
        "Up to $99,999",
        "Up to $149,999",
        "Up to $199,999",
        "$200,000 or more",
        "Median household income (dollars)",
        "Mean household income (dollars)"
    )

# Filling in missing state id 
colnames(state.id) <- c("Geography", "state.abbrev")
total.households <- full_join(state.id, total.households, by = "Geography")

household.long <- gather(total.households, 
                         key = "Incomes Range", 
                         value = "Amount", 
                         `Less than $10,000`,
                         `Up to $14,999`,
                         `Up to $24,999`,
                         `Up to $34,999`,
                         `Up to $49,999`,
                         `Up to $74,999`,
                         `Up to $99,999`,
                         `Up to $149,999`,
                         `Up to $199,999`,
                         `$200,000 or more`
) %>% 
    select(Geography, state.abbrev, `Incomes Range`, Amount)


# Getting all the unique values for users to select
incomes.types <- unique(household.long$`Incomes Range`)

# Selecting and changing variable names to be ready for joining.
household.long <- select(household.long, State = `Geography`, Id = state.abbrev,
                         `Incomes Range`, Amount) %>% 
    filter(State != "District of Columbia")

# Changing the amount to numbers so they can be graphed
household.long$Amount <- as.numeric(household.long$Amount)

##########################################################

# Tim's Portion
poverty.csv <- read.csv("data/Poverty_2015.csv", stringsAsFactors = FALSE)

poverty.table <- data.frame(poverty.csv)

filtered.poverty <- select(poverty.table, GEO.display.label, HC02_EST_VC01, HC02_MOE_VC01, HC03_EST_VC01, 
                           HC03_MOE_VC01,  HC02_EST_VC03, HC02_MOE_VC03, HC03_EST_VC03, HC03_MOE_VC03) %>% 
    filter(GEO.display.label != "Puerto Rico" & GEO.display.label != "Geography")

colnames(filtered.poverty) <- c("State", "Total Poverty", "Error.Total", "Percent Total Poverty", 
                                "Error.Percent.Total", "Total Poverty (Under 18)", "Error.Under.18",
                                "Percent Total Poverty (Under 18)", "Error.Percent.Under.18")

filtered.poverty$`Total Poverty` <- as.numeric(filtered.poverty$`Total Poverty`)
filtered.poverty$Error.Total <- as.numeric(filtered.poverty$Error.Total)
filtered.poverty$`Percent Total Poverty` <- as.numeric(filtered.poverty$`Percent Total Poverty`)
filtered.poverty$Error.Percent.Total <- as.numeric(filtered.poverty$Error.Percent.Total)
filtered.poverty$`Total Poverty (Under 18)` <- as.numeric(filtered.poverty$`Total Poverty (Under 18)`)
filtered.poverty$Error.Under.18 <- as.numeric(filtered.poverty$Error.Under.18)
filtered.poverty$`Percent Total Poverty (Under 18)` <- as.numeric(filtered.poverty$`Percent Total Poverty (Under 18)`)
filtered.poverty$Error.Percent.Under.18 <- as.numeric(filtered.poverty$Error.Percent.Under.18)

# All Regions
non <- "Not a state"

Northwest <-  c("Alaska", "Washington",
                "Oregon", "Idaho", "Montana",
                "Wyoming", non, non, non, non,
                non, non, non)
South <- c("Florida", "Mississippi",
           "Louisiana", "Alabama", "Georgia",
           "South Carolina", "North Carolina",
           "Tennessee", "Arkansas", "Kentucky",
           "Virginia", non, non)
Northeast <- c("Maine", "New Hampshire",
               "Vermont", "Massachusetts",
               "Rhode Island", "Connecticut",
               "New York", "Pennsylvania", "Delaware",
               "District of Columbia", "Maryland",
               "New Jersey", "West Virginia")
Midwest <- c("Ohio", "Indiana", "Michigan",
             "Illinois", "Wisconsin",
             "Missouri", "Iowa", "Minnesota",
             "Kansas", "Nebraska", "South Dakota",
             "North Dakota", non)
Southwest <- c("California", "Hawaii", "Nevada",
               "Utah", "Colorado",
               "Arizona", "New Mexico",
               "Oklahoma", "Texas", non, non, non, non)

regions <- data.frame(Northwest, South, Northeast, Midwest, Southwest)

regions.long <- gather(regions, key = Region, value = State,
                       Northwest, South, Northeast, Midwest, Southwest)

regions.long <- filter(regions.long, State != "Not a state")

joined.one <- left_join(regions.long, filtered.poverty, by = "State")

the.us <- map_data("state")

joined.one$State <- tolower(joined.one$State)

ultra.join <- left_join(joined.one, the.us, by = c("State" = "region"))

mean.table <- group_by(ultra.join, Region) %>% 
    summarize("mean.percent" = mean(`Percent Total Poverty`), 
              "mean.percent.18" = mean(`Percent Total Poverty (Under 18)`))

##########################################################

# Sam's Portion
filtered.employment <- select(income.data, GEO.display.label,HC03_VC06, HC03_VC07, HC03_VC08) %>% 
    filter(GEO.display.label != "Puerto Rico" & GEO.display.label != "Geography")

colnames(filtered.employment) <- c("State", "Employed labor force", "Unemployed labor force", "Armed labor force")


filtered.employment$`Employed labor force` <- as.numeric(filtered.employment$`Employed labor force`)
filtered.employment$`Unemployed labor force` <- as.numeric(filtered.employment$`Unemployed labor force`)
filtered.employment$`Armed labor force` <- as.numeric(filtered.employment$`Armed labor force`)

regions <- data.frame(Northwest, South, Northeast, Midwest, Southwest)
# Gather different region into one.
regions.long <- gather(regions, key = Region, value = State,
                       Northwest, South, Northeast, Midwest, Southwest)

regions.long <- filter(regions.long, State != "Not a state")

# join two data frame into one by state name.
joined <- left_join(regions.long, filtered.employment, by = "State")

# gather three categories into one columns with value besides it.
long.join <- gather(joined, "type.of.force", "value", `Employed labor force`,
                    `Unemployed labor force`, `Armed labor force`)

long.join$State <- tolower(long.join$State)

us.maps <- map_data("state")

joined$State <- tolower(joined$State)

# Join us.maps which has logitude and latitude and long.join into one data frame.
State.join <- left_join(joined, us.maps, by = c("State" = "region"))

##########################################################

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
    
    # Rhea's Portion
    filtered.rhea <- reactive({
        data <- filter(household.long, `Incomes Range` == input$facet.rhea)
        
        if (input$state1.rhea != "United States" && input$state2.rhea != "United States") {
            data <- filter(data, State %in% c(input$state1.rhea, input$state2.rhea))
        }
        
        return (data)
    })
    
    output$incomes.map <- renderPlotly({
        p <- plot_geo(data = filtered.rhea(), locationmode = 'USA-states') %>%
            add_trace(z = ~Amount, locations = ~Id,
                      marker = list(colorbar = list(title = "Num of Households")),
                      colors = "Greens") %>%
            layout(
                title = paste(input$facet.rhea),
                geo = list(scope = 'usa')
            )
        
        return (p)
    })
    
    output$state.info.rhea <- renderPlotly({
        if (input$state1.rhea != "United States" && input$state2.rhea != "United States") {
            choice1 = input$state1.rhea
            choice2 = input$state2.rhea
            
            data1 <- filter(filtered.rhea(), State == choice1 | State == choice2)
            
            p <- plot_ly(data = data1, x = ~State, y = ~Amount,
                         type = 'bar', name = ~State, color = ~State) %>%
                layout(
                    showlegend = FALSE,
                    title = paste(choice1, "vs", choice2, "incomes"),
                    xaxis = list(title = "Income"),
                    yaxis = list(title = "Number of Households"))
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
        
        p <- p + coord_fixed(ratio = 1.4) +
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
                          it shows the state which has least percentage of ",input$labor.type, " to state which has largest percentage of ", input$labor.type, ".")
        
        return(message)
    })
    
    output$description2 <- renderText({
        message <- paste0("This bar graph shows the percentage of three categories(Employed labor force,
                          Unemployed labor force, Armed labor force) in ",input$state, ".")
        
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
        
        p <- p + coord_fixed(ratio = 1.4) +
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
    
    # This bar graph shows the percentage of three categories(Employed labor force,
    # Unemployed labor force, Armed labor force) in state that user choose.
    output$plot2.sam <- renderPlot({
        my.plot <- ggplot(data = filtered.sam()) + 
            geom_bar(mapping = aes(x = type.of.force, y = value, fill = type.of.force),
                     stat = "identity") +
            labs(x = "Type of Labor Force", y = "Population (%)") +
            guides(fill = FALSE)
        
        return(my.plot)
    })
}

shinyServer(server)