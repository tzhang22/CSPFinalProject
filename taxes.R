library(ggplot2)
library(dplyr)
library(shiny)
library(plotly)

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


ui <- fluidPage(
    navbarPage(
        title = "United States Economics (2015)",
        tabPanel(title = "About",
            mainPanel(
                p("Hello")
            )
        ),
        
        tabPanel(title = "Taxes",
            sidebarLayout(
                sidebarPanel(
                    selectInput('facet', label = "Tax Types", choices = tax.types),
                    selectInput('state1', label = "Choose a state to compare to",
                                choices = c("United States", states)),
                    selectInput('state2', label = "Choose a state to compare against",
                                choices = c("United States", states)),
                    br(),
                    p("The above widget examines the different tax data gathered in 2015. 
                      The first selection drop down allows the specification for different
                      tax types and how the amounts differ throughout the United States as
                      a heatmap. Hover over individual states to view more specific dollar
                      amounts. While the second and third selections allow you to pick two
                      states to compare in more detail in a grouped bar chart. Hover over
                      the different bars to display more accurate numerical data.")
                ),
                
                mainPanel(
                    plotlyOutput('taxes.map'),
                    plotlyOutput('state.info')
                )
            )
        )
    )
)

server <- function(input, output) {
    
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
}

shinyApp(ui = ui, server = server)
