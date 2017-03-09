# Load the shiny, ggplot2, and dplyr libraries
library("shiny")
library("ggplot2")
library("dplyr")
library("tidyr")
library("plotly")
library("shinythemes")

tax.data <-
  read.csv("data/Taxes_2015.csv", stringsAsFactors = FALSE)
income.data <-
  read.csv("data/Income_Employment_2015.csv", stringsAsFactors = FALSE)
poverty.data <-
  read.csv("data/Poverty_2015.csv", stringsAsFactors = FALSE)

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
state.id <- read.csv(file = "data/states.csv", stringsAsFactors = FALSE)
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
states <- unique(household.long$Geography)


# Selecting and changing variable names to be ready for joining.
household.long <- select(household.long, State = `Geography`, Id = state.abbrev,
                   `Incomes Range`, Amount) %>% 
  filter(State != "District of Columbia")

# Changing the amount to numbers so they can be graphed
household.long$Amount <- as.numeric(household.long$Amount)

# Define a shiny ui
ui <- fluidPage(theme = shinytheme("journal"),
  navbarPage(title = "United States Economics",
             tabPanel(title = "Incomes",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput('facet', label = "Incomes Range", 
                                      choices = incomes.types),
                          selectInput('state1', label = "Choose a state to compare to",
                                      choices = c("United States", states)),
                          selectInput('state2', label = "Choose a state to compare against",
                                      choices = c("United States", states))
                        ),
                        
                        mainPanel(
                          plotlyOutput('incomes.map'),
                          plotlyOutput('state.info')
                        )
                      )
             )
  )
)

# Define a server
server <- function(input, output) {
  filtered <- reactive({
    data <- filter(household.long, `Incomes Range` == input$facet)
    
    if (input$state1 != "United States" && input$state2 != "United States") {
      data <- filter(data, State %in% c(input$state1, input$state2))
    }
    
    return (data)
  })
  
  states <- reactive({
    choice1 = input$state1
    choice2 = input$state2
    
    state1 <- filtered() %>% 
      filter(State == choice1)
    
    state2 <- filtered() %>% 
      filter(State == choice2)
    
    data <- left_join(state1, state2)
    
    return(data)
  })
  
  output$incomes.map <- renderPlotly({
    p <- plot_geo(data = filtered(), locationmode = 'USA-states') %>%
      add_trace(z = ~Amount, locations = ~Id, colors = "Blues") %>%
      layout(
        title = paste(input$facet),
        geo = list(scope = 'usa')
      )

    return (p)
  })
  
  output$state.info <- renderPlotly({
    if (input$state1 != "United States" && input$state2 != "United States") {
      choice1 = input$state1
      choice2 = input$state2
      
      data1 <- filter(filtered(), State == choice1 | State == choice2)
      
      p <- plot_ly(data = data1, x = ~State, y = ~Amount,
                   type = 'bar', name = ~State) %>%
        layout(
          title = paste(choice1, "vs", choice2, "incomes"),
          xaxis = list(title = "Incomes Range (Hover for more details)", showticklabels = FALSE),
          yaxis = list(title = "Amount ($)"))
    } else {
      plotly_empty()
    }
  })
}

shinyApp(ui = ui, server = server)