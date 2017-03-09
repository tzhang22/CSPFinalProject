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
    "$10,000 to $14,999",
    "$15,000 to $24,999",
    "$25,000 to $34,999",
    "$35,000 to $49,999",
    "$50,000 to $74,999",
    "$75,000 to $99,999",
    "$100,000 to $149,999",
    "$150,000 to $199,999",
    "$200,000 or more",
    "Median household income (dollars)",
    "Mean household income (dollars)"
  )

household.long <- gather(total.households, 
                         key = "Incomes Range", 
                         value = "Amount", 
                         `Less than $10,000`,
                         `$10,000 to $14,999`,
                         `$15,000 to $24,999`,
                         `$25,000 to $34,999`,
                         `$35,000 to $49,999`,
                         `$50,000 to $74,999`,
                         `$75,000 to $99,999`,
                         `$100,000 to $149,999`,
                         `$150,000 to $199,999`,
                         `$200,000 or more`
                         ) %>% 
                   select(Geography, `Incomes Range`, Amount)

with.others <-
  select(
    income.data,
    GEO.display.label,
    HC01_VC89,
    HC01_VC90,
    HC01_VC91,
    HC01_VC92,
    HC01_VC93,
    HC01_VC94,
    HC01_VC97,
    HC01_VC98,
    HC01_VC99,
    HC01_VC100,
    HC01_VC101
  )
with.others <- with.others[2:53,]

colnames(with.others) <-
  c(
    "Geography",
    "With earnings",
    "Mean earnings (dollars)",
    "With Social Security",
    "Mean Social Security income (dollars)",
    "With retirement income",
    "Mean retirement income (dollars)",
    "With Supplemental Security Income",
    "Mean Supplemental Security Income (dollars)",
    "With cash public assistance income",
    "Mean cash public assistance income (dollars)",
    "With Food Stamp/SNAP benefits in the past 12 months"
  )



# Getting all the unique values for users to select
incomes.types <- household.long$`Incomes Range`
states <- household.long$Geography

household.long$Amount <- as.numeric(household.long$Amount)

ui <- fluidPage(theme = shinytheme("journal"),
  navbarPage(title = "United States Economics",
             tabPanel(title = "Incomes",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput('facet', label = "Incomes Range", 
                                      choices = household.long$`Incomes Range`,
                                      selected = "Less than $10,000"),
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

server <- function(input, output) {
  filtered <- reactive({
    data <- filter(household.long, `Incomes Range` == input$facet)
    
    if (input$state1 != "United States" && input$state2 != "United States") {
      data <- filter(data, Geography == input$state1 || Geography == input$state2)
    }
    
    return (data)
  })
  
  states <- reactive({
    choice1 = input$state1
    choice2 = input$state2
    
    state1 <- household.long %>% 
      filter(State ==  choice1, input$facet != "Total Households") %>% 
      mutate(choice1 = Amount) %>% 
      select(choice1, `Incomes Range`, Amount)
    
    state2 <- household.long %>% 
      filter(State == choice2, input$facet != "Total Households") %>% 
      mutate(choice2 = Amount) %>% 
      select(choice2, `Incomes Range`, Amount)
    
    data <- left_join(state1, state2)
    
    return(data)
  })
  
  output$incomes.map <- renderPlotly({
    p <- plot_geo(data = filtered(), locationmode = 'USA-states') %>%
      add_trace(z = ~Amount, locations = ~Geography,
                color = ~Amount, colors = "Blues") %>%
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
      
      p <- plot_ly(data = states(), x = ~`Incomes Range`, y = ~choice1,
                   type = 'bar', name = choice1) %>%
        add_trace(y = ~choice2, name = choice2) %>%
        layout(
          title = paste(choice1, "vs", choice2, "incomes"),
          xaxis = list(title = "Incomes Range (Hover for more details)", showticklabels = FALSE),
          yaxis = list(title = "Amount ($)"),
          barmode = 'group')
    } else {
      plotly_empty()
    }
  })
}

shinyApp(ui = ui, server = server)