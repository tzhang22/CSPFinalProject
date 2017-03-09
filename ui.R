library("tidyr")
library("maps")
library("shiny")
library("ggplot2")
library("dplyr")
library("plotly")

tax.data <- read.csv("data/Taxes_2015.csv", stringsAsFactors = FALSE)
income.data <- read.csv("data/Income_Employment_2015.csv", stringsAsFactors = FALSE)
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


non <- "Not a state"

# All Regions
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

# Makes a UI using a fluidPage layout
ui <- fluidPage(
  
  tabsetPanel(type = "tabs", 
              tabPanel("Map", 
                       sidebarLayout(
                         
                         # Includes three items on the left panel
                         sidebarPanel(
                           
                           radioButtons("radio.region", label = h4("Region"), 
                                        choices = list("All" = "All",
                                                       "Northwest" = "Northwest",
                                                       "South" = "South",
                                                       "Northeast" = "Northeast",
                                                       "Midwest" = "Midwest",
                                                       "Southwest" = "Southwest"),
                                        selected = "All"),
                           
                           radioButtons("radio.type", label = h4("Statistic"),
                                        choices = list("Total Population Under Poverty Line" = 1,
                                                       "Percentage of Population Below Poverty Line" = 2,
                                                       "Total Population Under 18" = 3,
                                                       "Percentage of Population Under 18 Below Poverty Line" = 4),
                                        selected = 1),
                           br(),
                           p("This section shows different measurements of poverty per state,
                              throughout the country. The darker the state, the more poverty there is - measured
                              either by percentage or raw number. There is also the option to choose a specific
                              region to see how a state compares to other states nearby.")
                         ),
                         mainPanel(
                           h4(textOutput("summary")), plotOutput("plot")
                         )
                       )
              ), 
              tabPanel("Graphs",
                       sidebarLayout(
                         
                         # Includes three items on the left panel
                         sidebarPanel(
                           
                           selectInput("state1", label = h3("Select First State"), 
                                       choices = filtered.poverty$State,
                                       selected = "Alabama"),
                           selectInput("state2", label = h3("Select Second State"),
                                       choices = filtered.poverty$State,
                                       selected = "Alaska"),
                           radioButtons("radio", label = h4("Statistic"),
                                        choices = list("Total Population Under Poverty Line" = 1,
                                                       "Percentage of Population Below Poverty Line" = 2,
                                                       "Total Population Under 18" = 3,
                                                       "Percentage of Population Under 18 Below Poverty Line" = 4),
                                        selected = 1),
                           br(),
                           p("This graph is useful for comparing how two different States compare with
                              eachother. There are still the same four metrics from before
                              that can be used to compare the two chosen states.")
                         ),
                         mainPanel(
                           plotOutput("plot2")
                         )
                       )
              ),
              tabPanel("Regions",
                       sidebarLayout(
                         
                         # Includes three items on the left panel
                         sidebarPanel(
                           
                           radioButtons("statistic", label = h4("Statistic"),
                                        choices = list("Percentage of Population Below Poverty Line" = 1,
                                                       "Percentage of Population Under 18 Below Poverty Line" = 2),
                                        selected = 1),
                           
                           radioButtons("radio.for.regions", label = h4("Region"), 
                                        choices = list("All" = "All",
                                                       "Midwest" = "Midwest",
                                                       "Northeast" = "Northeast",
                                                       "Northwest" = "Northwest",
                                                       "South" = "South",
                                                       "Southwest" = "Southwest"),
                                        selected = "All"),
                           br(),
                           p("This section compares the regions altogether, and breaks down a region
                              into the states that contribute to its overall poverty. If a specific region
                              is chosen, a pie graph will show the percentage of a the chosen statistic for
                              each state in relation to the region as a whole.")
                         ),
                         mainPanel(
                           plotOutput("plot3"),
                           plotlyOutput("pie.plot")
                         )
                       )   
                       
              )
  )
)

# Makes the UI
shinyUI(ui)