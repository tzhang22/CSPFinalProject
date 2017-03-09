# Load the shiny, ggplot2, and dplyr libraries
library("tidyr")
library("maps")
library("shiny")
library("ggplot2")
library("dplyr")
library("plotly")

income.data <- read.csv("data/Income_Employment_2015.csv", stringsAsFactors = FALSE)



filtered.employment <- select(income.data, GEO.display.label,HC03_VC06, HC03_VC07, HC03_VC08) %>% 
  filter(GEO.display.label != "Puerto Rico" & GEO.display.label != "Geography")

colnames(filtered.employment) <- c("State", "Employed labor force", "Unemployed labor force", "Armed labor force")


filtered.employment$`Employed labor force` <- as.numeric(filtered.employment$`Employed labor force`)
filtered.employment$`Unemployed labor force` <- as.numeric(filtered.employment$`Unemployed labor force`)
filtered.employment$`Armed labor force` <- as.numeric(filtered.employment$`Armed labor force`)


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





my.ui <- fluidPage(
  
  # Makes a new title Panel
  titlePanel("2015 Employment status in the United States"),
  
  tabsetPanel(type = "tabs", 
              tabPanel("Map", 
                       sidebarLayout(
                         
                         # Includes three items on the left panel
                         sidebarPanel(
                           # User can choose region.
                           radioButtons("radio.region", label = h4("Region"), 
                                        choices = list("All" = "All",
                                                       "Northwest" = "Northwest",
                                                       "South" = "South",
                                                       "Northeast" = "Northeast",
                                                       "Midwest" = "Midwest",
                                                       "Southwest" = "Southwest"),
                                        selected = "All"),
                           
                           # User can choose labor force type.
                           selectInput("labor.type", label = h3("Labor.force.type"), 
                                       choices = c("Employed labor force", "Unemployed labor force", "Armed labor force"),
                                       selected = "Employed labor force"),
                           
                           # User can select a state.
                           selectInput("state", label = h3("Select State"), 
                                     choices = joined$State,
                                     selected = "Alabama")
                         ),
                         mainPanel(
                           textOutput("description1"),
                           plotOutput("plot1"),
                           textOutput("description2"),
                           plotOutput("plot2")
                         )
                       )
              )
  )
)



shinyUI(my.ui)