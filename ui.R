# Load the shiny, ggplot2, and dplyr libraries
library("tidyr")
library("maps")
library("shiny")
library("ggplot2")
library("dplyr")
library("plotly")

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

# Sam's Portion
income.data <- read.csv("data/Income_Employment_2015.csv", stringsAsFactors = FALSE)

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




ui <- fluidPage(
    navbarPage(
        title = "United States Economics (2015)",
        tabPanel(
            title = "About",
            mainPanel(
                p("Hello")
                )
            ),
        tabPanel(
            title = "Taxes",
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
            ),
        tabPanel(
            title = "Poverty",
            tabsetPanel(
                type = "tabs",
                tabPanel(
                    "Map",
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
                tabPanel(
                    "Graphs",
                    sidebarLayout(
                        # Includes three items on the left panel
                        sidebarPanel(
                            selectInput("state1.tim", label = h3("Select First State"),
                                        choices = filtered.poverty$State,
                                        selected = "Alabama"),
                            selectInput("state2.tim", label = h3("Select Second State"),
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
                tabPanel(
                    "Regions",
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
            ),
        tabPanel(
            title = "Employment Rates",
            sidebarLayout(
                # Includes three items on the left panel
                sidebarPanel(
                    # User can choose region.
                    radioButtons("radio.region.sam", label = h4("Region"),
                                 choices = list("All" = "All",
                                                "Northwest" = "Northwest",
                                                "South" = "South",
                                                "Northeast" = "Northeast",
                                                "Midwest" = "Midwest",
                                                "Southwest" = "Southwest"),
                                 selected = "All"),
                    # User can choose labor force type.
                    selectInput("labor.type", label = h3("Labor Force Type"),
                                choices = c("Employed labor force", "Unemployed labor force", "Armed labor force"),
                                selected = "Employed labor force"),
                    # User can select a state.
                    selectInput("state", label = h3("Select State"),
                                choices = joined$State,
                                selected = "Alabama")
                    ),
                mainPanel(
                    textOutput("description1"),
                    plotOutput("plot1.sam"),
                    textOutput("description2"),
                    plotOutput("plot2.sam")
                    )
                )
            )
        )
)

# Makes the UI
shinyUI(ui)