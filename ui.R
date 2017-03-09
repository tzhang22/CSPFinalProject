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

ui <- fluidPage(
    theme = shinytheme("yeti"),
    navbarPage(
        title = "United States Economics (2015)",
        tabPanel(
            title = "About",
            mainPanel(
                h1("United States Economic Prosperity in 2015"),
                h4("This in-depth analysis covers the wealth and prosperity of the United States
                   in 2015. Our goal with this project was to allow users to quickly look at
                   visualizations of the US, and then come to conclusions about the difference
                   between states. One aspect our project is especially good at is comparing two states.
                   This could be helpful if a user is looking to move to a new state, and they want
                   to see what certain key statistics are for that state. There are four main topics
                   that we covered in this project: Taxes, Employment, Income, and Poverty Rates."),
                h3("Taxes"),
                h5("The taxes data set contains many different types of taxes (for example: Sales Tax,
                  Property Tax, Motor Fuels Tax), and graphs how certain states compare with each other
                  in relation to these taxes. This tab also has a feature that compares two states directly,
                  lining up a comparison based on the tax of the user’s choice."),
                h3("Employment"),
                h5("This data set offers employment data for all 50 states across many different demographics.
                  The graph featured in this section has the ability to break the United States into five regions,
                  in order to get a clearer picture of how a certain state compares to the states nearby."),
                h3("Income"),
                h5("The United States Income data for 2015 had such statistics as: Average Income per Household,
                  Average Income per Person, and the Amount of People in a State who had an Income of over $200,000.
                  This data was graphed by use of a country map, plotting the density of the population who were in
                  a selected income bracket for each state. If two specific states are selected, it shows a
                  side-by-side comparison of the two states and the amount of people in the state within the
                  selected bracket."),
                h3("Poverty Rates"),
                h5("This data set had four main measurements for every state: Total Population under the Poverty Line,
                  Percentage of Population under the Poverty Line, Total Population under 18 below the Poverty Line,
                  and Percentage of Population under 18 below the Poverty line. These 4 metrics are graphed in the
                  main graph, which shows the density of the selected statistic on a United States map. There is
                  also an option to compare two states in relation to one of the above statistics. Lastly, there
                  is an option to compare Poverty across the different regions of the United States, and see a
                  breakdown of the states within the selected region.")
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
                    plotOutput("plot1.sam"),
                    textOutput("description1"),
                    br(),
                    br(),
                    plotOutput("plot2.sam"),
                    textOutput("description2")
                    )
                )
            ),
        tabPanel(
            title = "Income",
            sidebarLayout(
                sidebarPanel(
                    selectInput('facet.rhea', label = "Incomes Range",
                                choices = incomes.types),
                    selectInput('state1.rhea', label = "Choose a state to compare to",
                                choices = c("United States", states)),
                    selectInput('state2.rhea', label = "Choose a state to compare against",
                                choices = c("United States", states)),
                    p("This graph shows the Population of People per State that are in the
                             selected Income Bracket. If two states are chosen, an additional
                      graph will appear contrasting the two states' bracket population directly.")
                    ),
                mainPanel(
                    plotlyOutput('incomes.map'),
                    plotlyOutput('state.info.rhea')
                    )
                )
            )
        )
)

# Makes the UI
shinyUI(ui)