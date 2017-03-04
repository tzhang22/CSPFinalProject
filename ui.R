<<<<<<< HEAD
library("tidyr")

econ.csv <- read.csv("data/ECN_2012_US_52A1.csv", stringsAsFactors = FALSE)

econ.table <- data.frame(econ.csv)

filtered.econ <- filter(econ.table, NAICS.display.label == "Finance and insurance") %>% 
  select(GEO.display.label, PAYANN, ESTAB, EMP)

filtered.econ$PAYANN <- as.numeric(filtered.econ$PAYANN)
filtered.econ$ESTAB <- as.numeric(filtered.econ$ESTAB)
filtered.econ$EMP <- as.numeric(filtered.econ$EMP)

filtered.econ$EMP <- cut(filtered.econ$EMP, breaks <- c(0, 10000,20000,40000,80000,160000,
                                                        320000,640000))

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

colnames(filtered.econ) <- c("State", "PAYANN", "ESTAB", "EMP")

joined.one <- left_join(regions.long, filtered.econ, by = "State")


# Makes a UI using a fluidPage layout
ui <- fluidPage(
  
  # Makes a new title Panel
  titlePanel("2012 Economic Revenue in the United States"),
  
  sidebarLayout(
    
    # Includes three items on the left panel
    sidebarPanel(
      
      # A checkboxGroupInput that lets the user select which flowers are shown
      checkboxGroupInput("checkGroup", label = h4("Region"), 
                         choices = list("Northwest" = "Northwest",
                                        "South" = "South",
                                        "Northeast" = "Northeast",
                                        "Midwest" = "Midwest",
                                        "Southwest" = "Southwest"),
                         selected = c("Northwest", "South", "Northeast", "Midwest",
                                      "Southwest"))
      
      # A sliderInput labeled "Sepal Length" that lets the user select the range of
      # Sepal length to be shown
#      sliderInput('sepal.choice', label="Sepal Length", min=round(sepal.length.range[1], 2), 
#                  max=round(sepal.length.range[2], 2), value=round(sepal.length.range, 2)),
      
      # A selectInput labeled "Color Scheme", that allows the user to select which color scheme they want
#      selectInput('color.choice', label="Color Scheme", choices=c('Primary', 'Cool', 'Hot'))
    ),
    
    # The main Panel includes both the plot and table in two tabs
    mainPanel(
      
      tabsetPanel(type = "tabs", 
                  tabPanel("Plot", h4(textOutput("summary")), plotOutput("plot", click = "plot_click"), 
                           verbatimTextOutput("info")), 
                  tabPanel("Table", h4(textOutput("table.summary")), tableOutput("table"))
      )
    )
  )
)

# Makes the UI
shinyUI(ui)
