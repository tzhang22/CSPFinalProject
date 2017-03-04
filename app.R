library("httr")
library("jsonlite")
library("shiny")
library("ggplot2")
library("dplyr")

source("keyFile.R")

base.url <- "http://api.musixmatch.com/ws/1.1/tracking.url.get?"
  
econ.csv <- read.csv("data/ECN_2012_US_52A1.csv", stringsAsFactors = FALSE)

econ.table <- data.frame(econ.csv)

filtered.econ <- filter(econ.table, NAICS.display.label == "Finance and insurance") %>% 
            select(GEO.display.label, PAYANN, ESTAB, EMP)

filtered.econ$EMP <- cut(as.numeric(filtered.econ$EMP), breaks <- c(0, 10000,20000,40000,80000,160000,
                                                       320000,640000))

View(filtered.econ)

ggplot(data = filtered.econ) +
  geom_point(mapping = aes(x = ESTAB, y = PAYANN, color = EMP), size = 3) +
  scale_color_brewer(palette = "BuPu")
  

ui <- fluidPage(
  
  
)

server <- function(input, output) {
  
  
}

shinyApp(ui = ui, server = server)