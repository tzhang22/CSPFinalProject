library("httr")
library("jsonlite")
library("shiny")
library("ggplot2")
library("dplyr")

source("keyFile.R")

print(my.key)

base.url <- "http://ws.audioscrobbler.com/2.0/"

reportTopSong <- function(state.name) {
  country.data <- GET(paste0(base.url, "?method=geo.gettoptracks&country=",
                             state.name, "&api_key=", my.key, "&format=json"))
  
  country.frame <- fromJSON(content(country.data, "text"))
  
  country.frame <- data.frame(country.frame, stringsAsFactors = FALSE)
  
  country.frame <- flatten(country.frame)
  
  top.song <- filter(country.frame, tracks.track.listeners == max(as.numeric(tracks.track.listeners))) %>% 
              select(tracks.track.listeners, tracks.track.name)
  
  return(top.song)
}

result <- reportTopSong("spain")
print(result)

ui <- fluidPage(
  
  
)

server <- function(input, output) {
  
  
}

shinyApp(ui = ui, server = server)