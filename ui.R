# Load the shiny, ggplot2, and dplyr libraries
library("shiny")
library("ggplot2")
library("dplyr")

responses <- read.csv("data/intro-survey.csv", stringsAsFactors = FALSE)
shinyUI(my.ui)