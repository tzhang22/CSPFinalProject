# Load the shiny, ggplot2, and dplyr libraries
library("shiny")
library("ggplot2")
library("dplyr")

tax.data <- read.csv("data/Taxes_2015.csv", stringsAsFactors = FALSE)
income.data <- read.csv("data/Income_Employment_2015.csv", stringsAsFactors = FALSE)
poverty.data <- read.csv("data/Poverty_2015.csv", stringsAsFactors = FALSE)


shinyUI(my.ui)