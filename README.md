# CSPFinalProject

## About Dataset

Our project works with four US Economic-related data sets, created and maintained by the US Government. 

## Our Investigation

As part of our investigation of this data we compare the Poverty, Taxes, Income, and Employment metrics for each state.

## About Our Analysis

We used a Shiny App to document our investigation and analysis of the data. Largely through the use of `plotly`, `dplyr`, `ggplot` we were able to produce the outputs we wanted. In order to avoid too much data manipulation every time the app is loaded, we created and wrote many datasets and stored them so that we just have to read them in. A challenge that we faced was getting the country map to properly display regions, or specific states when they were selected. This was solved though the use of the `state.csv` file and using `full_join` to join the data with the abbreviations of states' names.

## Links to the data sets we used:
- [Taxes](https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=STC_2015_00A1&prodType=table)
- [Employment Status](https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_15_5YR_DP03&prodType=table)
- [Income per Household](https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_15_5YR_DP03&prodType=table)
- [Poverty Rates](https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_15_5YR_S1701&prodType=table)

## Team Members

 - Tony Zhang

 - Timothy McAleer

 - Xinyu Rhea Chen

 - Zhihao Sam Yang
