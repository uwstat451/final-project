library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(readxl)


ui <- bslib::page_navbar(
  title = "STAT451 Group Shiny Application Draft",
  
  bslib::nav_panel("Panel 1",
                   h3("This panel is reserved for another team member.")
  ),
  
  bslib::nav_panel("Panel 2",
                   h3("This panel is reserved for another team member.")
  ),
  
  bslib::nav_panel("Panel 3",
                   h3("This panel is reserved for another team member.")
  ),
  
  bslib::nav_panel("GHG Emissions (Yikai Wang)",
                   sidebarLayout(
                     sidebarPanel(
                       selectInput("selectPlot", "Choose visualization:",
                                   choices = c(
                                     "CO2 vs Other Gases",
                                     "Non-CO2 Gases Comparison",
                                     "Top CO2 Industries"
                                   ))
                     ),
                     mainPanel(
                       plotOutput("myPlot", height = "600px")
                     )
                   )
  )
)