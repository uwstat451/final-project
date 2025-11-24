library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(gapminder)

ui <- page_navbar(
  title = "My CO2 application",
  
  nav_panel("Panel 1",
            sidebarLayout(
              sidebarPanel(
                checkboxInput("linear1", label = "Add trend line?", value = FALSE),
                
                h4("Key Findings"),
                p("Global CO2 emissions rose steadily from the 1990s to the mid-2010s. When compared to the 2020 baseline, most years before 2020 show positive percent changes, indicating higher global totals than in 2020."),
                p("Türkiye, Russia, and Kazakhstan have the largest increases in total emissions between 2000 and 2021."),
                p("The USA, UK, Germany, and Japan show the largest decreases over the same period."),
                p("Overall, the results show a long-term global rise, but with clear differences across countries.")
              ),
              
              mainPanel(
                h4("Research Question: How have CO2 emissions changed over time, and which countries show the largest increases or decreases?"),
                plotOutput("plot1"),
                plotOutput("plot2"),
                plotOutput("plot3")
              )
            )
  )
)
