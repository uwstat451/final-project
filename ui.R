# root/ui.R

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(forcats)
library(readxl)
library(gapminder)

app_theme <- bs_theme(version = 5, bootswatch = "flatly")

ui <- bslib::page_navbar(
  theme = app_theme,
  title = "STAT 451 Final Project — CO2 & Embodied Emissions",
  
  # =====================
  # Panel 1 — Haoquan
  # =====================
  bslib::nav_panel(
    "Embodied GHG - Haoquan",
    sidebarLayout(
      sidebarPanel(
        tags$h5("Intensity (kg CO2e per $) — Controls"),
        sliderInput("top_n", "Show top N sectors by intensity:",
                    min = 10, max = 30, value = 20, step = 1),
        checkboxInput("dedup_geo",
                      "Deduplicate geography (aggregate US-GA & RoUS by sector code)",
                      value = TRUE),
        checkboxInput("sort_legend_by_intensity",
                      "Order legend by average intensity", value = TRUE),
        tags$hr(),
        tags$h5("Totals (Mt CO2e) — Controls"),
        selectInput("tot_loc_mode", "Totals location:",
                    choices = c("All (sum across locations)" = "all",
                                "US-GA only" = "US-GA",
                                "RoUS only" = "RoUS")),
        checkboxInput("tot_show_other", "Show 'Other' category", value = FALSE),
        helpText("Tip: Uncheck to hide 'Other' and show coverage in the subtitle."),
        tags$hr(),
        helpText("Data source: U.S. EPA USEEIO via {useeior}. First build may take a minute.")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Intensity (kg CO2e per $)",
                   plotOutput("plot_intensity", height = "650px"),
                   tags$small(uiOutput("intensity_explain")),
                   tags$small(uiOutput("intensity_updates"))
          ),
          tabPanel("Totals (Mt CO2e)",
                   plotOutput("plot_totals", height = "550px"),
                   tags$small(uiOutput("totals_explain")),
                   tags$small(uiOutput("totals_updates"))
          )
        )
      )
    )
  ),
  
  # =====================
  # Panel 2 — Ruichen
  # =====================
  bslib::nav_panel(
    "CO2 Over Time - Ruichen",
    sidebarLayout(
      sidebarPanel(
        checkboxInput("linear1", label = "Add trend line?", value = FALSE),
        
        h4("Key Findings"),
        p("Global CO2 emissions increased from the 1990s to the 2010s, with recent years remaining close to the 2020 baseline."),
        p("Türkiye, Russia, and Kazakhstan show the largest increases in total emissions from 2000 to 2021."),
        p("The USA, UK, Germany, and Japan show the largest decreases."),
        p("These patterns suggest mixed regional changes across countries.")
      ),
      mainPanel(
        h4("Research Question: How have CO2 emissions changed over time globally, and which countries show the largest increases or decreases?"),
        plotOutput("plot1"),
        plotOutput("plot2"),
        plotOutput("plot3")
      )
    )
  ),
  
  # =====================
  # Panel 3 — Yikai
  # =====================
  bslib::nav_panel(
    "GHG Emissions - Yikai",
    sidebarLayout(
      sidebarPanel(
        selectInput("selectPlot", "Choose visualization:",
                    choices = c(
                      "CO2 vs Other Gases",
                      "Non-CO2 Gases Comparison",
                      "Top CO2 Industries"
                    )),
        h4("How to read this panel"),
        p("Use the dropdown to switch between different views of the supply-chain GHG data."),
        p("Compare CO2 versus other gases, focus on non-CO2 gases, or see which industries have the highest CO2 intensities.")
      ),
      mainPanel(
        plotOutput("myPlot", height = "600px")
      )
    )
  ),
  
  # =====================
  # Panel 4 — Eric
  # =====================
  bslib::nav_panel(
    "Price and CO2 - Eric",
    sidebarLayout(
      sidebarPanel(
        tags$h5("Industrial electricity prices (USD/MWh)"),
        sliderInput("n_price", "Show top N cheapest states:", 5, 21, 15),
        checkboxInput("avg_price", "Show national average price", TRUE),
        tags$hr(),
        tags$h5("Industrial CO2 emissions (million tons)"),
        sliderInput("n_emiss", "Show top N emitters:", 5, 30, 15),
        checkboxInput("avg_emiss", "Show national average emissions", TRUE),
        tags$hr(),
        h4("How to use this panel"),
        p("Use the sliders to control how many states are shown in the price and emissions views."),
        p("Toggle the national average lines on or off to see how each state compares to the U.S. mean.")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Price 2023", plotOutput("price_plot", height = 520)),
          tabPanel("CO2 2023",   plotOutput("emiss_plot", height = 520))
        )
      )
    )
  )
)