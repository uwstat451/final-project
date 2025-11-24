library(shiny)
library(bslib)

app_theme <- bs_theme(version = 5, bootswatch = "flatly")

ui <- page_navbar(
  theme = app_theme,
  title = "AI Siting: Price & CO2 (2023)",
  nav_panel(
    "Eric — AI Siting",
    sidebarLayout(
      sidebarPanel(
        tags$h5("Industrial electricity prices (USD/MWh)"),
        sliderInput("n_price", "Show top N cheapest states:", 5, 21, 15),
        checkboxInput("avg_price", "Show national average price", TRUE),
        tags$hr(),
        tags$h5("Industrial CO₂ emissions (million tons)"),
        sliderInput("n_emiss", "Show top N emitters:", 5, 30, 15),
        checkboxInput("avg_emiss", "Show national average emissions", TRUE)
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Price 2023", plotOutput("price_plot", height = 520)),
          tabPanel("CO2 2023", plotOutput("emiss_plot", height = 520))
        )
      )
    )
  )
)