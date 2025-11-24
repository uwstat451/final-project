library(shiny)
library(bslib)

app_theme <- bs_theme(version = 5, bootswatch = "flatly")

ui <- bslib::page_navbar(
  theme = app_theme,
  title = "Final Project — Stage 3",
  bslib::nav_panel(
    "Embodied Emissions",
    sidebarLayout(
      sidebarPanel(
        tags$h5("Intensity (kg CO2e per $) — Controls"),
        sliderInput("top_n", "Show top N sectors by intensity:", min = 10, max = 30, value = 20, step = 1),
        checkboxInput("dedup_geo", "Deduplicate geography (aggregate US-GA & RoUS by sector code)", value = TRUE),
        checkboxInput("sort_legend_by_intensity", "Order legend by average intensity", value = TRUE),
        tags$hr(),
        tags$h5("Totals (Mt CO2e) — Controls"),
        selectInput("tot_loc_mode", "Totals location:", choices = c("All (sum across locations)"="all",
                                                                   "US-GA only"="US-GA",
                                                                   "RoUS only"="RoUS")),
        checkboxInput("tot_show_other", "Show 'Other' category", value = FALSE),
        helpText("Tip: Uncheck to hide 'Other' and show coverage in the subtitle."),
        tags$hr(),
        helpText("Data source: U.S. EPA USEEIO via {useeior}. First build may take a minute to download models.")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Intensity (kg CO2e per $)",
                   plotOutput("plot_intensity", height = "650px"),
                   tags$small(uiOutput("intensity_explain"))
          ),
          tabPanel("Totals (Mt CO2e)",
                   plotOutput("plot_totals", height = "550px"),
                   tags$small(uiOutput("totals_explain"))
          )
        )
      )
    )
  )
)

