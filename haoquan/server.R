library(shiny)
library(ggplot2)
library(dplyr)
library(tibble)
library(forcats)
suppressPackageStartupMessages(library(useeior))

source("haoquan/data_prep.R")

server <- function(input, output, session){

  mdl <- reactive({
    m <- build_model_safe()
    validate(need(!is.null(m), "Failed to build a USEEIO model; please check your useeior installation."))
    m
  })

  intensity_dat <- reactive({
    intensity_data(mdl(),
                   top_n = input$top_n,
                   dedup_geo = isTRUE(input$dedup_geo))
  })

  legend_levels <- reactive({
    if (!isTRUE(input$sort_legend_by_intensity)) return(cat_levels())
    df <- intensity_dat()
    if (!all(c("supercat","GHG") %in% names(df))) return(cat_levels())
    ord <- df %>% group_by(supercat) %>% summarise(avg = mean(GHG, na.rm=TRUE), .groups="drop") %>%
      arrange(desc(avg)) %>% pull(supercat) %>% as.character()
    unique(c(ord, cat_levels()))
  })

  output$plot_intensity <- renderPlot({
    df <- intensity_dat()
    pal <- cat_palette()

    df$supercat <- factor(df$supercat, levels = legend_levels())
    
    caption_text <-
      "Source: U.S. EPA USEEIO via useeior package. Intensities show how carbon-dense production is; high intensity ≠ high total emissions. \
      Values are model-based estimates; regional technologies, imports, and data vintage introduce uncertainty. \
      Labels show sector code; check 'Deduplicate geography' to collapse US-GA/RoUS."

    ggplot(df, aes(x = display, y = GHG, fill = supercat)) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = pal, drop = FALSE, name = "Category",
                        guide = guide_legend(reverse = FALSE)) +
      labs(
        title = "Top sectors by embodied GHG INTENSITY",
        subtitle = df$._subtitle_[1],
        x = NULL,
        y = "kg CO2e per dollar (purchaser prices, 2021)",
        caption = caption_text
      ) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "right")
  })

  output$intensity_explain <- renderUI({
    df <- intensity_dat()
    tags$div(
      tags$b("How to use the UI"), tags$br(),
      "Use the slider to choose the top N sectors by intensity. Toggle 'Deduplicate geography' to collapse ",
      "US-GA/RoUS into a single bar per sector code (or leave it off to compare locations explicitly). ",
      "Toggle 'Order legend by average intensity' to rank the legend by current data (useful when the top N changes). ",
      "Read from top to bottom: higher bars are more carbon-dense per dollar; colors show the broad category.", tags$br(), tags$br(),
      
      tags$b("Question to answer"), tags$br(),
      "The figure answers: Which sectors are the most carbon-intensive per dollar of output, and what broad patterns emerge across the economy? ",
      "Because it uses embodied GHG intensity (kg CO2e per dollar), it isolates how “dirty per dollar” production is, independent of how much we buy.", tags$br(), tags$br(),
      
      tags$b("What to conclude"), tags$br(),
      "Waste management (landfill methane) is the most carbon-dense; fossil extraction and electricity follow; ",
      "agriculture is high; pipelines/refining, air & water transport, and cement/metals are also high, while public services/services are lower. ",
      "Priority levers are methane control, grid decarbonization, reduced fuel-supply leaks, transport efficiency/modal shift, and lower-carbon materials. ",
      "Pair this with the totals view to target sectors that are both intense and large.", tags$br(), tags$br(),
      
      tags$b("Term explanation"), tags$br(),
      "Embodied (supply-chain) GHG intensity is the kg of CO2-equivalent per 1 USD of sector output, ",
      "converted to 2021 purchaser prices using the 100-year GWP convention. Labels show the sector code ",
      "and geography (US-GA = Georgia; RoUS = rest of U.S.). A high intensity indicates production that emits a lot per dollar, ",
      "but it does not automatically mean that a sector has the largest total emissions.", tags$br(), tags$br()
    )
  })

  totals_dat <- reactive({
    mode <- input$tot_loc_mode
    show_other <- isTRUE(input$tot_show_other)
    totals_data(mdl(), loc_mode = mode, include_other = show_other)
  })

  output$plot_totals <- renderPlot({
    td <- totals_dat()
    pal <- cat_palette()

    td$supercat <- factor(td$supercat, levels = cat_levels())
    cov_txt <- sprintf("Coverage of shown categories: %.1f%%", 100*td$._coverage_[1])

    ggplot(td, aes(x = fct_reorder(supercat, GHG), y = GHG/1e9, fill = supercat)) +
      geom_col(show.legend = TRUE) +  # legend helpful if 'Other' hidden
      coord_flip() +
      scale_fill_manual(values = pal, drop = FALSE, name = "Category") +
      labs(
        title = "Consumption-based GHG by category",
        subtitle = paste0(td$._subtitle_[1], "\n", cov_txt),
        x = NULL,
        y = "Mt CO2e (billions of kg)",
        caption = "Source: U.S. EPA USEEIO via useeior package.\nNote: Summing locations matches a combined demand scenario; it is not a population-weighted average. \
                  You can choose location (US-GA or RoUS or both) on the sidebar; hide 'Other' to focus on headline categories."
      ) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "right")
  })

  output$totals_explain <- renderUI({
    td <- totals_dat()
    tags$div(
      tags$b("How to use the UI"), tags$br(),
      "Choose the location mode: 'All (sum)', 'US-GA only', or 'RoUS only'. ",
      "Use 'Show Other' to include or hide the residual categories; watch the coverage percentage in the subtitle to understand how much of the total is shown. ",
      "Bars are ordered from largest to smallest; colors match the intensity plot to make cross-panel comparisons easy. ",
      "Start with 'All (sum)' to see the big emitters, then toggle locations to check how the mix shifts.", tags$br(), tags$br(),
      
      tags$b("Question to answer"), tags$br(),
      "This chart summarizes the total (consumption-based) greenhouse-gas emissions after summing all locations in the model, ",
      "rolled up to broad, reader-friendly categories. It answers: Which kinds of spending contribute the most to overall, ",
      "supply-chain emissions when we pool all regions together?", tags$br(), tags$br(),
      
      tags$b("What to conclude"), tags$br(),
      "In the results, 'Other' is the largest block (the residual of many smaller sectors not yet assigned to a headline category), ",
      "followed by a tight cluster—Materials, Food Systems, Electricity & Utilities, and Public Services—",
      "with Transport and Energy & Fuels smaller and Waste the smallest. ",
      "The actionable takeaway is to prioritize high-ton categories—construction/materials (cement, metals, chemicals), ",
      "food systems (diet and agricultural practices), and grid decarbonization—while shrinking 'Other' via finer mapping ",
      "so recommendations can be more specific. This complements the first visualization (intensity per dollar) by focusing on scale: ",
      "where the biggest absolute tons sit.", tags$br(), tags$br(),
      
      tags$b("Term explanation"), tags$br(),
      "Totals are consumption-based, so they include direct and supply-chain emissions, including imports. ",
      "Units are Mt CO2e (billions of kg), and the bars reflect the sum of each location's Consumption demand vector ",
      "(this is not population-weighted or per-capita). Category names match the first visualization. ",
      "'Other' groups sectors we haven't explicitly mapped yet (e.g., services, manufacturing niches). ",
      "A category can be moderate in intensity but large in total if people spend a lot on it (e.g., public services or materials used widely in the economy).",
      tags$br(), tags$br()
    )
  })

}
