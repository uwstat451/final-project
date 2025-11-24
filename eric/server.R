library(dplyr)
library(ggplot2)
library(readxl)
library(readr)   
library(scales)

validate_needed <- function(path, label) {
  validate(need(file.exists(path), paste0(label, " not found at ", path)))
}

price_path <- "eric/data_price.xlsx"      # EIA industrial price by state (annual)
emiss_path <- "eric/data_emiss.xlsx" # EIA-923 CO2 by plant (2023)

# Prices (USD/MWh) by state, 2023
validate_needed(price_path, "Price file")
price_state <- read_excel(price_path, skip = 2) |>
  filter(!is.na(State), nchar(State) == 2) |>
  transmute(State, price_MWh = parse_number(`Average Price (cents/kWh)`) * 10) |>
  group_by(State) |>
  summarise(price_MWh = mean(price_MWh, na.rm = TRUE), .groups = "drop") |>
  arrange(price_MWh)
avg_price <- mean(price_state$price_MWh, na.rm = TRUE)

# Industrial CO2 (million tons) by state, 2023
validate_needed(emiss_path, "CO2 file")
emiss_state <- read_excel(emiss_path, skip = 1) |>
  filter(`Sector Group` == "INDUSTRIAL") |>
  group_by(State) |>
  summarise(co2_million = sum(`Tons of CO2 Emissions`, na.rm = TRUE) / 1e6,
            .groups = "drop") |>
  arrange(desc(co2_million))
avg_million <- mean(emiss_state$co2_million, na.rm = TRUE)

server <- function(input, output, session){
  
  output$price_plot <- renderPlot({
    d <- price_state |>
      slice_min(price_MWh, n = input$n_price)
    
    p <- ggplot(d, aes(reorder(State, price_MWh), price_MWh)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      scale_y_continuous(labels = \(x) sprintf("$%.0f", x),
                         expand = expansion(mult = c(0, .08))) +
      labs(title = "Industrial electricity prices by state (2023)",
           subtitle = "Cheapest to most expensive",
           x = "State", y = "USD per MWh") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank(),
            plot.title = element_text(face = "bold", hjust = .5))
    if (input$avg_price)
      p + geom_hline(yintercept = avg_price, linetype = "dashed", color = "firebrick") +
      annotate("text", x = Inf, y = avg_price,
               label = sprintf("Average: $%.0f", avg_price),
               hjust = 1.05, vjust = -0.4, color = "firebrick", size = 3.5)
    else p
  })
  
  output$emiss_plot <- renderPlot({
    d <- emiss_state |>
      slice_head(n = input$n_emiss)
    
    p <- ggplot(d, aes(co2_million, reorder(State, co2_million))) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      scale_x_continuous(labels = label_number(accuracy = .1, suffix = " Mt"),
                         expand = expansion(mult = c(0, .08))) +
      labs(title = "Top states by industrial CO2 emissions (2023)",
           x = "Million metric tons", y = "State") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank(),
            plot.title = element_text(face = "bold", hjust = .5))
    if (input$avg_emiss)
      p + geom_vline(xintercept = avg_million, linetype = "dashed", color = "firebrick") +
      annotate("text", x = avg_million, y = Inf,
               label = sprintf("Average: %.1f Mt", avg_million),
               hjust = -.05, vjust = 1.3, color = "firebrick", size = 3.5)
    else p
  })
}
