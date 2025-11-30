
library(shiny)
library(tidyverse)
library(readxl)
library(scales)



co2_raw <- read_excel("ruichen/data.xlsx", sheet = "Data")


co2_long <- co2_raw %>%
  pivot_longer(
    cols = matches("^\\d{4}$"),
    names_to = "year",
    values_to = "emission"
  ) %>%
  mutate(
    year = as.integer(year)
  )

co2_clean <- co2_long %>%
  mutate(emission = as.numeric(emission)) %>%
  filter(!is.na(emission))



server <- function(input, output) {
  output$plot1 <- renderPlot({
    
    global_avg <- co2_clean %>%
      group_by(year) %>%
      summarise(
        avg_emission = mean(emission, na.rm = TRUE)
      )
    
    p <- ggplot(global_avg, aes(x = year, y = avg_emission)) +
      geom_line(color = "steelblue") +
      geom_point(color = "steelblue") +
      scale_x_continuous(
        breaks = global_avg$year,
        labels = global_avg$year
      ) +
      scale_y_continuous(
        limits = c(0, NA),
        breaks = seq(0, max(global_avg$avg_emission), by = 50000),
        labels = scales::comma
      ) +
      labs(
        title = "Global Average CO2 Emissions Over Time",
        subtitle = "Based on available countries each year (in thousand tonnes of CO2)",
        x = "Year",
        y = "Average CO2 Emission"
      ) +
      theme_minimal() +
      theme(
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    p
  })
  
  
  output$plot2 <- renderPlot({
    
    global_total <- co2_clean %>%
      group_by(year) %>%
      summarise(total_emission = sum(emission, na.rm = TRUE))
    
    baseline_2020 <- global_total %>%
      filter(year == 2020) %>%
      pull(total_emission)
    
    global_pct <- global_total %>%
      mutate(
        pct_change = (total_emission - baseline_2020) / baseline_2020 * 100
      )
    
    ggplot(global_pct, aes(x = year, y = pct_change)) +
      geom_line(color = "darkgreen", size = 1) +
      geom_point(color = "darkgreen", size = 2) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
      annotate("text", x = 2022, y = 2, label = "2020 baseline", color = "gray30", hjust = 0) +
      labs(
        title = "Global Percent Change in Total CO2 Emissions (Relative to 2020)",
        subtitle = "Percent change in global total emissions using 2020 as baseline",
        x = "Year",
        y = "Percent Change (%)"
      ) +
      theme_minimal() +
      theme(
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  

  output$plot3 <- renderPlot({
    co2_change <- co2_clean %>%
      filter(year %in% c(2000, 2021)) %>%
      select(Country, year, emission) %>%
      pivot_wider(
        names_from = year,
        values_from = emission,
        names_prefix = "year_"
      ) %>%
      filter(!is.na(year_2000), !is.na(year_2021)) %>%
      mutate(change = year_2021 - year_2000)
    co2_change$Country[co2_change$Country ==
                         "United Kingdom of Great Britain and Northern Ireland"] <- "UK"
    co2_change$Country[co2_change$Country ==
                         "United States of America"] <- "USA"
    
    top5_increase <- co2_change %>%
      arrange(desc(change)) %>%
      slice_head(n = 5) %>%
      mutate(type = "Increase")
    
    top5_decrease <- co2_change %>%
      arrange(change) %>%
      slice_head(n = 5) %>%
      mutate(type = "Decrease")
    
    combined <- bind_rows(top5_increase, top5_decrease)
    
    combined$type <- factor(combined$type,
                            levels = c("Increase", "Decrease"))
    
    ggplot(combined, aes(x = reorder(Country, change), y = change, fill = type)) +
      geom_bar(stat = "identity", width = 0.5) +
      coord_flip() +
      scale_fill_manual(values = c("Increase" = "firebrick", "Decrease" = "steelblue")) +
      labs(
        title = "Change in CO2 Emissions (2000–2021)",
        subtitle = "Top 5 Countries with Increase and Decrease in Total Emissions",
        x = "Country",
        y = "Change in Emissions (Thousand Tonnes)"
      ) +
      theme_minimal()
  })
  
  
}
  

