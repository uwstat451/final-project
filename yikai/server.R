library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(gapminder)

df <- read_excel("yikai/data.xlsx")
server <- function(input, output) {
## Panel 4

  output$myPlot <- renderPlot({
    
    # ---- Plot A: CO2 vs Other Gases ----
    if (input$selectPlot == "CO2 vs Other Gases") {
      
      df_simple <- df %>%
        select(GHG, Emission = `Supply Chain Emission Factors with Margins`) %>%
        group_by(GHG) %>%
        summarise(total_emission = sum(Emission, na.rm = TRUE)) %>%
        mutate(category = ifelse(GHG == "Carbon dioxide", "CO2", "Other gases")) %>%
        group_by(category) %>%
        summarise(total_emission = sum(total_emission))
      
      p <- ggplot(df_simple, aes(x = category, y = total_emission, fill = category)) +
        geom_col() +
        labs(title = "CO2 vs Other Greenhouse Gases",
             x = "",
             y = "Total Emissions") +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5))
      
      return(print(p))
    }
    
    # ---- Plot B: Non-CO2 gases comparison ----
    if (input$selectPlot == "Non-CO2 Gases Comparison") {
      
      df_nonco2 <- df %>%
        filter(GHG != "Carbon dioxide") %>%
        select(GHG, Emission = `Supply Chain Emission Factors with Margins`) %>%
        group_by(GHG) %>%
        summarise(total_emission = sum(Emission, na.rm = TRUE)) %>%
        arrange(desc(total_emission))
      
      p <- ggplot(df_nonco2, aes(
        x = total_emission,
        y = reorder(GHG, total_emission),
        fill = GHG)) +
        geom_col() +
        labs(title = "Emissions of Non-CO2 Gases",
             x = "Total Emissions",
             y = "Gas Type") +
        theme_minimal(base_size = 12) +
        theme(plot.title = element_text(face = "bold"),
              legend.position = "none")
      
      return(print(p))
    }
    
    # ---- Plot C: Top CO2 industries ----
    if (input$selectPlot == "Top CO2 Industries") {
      
      top15_co2 <- df %>%
        filter(GHG == "Carbon dioxide") %>%
        group_by(Industry = `2017 NAICS Title`) %>%
        summarise(Total_CO2 = sum(`Supply Chain Emission Factors with Margins`, na.rm = TRUE)) %>%
        arrange(desc(Total_CO2)) %>%
        slice_head(n = 15)
      
      p <- ggplot(top15_co2, aes(
        x = Total_CO2,
        y = reorder(Industry, Total_CO2))) +
        geom_col(fill = "red") +
        labs(title = "Top 15 Industries by CO2 Emission",
             x = "CO2 emission intensity (kg per 2021 USD)",
             y = NULL) +
        theme_minimal(base_size = 12) +
        theme(plot.title = element_text(face = "bold"),
              axis.text.y = element_text(size = 8))
      
      return(print(p))
    }
    
  })
}