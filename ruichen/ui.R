library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(gapminder)

ui <- page_navbar(
  title = "My CO2 application",
  
  nav_panel("Panel 1",
            
            sidebarLayout(
              
              sidebarPanel(),
              
              mainPanel(
                
                h4("Research Question: How have CO2 emissions changed over time, and which countries show the largest increases or decreases?"),
                
                tabsetPanel(
                  
                  # ------------ Panel 1: Global Trends ------------
                  tabPanel(
                    "Global Trends",
                    
                    # ----- plot1 -----
                    plotOutput("plot1"),
                    
                    h5("Key Findings for Global Average Trend"),
                    p("• Global average CO₂ emissions increased steadily from 1990 to the mid-2000s."),
                    p("• After 2010, emissions fluctuate but stay at a higher overall level."),
                    p("• The long-term pattern suggests a continuous global rise with short-term variation."),
                    
                    tags$hr(),
                    
                    # ----- plot2 -----
                    plotOutput("plot2"),
                    
                    h5("Key Findings for Percent Change (Relative to 2020)"),
                    p("• Most years before 2020 show positive percent changes, meaning higher global totals than the 2020 baseline."),
                    p("• Emissions drop sharply around 2020, then become relatively stable in later years."),
                    p("• Overall, the percent-change pattern confirms that 2020 is a low-emission year compared with earlier decades.")
                  ),
                  
                  # ------------ Panel 2: Country Comparison ------------
                  tabPanel(
                    "Country Comparison",
                    
                    # ----- plot3 -----
                    plotOutput("plot3"),
                    
                    h5("Key Findings for Country Comparison (2000–2021)"),
                    p("• Russia, Türkiye, and Kazakhstan show the largest increases in total CO₂ emissions from 2000 to 2021."),
                    p("• Russia has the biggest increase, reflecting strong industrial growth and energy production during the 2000s–2010s."),
                    p("• Türkiye and Kazakhstan also rise significantly, likely due to economic expansion and continued reliance on fossil fuels."),
                    p("• Poland and Belarus have smaller increases compared to the top three."),
                    
                    tags$hr(),
                    
                    h5("Additional Observations"),
                    p("• The USA shows the largest decrease among all countries, driven by fuel switching (coal → natural gas), clean energy adoption, and environmental policies after 2005."),
                    p("• The UK and Germany also reduce emissions substantially due to strong national climate policies and EU-wide regulations."),
                    p("• Japan and Italy show consistent emission reductions despite economic fluctuations."),
                    
                    tags$hr(),
                    
                    h5("Conclusion"),
                    p("• Developing or rapidly industrializing countries tend to show emission increases because economic growth usually depends on higher energy use and fossil fuels."),
                    p("• High-income countries show emission decreases, reflecting long-term clean energy transitions, policy interventions, and more efficient industries."),
                    p("• Overall, the dataset highlights a global divergence: rising emissions in growing economies versus declines in mature economies.")
                  )
                )
              )
            )
  )
)






