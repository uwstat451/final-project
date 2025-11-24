# STAT 451 Final Project — CO2 & Embodied Emissions

This repo contains our group Shiny dashboard for exploring:
- Embodied supply-chain GHG intensity and totals by sector (Haoquan),
- Time trends in CO2 emissions across countries (Ruichen),
- Supply-chain GHG by gas and industry (Yikai).

## Repo Structure

```
final-project/
  ui.R
  server.R
  haoquan/
    ui.R
    server.R
    data_prep.R
  ruichen/
    ui.R 
    server.R
    data.xlsx
  yikai/
    ui.R
    server.R
    data.xlsx
  README.md
```

## Installation

First, clone this repo and move to the directory of the repo.
```
git clone https://github.com/uwstat451/final-project
cd final-project
```

Then, install the pacakages as required.
```r
install.packages(c("shiny","bslib","ggplot2","dplyr","forcats",
                   "readxl","gapminder","tidyverse","scales"))
remotes::install_github("USEPA/useeior")
```

Note that Haoquan's data are included in `USEPA/useeior` package, and data from all other team members are already stored in this repo as separate files.


## Execution

Once you are under the root directory of this repo, you can run the app through the following command:

```r
shiny::runApp(".")
```