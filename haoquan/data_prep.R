suppressPackageStartupMessages({
  library(dplyr); library(tibble); library(forcats); library(ggplot2)
  library(useeior)
})

cat_levels <- function(){
  c("Electricity & Utilities",
    "Energy & Fuels",
    "Food Systems",
    "Materials",
    "Public Services",
    "Transport",
    "Waste",
    "Other")
}

cat_palette <- function(){
  c("Electricity & Utilities" = "#F8766D",
    "Energy & Fuels"          = "#C49A00",
    "Food Systems"            = "#00BA38",
    "Materials"               = "#00BFC4",
    "Public Services"         = "#00B0F6",
    "Transport"               = "#9590FF",
    "Waste"                   = "#F564E3",
    "Other"                   = "#7F7F7F")
}

map_tbl <- tibble::tribble(
  ~root,   ~label,                                   ~supercat,
  "562",   "Waste management & remediation",         "Waste",
  "211",   "Oil & gas extraction",                   "Energy & Fuels",
  "22",    "Electric power & utilities",             "Electricity & Utilities",
  "486",   "Pipeline transportation",                "Energy & Fuels",
  "324",   "Refining: petroleum & coal products",    "Energy & Fuels",
  "483",   "Water transportation",                   "Transport",
  "481",   "Air transportation",                     "Transport",
  "482",   "Transportation",                         "Transport",
  "484",   "Transportation",                         "Transport",
  "485",   "Transportation",                         "Transport",
  "327",   "Cement & nonmetallic minerals",          "Materials",
  "331",   "Primary metals (e.g., steel)",           "Materials",
  "212",   "Mining (excl. oil & gas)",               "Materials",
  "322",   "Materials",                              "Materials",
  "325",   "Materials",                              "Materials",
  "311FT", "Food, beverage & tobacco",               "Food Systems",
  "111CA", "Agriculture: crops & animals",           "Food Systems",
  "GSLE",  "State & local gov. enterprises",         "Public Services"
)

categorize_super <- function(root){
  dplyr::case_when(
    grepl("^(22|221)", root)                                ~ "Electricity & Utilities",
    grepl("^(211|324|486|447|454|2212)", root)              ~ "Energy & Fuels",
    grepl("^(48|49|481|482|483|484|485)", root)             ~ "Transport",
    grepl("^(11|115|311|312)", root)                        ~ "Food Systems",
    grepl("^(23|321|322|325|327|331|332|333|212)", root)    ~ "Materials",
    grepl("^(562)", root)                                   ~ "Waste",
    grepl("^(92|GSLE|GSL|GFG|HSLE)", root)                  ~ "Public Services",
    TRUE                                                    ~ "Other"
  )
}

build_model_safe <- function(){
  avail <- tryCatch(seeAvailableModels(), error = function(e) character(0))
  model_name <- if (length(avail)) as.character(avail[1]) else "USEEIOv2.0"
  tryCatch(buildModel(model_name), error = function(e) NULL)
}

find_ghg_dim <- function(M){
  pat <- "(?i)gwp|warming|greenhouse|co2e|co2"
  list(
    ghg_col = grep(pat, colnames(M), value = TRUE)[1],
    ghg_row = grep(pat, rownames(M), value = TRUE)[1]
  )
}

intensity_data <- function(model, top_n = 20, dedup_geo = TRUE){
  N_adj <- adjustResultMatrixPrice("N", currency_year = 2021, purchaser_price = TRUE, model)
  hits <- find_ghg_dim(N_adj)
  stopifnot(!is.na(hits$ghg_col) || !is.na(hits$ghg_row))

  base <- if (!is.na(hits$ghg_col)) {
    as.data.frame(N_adj) |>
      tibble::rownames_to_column("sector") |>
      dplyr::select(sector, GHG = dplyr::all_of(hits$ghg_col))
  } else {
    tibble::tibble(sector = colnames(N_adj), GHG = as.numeric(N_adj[hits$ghg_row, ]))
  }

  base <- base |>
    mutate(root = sub("/.*", "", sector),
           geo  = sub(".*/", "", sector)) |>
    left_join(map_tbl, by = "root") |>
    mutate(supercat = coalesce(supercat, categorize_super(root)),
           label    = coalesce(label, paste0("Sector ", root)))

  if (isTRUE(dedup_geo)){
    base <- base |>
      group_by(root, label, supercat) |>
      summarise(GHG = max(GHG, na.rm=TRUE), .groups="drop") |>
      mutate(display = paste0(label, " (", root, ")"))
  } else {
    base <- base |>
      mutate(display = paste0(label, " (", root, ") — ", geo))
  }

  base <- base |>
    arrange(desc(GHG)) |>
    slice_head(n = top_n)
  
  ord <- base %>% pull(display)
  base <- base %>% mutate(display = factor(display, levels = rev(ord)))

  model_name <- attr(model, "model")$name %||% "USEEIO"
  indicator_text <- ifelse(!is.na(hits$ghg_col),
                           paste0("column: ", hits$ghg_col),
                           paste0("row: ", hits$ghg_row))
  base$._subtitle_ <- paste0(
    "Embodied = supply-chain emissions per $ of sector output.\n",
    "Colors show broad sector groups. Labels keep NAICS code and geography\n",
    "(US-GA = Georgia; RoUS = rest of U.S.).\n",
    "Model: ", model_name, " | Indicator found in ", indicator_text, "."
  )
  
  base$._model_ <- model_name
  base$._indicator_ <- indicator_text
  base
}

totals_by_location <- function(model, location){
  res <- calculateEEIOModel(model,
                            perspective = "FINAL",
                            demand = "Consumption",
                            location = location,
                            use_domestic_requirements = FALSE)
  `%OR%` <- function(x, y) if (!is.null(x)) x else y
  Hmat <- res$H_l %OR% res$H %OR% res$H_r
  hits <- find_ghg_dim(Hmat)
  stopifnot(!is.na(hits$ghg_col) || !is.na(hits$ghg_row))

  df <- if (!is.na(hits$ghg_col)) {
    as.data.frame(Hmat) |>
      tibble::rownames_to_column("sector") |>
      dplyr::select(sector, GHG = dplyr::all_of(hits$ghg_col))
  } else {
    tibble::tibble(sector = colnames(Hmat), GHG = as.numeric(Hmat[hits$ghg_row, ]))
  }

  df |>
    mutate(root = sub("/.*", "", sector)) |>
    left_join(map_tbl, by = "root") |>
    mutate(supercat = coalesce(supercat, categorize_super(root))) |>
    group_by(supercat) |>
    summarise(GHG = sum(GHG, na.rm=TRUE), .groups="drop") |>
    mutate(location = location)
}

totals_data <- function(model, loc_mode = c("all","US-GA","RoUS"), include_other = FALSE){
  loc_mode <- match.arg(loc_mode)

  locs <- c("US-GA","RoUS")
  if (loc_mode == "US-GA") locs <- "US-GA"
  if (loc_mode == "RoUS")  locs <- "RoUS"

  pieces <- lapply(locs, function(l) totals_by_location(model, l))
  tot <- bind_rows(pieces) |>
    group_by(supercat) |>
    summarise(GHG = sum(GHG), .groups="drop")

  total_sum <- sum(tot$GHG, na.rm=TRUE)
  if (!isTRUE(include_other)){
    shown <- tot |> filter(supercat != "Other")
    coverage <- sum(shown$GHG)/total_sum
    tot <- shown
  } else {
    coverage <- sum(tot$GHG)/total_sum
  }

  mode_label <- if (loc_mode == "all") "All (sum)" else loc_mode
  model_name <- attr(model, "model")$name %||% "USEEIO"
  tot$._subtitle_ <- paste0("Sum of per-location Consumption demand vectors",
                            x = NULL, y = "Mt CO2e (billions of kg)")
  tot$._coverage_ <- coverage
  tot$._loc_mode_ <- mode_label
  tot
}

`%||%` <- function(a,b) if (!is.null(a)) a else b
