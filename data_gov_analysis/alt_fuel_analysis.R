# Alternative Fuel Stations Analysis Script
# Based on "Using data.gov APIs in R" - University of Virginia Library
# https://library.virginia.edu/data/articles/using-data-gov-apis-in-r

# Load required libraries
library(tidyverse)
library(httr)
library(jsonlite)
library(lubridate)
library(maps)
library(viridis)
library(patchwork)
library(DT)
library(scales)

# API Configuration - セキュア版
# .envファイルから環境変数を読み込み
if (file.exists(".env")) {
  readRenviron(".env")
}

# API Configuration
api_key <- Sys.getenv("DATA_GOV_API_KEY")
if (nchar(api_key) == 0) {
  stop("API key not found. Please set DATA_GOV_API_KEY in .env file")
}

# Data Collection Function
collect_fuel_station_data <- function(states = c("CA", "TX", "NY", "FL", "WA"), 
                                     limit = 200) {
  url <- "https://developer.nrel.gov/api/alt-fuel-stations/v1.json"
  all_stations <- list()
  
  for (state in states) {
    cat("Collecting data for", state, "...\n")
    
    response <- GET(url, query = list(
      api_key = api_key, 
      state = state, 
      limit = limit
    ))
    
    if (status_code(response) == 200) {
      data <- content(response, "parsed")
      stations <- data$fuel_stations
      
      # Convert to data frame
      state_data <- tibble(
        id = map_chr(stations, ~as.character(.x$id %||% NA)),
        station_name = map_chr(stations, ~.x$station_name %||% NA_character_),
        fuel_type = map_chr(stations, ~.x$fuel_type_code %||% NA_character_),
        city = map_chr(stations, ~.x$city %||% NA_character_),
        state = map_chr(stations, ~.x$state %||% NA_character_),
        zip = map_chr(stations, ~.x$zip %||% NA_character_),
        status = map_chr(stations, ~.x$status_code %||% NA_character_),
        access_code = map_chr(stations, ~.x$access_code %||% NA_character_),
        latitude = map_dbl(stations, ~.x$latitude %||% NA_real_),
        longitude = map_dbl(stations, ~.x$longitude %||% NA_real_),
        owner_type = map_chr(stations, ~.x$owner_type_code %||% NA_character_),
        open_date = map_chr(stations, ~.x$open_date %||% NA_character_),
        updated_at = map_chr(stations, ~.x$updated_at %||% NA_character_)
      )
      
      all_stations[[state]] <- state_data
      cat("Collected", nrow(state_data), "stations for", state, "\n")
    } else {
      cat("Failed to collect data for", state, "- Status:", status_code(response), "\n")
    }
  }
  
  return(bind_rows(all_stations))
}

# Data Preprocessing Function
preprocess_data <- function(raw_data) {
  processed_data <- raw_data |>
    mutate(
      # Convert date variables
      open_date = as.Date(open_date),
      updated_at = as_datetime(updated_at),
      
      # Convert fuel type to readable format
      fuel_type_full = case_when(
        fuel_type == "CNG" ~ "Compressed Natural Gas",
        fuel_type == "LNG" ~ "Liquefied Natural Gas",
        fuel_type == "LPG" ~ "Liquefied Petroleum Gas",
        fuel_type == "ELEC" ~ "Electric",
        fuel_type == "HY" ~ "Hydrogen",
        fuel_type == "BD" ~ "Biodiesel",
        fuel_type == "E85" ~ "Ethanol (E85)",
        TRUE ~ fuel_type
      ),
      
      # Convert owner type to readable format
      owner_type_full = case_when(
        owner_type == "P" ~ "Private",
        owner_type == "T" ~ "Public/Government",
        owner_type == "LG" ~ "Local Government",
        owner_type == "FG" ~ "Federal Government",
        owner_type == "SG" ~ "State Government",
        TRUE ~ owner_type
      ),
      
      # Calculate installation year
      open_year = year(open_date),
      
      # Calculate station age
      station_age = as.numeric(Sys.Date() - open_date) / 365.25
    )
  
  return(processed_data)
}

# Analysis Functions

# 1. Fuel Type Analysis
analyze_fuel_types <- function(data) {
  fuel_summary <- data |>
    count(fuel_type_full, sort = TRUE) |>
    mutate(
      percentage = n / sum(n) * 100,
      fuel_type_full = fct_reorder(fuel_type_full, n)
    )
  
  # Bar chart
  p1 <- fuel_summary |>
    ggplot(aes(x = fuel_type_full, y = n, fill = fuel_type_full)) +
    geom_col(alpha = 0.8) +
    coord_flip() +
    scale_fill_viridis_d(name = "Fuel Type") +
    labs(
      title = "Number of Stations by Fuel Type",
      x = "Fuel Type",
      y = "Number of Stations",
      caption = "Source: data.gov Alternative Fuel Stations API"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  
  # Pie chart
  p2 <- fuel_summary |>
    ggplot(aes(x = "", y = n, fill = fuel_type_full)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    scale_fill_viridis_d(name = "Fuel Type") +
    labs(
      title = "Fuel Type Distribution (Percentage)",
      caption = "Source: data.gov Alternative Fuel Stations API"
    ) +
    theme_void() +
    theme(legend.position = "bottom")
  
  return(list(summary = fuel_summary, plot = p1 / p2))
}

# 2. Geographic Analysis
analyze_geography <- function(data) {
  # State analysis
  state_fuel_summary <- data |>
    count(state, fuel_type_full) |>
    group_by(state) |>
    mutate(
      state_total = sum(n),
      percentage = n / state_total * 100
    ) |>
    ungroup()
  
  state_totals <- data |>
    count(state, sort = TRUE) |>
    mutate(state = fct_reorder(state, n))
  
  # State plot
  p3 <- state_totals |>
    ggplot(aes(x = state, y = n, fill = state)) +
    geom_col(alpha = 0.8) +
    scale_fill_viridis_d(name = "State") +
    labs(
      title = "Alternative Fuel Stations by State",
      x = "State",
      y = "Number of Stations"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  
  # Stacked bar chart
  p4 <- state_fuel_summary |>
    ggplot(aes(x = state, y = n, fill = fuel_type_full)) +
    geom_col(position = "stack", alpha = 0.8) +
    scale_fill_viridis_d(name = "Fuel Type") +
    labs(
      title = "Station Count by State and Fuel Type",
      x = "State",
      y = "Number of Stations"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # Geographic map
  usa_map <- map_data("state")
  
  p5 <- ggplot() +
    geom_polygon(data = usa_map, aes(x = long, y = lat, group = group), 
                 fill = "lightgray", color = "white", alpha = 0.3) +
    geom_point(data = data, 
               aes(x = longitude, y = latitude, color = fuel_type_full), 
               alpha = 0.6, size = 0.8) +
    scale_color_viridis_d(name = "Fuel Type") +
    labs(
      title = "Geographic Distribution of Alternative Fuel Stations",
      subtitle = "Target States: CA, TX, NY, FL, WA",
      caption = "Source: data.gov Alternative Fuel Stations API"
    ) +
    theme_void() +
    theme(legend.position = "bottom") +
    coord_quickmap(xlim = c(-125, -65), ylim = c(25, 50))
  
  return(list(
    state_summary = state_fuel_summary,
    state_plot = p3 / p4,
    map_plot = p5
  ))
}

# 3. Temporal Analysis
analyze_temporal <- function(data) {
  yearly_trend <- data |>
    filter(!is.na(open_date)) |>
    count(open_year, fuel_type_full) |>
    group_by(open_year) |>
    mutate(yearly_total = sum(n)) |>
    ungroup()
  
  # Annual trend
  p5 <- yearly_trend |>
    group_by(open_year) |>
    summarise(total = sum(n), .groups = "drop") |>
    ggplot(aes(x = open_year, y = total)) +
    geom_line(color = "steelblue", linewidth = 1) +
    geom_point(color = "steelblue", size = 2) +
    scale_x_continuous(breaks = seq(1985, 2025, 5)) +
    labs(
      title = "Annual Alternative Fuel Station Installations",
      x = "Installation Year",
      y = "Number of Installations"
    ) +
    theme_minimal()
  
  # By fuel type
  p6 <- yearly_trend |>
    filter(open_year >= 2000) |>
    ggplot(aes(x = open_year, y = n, color = fuel_type_full)) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.5) +
    scale_color_viridis_d(name = "Fuel Type") +
    scale_x_continuous(breaks = seq(2000, 2025, 5)) +
    labs(
      title = "Annual Installations by Fuel Type (2000 onwards)",
      x = "Installation Year",
      y = "Number of Installations"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  return(list(
    yearly_data = yearly_trend,
    plot = p5 / p6
  ))
}

# 4. Ownership Analysis
analyze_ownership <- function(data) {
  ownership_summary <- data |>
    count(owner_type_full, fuel_type_full) |>
    group_by(owner_type_full) |>
    mutate(
      owner_total = sum(n),
      percentage = n / owner_total * 100
    ) |>
    ungroup()
  
  owner_totals <- data |>
    count(owner_type_full, sort = TRUE) |>
    mutate(owner_type_full = fct_reorder(owner_type_full, n))
  
  # Owner type plot
  p7 <- owner_totals |>
    ggplot(aes(x = owner_type_full, y = n, fill = owner_type_full)) +
    geom_col(alpha = 0.8) +
    coord_flip() +
    scale_fill_viridis_d(name = "Owner Type") +
    labs(
      title = "Number of Stations by Owner Type",
      x = "Owner Type",
      y = "Number of Stations"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  
  # Percentage plot
  p8 <- ownership_summary |>
    ggplot(aes(x = owner_type_full, y = n, fill = fuel_type_full)) +
    geom_col(position = "fill", alpha = 0.8) +
    coord_flip() +
    scale_fill_viridis_d(name = "Fuel Type") +
    scale_y_continuous(labels = percent) +
    labs(
      title = "Fuel Type Distribution by Owner Type (Percentage)",
      x = "Owner Type",
      y = "Percentage"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  return(list(
    summary = ownership_summary,
    plot = p7 / p8
  ))
}

# 5. City Analysis
analyze_cities <- function(data) {
  top_cities <- data |>
    count(city, state, sort = TRUE) |>
    mutate(city_state = paste(city, state, sep = ", ")) |>
    slice_head(n = 15)
  
  # Top cities plot
  p9 <- ggplot(top_cities, aes(x = fct_reorder(city_state, n), y = n)) +
    geom_col(fill = "steelblue", alpha = 0.8) +
    coord_flip() +
    labs(
      title = "Top 15 Cities by Alternative Fuel Station Count",
      x = "City",
      y = "Number of Stations"
    ) +
    theme_minimal()
  
  # Fuel type in top cities
  top_cities_fuel <- data |>
    mutate(city_state = paste(city, state, sep = ", ")) |>
    filter(city_state %in% top_cities$city_state) |>
    count(city_state, fuel_type_full) |>
    group_by(city_state) |>
    mutate(
      city_total = sum(n),
      percentage = n / city_total * 100
    ) |>
    ungroup()
  
  top_10_cities <- top_cities$city_state[1:10]
  
  p10 <- top_cities_fuel |>
    filter(city_state %in% top_10_cities) |>
    ggplot(aes(x = fct_reorder(city_state, -city_total), y = n, fill = fuel_type_full)) +
    geom_col(position = "stack", alpha = 0.8) +
    scale_fill_viridis_d(name = "Fuel Type") +
    labs(
      title = "Fuel Type Distribution in Top 10 Cities",
      x = "City",
      y = "Number of Stations"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
  
  return(list(
    top_cities = top_cities,
    city_plot = p9,
    fuel_plot = p10
  ))
}

# Main Analysis Function
run_complete_analysis <- function(data_file = "alt_fuel_data.RData") {
  # Load or collect data
  if (file.exists(data_file)) {
    cat("Loading existing data...\n")
    load(data_file)
  } else {
    cat("Collecting new data...\n")
    combined_data <- collect_fuel_station_data()
    combined_data <- preprocess_data(combined_data)
    save(combined_data, file = data_file)
  }
  
  cat("Dataset summary:\n")
  cat("Total stations:", nrow(combined_data), "\n")
  cat("Target states:", paste(unique(combined_data$state), collapse = ", "), "\n")
  cat("Fuel types:", length(unique(combined_data$fuel_type_full)), "\n")
  cat("Date range:", min(combined_data$open_date, na.rm = TRUE), "to", max(combined_data$open_date, na.rm = TRUE), "\n")
  
  # Run all analyses
  cat("\nRunning fuel type analysis...\n")
  fuel_analysis <- analyze_fuel_types(combined_data)
  
  cat("Running geographic analysis...\n")
  geo_analysis <- analyze_geography(combined_data)
  
  cat("Running temporal analysis...\n")
  temporal_analysis <- analyze_temporal(combined_data)
  
  cat("Running ownership analysis...\n")
  ownership_analysis <- analyze_ownership(combined_data)
  
  cat("Running city analysis...\n")
  city_analysis <- analyze_cities(combined_data)
  
  # Generate key insights
  total_stations <- nrow(combined_data)
  most_common_fuel <- combined_data |> count(fuel_type_full, sort = TRUE) |> slice(1) |> pull(fuel_type_full)
  most_common_fuel_pct <- combined_data |> count(fuel_type_full, sort = TRUE) |> slice(1) |> pull(n) / total_stations * 100
  
  peak_year <- combined_data |> 
    filter(!is.na(open_date)) |> 
    count(open_year, sort = TRUE) |> 
    slice(1) |> 
    pull(open_year)
  
  private_pct <- combined_data |> 
    count(owner_type_full) |> 
    filter(owner_type_full == "Private") |> 
    pull(n) / total_stations * 100
  
  cat("\nKey Findings:\n")
  cat("• Total stations:", format(total_stations, big.mark = ","), "\n")
  cat("• Most common fuel type:", most_common_fuel, "(", round(most_common_fuel_pct, 1), "%)\n")
  cat("• Peak installation year:", peak_year, "\n")
  cat("• Private ownership percentage:", round(private_pct, 1), "%\n")
  
  # Return all results
  return(list(
    data = combined_data,
    fuel = fuel_analysis,
    geography = geo_analysis,
    temporal = temporal_analysis,
    ownership = ownership_analysis,
    cities = city_analysis
  ))
}

# Execute the analysis
# Uncomment the line below to run the complete analysis
# results <- run_complete_analysis()

# Example usage:
# To run the analysis and display specific plots:
# results <- run_complete_analysis()
# print(results$fuel$plot)
# print(results$geography$state_plot)
# print(results$temporal$plot)
# print(results$ownership$plot)
# print(results$cities$city_plot)