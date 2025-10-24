library(shiny)
library(dplyr)
library(readr)
library(stringr)
library(sf)

city_bounds <- list(
  
  Ahmedabad = list(lat_min = 22.80, lat_max = 23.30, lon_min = 72.30, lon_max = 72.85),
  Mumbai = list(lat_min = 18.80, lat_max = 19.35, lon_min = 72.70, lon_max = 73.10),
  Pune = list(lat_min = 18.40, lat_max = 18.75, lon_min = 73.70, lon_max = 74.20),
  Surat = list(lat_min = 21.10, lat_max = 21.35, lon_min = 72.75, lon_max = 73.00),
  
  Bengaluru = list(lat_min = 12.80, lat_max = 13.15, lon_min = 77.40, lon_max = 77.85),
  Chennai = list(lat_min = 12.90, lat_max = 13.25, lon_min = 80.10, lon_max = 80.40),
  Hyderabad = list(lat_min = 17.30, lat_max = 17.55, lon_min = 78.30, lon_max = 78.60),
  
  Delhi = list(lat_min = 28.35, lat_max = 28.95, lon_min = 76.80, lon_max = 77.45),
  Kolkata = list(lat_min = 22.45, lat_max = 22.70, lon_min = 88.25, lon_max = 88.55),
  Jaipur = list(lat_min = 26.80, lat_max = 27.05, lon_min = 75.70, lon_max = 76.00),
  Lucknow = list(lat_min = 26.75, lat_max = 27.00, lon_min = 80.85, lon_max = 81.10)
)

carrier_map <- tribble(
  ~MNC, ~CarrierName,
  872, "Airtel",
  870, "Airtel",
  871, "Airtel",
  873, "Airtel",
  874, "Airtel",
  861, "Airtel",
  860, "Airtel", 
  865, "Airtel",
  866, "Airtel",
  868, "Vodafone Idea",
  862, "Vodafone Idea",
  869, "Vodafone Idea",
  863, "Vodafone Idea",
  864, "Vodafone Idea", 
  867, "Vodafone Idea",
  858, "Reliance Jio",
  888, "BSNL",
  999, "Other/Unknown" 
)

raw_full_data <- read_csv("Network Features Data.csv")  #Stores the data as tibble

initial_data <- raw_full_data %>% 
  rename(
    RXLEV_dBm = `RXLEV (dBm)`, 
    SNR_dB = `SNR (dB)`, 
    DL_Speed_kbps = `DL Speed (kbps)`,
    BAND_MHz = BAND
  ) %>%
  #Reorders and selects
  select(City, MNC, RXLEV_dBm, SNR_dB, DL_Speed_kbps, Latitude, Longitude, BAND_MHz, everything()) #Selects all other columns that are not explicitly mentioned

is_valid_geospatial <- function(lat, lon, city) {
  
  bounds <- city_bounds[[city]]
  
  if (!is.null(bounds)) {
    return(lat >= bounds$lat_min & lat <= bounds$lat_max &
             lon >= bounds$lon_min & lon <= bounds$lon_max)
  }
  return(FALSE) 
}

cleaned_data <- initial_data %>%
  
  #Geospatial filtering
  rowwise() %>% 
  mutate(                      #add or modify a col
    is_valid = is_valid_geospatial(Latitude, Longitude, City)  
  ) %>%
  ungroup() %>%                #reverts the data frame
  filter(is_valid == TRUE) %>% # Keep only the valid rows
  select(-is_valid) %>%        #drop col is_valid
  
  #Carrier mapping
  left_join(carrier_map, by = "MNC") %>%  #joins carrier_map with main data with MNC as common key
  
  mutate(
    Carrier = if_else(is.na(CarrierName), "Other/Unknown", CarrierName)
  ) %>%
  select(-CarrierName) %>% 
  
  #Feature engineering
  mutate(
    Signal_Strength_Score = cut(RXLEV_dBm,
                                breaks = c(-Inf, -100, -90, -80, -70, -50, Inf),
                                labels = c("1 (Poor)", "2 (Fair)", "3 (Good)", "4 (Very Good)", "5 (Excellent)", "1 (Poor)"),
                                right = FALSE,                #Right interval excluded
                                ordered_result = TRUE),       #Makes ordinal
    
    SNR_Quality = cut(SNR_dB,
                      breaks = c(-Inf, 5, 13, 20, Inf), 
                      labels = c("Poor", "Fair", "Good", "Excellent"),
                      right = FALSE,
                      ordered_result = TRUE)
  )
final_data_ready <- cleaned_data %>%
  mutate(
    #Population_Density = 
    
    City = as.factor(City),       #factor-Represent categorical variables
    Carrier = as.factor(Carrier),
    BAND_MHz = as.factor(BAND_MHz)
  )

ui <- fluidPage(
  
  titlePanel("AuraNet"),
  
)

server <- function(input, output) {
  
  
}
 
shinyApp(ui = ui, server = server)