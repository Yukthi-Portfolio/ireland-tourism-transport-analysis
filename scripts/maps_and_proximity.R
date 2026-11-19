
getwd()
if(!require(readr)) install.packages('readr')
if(!require(dplyr)) install.packages('dplyr')
if(!require(ggplot2)) install.packages('ggplot2')
if(!require(tidyverse)) install.packages('tidyverse')
if(!require(leaflet)) install.packages('leaflet')
if(!require(tidygeocoder)) install.packages('tidygeocoder')
if(!require(geosphere)) install.packages('geosphere')
if(!require(leaflet.extras)) install.packages('leaflet.extras')

library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(leaflet)
library(tidygeocoder)
library(geosphere)
library(leaflet.extras)


print("Loading Accommodation and Attraction Data...")

# Load Data
df_accom <- read_csv("Accommodation_Fixed_Final.csv") 
df_attr  <- read_csv("Attractions.csv")   


# --- Handling Missing Coordinates ---
missing_data <- df_accom %>%
  filter(is.na(Latitude) | is.na(Longitude))

if(nrow(missing_data) > 0) {
  print(paste("Found", nrow(missing_data), "rows with missing coordinates. Starting geocoding..."))
  
  missing_data <- missing_data %>%
    mutate(search_address = paste(`Address Line 1`, 
                                  `Address City/Town`, 
                                  `Address County`, 
                                  "Ireland", 
                                  sep = ", "))
  
  # Note: Geocoding requires internet connection
  geocoded_results <- missing_data %>%
    geocode(address = search_address, method = 'osm', lat = new_lat, long = new_long)
  
  df_accom_fixed <- df_accom %>%
    left_join(geocoded_results %>% select(`Property Reg Number`, new_lat, new_long), 
              by = "Property Reg Number") %>%
    mutate(
      Latitude = ifelse(is.na(Latitude), new_lat, Latitude),
      Longitude = ifelse(is.na(Longitude), new_long, Longitude)
    ) %>%
    select(-new_lat, -new_long)
  
  write_csv(df_accom_fixed, "Accommodation_Fixed.csv")
  
} else {
  df_accom_fixed <- df_accom
}

missing_summary <- data.frame(
  Column = c("Latitude", "Longitude"),
  Missing_Pct = c(mean(is.na(df_accom$Latitude)) * 100, 
                  mean(is.na(df_accom$Longitude)) * 100)
)

# 3. Create the Bar Chart (Percentage Wise)
ggplot(missing_summary, aes(x = Column, y = Missing_Pct)) +
  geom_bar(stat = "identity", fill = "salmon", width = 0.5) +
  geom_text(aes(label = sprintf("%.1f%%", Missing_Pct)), vjust = -0.5) +
  labs(title = "Percentage of Missing Coordinates",
       y = "Percentage Missing (%)",
       x = "") +
  ylim(0, 100) + # Scale from 0 to 100%
  theme_minimal()


df_accom_clean <- df_accom %>% filter(!is.na(Latitude) & !is.na(Longitude))
df_attr_clean  <- df_attr %>% filter(!is.na(Latitude) & !is.na(Longitude))


missing_summary <- data.frame(
  Column = c("Latitude", "Longitude"),
  Missing_Pct = c(mean(is.na(df_accom_clean$Latitude)) * 100, 
                  mean(is.na(df_accom_clean$Longitude)) * 100)
)

# 3. Create the Bar Chart (Percentage Wise)
ggplot(missing_summary, aes(x = Column, y = Missing_Pct)) +
  geom_bar(stat = "identity", fill = "salmon", width = 0.5) +
  geom_text(aes(label = sprintf("%.1f%%", Missing_Pct)), vjust = -0.5) +
  labs(title = "Percentage of Missing Coordinates",
       y = "Percentage Missing (%)",
       x = "") +
  ylim(0, 100) + # Scale from 0 to 100%
  theme_minimal()

# ==============================================================================
# 3. STOP POINTS (MODIFIED: BUS ONLY)
# ==============================================================================

print("Loading and Cleaning Stop Points Data (BUS ONLY)...")

# Load Data
stop_points <- read_csv("NaPTAN_Stop_Points.csv", col_types = cols(.default = "c"))

# Clean Stop Points & FILTER FOR BUSES
stop_points_clean <- stop_points %>%
  mutate(
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(Longitude),
    StopType = str_trim(StopType), # Remove whitespace
    StopType = ifelse(is.na(StopType) | StopType == "", "Unknown", StopType)
  ) %>%
  filter(!is.na(Latitude) & !is.na(Longitude)) %>%
  # Filter strictly for Ireland coordinates
  filter(Latitude >= 51 & Latitude <= 56,
         Longitude >= -11 & Longitude <= -5) %>%
  # --- NEW FILTER: Select only Bus Stops (BCT) and Bus Stations (BCS) ---
  filter(StopType %in% c("BCT", "BCS"))

write_csv(stop_points_clean, "stop_points_bus_only.csv")
print(paste("Bus Stops cleaned. Rows remaining:", nrow(stop_points_clean)))


# ==============================================================================
# 4. LEAFLET MAPS
# ==============================================================================

print("Generating Combined Leaflet Map...")

m_combined <- leaflet() %>%
  addTiles() %>% 
  
  # Layer 1: Accommodation
  addCircleMarkers(
    data = df_accom_clean,
    lng = ~Longitude, lat = ~Latitude,
    radius = 3, color = "blue", stroke = FALSE, fillOpacity = 0.5,
    group = "Accommodation",
    popup = ~paste("Name:", `Account Name`, "<br>", "Type:", `Sector`)
  ) %>%
  
  # Layer 2: Attractions
  addCircleMarkers(
    data = df_attr_clean,
    lng = ~Longitude, lat = ~Latitude,
    radius = 3, color = "red", stroke = FALSE, fillOpacity = 0.5,
    group = "Attractions",
    popup = ~paste("Name:", `Name`, "<br>", "Type:", `Tags`)
  ) %>%
  
  # Layer Control
  addLayersControl(
    overlayGroups = c("Accommodation", "Attractions"),
    options = layersControlOptions(collapsed = FALSE)
  )

print(m_combined)





# Area Density of both Attractions and Accommodations throughout Ireland and stop points location
# Accommodations: Ensure coords are numeric
df_clean_accom <- df_accom %>% 
  mutate(
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(Longitude)
  ) %>%
  filter(!is.na(Latitude) & !is.na(Longitude))

# Attractions: Ensure coords are numeric
df_clean_attr <- df_attr %>% 
  mutate(
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(Longitude)
  ) %>%
  filter(!is.na(Latitude) & !is.na(Longitude))

# Stop Points: Ensure coords are numeric AND filter for Buses
df_clean_stops <- stop_points %>% 
  mutate(
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(Longitude)
  ) %>%
  filter(!is.na(Latitude) & !is.na(Longitude)) %>%
  filter(StopType %in% c("BCT", "BCS"))

# 2. Calculate Densities (for Blue/Red bubbles)
# ------------------------------------------------------------------------------
dens_accom <- df_clean_accom %>%
  group_by(`Address County`) %>%
  summarise(
    Count = n(),
    Lat = mean(Latitude, na.rm = TRUE),
    Long = mean(Longitude, na.rm = TRUE)
  )

dens_attr <- df_clean_attr %>%
  group_by(`County`) %>%
  summarise(
    Count = n(),
    Lat = mean(Latitude, na.rm = TRUE),
    Long = mean(Longitude, na.rm = TRUE)
  )

leaflet() %>%
  addTiles() %>%
  
  # --- Layer 1: Accommodation Density (Blue Bubbles) ---
  addCircleMarkers(
    data = dens_accom,
    lng = ~Long, lat = ~Lat,
    radius = ~sqrt(Count) * 2, 
    color = "blue",
    fillOpacity = 0.4, 
    stroke = FALSE,
    group = "Accommodations (Density)",
    popup = ~paste("<b>", `Address County`, "</b><br>Accommodations:", Count)
  ) %>%
  
  # --- Layer 2: Attraction Density (Red Bubbles) ---
  addCircleMarkers(
    data = dens_attr,
    lng = ~Long, lat = ~Lat,
    radius = ~sqrt(Count) * 2,
    color = "red",
    fillOpacity = 0.6,
    stroke = FALSE,
    group = "Attractions (Density)",
    popup = ~paste("<b>", `County`, "</b><br>Attractions:", Count)
  ) %>%
  
  # --- Layer 3: Exact Stop Points (Green Dots) ---
  addCircleMarkers(
    data = df_clean_stops,
    lng = ~Longitude, lat = ~Latitude,
    radius = 1,        # Small dot for precision
    color = "green",
    fillOpacity = 1,   # Solid color
    stroke = FALSE,
    group = "Bus Stops (Exact)",
    popup = ~paste("<b>Stop:</b>", CommonName)
  ) %>%
  
  # --- Layer Control ---
  addLayersControl(
    overlayGroups = c("Accommodations (Density)", "Attractions (Density)", "Bus Stops (Exact)"),
    options = layersControlOptions(collapsed = FALSE)
  )





accom_counts <- df_accom %>%
  mutate(County = str_to_title(str_trim(`Address County`))) %>% # Clean County Name
  group_by(County) %>%
  summarise(Count = n()) %>%
  mutate(Type = "Accommodation") # Add a label column

# Count Attractions per County
attr_counts <- df_attr %>%
  mutate(County = str_to_title(str_trim(County))) %>% # Clean County Name
  group_by(County) %>%
  summarise(Count = n()) %>%
  mutate(Type = "Attraction") # Add a label column

# Combine the two datasets
plot_data <- bind_rows(accom_counts, attr_counts) %>%
  filter(!is.na(County)) # Remove any NAs if they exist

# 3. Generate the Bar Graph
# ------------------------------------------------------------------------------
ggplot(plot_data, aes(x = reorder(County, -Count), y = Count, fill = Type)) +
  
  # Create Bars (side-by-side)
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  
  # Colors
  scale_fill_manual(values = c("Accommodation" = "blue", "Attraction" = "red")) +
  
  # Labels and Titles
  labs(
    title = "Tourism Infrastructure by County",
    subtitle = "Number of Accommodations vs. Attractions",
    x = "County",
    y = "Total Count",
    fill = "Facility Type"
  ) +
  
  # Styling
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), # Rotate x-axis labels vertical
    panel.grid.major.x = element_blank() # Remove vertical grid lines for cleaner look
  )


# ==============================================================================
# 5. PROXIMITY ANALYSIS (Accom/Attractions vs BUS STOPS)
# ==============================================================================

print("Starting Proximity Analysis (Distance to nearest BUS Stop)...")

calculate_min_distance <- function(lat, lon, stop_data) {
  current_point <- c(lon, lat)
  stop_locs <- cbind(stop_data$Longitude, stop_data$Latitude)
  distances <- distHaversine(current_point, stop_locs) / 1000 # km
  return(min(distances, na.rm = TRUE))
}

# --- Sampling (1000 rows) ---
set.seed(123)
n_sample <- 1000

accom_sample <- df_accom_clean %>% sample_n(min(n_sample, nrow(df_accom_clean)))
attr_sample <- df_attr_clean %>% sample_n(min(n_sample, nrow(df_attr_clean)))

print("Calculating distances for Accommodation sample...")
accom_sample$min_dist_km <- mapply(
  calculate_min_distance,
  accom_sample$Latitude,
  accom_sample$Longitude,
  MoreArgs = list(stop_data = stop_points_clean) # Now using only BUS stops
)

print("Calculating distances for Attraction sample...")
attr_sample$min_dist_km <- mapply(
  calculate_min_distance,
  attr_sample$Latitude,
  attr_sample$Longitude,
  MoreArgs = list(stop_data = stop_points_clean) # Now using only BUS stops
)

# Categorize distances
distance_breaks <- c(0, 0.5, 1, 2, 5, Inf)
distance_labels <- c("< 0.5 km", "0.5-1 km", "1-2 km", "2-5 km", "> 5 km")

accom_sample$dist_cat <- cut(accom_sample$min_dist_km, breaks = distance_breaks, labels = distance_labels)
attr_sample$dist_cat  <- cut(attr_sample$min_dist_km, breaks = distance_breaks, labels = distance_labels)

# Combine for Plotting
prox_accom <- accom_sample %>% count(dist_cat) %>% mutate(Type = "Accommodation", Pct = n/sum(n)*100)
prox_attr <- attr_sample %>% count(dist_cat) %>% mutate(Type = "Attraction", Pct = n/sum(n)*100)

proximity_combined <- bind_rows(prox_accom, prox_attr)


#Heatmap of accommodation and bus stop density
leaflet() %>%
  addTiles() %>%
  addHeatmap(
    data = df_accom_clean,
    lng = ~Longitude, lat = ~Latitude,
    blur = 20, max = 0.05, radius = 15,
    group = "Accommodation Density"
  ) %>%
  addHeatmap(
    data = stop_points_clean,
    lng = ~Longitude, lat = ~Latitude,
    blur = 20, max = 0.05, radius = 15,
    group = "Bus Stop Density" # Compare this layer to see overlaps
  ) %>%
  addLayersControl(
    overlayGroups = c("Accommodation Density", "Bus Stop Density"),
    options = layersControlOptions(collapsed = FALSE)
  )


# ==============================================================================
# 6. VISUALIZATION OF PROXIMITY
# ==============================================================================

print("Generating Proximity Chart...")

proximity_plot <- ggplot(proximity_combined, aes(x = dist_cat, y = Pct, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  
  # --- NEW: Add labels on top of bars ---
  geom_text(
    aes(label = sprintf("%.1f%%", Pct)),       # Format as percentage (e.g., 25.5%)
    position = position_dodge(width = 0.7),    # Align with the dodged bars
    vjust = -0.5,                              # Move text slightly above the bar
    size = 3.5                                 # Adjust text size
  ) +
  
  scale_fill_manual(values = c("Accommodation" = "blue", "Attraction" = "red")) +
  labs(
    title = "Accessibility: Distance to Nearest BUS Stop",
    subtitle = "Comparison between Accommodations and Attractions",
    x = "Distance (km)",
    y = "Percentage (%)"
  ) +
  theme_minimal()

print(proximity_plot)



# ==============================================================================
# 7. SUMMARY STATS
# ==============================================================================

df_accom_clean <- df_accom_clean %>% mutate(County = str_to_title(`Address County`))
df_attr_clean  <- df_attr_clean %>% mutate(County = str_to_title(County))

accom_counts <- df_accom_clean %>% group_by(County) %>% summarise(Accom_Total = n())
attr_counts  <- df_attr_clean  %>% group_by(County) %>% summarise(Attr_Total = n())

county_stats <- full_join(accom_counts, attr_counts, by = "County") %>%
  mutate(
    Accom_Total = replace_na(Accom_Total, 0),
    Attr_Total  = replace_na(Attr_Total, 0)
  )

print("Final County Statistics:")
print(county_stats)


# ==============================================================================
# C. COUNTY INFRASTRUCTURE RATIO (CORRECTED)
# ==============================================================================
print("Calculating Infrastructure Ratios...")

  infrastructure_summary <- county_stats %>%
    mutate(
      # Ensure numeric type and avoid division by zero
      Accom_Total = as.numeric(Accom_Total),
      Attr_Total = as.numeric(Attr_Total),
      
      # Use pmax to treat 0 accommodations as 1 to avoid 'Inf' errors
      Accom_Safe = pmax(Accom_Total, 1),
      
      # Calculate Ratio
      Attr_per_Accom = round(Attr_Total / Accom_Safe, 2)
    ) %>%
    arrange(desc(Attr_per_Accom)) %>%
    select(County, Accom_Total, Attr_Total, Attr_per_Accom)
  
  # FIX: Convert to standard data.frame before printing to avoid "width" errors
  print("Top Counties by Attractions-to-Accommodation Ratio:")
  print(as.data.frame(head(infrastructure_summary, 10)))
  
  print("Bottom Counties (High Hotels, Low Attractions):")
  print(as.data.frame(tail(infrastructure_summary, 10)))
  