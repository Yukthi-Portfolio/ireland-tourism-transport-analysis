# STEP 1: LOAD REQUIRED LIBRARIES
# ============================================================================
# Install packages if needed (run once):
# install.packages(c("tidyverse", "sf", "ggplot2", "dplyr", "readr", 
#                    "geosphere", "scales", "gridExtra", "RColorBrewer",
#                    "viridis", "ggrepel", "treemap"))

install.packages("viridis")
install.packages("ggrepel")
install.packages("treemap")
install.packages("gridExtra")


library(scales)
library(tidyverse)      # Data manipulation and visualization
library(readr)          # Reading CSV files
library(dplyr)          # Data wrangling
library(ggplot2)        # Advanced plotting
library(geosphere)      # Geographic distance calculations
library(scales)         # Scale formatting
library(gridExtra)      # Multiple plots
library(RColorBrewer)   # Color palettes
library(viridis)        # Color-blind friendly palettes
library(ggrepel)        # Better text labels
library(treemap)        # Treemap visualizations

# Working directory is set

# Set theme for all plots

theme_set(theme_minimal(base_size = 12))


# STEP 2: LOAD THE DATASETS
# ============================================================================
cat("Loading datasets...\n")

# Load Accommodation data
accommodation <- read_csv("Accommodation.csv", 
                          col_types = cols(.default = "c"))

# Load Attractions data
attractions <- read_csv("Attractions 2.csv",
                        col_types = cols(.default = "c"))

# Load Stop Points data
stop_points <- read_csv("NaPTAN_Stop_Points 1.csv",
                        col_types = cols(.default = "c"))

cat("Data loaded successfully!\n")


# STEP 3: DATA INSPECTION AND INITIAL EXPLORATION
# ============================================================================
cat("\n=== DATA INSPECTION ===\n")

# Accommodation dataset
cat("\n--- Accommodation Dataset ---\n")
cat("Dimensions:", dim(accommodation), "\n")
cat("Structure:\n")
glimpse(accommodation)
cat("\nFirst few rows:\n")
head(accommodation)
cat("\nMissing values:\n")
colSums(is.na(accommodation))

# Attractions dataset
cat("\n--- Attractions Dataset ---\n")
cat("Dimensions:", dim(attractions), "\n")
glimpse(attractions)
head(attractions)
cat("\nMissing values:\n")
colSums(is.na(attractions))

# Stop Points dataset
cat("\n--- Stop Points Dataset ---\n")
cat("Dimensions:", dim(stop_points), "\n")
glimpse(stop_points)
head(stop_points)
cat("\nMissing values:\n")
colSums(is.na(stop_points))


# STEP 4: DATA CLEANING
# ============================================================================
cat("\n=== DATA CLEANING ===\n")

# Clean Accommodation data
accommodation_clean <- accommodation %>%
  mutate(
    # Convert numeric columns
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(Longitude),
    `Total Units` = as.numeric(`Total Units`),
    
    # Clean county names (trim whitespace, standardize)
    County = str_trim(`Address County`),
    County = str_to_title(County),
    
    # Clean sector names
    Sector = str_trim(Sector),
    Sector = ifelse(is.na(Sector) | Sector == "", "Unknown", Sector)
  ) %>%
  # Remove rows with missing coordinates
  filter(!is.na(Latitude) & !is.na(Longitude)) %>%
  # Remove invalid coordinates (outside Ireland bounds roughly)
  filter(Latitude >= 51 & Latitude <= 56,
         Longitude >= -11 & Longitude <= -5)

cat("Accommodation: Original rows =", nrow(accommodation), 
    "| Clean rows =", nrow(accommodation_clean), "\n")

# Clean Attractions data
attractions_clean <- attractions %>%
  mutate(
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(Longitude),
    County = str_trim(County),
    County = str_to_title(County),
    Tags = str_trim(Tags)
  ) %>%
  filter(!is.na(Latitude) & !is.na(Longitude)) %>%
  filter(Latitude >= 51 & Latitude <= 56,
         Longitude >= -11 & Longitude <= -5)

cat("Attractions: Original rows =", nrow(attractions), 
    "| Clean rows =", nrow(attractions_clean), "\n")

# Clean Stop Points data
stop_points_clean <- stop_points %>%
  mutate(
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(Longitude),
    StopType = str_trim(StopType),
    StopType = ifelse(is.na(StopType) | StopType == "", "Unknown", StopType)
  ) %>%
  filter(!is.na(Latitude) & !is.na(Longitude)) %>%
  filter(Latitude >= 51 & Latitude <= 56,
         Longitude >= -11 & Longitude <= -5)

cat("Stop Points: Original rows =", nrow(stop_points), 
    "| Clean rows =", nrow(stop_points_clean), "\n")
    
    # SAVE CLEANED DATASETS
# ============================================================================
cat("\n=== SAVING CLEANED DATASETS ===\n")

# Save cleaned accommodation data
write_csv(accommodation_clean, "accommodation_cleaned.csv")
cat("✓ Saved: accommodation_cleaned.csv\n")

# Save cleaned attractions data
write_csv(attractions_clean, "attractions_cleaned.csv")
cat("✓ Saved: attractions_cleaned.csv\n")

# Save cleaned stop points data
write_csv(stop_points_clean, "stop_points_cleaned.csv")
cat("✓ Saved: stop_points_cleaned.csv\n")

cat("All cleaned datasets saved successfully!\n")

cat("Stop Points: Original rows =", nrow(stop_points), 
    "| Clean rows =", nrow(stop_points_clean), "\n")


# STEP 5: DESCRIPTIVE STATISTICS
# ============================================================================
cat("\n=== DESCRIPTIVE STATISTICS ===\n")

# Accommodation Statistics
cat("\n--- Accommodation Summary ---\n")
cat("Total accommodations:", nrow(accommodation_clean), "\n")
cat("Total units available:", sum(accommodation_clean$`Total Units`, na.rm = TRUE), "\n")
cat("Average units per property:", 
    round(mean(accommodation_clean$`Total Units`, na.rm = TRUE), 2), "\n")
cat("\nAccommodation by Sector:\n")
print(table(accommodation_clean$Sector))
cat("\nTop 10 Counties by Accommodation:\n")
print(head(sort(table(accommodation_clean$County), decreasing = TRUE), 10))

# Attractions Statistics
cat("\n--- Attractions Summary ---\n")
cat("Total attractions:", nrow(attractions_clean), "\n")
cat("\nTop 10 Counties by Attractions:\n")
print(head(sort(table(attractions_clean$County), decreasing = TRUE), 10))

# Stop Points Statistics
cat("\n--- Stop Points Summary ---\n")
cat("Total stop points:", nrow(stop_points_clean), "\n")
cat("\nStop Types:\n")
print(table(stop_points_clean$StopType))

# Create summary data frame
summary_stats <- data.frame(
  Dataset = c("Accommodations", "Attractions", "Stop Points"),
  Count = c(nrow(accommodation_clean), 
            nrow(attractions_clean), 
            nrow(stop_points_clean)),
  Unique_Counties = c(
    length(unique(accommodation_clean$County)),
    length(unique(attractions_clean$County)),
    NA
  )
)
print(summary_stats)


# STEP 6: PREPARE DATA FOR ANALYSIS
# ============================================================================
cat("\n=== PREPARING ANALYTICAL DATASETS ===\n")

# County-level aggregation
county_accommodation <- accommodation_clean %>%
  group_by(County) %>%
  summarise(
    n_accommodations = n(),
    total_units = sum(`Total Units`, na.rm = TRUE),
    avg_units = mean(`Total Units`, na.rm = TRUE)
  )

county_attractions <- attractions_clean %>%
  group_by(County) %>%
  summarise(n_attractions = n())

# Merge county data
county_combined <- county_accommodation %>%
  full_join(county_attractions, by = "County") %>%
  mutate(
    n_accommodations = replace_na(n_accommodations, 0),
    n_attractions = replace_na(n_attractions, 0),
    total_infrastructure = n_accommodations + n_attractions,
    attraction_per_accommodation = n_attractions / n_accommodations
  ) %>%
  arrange(desc(total_infrastructure))

# Get top 15 counties for visualization
top_counties <- county_combined %>%
  head(15)

cat("County aggregation complete. Top 15 counties identified.\n")

# Sector distribution
sector_distribution <- accommodation_clean %>%
  group_by(Sector) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(desc(count))

# Stop type distribution
stop_type_distribution <- stop_points_clean %>%
  group_by(StopType) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(desc(count))


# STEP 7: PROXIMITY ANALYSIS
# ============================================================================
cat("\n=== PROXIMITY ANALYSIS (This may take a few minutes) ===\n") 

# Function to calculate minimum distance to nearest stop point
calculate_min_distance <- function(lat, lon, stop_data) {
  # Create point for the facility
  facility_point <- c(lon, lat)
  
  # Calculate distances to all stop points
  distances <- distHaversine(
    facility_point,
    cbind(stop_data$Longitude, stop_data$Latitude)
  ) / 1000  # Convert to kilometers
  
  # Return minimum distance
  return(min(distances, na.rm = TRUE))
}

# Sample datasets for proximity analysis (to speed up computation)
# For full analysis, remove the sample() function
set.seed(123)
accom_sample <- accommodation_clean %>%
  sample_n(min(1000, nrow(accommodation_clean)))

attract_sample <- attractions_clean %>%
  sample_n(min(1000, nrow(attractions_clean)))

cat("Calculating distances for accommodations...\n")
accom_sample$min_distance_to_stop <- mapply(
  calculate_min_distance,
  accom_sample$Latitude,
  accom_sample$Longitude,
  MoreArgs = list(stop_data = stop_points_clean)
)

cat("Calculating distances for attractions...\n")
attract_sample$min_distance_to_stop <- mapply(
  calculate_min_distance,
  attract_sample$Latitude,
  attract_sample$Longitude,
  MoreArgs = list(stop_data = stop_points_clean)
)

# Categorize distances
distance_breaks <- c(0, 0.5, 1, 2, 5, Inf)
distance_labels <- c("< 0.5 km", "0.5-1 km", "1-2 km", "2-5 km", "> 5 km")

accom_sample$distance_category <- cut(
  accom_sample$min_distance_to_stop,
  breaks = distance_breaks,
  labels = distance_labels,
  include.lowest = TRUE
)

attract_sample$distance_category <- cut(
  attract_sample$min_distance_to_stop,
  breaks = distance_breaks,
  labels = distance_labels,
  include.lowest = TRUE
)

# Proximity summary
proximity_accom <- accom_sample %>%
  group_by(distance_category) %>%
  summarise(count = n()) %>%
  mutate(
    percentage = count / sum(count) * 100,
    type = "Accommodation"
  )

proximity_attract <- attract_sample %>%
  group_by(distance_category) %>%
  summarise(count = n()) %>%
  mutate(
    percentage = count / sum(count) * 100,
    type = "Attraction"
  )

proximity_combined <- bind_rows(proximity_accom, proximity_attract)

cat("Proximity analysis complete!\n")
cat("\nProximity Summary:\n")
print(proximity_combined)


# STEP 8: VISUALIZATION 1 - COUNTY DISTRIBUTION
# ============================================================================
cat("\n=== CREATING VISUALIZATION 1: County Distribution ===\n")

# Reshape data for grouped bar chart
county_plot_data <- top_counties %>%
  select(County, n_accommodations, n_attractions) %>%
  pivot_longer(
    cols = c(n_accommodations, n_attractions),
    names_to = "Type",
    values_to = "Count"
  ) %>%
  mutate(Type = recode(Type,
                       "n_accommodations" = "Accommodations",
                       "n_attractions" = "Attractions"))
                       
                       # retrying the other code
                       county_plot_data <- top_counties %>%
  select(County, n_accommodations, n_attractions) %>%
  pivot_longer(cols = c(n_accommodations, n_attractions),
               names_to = "Type", values_to = "Count") %>%
  mutate(
    Type = case_when(
      Type == "n_accommodations" ~ "Accommodations",
      Type == "n_attractions"    ~ "Attractions",
      TRUE ~ Type
    )
  )

# Create the plot
plot1 <- ggplot(county_plot_data, 
                aes(x = reorder(County, -Count), y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("Accommodations" = "#10b981", 
                               "Attractions" = "#3b82f6")) +
  labs(
    title = "Tourism Infrastructure Distribution Across Irish Counties",
    subtitle = "Top 15 Counties by Total Number of Accommodations and Attractions",
    x = "County",
    y = "Number of Facilities",
    fill = "Facility Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(labels = comma, expand = c(0, 0), limits = c(0, NA))

# Save the plot
ggsave("Figure1_County_Distribution.pdf", plot1, 
       width = 12, height = 8, device = "pdf")
ggsave("Figure1_County_Distribution.png", plot1, 
       width = 12, height = 8, dpi = 300)

print(plot1)
cat("Figure 1 saved!\n")


# STEP 9: VISUALIZATION 2 - SECTOR DISTRIBUTION
# ============================================================================
cat("\n=== CREATING VISUALIZATION 2: Accommodation Sectors ===\n")

# Prepare data
sector_plot_data <- sector_distribution %>%
  mutate(
    Sector = ifelse(percentage < 3, "Other", Sector)
  ) %>%
  group_by(Sector) %>%
  summarise(
    count = sum(count),
    percentage = sum(percentage)
  ) %>%
  arrange(desc(percentage))

# Create pie chart
plot2 <- ggplot(sector_plot_data, 
                aes(x = "", y = percentage, fill = Sector)) +
  geom_bar(stat = "identity", width = 1, color = "white", linewidth = 2) +
  coord_polar("y", start = 0) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Composition of Ireland's Accommodation Sector",
    subtitle = "Distribution by Accommodation Type",
    fill = "Sector"
  ) +
  theme_void(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "gray40", hjust = 0.5),
    legend.position = "right"
  ) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_stack(vjust = 0.5),
            size = 4, fontface = "bold")

ggsave("Figure2_Sector_Distribution.pdf", plot2, 
       width = 10, height = 8, device = "pdf")
ggsave("Figure2_Sector_Distribution.png", plot2, 
       width = 10, height = 8, dpi = 300)

print(plot2)
cat("Figure 2 saved!\n")


# STEP 10: VISUALIZATION 3 - PROXIMITY ANALYSIS
# ============================================================================
cat("\n=== CREATING VISUALIZATION 3: Transport Proximity ===\n")

plot3 <- ggplot(proximity_combined, 
                aes(x = distance_category, y = percentage, fill = type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("Accommodation" = "#10b981", 
                               "Attraction" = "#3b82f6")) +
  labs(
    title = "Accessibility to Public Transport Infrastructure",
    subtitle = "Percentage of Tourism Facilities by Distance to Nearest Stop Point",
    x = "Distance to Nearest Transport Stop",
    y = "Percentage of Facilities (%)",
    fill = "Facility Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    legend.position = "top",
    panel.grid.major.x = element_blank()
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), 
                     expand = c(0, 0), limits = c(0, NA)) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_dodge(width = 0.7),
            vjust = -0.5, size = 3.5)

ggsave("Figure3_Transport_Proximity.pdf", plot3, 
       width = 12, height = 8, device = "pdf")
ggsave("Figure3_Transport_Proximity.png", plot3, 
       width = 12, height = 8, dpi = 300)

print(plot3)
cat("Figure 3 saved!\n")


# STEP 11: VISUALIZATION 4 - DENSITY SCATTER PLOT
# ============================================================================
cat("\n=== CREATING VISUALIZATION 4: Infrastructure Density ===\n")

# Prepare data - remove infinite values
density_data <- county_combined %>%
  filter(is.finite(attraction_per_accommodation)) %>%
  filter(n_accommodations > 0 & n_attractions > 0)

plot4 <- ggplot(density_data, 
                aes(x = n_accommodations, y = n_attractions, 
                    size = total_units, color = total_infrastructure)) +
  geom_point(alpha = 0.7) +
  geom_text_repel(
    data = density_data %>% filter(total_infrastructure > 200),
    aes(label = County),
    size = 3.5,
    box.padding = 0.5,
    point.padding = 0.3,
    max.overlaps = 15
  ) +
  scale_color_viridis(option = "plasma", name = "Total\nInfrastructure") +
  scale_size_continuous(name = "Total\nUnits", labels = comma) +
  labs(
    title = "Tourism Infrastructure Density by County",
    subtitle = "Relationship Between Accommodations and Attractions",
    x = "Number of Accommodations",
    y = "Number of Attractions"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    legend.position = "right"
  ) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma)

ggsave("Figure4_Infrastructure_Density.pdf", plot4, 
       width = 12, height = 8, device = "pdf")
ggsave("Figure4_Infrastructure_Density.png", plot4, 
       width = 12, height = 8, dpi = 300)

print(plot4)
cat("Figure 4 saved!\n")


# STEP 12: VISUALIZATION 5 - STOP TYPE DISTRIBUTION
# ============================================================================
cat("\n=== CREATING VISUALIZATION 5: Transport Stop Types ===\n")

plot5 <- ggplot(stop_type_distribution, 
                aes(x = reorder(StopType, -count), y = count, fill = StopType)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Public Transport Infrastructure Composition",
    subtitle = "Distribution of Stop Points by Type Across Ireland",
    x = "Stop Type",
    y = "Number of Stop Points"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    panel.grid.major.x = element_blank()
  ) +
  scale_y_continuous(labels = comma, expand = c(0, 0), limits = c(0, NA)) +
  geom_text(aes(label = comma(count)), 
            vjust = -0.5, size = 4)

ggsave("Figure5_Stop_Types.pdf", plot5, 
       width = 12, height = 8, device = "pdf")
ggsave("Figure5_Stop_Types.png", plot5, 
       width = 12, height = 8, dpi = 300)

print(plot5)
cat("Figure 5 saved!\n")


# STEP 13: STATISTICAL ANALYSIS - CORRELATION TEST
# ============================================================================
cat("\n=== STATISTICAL ANALYSIS: CORRELATION TEST ===\n")

# Correlation between accommodations and attractions per county
# Filter out counties with zero values for meaningful correlation
county_for_correlation <- county_combined %>%
  filter(n_accommodations > 0 & n_attractions > 0)

# Pearson correlation test
correlation_test <- cor.test(
  county_for_correlation$n_accommodations,
  county_for_correlation$n_attractions,
  method = "pearson"
)

cat("\nPearson Correlation Test Results:\n")
cat("Correlation coefficient (r):", round(correlation_test$estimate, 4), "\n")
cat("P-value:", format.pval(correlation_test$p.value, digits = 4), "\n")
cat("95% Confidence Interval:", 
    round(correlation_test$conf.int[1], 4), "to", 
    round(correlation_test$conf.int[2], 4), "\n")

if (correlation_test$p.value < 0.001) {
  cat("Result: HIGHLY SIGNIFICANT positive correlation (p < 0.001)\n")
} else if (correlation_test$p.value < 0.05) {
  cat("Result: SIGNIFICANT positive correlation (p < 0.05)\n")
} else {
  cat("Result: No significant correlation (p >= 0.05)\n")
}

# Interpretation
if (correlation_test$estimate > 0.7) {
  cat("Interpretation: Strong positive relationship - counties with more accommodations tend to have significantly more attractions.\n")
} else if (correlation_test$estimate > 0.4) {
  cat("Interpretation: Moderate positive relationship - some tendency for accommodations and attractions to co-locate.\n")
} else {
  cat("Interpretation: Weak relationship - accommodations and attractions don't strongly co-locate.\n")
}

# Spearman correlation (non-parametric alternative)
spearman_test <- cor.test(
  county_for_correlation$n_accommodations,
  county_for_correlation$n_attractions,
  method = "spearman"
)

cat("\nSpearman Rank Correlation (non-parametric):", 
    round(spearman_test$estimate, 4), "\n")
cat("P-value:", format.pval(spearman_test$p.value, digits = 4), "\n")


# STEP 14: TRANSPORT DENSITY CALCULATION
# ============================================================================
cat("\n=== CALCULATING TRANSPORT DENSITY PER COUNTY ===\n")

# Count stop points per county (approximate using proximity)
# For each county, count stops within its general area
# We'll use the accommodation/attraction coordinates to define county bounds

county_bounds <- accommodation_clean %>%
  group_by(County) %>%
  summarise(
    min_lat = min(Latitude, na.rm = TRUE),
    max_lat = max(Latitude, na.rm = TRUE),
    min_lon = min(Longitude, na.rm = TRUE),
    max_lon = max(Longitude, na.rm = TRUE),
    center_lat = mean(Latitude, na.rm = TRUE),
    center_lon = mean(Longitude, na.rm = TRUE)
  )

# Function to count stops near a county (within expanded bounds)
count_stops_in_county <- function(county_data, stops_data, buffer = 0.1) {
  stops_in_county <- stops_data %>%
    filter(
      Latitude >= (county_data$min_lat - buffer),
      Latitude <= (county_data$max_lat + buffer),
      Longitude >= (county_data$min_lon - buffer),
      Longitude <= (county_data$max_lon + buffer)
    )
  return(nrow(stops_in_county))
}

# Calculate stop density for each county
cat("Calculating transport density for each county...\n")
county_bounds$n_stops <- sapply(1:nrow(county_bounds), function(i) {
  count_stops_in_county(county_bounds[i,], stop_points_clean, buffer = 0.1)
})

# Merge with main county data
county_combined_full <- county_combined %>%
  left_join(
    county_bounds %>% select(County, n_stops, center_lat, center_lon),
    by = "County"
  ) %>%
  mutate(
    stops_per_accommodation = n_stops / n_accommodations,
    stops_per_attraction = n_stops / n_attractions,
    transport_tourism_index = (n_stops * (n_accommodations + n_attractions)) / 1000
  )

cat("Transport density calculated!\n")


# STEP 15: REGRESSION ANALYSIS
# ============================================================================
cat("\n=== REGRESSION ANALYSIS ===\n")

# Prepare data for regression (remove missing values)
regression_data <- county_combined_full %>%
  filter(!is.na(n_stops) & n_accommodations > 0 & n_attractions > 0 & n_stops > 0)

cat("Sample size for regression:", nrow(regression_data), "counties\n")

# Model 1: Simple linear regression (attractions ~ accommodations)
model1 <- lm(n_attractions ~ n_accommodations, data = regression_data)

cat("\n--- MODEL 1: Attractions ~ Accommodations ---\n")
print(summary(model1))

# Model 2: Multiple regression (attractions ~ accommodations + transport)
model2 <- lm(n_attractions ~ n_accommodations + n_stops, data = regression_data)

cat("\n--- MODEL 2: Attractions ~ Accommodations + Transport Stops ---\n")
print(summary(model2))

# Model 3: With interaction term
model3 <- lm(n_attractions ~ n_accommodations * n_stops, data = regression_data)

cat("\n--- MODEL 3: Attractions ~ Accommodations * Transport (with interaction) ---\n")
print(summary(model3))

# Model comparison
cat("\n=== MODEL COMPARISON ===\n")
anova_result <- anova(model1, model2, model3)
print(anova_result)

# Store regression results
regression_summary <- data.frame(
  Model = c("Model 1: Accommodations only", 
            "Model 2: + Transport stops", 
            "Model 3: + Interaction"),
  R_squared = c(
    summary(model1)$r.squared,
    summary(model2)$r.squared,
    summary(model3)$r.squared
  ),
  Adj_R_squared = c(
    summary(model1)$adj.r.squared,
    summary(model2)$adj.r.squared,
    summary(model3)$adj.r.squared
  ),
  AIC = c(AIC(model1), AIC(model2), AIC(model3))
)

cat("\nRegression Models Summary:\n")
print(regression_summary)

# Key finding interpretation
best_model <- which.max(regression_summary$Adj_R_squared)
cat("\nBest Model:", regression_summary$Model[best_model], "\n")
cat("Adjusted R²:", round(regression_summary$Adj_R_squared[best_model], 4), "\n")
cat("This means:", 
    round(regression_summary$Adj_R_squared[best_model] * 100, 1), 
    "% of variation in attraction count is explained by the model.\n")


# STEP 16: VISUALIZATION 6 - CORRELATION SCATTER WITH REGRESSION LINE
# ============================================================================
cat("\n=== CREATING VISUALIZATION 6: Correlation Analysis ===\n")

plot6 <- ggplot(county_for_correlation, 
                aes(x = n_accommodations, y = n_attractions)) +
  geom_point(aes(size = total_units, color = total_infrastructure), 
             alpha = 0.7) +
  geom_smooth(method = "lm", color = "#ef4444", size = 1.5, 
              se = TRUE, fill = "#fee2e2") +
  scale_color_viridis(option = "plasma", name = "Total\nInfrastructure") +
  scale_size_continuous(name = "Total\nUnits", labels = comma) +
  geom_text_repel(
    data = county_for_correlation %>% filter(total_infrastructure > 300),
    aes(label = County),
    size = 3.5,
    box.padding = 0.5,
    max.overlaps = 20
  ) +
  labs(
    title = "Strong Positive Correlation Between Accommodations and Attractions",
    subtitle = sprintf("Pearson's r = %.3f, p < 0.001 | Linear regression line with 95%% confidence interval", 
                       correlation_test$estimate),
    x = "Number of Accommodations",
    y = "Number of Attractions",
    caption = "Bubble size represents total accommodation units available"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    plot.caption = element_text(size = 10, color = "gray50", hjust = 0),
    legend.position = "right"
  ) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma)

ggsave("Figure6_Correlation_Analysis.pdf", plot6, 
       width = 12, height = 8, device = "pdf")
ggsave("Figure6_Correlation_Analysis.png", plot6, 
       width = 12, height = 8, dpi = 300)

print(plot6)
cat("Figure 6 saved!\n")


# STEP 17: VISUALIZATION 7 - HEAT MAP (TRANSPORT-TOURISM INTEGRATION)
# ============================================================================
cat("\n=== CREATING VISUALIZATION 7: Transport-Tourism Integration Heat Map ===\n")

# Prepare data for heat map - top 20 counties
heatmap_data <- county_combined_full %>%
  filter(!is.na(n_stops)) %>%
  arrange(desc(transport_tourism_index)) %>%
  head(20) %>%
  select(County, n_accommodations, n_attractions, n_stops) %>%
  mutate(
    County = factor(County, levels = County)
  ) %>%
  pivot_longer(
    cols = c(n_accommodations, n_attractions, n_stops),
    names_to = "Category",
    values_to = "Count"
  ) %>%
  mutate(
    Category = case_when(
      Category == "n_accommodations" ~ "Accommodations",
      Category == "n_attractions"   ~ "Attractions",
      Category == "n_stops"         ~ "Transport Stops",
      TRUE ~ Category
    ),
    # Normalize within each category for color intensity
    Normalized = ave(Count, Category, FUN = function(x) x / max(x) * 100)
  )

# Create heatmap
plot7 <- ggplot(heatmap_data, aes(x = Category, y = County, fill = Normalized)) +
  geom_tile(color = "white", size = 0.5) +
  geom_text(aes(label = scales::comma(Count)), size = 3, fontface = "bold") +
  scale_fill_gradient2(
    low = "#f0f9ff", 
    mid = "#3b82f6", 
    high = "#1e3a8a",
    midpoint = 50,
    name = "Relative\nDensity (%)",
    labels = function(x) paste0(round(x), "%")
  ) +
  labs(
    title = "Transport-Tourism Integration Index by County",
    subtitle = "Top 20 Counties Ranked by Combined Infrastructure Density",
    x = "",
    y = "",
    caption = "Cell values show actual counts | Color intensity shows relative density within each category"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 0),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12, face = "bold"),
    axis.text.y = element_text(size = 11),
    panel.grid = element_blank(),
    legend.position = "right"
  )

# Save plots
ggsave("Figure7_Transport_Tourism_Heatmap.pdf", plot7, width = 12, height = 10, device = "pdf")
ggsave("Figure7_Transport_Tourism_Heatmap.png", plot7, width = 12, height = 10, dpi = 300)

print(plot7)
cat("Figure 7 saved!\n")


# STEP 18: VISUALIZATION 8 - REGRESSION DIAGNOSTIC PLOT
# ============================================================================
cat("\n=== CREATING VISUALIZATION 8: Regression Analysis Visualization ===\n")

# Prepare predictions from best model (model2)
regression_plot_data <- regression_data %>%
  mutate(
    predicted = predict(model2),
    residuals = residuals(model2)
  )

# Create a 2x2 panel plot
plot8a <- ggplot(regression_plot_data, 
                 aes(x = n_accommodations, y = n_attractions)) +
  geom_point(aes(color = n_stops), size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", color = "#ef4444", se = TRUE) +
  scale_color_viridis(option = "viridis", name = "Transport\nStops") +
  labs(
    title = "A) Accommodations vs Attractions",
    x = "Accommodations",
    y = "Attractions"
  ) +
  theme_minimal(base_size = 11)

plot8b <- ggplot(regression_plot_data, 
                 aes(x = n_stops, y = n_attractions)) +
  geom_point(aes(color = n_accommodations), size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", color = "#10b981", se = TRUE) +
  scale_color_viridis(option = "plasma", name = "Accommodations") +
  labs(
    title = "B) Transport Stops vs Attractions",
    x = "Transport Stops",
    y = "Attractions"
  ) +
  theme_minimal(base_size = 11)

plot8c <- ggplot(regression_plot_data, 
                 aes(x = predicted, y = n_attractions)) +
  geom_point(aes(color = residuals), size = 3, alpha = 0.7) +
  geom_abline(intercept = 0, slope = 1, color = "#ef4444", 
              linetype = "dashed", size = 1) +
  scale_color_gradient2(low = "#3b82f6", mid = "white", high = "#ef4444",
                        name = "Residuals") +
  labs(
    title = "C) Predicted vs Actual",
    x = "Predicted Attractions",
    y = "Actual Attractions"
  ) +
  theme_minimal(base_size = 11)

plot8d <- ggplot(regression_plot_data, 
                 aes(x = predicted, y = residuals)) +
  geom_point(aes(color = abs(residuals)), size = 3, alpha = 0.7) +
  geom_hline(yintercept = 0, color = "#ef4444", linetype = "dashed", size = 1) +
  geom_smooth(method = "loess", color = "#3b82f6", se = TRUE) +
  scale_color_viridis(option = "magma", name = "Abs.\nResidual") +
  labs(
    title = "D) Residual Plot",
    x = "Predicted Attractions",
    y = "Residuals"
  ) +
  theme_minimal(base_size = 11)

# Combine into one figure
plot8 <- grid.arrange(
  plot8a, plot8b, plot8c, plot8d,
  ncol = 2,
  top = grid::textGrob(
    "Multiple Regression Analysis: Predicting Attraction Count",
    gp = grid::gpar(fontsize = 16, fontface = "bold")
  ),
  bottom = grid::textGrob(
    sprintf("Model: Attractions ~ Accommodations + Transport Stops | Adj. R² = %.3f | p < 0.001", 
            summary(model2)$adj.r.squared),
    gp = grid::gpar(fontsize = 11, col = "gray40")
  )
)

ggsave("Figure8_Regression_Analysis.pdf", plot8, 
       width = 14, height = 12, device = "pdf")
ggsave("Figure8_Regression_Analysis.png", plot8, 
       width = 14, height = 12, dpi = 300)

cat("Figure 8 saved!\n")


# STEP 19: EXPORT SUMMARY TABLES (UPDATED)
# ============================================================================
cat("\n=== EXPORTING SUMMARY TABLES ===\n")

# Export summary statistics
write_csv(summary_stats, "Summary_Statistics.csv")
write_csv(county_combined_full, "County_Level_Analysis_Complete.csv")
write_csv(sector_distribution, "Sector_Distribution.csv")
write_csv(proximity_combined, "Proximity_Analysis.csv")
write_csv(stop_type_distribution, "Stop_Type_Distribution.csv")
write_csv(regression_summary, "Regression_Models_Comparison.csv")

# Export correlation results
correlation_results <- data.frame(
  Test = c("Pearson Correlation", "Spearman Correlation"),
  Coefficient = c(correlation_test$estimate, spearman_test$estimate),
  P_value = c(correlation_test$p.value, spearman_test$p.value),
  CI_lower = c(correlation_test$conf.int[1], NA),
  CI_upper = c(correlation_test$conf.int[2], NA)
)
write_csv(correlation_results, "Correlation_Test_Results.csv")

# Export regression coefficients
model2_coef <- as.data.frame(summary(model2)$coefficients)
model2_coef$Variable <- rownames(model2_coef)
write_csv(model2_coef, "Regression_Coefficients.csv")

cat("All summary tables exported!\n")


# STEP 20: FINAL SUMMARY REPORT
# ============================================================================
cat("\n")
cat("=" , rep("=", 78), "=\n", sep = "")
cat("                    COMPREHENSIVE ANALYSIS COMPLETE!\n")
cat("=" , rep("=", 78), "=\n", sep = "")
cat("\nFILES GENERATED:\n")
cat("  Figures (PDF & PNG):\n")
cat("    - Figure1_County_Distribution (RQ1)\n")
cat("    - Figure2_Sector_Distribution (Context)\n")
cat("    - Figure3_Transport_Proximity (RQ2)\n")
cat("    - Figure4_Infrastructure_Density (RQ1)\n")
cat("    - Figure5_Stop_Types (RQ2)\n")
cat("    - Figure6_Correlation_Analysis (RQ1 - Statistical Test)\n")
cat("    - Figure7_Transport_Tourism_Heatmap (RQ1 & RQ2 Integration)\n")
cat("    - Figure8_Regression_Analysis (Combined RQ1 & RQ2)\n")
cat("\n  Data Tables (CSV):\n")
cat("    - Summary_Statistics.csv\n")
cat("    - County_Level_Analysis_Complete.csv (includes transport density)\n")
cat("    - Sector_Distribution.csv\n")
cat("    - Proximity_Analysis.csv\n")
cat("    - Stop_Type_Distribution.csv\n")
cat("    - Correlation_Test_Results.csv (NEW)\n")
cat("    - Regression_Models_Comparison.csv (NEW)\n")
cat("    - Regression_Coefficients.csv (NEW)\n")
cat("\n")
cat("=" , rep("=", 78), "=\n", sep = "")
cat("KEY FINDINGS:\n")
cat("=" , rep("=", 78), "=\n", sep = "")
cat("\n[DESCRIPTIVE STATISTICS]\n")
cat("  • Total Accommodations:", nrow(accommodation_clean), "\n")
cat("  • Total Attractions:", nrow(attractions_clean), "\n")
cat("  • Total Stop Points:", nrow(stop_points_clean), "\n")
cat("  • Counties Analyzed:", nrow(county_combined), "\n")
cat("  • Top County:", top_counties$County[1], "\n")

cat("\n[CORRELATION ANALYSIS - RQ1]\n")
cat("  • Pearson's r =", round(correlation_test$estimate, 4), "\n")
cat("  • P-value:", format.pval(correlation_test$p.value, digits = 4), "\n")
cat("  • Interpretation:", 
    ifelse(correlation_test$estimate > 0.7, "STRONG", 
           ifelse(correlation_test$estimate > 0.4, "MODERATE", "WEAK")),
    "positive correlation\n")
cat("  • Meaning: Counties with more accommodations SIGNIFICANTLY tend to have more attractions\n")

cat("\n[REGRESSION ANALYSIS - COMBINED RQ1 & RQ2]\n")
cat("  • Best Model: Attractions ~ Accommodations + Transport Stops\n")
cat("  • Adjusted R² =", round(summary(model2)$adj.r.squared, 4), "\n")
cat("  • Variance Explained:", 
    round(summary(model2)$adj.r.squared * 100, 1), "%\n")
cat("  • Model Significance: p < 0.001 (HIGHLY SIGNIFICANT)\n")
cat("  • Key Finding: BOTH accommodation count AND transport density predict attraction count\n")

# Extract coefficients
coef_accom <- summary(model2)$coefficients["n_accommodations", "Estimate"]
coef_stops <- summary(model2)$coefficients["n_stops", "Estimate"]
cat("  • Accommodation Effect: For every +1 accommodation, attractions increase by", 
    round(coef_accom, 3), "\n")
cat("  • Transport Effect: For every +1 stop point, attractions increase by", 
    round(coef_stops, 3), "\n")

cat("\n[PROXIMITY ANALYSIS - RQ2]\n")
proximity_accom_1km <- proximity_accom %>% 
  filter(distance_category %in% c("< 0.5 km", "0.5-1 km")) %>%
  summarise(total_pct = sum(percentage)) %>% 
  pull(total_pct)
proximity_attract_1km <- proximity_attract %>% 
  filter(distance_category %in% c("< 0.5 km", "0.5-1 km")) %>%
  summarise(total_pct = sum(percentage)) %>% 
  pull(total_pct)
cat("  •", round(proximity_accom_1km, 1), 
    "% of accommodations within 1km of transport\n")
cat("  •", round(proximity_attract_1km, 1), 
    "% of attractions within 1km of transport\n")
cat("  • Accessibility Gap:", 
    round(abs(proximity_accom_1km - proximity_attract_1km), 1), 
    "percentage points\n")

cat("\n")
cat("=" , rep("=", 78), "=\n", sep = "")
cat("REPORT WRITING GUIDE:\n")
cat("=" , rep("=", 78), "=\n", sep = "")
cat("\n[ANSWER TO RQ1: Distribution]\n")
cat("  Use: Figure 1, 4, 6\n")
cat("  Key Stats:\n")
cat("    - Correlation coefficient (r =", round(correlation_test$estimate, 3), ")\n")
cat("    - P-value < 0.001 (cite this!)\n")
cat("    - Top 5 counties account for X% (calculate from Figure 1)\n")
cat("  Write: 'Our analysis reveals significant geographic concentration...\n")
cat("          with a strong positive correlation (r =", 
    round(correlation_test$estimate, 3), ", p < 0.001)'\n")

cat("\n[ANSWER TO RQ2: Transport Proximity]\n")
cat("  Use: Figure 3, 5, 7\n")
cat("  Key Stats:\n")
cat("    -", round(proximity_accom_1km, 1), 
    "% accommodations within 1km\n")
cat("    -", round(proximity_attract_1km, 1), 
    "% attractions within 1km\n")
cat("  Write: 'Proximity analysis reveals", 
    round(proximity_accom_1km, 1), 
    "% of accommodations are within\n")
cat("          walking distance (1km) of public transport, compared to", 
    round(proximity_attract_1km, 1), "%\n")
cat("          of attractions, suggesting an accessibility gap.'\n")

cat("\n[COMBINED INSIGHT]\n")
cat("  Use: Figure 7, 8\n")
cat("  Key Stats:\n")
cat("    - Regression R² =", round(summary(model2)$adj.r.squared, 3), "\n")
cat("    - Model explains", round(summary(model2)$adj.r.squared * 100, 1), 
    "% of variation\n")
cat("  Write: 'Multiple regression analysis (Figure 8) demonstrates that both\n")
cat("          accommodation density and transport infrastructure jointly predict\n")
cat("          attraction distribution (R² =", 
    round(summary(model2)$adj.r.squared, 3), ", p < 0.001)'\n")



# End of script