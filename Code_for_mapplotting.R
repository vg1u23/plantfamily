
install.packages("rnaturalearthdata")
install.packages("tmap")

install.packages("terra", type = "binary")
install.packages(c("tmap", "rnaturalearth", "sf", "countrycode"))
setwd("C:/Users/vg1u23/OneDrive - University of Southampton/Desktop/PhD_Gschiel/Project/Project1_Bioinformatics/Results")
list.files()


#

# Load required libraries
library(readxl)
library(dplyr)
library(tmap)
library(rnaturalearth)
library(sf)

# Set working directory
setwd("C:/Users/vg1u23/OneDrive - University of Southampton/Desktop/PhD_Gschiel/Project/Project1_Bioinformatics/Results")

# Step 1: Load metadata
metadata <- read_excel("combined_metadata_all_formap.xlsx")

# Step 2: Manually map known problematic country names
# rnaturalearth uses "United States of America", not "USA" or "United States"
metadata <- metadata %>%
  mutate(country_fixed = case_when(
    country %in% c("USA", "U.S.A.", "US", "United States") ~ "United States of America",
    country %in% c("UK", "U.K.") ~ "United Kingdom",
    country == "Russia" ~ "Russian Federation",
    TRUE ~ country
  ))

# Step 3: Count samples per country
country_counts <- metadata %>%
  filter(!is.na(country_fixed)) %>%
  count(country_fixed, name = "sample_count")

# Step 4: Load world map
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# Step 5: Merge sample counts into map data
map_data <- left_join(world, country_counts, by = c("name" = "country_fixed"))

# Step 6: Plot the map
tmap_mode("plot")  # Use "view" for interactive map

tm_shape(map_data) +
  tm_polygons("sample_count", title = "Sample Count",
              palette = "viridis", textNA = "No data") +
  tm_layout(
    title = "",
    legend.outside = TRUE,
    legend.outside.position = "right",
    legend.title.size = 2.5,
    legend.text.size = 2
  )

tmap_save(
  tm_shape(map_data) +
    tm_polygons("sample_count", title = "Sample Count",
                palette = "viridis", textNA = "No data") +
    tm_layout(
      title = "",
      legend.outside = TRUE,
      legend.outside.position = "right",
      legend.title.size = 2.5,
      legend.text.size = 2
    ),
  filename = "seed_origin_map.png",
  width = 14, height = 6, units = "in", dpi = 300
)

#adress country issue
library(dplyr)
library(stringr)

# Step 1: Create a cleaned country column just for counting
country_counts <- metadata %>%
  mutate(country_clean = str_extract(country, "^[^:;,]+") %>% str_trim()) %>%
  mutate(country_fixed = case_when(
    country_clean %in% c("USA", "U.S.A.", "US", "United States") ~ "United States of America",
    country_clean %in% c("UK", "U.K.") ~ "United Kingdom",
    country_clean == "Russia" ~ "Russian Federation",
    TRUE ~ country_clean
  )) %>%
  count(country_fixed, name = "sample_count")

#table for overview about the included studies

