# ============================================ 
# MT_03_General Area Metrics and Analysis
# ============================================

# AUTHOR: Sam Huhnke, M.Sc. University of Helsinki
#
#
# This code provides simple spatial analyses of the Copenhagen and Helsinki administrative boundary areas. The analyses
# are based on the CLMS Urban Atlas 2018 data and are intended to give an idea of the amount of different land uses.  
# 
# NOTE: The data used for this analysis is owned by the CO-CARBON research project. To gain access to the data,
# contact the CO-CARBON research project. Any use of the data and this code must be in accordance with the CO-CARBON 
# guidelines.

# ============================================
# 0) Set working directory + clear environment (if needed)
# ============================================

# Clear environment
rm(list = ls(globalenv()))

# Set working directory to whatever needed
setwd("C:/Users/samhu/Desktop/Code Projects/Huhnke_2026/data")

# ============================================
# 1) Load necessary packagess
# ============================================

library(tidyverse)

# ============================================
# 2) Load data
# ============================================

HEL_LU_raw <- read.csv("CSVs/Helsinki_Urban Atlas 2018.csv", sep = ",")  # Helsinki Urban Atlas 2018 data
CPH_LU_raw <- read.csv("CSVs/Copenhagen_Urban Atlas 2018.csv", sep = ",") # Copenhagen Urban Atlas 2018 data


# ============================================
# 3) Data Wrangling
# ============================================

# 3.1) Create reclassification tables 
{
  # Helsinki
  HEL_LU_categories <- HEL_LU_raw |> 
    # 1) Reclassification into overarching categories
    mutate(category_2018 = case_when(
      # Urban Fabric = 1
      code_2018 == 11100 | code_2018 == 11210 | code_2018 == 11220 | code_2018 == 11230 |
        code_2018 == 11240 | code_2018 == 11300 ~ 1,
      # Industrial, Public, Military, Transport = 2
      code_2018 == 12100 | code_2018 == 12210 | code_2018 == 12220 | code_2018 == 12230 |
        code_2018 == 12300 | code_2018 == 12400 ~ 2,
      # Mine, Dump, Construction = 3
      code_2018 == 13100 | code_2018 == 13300 | code_2018 == 13400 ~ 3,
      # Artificial non-agricultural areas (i.e. urban parks and sports facilities) = 4
      code_2018 == 14100 | code_2018 == 14200 ~ 4,
      # Agricultural areas = 5
      code_2018 == 21000 | code_2018 == 22000 | code_2018 == 23000 | code_2018 == 24000 |
        code_2018 == 25000 ~ 5,
      # Natural and semi-natural areas = 6
      code_2018 == 31000 | code_2018 == 32000 ~ 6,
      # Wetlands = 7
      code_2018 == 40000 ~ 7,
      # Water = 8
      code_2018 == 50000 ~ 8,
      # Open areas with little or no vegetation = 9
      code_2018 == 33000 ~ 9)) |> 
    select(code_2018, category_2018) |> 
    arrange(category_2018) |> 
    unique()
  
  # Copenhagen
  CPH_LU_categories <- CPH_LU_raw |> 
    # 1) Reclassification into overarching categories
    mutate(category_2018 = case_when(
      # Urban Fabric = 1
      code_2018 == 11100 | code_2018 == 11210 | code_2018 == 11220 | code_2018 == 11230 |
        code_2018 == 11240 | code_2018 == 11300 ~ 1,
      # Industrial, Public, Military, Transport = 2
      code_2018 == 12100 | code_2018 == 12210 | code_2018 == 12220 | code_2018 == 12230 |
        code_2018 == 12300 | code_2018 == 12400 ~ 2,
      # Mine, Dump, Construction = 3
      code_2018 == 13100 | code_2018 == 13300 | code_2018 == 13400 ~ 3,
      # Artificial non-agricultural areas (i.e. urban parks and sports facilities) = 4
      code_2018 == 14100 | code_2018 == 14200 ~ 4,
      # Agricultural areas = 5
      code_2018 == 21000 | code_2018 == 22000 | code_2018 == 23000 | code_2018 == 24000 |
        code_2018 == 25000 ~ 5,
      # Natural and semi-natural areas = 6
      code_2018 == 31000 | code_2018 == 32000 ~ 6,
      # Wetlands = 7
      code_2018 == 40000 ~ 7,
      # Water = 8
      code_2018 == 50000 ~ 8,
      # Open areas with little or no vegetation = 9
      code_2018 == 33000 ~ 9)) |> 
    select(code_2018, category_2018) |> 
    arrange(category_2018) |> 
    unique()
  
}


# 3.2) Calculating Total Area
# NOTE: Use the "area_m2"  column. This is the column calculated in QGIS. 
# NOTE: See reclassification tables. [still to be created]
{
  # Helsinki
  HEL_LU_grouped <- HEL_LU_raw |> 
    # 1) Reclassification into overarching categories
    mutate(category_2018 = case_when(
      # Urban Fabric = 1
      code_2018 == 11100 | code_2018 == 11210 | code_2018 == 11220 | code_2018 == 11230 |
        code_2018 == 11240 | code_2018 == 11300 ~ 1,
      # Industrial, Public, Military, Transport = 2
      code_2018 == 12100 | code_2018 == 12210 | code_2018 == 12220 | code_2018 == 12230 |
        code_2018 == 12300 | code_2018 == 12400 ~ 2,
      # Mine, Dump, Construction = 3
      code_2018 == 13100 | code_2018 == 13300 | code_2018 == 13400 ~ 3,
      # Artificial non-agricultural areas (i.e. urban parks and sports facilities) = 4
      code_2018 == 14100 | code_2018 == 14200 ~ 4,
      # Agricultural areas = 5
      code_2018 == 21000 | code_2018 == 22000 | code_2018 == 23000 | code_2018 == 24000 |
        code_2018 == 25000 ~ 5,
      # Natural and semi-natural areas = 6
      code_2018 == 31000 | code_2018 == 32000 ~ 6,
      # Wetlands = 7
      code_2018 == 40000 ~ 7,
      # Water = 8
      code_2018 == 50000 ~ 8,
      # Open areas with little or no vegetation = 9
      code_2018 == 33000 ~ 9)) |> 
    # 2) Calculate area by category
    group_by(category_2018) |> 
    reframe(area_m2 = sum(area_m2),
            area_km2 = area_m2/1000000) |> 
    ungroup() |> 
    # 3) Calculate total area + percent covered by each category
    reframe(category_2018 = category_2018,
            area_m2 = area_m2,
            area_km2 = area_km2,
            total_km2 = sum(area_km2),
            area_percent = area_km2/total_km2 * 100) 
  
  # Copenhagen
  CPH_LU_grouped <- CPH_LU_raw |> 
    # 1) Reclassification into overarching categories
    mutate(category_2018 = case_when(
      # Urban Fabric = 1
      code_2018 == 11100 | code_2018 == 11210 | code_2018 == 11220 | code_2018 == 11230 |
        code_2018 == 11240 | code_2018 == 11300 ~ 1,
      # Industrial, Public, Military, Transport = 2
      code_2018 == 12100 | code_2018 == 12210 | code_2018 == 12220 | code_2018 == 12230 |
        code_2018 == 12300 | code_2018 == 12400 ~ 2,
      # Mine, Dump, Construction = 3
      code_2018 == 13100 | code_2018 == 13300 | code_2018 == 13400 ~ 3,
      # Artificial non-agricultural areas (i.e. urban parks and sports facilities) = 4
      code_2018 == 14100 | code_2018 == 14200 ~ 4,
      # Agricultural areas = 5
      code_2018 == 21000 | code_2018 == 22000 | code_2018 == 23000 | code_2018 == 24000 |
        code_2018 == 25000 ~ 5,
      # Natural and semi-natural areas = 6
      code_2018 == 31000 | code_2018 == 32000 ~ 6,
      # Wetlands = 7
      code_2018 == 40000 ~ 7,
      # Water = 8
      code_2018 == 50000 ~ 8,
      # Open areas with little or no vegetation = 9
      code_2018 == 33000 ~ 9)) |> 
    # 2) Calculate area by category
    group_by(category_2018) |> 
    reframe(area_m2 = sum(area_m2),
            area_km2 = area_m2/1000000) |> 
    ungroup() |> 
    # 3) Calculate total area + percent covered by each category
    reframe(category_2018 = category_2018,
            area_m2 = area_m2,
            area_km2 = area_km2,
            total_km2 = sum(area_km2),
            area_percent = area_km2/total_km2 * 100) 
}


# 3.3) Match reclassification tables and area for potential later use
HEL_LU_final <- HEL_LU_categories |> 
  left_join(HEL_LU_grouped, by = "category_2018")

CPH_LU_final <- CPH_LU_categories |> 
  left_join(CPH_LU_grouped, by = "category_2018")



# ============================================
# 4) Save as .csv-files
# ============================================

# Helsinki
if (!file.exists("CSVs/Helsinki_LU_Areas.csv")) {
  write.csv(HEL_LU_final, "CSVs/Helsinki_LU_Areas.csv")
}

# Copenhagen
if (!file.exists("CSVs/Copenhagen_LU_Areas.csv")) {
  write.csv(CPH_LU_final, "CSVs/Copenhagen_LU_Areas.csv")
}


# ============================================
# 5) Pie-Charts
# ============================================

# Libraries
library(ggplot2)
library(patchwork)

# Combined Pie Chart
# NOTE: These pie charts show data for the 200m buffered administrative boundary!
{
  # extract category and area percent
  h1 <- HEL_LU_final |> select(category_2018, area_percent) |> unique()
  c1 <- CPH_LU_final |> select(category_2018, area_percent) |> unique()
  
  # define colors
  cols <- c("#bf0000", "#959595", "#734d37", "#8cdc00", "#ffffa8", "#008c00", "#a6a6ff", "#80f2e6","#ccffcc")
  
  # define labels
  lu_labels <- c(
    "1" = "1: Urban fabric",
    "2" = "2: Industrial, commercial, transport",
    "3" = "3: Mine, dump, construction",
    "4" = "4: Artificial green spaces",
    "5" = "5: Agricultural land",
    "6" = "6: Semi-/Natural green spaces",
    "7" = "7: Wetlands",
    "8" = "8: Waterbodies",
    "9" = "9: Natural open spaces"
  )
  
  # helsinki
  p1 <- ggplot(h1, aes(x = "", y = area_percent, fill = as.factor(category_2018))) +
    geom_col(width = 1, color = "black", linewidth = 0.4) +
    coord_polar("y") +
    theme_void() +
    labs(title = "Helsinki LU",
         subtitle = "200m buffered boundaries data") +
    scale_fill_manual(
      values = cols,
      labels = lu_labels,
      name = "Land use"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 8))
  
  # copenhagen
  p2 <- ggplot(c1, aes(x = "", y = area_percent, fill = as.factor(category_2018))) +
    geom_col(width = 1, color = "black", linewidth = 0.4) +
    coord_polar("y") +
    theme_void() +
    labs(title = "Copenhagen LU",
         subtitle = "200m buffered boundaries data") +
    scale_fill_manual(
      values = cols,
      labels = lu_labels,
      name = "Land use"
    ) +
    theme(legend.position = "none") +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 8))
  
  # patchwork
  p1 + p2 + plot_layout(guides = "collect") &
    theme(legend.position = "right")
}




