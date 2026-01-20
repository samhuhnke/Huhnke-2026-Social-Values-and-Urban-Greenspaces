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
setwd("C:/Users/samhu/Desktop/Code Projects/Huhnke_2026/data/CSVs")

# ============================================
# 1) Load necessary packagess
# ============================================

library(tidyverse)

# ============================================
# 2) Load data
# ============================================

HEL_LU_raw <- read.csv("Helsinki_Urban Atlas 2018.csv", sep = ",")  # Helsinki Urban Atlas 2018 data
CPH_LU_raw <- read.csv("Copenhagen_Urban Atlas 2018.csv", sep = ",") # Copenhagen Urban Atlas 2018 data


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
      code_2018 == 31000 | code_2018 == 32000 | code_2018 == 33000 ~ 6,
      # Wetlands = 7
      code_2018 == 40000 ~ 7,
      # Water = 8
      code_2018 == 50000 ~ 8 )) |> 
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
      code_2018 == 31000 | code_2018 == 32000 | code_2018 == 33000 ~ 6,
      # Wetlands = 7
      code_2018 == 40000 ~ 7,
      # Water = 8
      code_2018 == 50000 ~ 8 )) |> 
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
      code_2018 == 31000 | code_2018 == 32000 | code_2018 == 33000 ~ 6,
      # Wetlands = 7
      code_2018 == 40000 ~ 7,
      # Water = 8
      code_2018 == 50000 ~ 8 )) |> 
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
      code_2018 == 31000 | code_2018 == 32000 | code_2018 == 33000 ~ 6,
      # Wetlands = 7
      code_2018 == 40000 ~ 7,
      # Water = 8
      code_2018 == 50000 ~ 8 )) |> 
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









