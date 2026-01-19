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

HEL_LU <- read.csv("Helsinki_Urban Atlas 2018.csv", sep = ",")  # Helsinki Urban Atlas 2018 data
CPH_LU <- read.csv("Copenhagen_Urban Atlas 2018.csv", sep = ",") # Copenhagen Urban Atlas 2018 data


# ============================================
# 3) Data Wrangling
# ============================================

# 3.1) Calculating Total Area
# NOTE: Use the "area_m2"  column. This is the column calculated in QGIS. 
{
  # helsinki
  HEL_LU_grouped <- HEL_LU |> 
    group_by(code_2018) |> 
    reframe(area_m2 = sum(area_m2),
            area_km2 = area_m2/1000000) |> 
    ungroup() |> 
    reframe(code_2018 = code_2018,
            area_m2 = area_m2,
            area_km2 = area_km2,
            total_km2 = sum(area_km2))
  
  # copenhagen
  CPH_LU_grouped <- CPH_LU |> 
    group_by(code_2018) |> 
    reframe(area_m2 = sum(area_m2),
            area_km2 = area_m2/1000000) |> 
    ungroup() |> 
    reframe(code_2018 = code_2018,
            area_m2 = area_m2,
            area_km2 = area_km2,
            total_km2 = sum(area_km2))
}

# 3.2) Reclassify into broader categories

## NOTE: See reclassification tables


