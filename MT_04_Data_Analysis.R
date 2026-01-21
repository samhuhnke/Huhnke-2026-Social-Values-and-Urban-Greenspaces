# ============================================ 
# MT_04_Data_Analysis
# ============================================

# AUTHOR: Sam Huhnke, M.Sc. University of Helsinki
#
#
# This code provides analyses of relations between mapped social values and tree canopy cover for
# Helsinki and Copenhagen. 
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
# 1) Load necessary packages
# ============================================

library(tidyverse)
library(ggplot2)

# ============================================
# 2) Load data
# ============================================

# Helsinki
HEL_raw <- read.csv("CSVs/Helsinki_SV_50mCC_LU.csv", sep = ",")
HEL_areas <- read.csv("CSVs/Helsinki_LU_Areas.csv", sep = ",")


# Copenhagen
CPH_raw <- read.csv("CSVs/Copenhagen_SV_50mCC_LU.csv", sep = ",")
CPH_areas <- read.csv("CSVs/Copenhagen_LU_Areas.csv", sep = ",")

# ============================================
# 3) Data Pre-Processing
# NOTE: This section joins the area data with the PPGIS based on the code_2018 column.
# NOTE: Further this section filters out any unneccessary columns for further analysis.
# ============================================

# Helsinki
HEL <- HEL_raw |> 
  left_join(HEL_areas, by = "code_2018") |> 
  select(Respondent, category_2018, code_2018, area_km2, area_percent, total_km2, SV_new, X_mean) |> 
  rename(CanopyCover_mean = X_mean)


# Helsinki
CPH <- CPH_raw |> 
  left_join(CPH_areas, by = "code_2018") |> 
  select(Respondent, category_2018, code_2018, area_km2, area_percent, total_km2, SV_new, X_mean) |> 
  rename(CanopyCover_mean = X_mean)


# ============================================
# 4) Data Analysis
# ============================================

# Plotting preperations
{
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
}


# How is canopy cover distributed overall? 

HEL |> ggplot(aes(x = CanopyCover_mean)) +
  geom_histogram()

CPH |> ggplot(aes(x = CanopyCover_mean)) +s
  geom_histogram()


# How is canopy cover distributed by land-use type?

ggplot(HEL, aes(x = CanopyCover_mean, fill = as.factor(category_2018))) +
  geom_histogram(
    position = "identity",
    alpha = 0.7,
    bins = 30
  ) +
  scale_fill_manual(values = cols)

ggplot(CPH, aes(x = CanopyCover_mean, fill = as.factor(category_2018))) +
  geom_histogram(
    position = "identity",
    alpha = 0.7,
    bins = 30
  ) +
  scale_fill_manual(values = cols)

HEL_raw |> filter(is.na(code_2018)) |> count()
