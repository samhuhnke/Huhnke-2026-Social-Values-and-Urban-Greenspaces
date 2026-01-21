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
{
  HEL <- HEL_raw |> 
    # add area data
    left_join(HEL_areas, by = "code_2018") |> 
    # select relevant variables
    select(Respondent, code_2018, category_2018, area_km2, area_percent, total_km2, SV_new, X_mean) |> 
    # rename canopy cover variable 
    rename(CanopyCover_mean = X_mean) |> 
    # further simplify classes into Greenspace, Forest, and Other
    mutate(type_2018 = case_when(category_2018 == 4 ~ "Greenspace",
                                 category_2018 == 6 ~ "Forest",
                                 category_2018 != 4 & category_2018 != 6 | is.na(category_2018) ~ "Other"))
  
  # calculate area per type
  HEL_type_area <- HEL |> 
    select(type_2018, area_km2) |> 
    group_by(type_2018) |> 
    unique() |> 
    reframe(type_area_km2 = sum(area_km2, na.rm = T))
  
  # join type area into complete data set
  HEL <- HEL |> 
    left_join(HEL_type_area, by = "type_2018")
}

# Copenhagen
{
  CPH <- CPH_raw |> 
    # add area data
    left_join(CPH_areas, by = "code_2018") |> 
    # select relevant variables
    select(Respondent, code_2018, category_2018, area_km2, area_percent, total_km2, SV_new, X_mean) |> 
    # rename canopy cover variable 
    rename(CanopyCover_mean = X_mean) |> 
    # further simplify classes into Greenspace, Forest, and Other
    mutate(type_2018 = case_when(category_2018 == 4 ~ "Greenspace",
                                 category_2018 == 6 ~ "Forest",
                                 category_2018 != 4 & category_2018 != 6 | is.na(category_2018) ~ "Other"))
  
  # calculate area per type
  CPH_type_area <- CPH |> 
    select(type_2018, area_km2) |> 
    group_by(type_2018) |> 
    unique() |> 
    reframe(type_area_km2 = sum(area_km2, na.rm = T))
  
  # join type area into complete data set
  CPH <- CPH |> 
    left_join(CPH_type_area, by = "type_2018")
}

# remove temporary layers
rm("CPH_type_area", "HEL_type_area")


# ============================================
# 4) Data Analysis
# ============================================

# Plotting preperations
{
  # define colors
  category_cols <- c("#bf0000", "#959595", "#734d37", "#8cdc00", "#ffffa8", "#008c00", "#a6a6ff", "#80f2e6","#ccffcc")
  
  type_cols <- c("#008c00", "#8cdc00", "#959595" )
  
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

CPH |> ggplot(aes(x = CanopyCover_mean)) +
  geom_histogram()


# How is canopy cover distributed by land-use type?

## Helsinki all
ggplot(HEL, aes(x = CanopyCover_mean, fill = as.factor(type_2018))) +
  geom_histogram(
    position = "identity",
    alpha = 0.7,
    bins = 30
  ) +
  scale_fill_manual(values = type_cols) +
  facet_wrap(~ type_2018)

## Copenhagen all
ggplot(CPH, aes(x = CanopyCover_mean, fill = as.factor(type_2018))) +
  geom_histogram(
    position = "identity",
    alpha = 0.7,
    bins = 30
  ) +
  scale_fill_manual(values = type_cols) +
  facet_wrap(~ type_2018)


# Point count & point desity per type
HEL |> group_by(type_2018) |> count() |> 
  left_join(HEL |> select(type_2018, type_area_km2) |> unique(), by = "type_2018") |> 
  mutate(point_density_km2 = n/type_area_km2)

CPH |> group_by(type_2018) |> count() |> 
  left_join(CPH |> select(type_2018, type_area_km2) |> unique(), by = "type_2018") |> 
  mutate(point_density_km2 = n/type_area_km2)
  



