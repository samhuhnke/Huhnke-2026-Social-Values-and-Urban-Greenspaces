# ============================================ 
# MT_04_Data_Zonal_Statistics_Code
# ============================================

# AUTHOR: Sam Huhnke, M.Sc. University of Helsinki
#
#
# This code provides non-spatial analyses of relations between mapped social values and tree canopy cover for
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
setwd("C:/Users/samhu/Desktop/Code Projects/Huhnke_2026/DATA/ZONAL STATISTICS")

# ============================================
# 1) Load necessary packages
# ============================================

library(tidyverse)
library(sf) 
library(ggplot2)

# ============================================
# 2) Load data
# ============================================

# Helsinki
{
  HEL_50m <- read.csv("Helsinki/HEL_Zonal_Stats_50m.csv") |> 
    select(-c("Publicatio", "Submitted", "First.Acti", "Publicat_1", "Zoom", "Language", "Index")) |>
    mutate(Buffer = "50m") 
}

# Copenhagen - TBD in QGIS
{
  CPH_50m <- read.csv("Copenhagen/CPH_Zonal_Stats_50m.csv") |> 
    select(-c("Submitted", "First.Acti", "Nature_typ", "Nature_t_1", "lat", "lon", "lat_jitter", "lon_jitter", "WKT_2", "GeoJSON_2", "Zoom")) |> 
    mutate(Buffer = "50m") 
}


# ============================================
# 3) Data Exploration
# ============================================


# ============================================
# 4) Preliminary Plots
# ============================================





# Prelim Plots for data exploration
HEL_50m |> 
  ggplot()+
  geom_histogram(aes(x=X_mean))



