# ============================================ 
# MT_04_Data_Processing
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

library(tidyverse) # used for data handling

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
# 3) Data Processing
# NOTE: This section joins the area data with the PPGIS based on the code_2018 column.
# NOTE: Further this section filters out any unnecessary columns for further analysis.
# ============================================

# Helsinki
{
  HEL <- HEL_raw |> 
    # add area data
    left_join(HEL_areas, by = "code_2018") |> 
    # select relevant variables
    select(geojson, Respondent, code_2018, category_2018, area_km2, area_percent, total_km2, SV_new, X_mean) |> 
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
    reframe(type_area_km2 = sum(area_km2, na.rm = T)) |> 
    ungroup() |> 
    reframe(type_2018 = type_2018,
            type_area_km2 = type_area_km2,
            type_area_per = type_area_km2/sum(type_area_km2)*100)
  
  # join type area into complete data set
  HEL <- HEL |> 
    left_join(HEL_type_area, by = "type_2018")
  
  # Turn SV_new into categorical value (or factor in R)
  HEL$SV_new <- factor(HEL$SV_new)

}

# Copenhagen
{
  CPH <- CPH_raw |> 
    # add area data
    left_join(CPH_areas, by = "code_2018") |> 
    # select relevant variables
    select(GeoJSON, Respondent, code_2018, category_2018, area_km2, area_percent, total_km2, SV_new, X_mean) |> 
    # rename canopy cover variable 
    rename(CanopyCover_mean = X_mean,
           geojson = GeoJSON) |> 
    # further simplify classes into Greenspace, Forest, and Other
    mutate(type_2018 = case_when(category_2018 == 4 ~ "Greenspace",
                                 category_2018 == 6 ~ "Forest",
                                 category_2018 != 4 & category_2018 != 6 | is.na(category_2018) ~ "Other"))
  
  # calculate area per type
  CPH_type_area <- CPH |> 
    select(type_2018, area_km2) |> 
    group_by(type_2018) |> 
    unique() |> 
    reframe(type_area_km2 = sum(area_km2, na.rm = T)) |> 
    ungroup() |> 
    reframe(type_2018 = type_2018,
            type_area_km2 = type_area_km2,
            type_area_per = type_area_km2/sum(type_area_km2)*100)
  
  # join type area into complete data set
  CPH <- CPH |> 
    left_join(CPH_type_area, by = "type_2018")
  
  # Turn SV_new into categorical value (or factor in R)
  CPH$SV_new <- factor(CPH$SV_new)

}

# remove temporary layers
rm("CPH_type_area", "HEL_type_area", "HEL_areas", "CPH_areas")

# ============================================
# 4) Save data
# ============================================

# Helsinki
if (!file.exists("CSVs/Helsinki_Final.csv")) {
  write.csv(HEL, "CSVs/Helsinki_Final.csv")
}

# Copenhagen
if (!file.exists("CSVs/Copenhagen_Final.csv")) {
  write.csv(CPH, "CSVs/Copenhagen_Final.csv")
}



