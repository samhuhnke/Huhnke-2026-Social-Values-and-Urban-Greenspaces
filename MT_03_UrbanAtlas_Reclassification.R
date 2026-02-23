# ============================================ 
# MT_02_UrbanAtlas_Reclassification
# ============================================
#
# AUTHOR: Sam Huhnke, M.Sc. University of Helsinki
#
#
# This code is intended to reclassify the 2018 Urban Atlas data into three overarching classes: Forest, Greenspaces, Other
# 


# ============================================
# 0) Set working directory + clear environment (if needed)
# ============================================

# Clear environment
rm(list = ls(globalenv()))

# Set working directory to whatever needed
setwd("C:/Users/samhu/Desktop/Code Projects/Huhnke_2026/data/")

# ============================================
# 1) Load necessary packages
# ============================================

library(tidyverse)
library(sf) # to work with sf files

# ============================================
# 2) Load data
# ============================================
HEL_UA <- read_sf(dsn = "Helsinki_Final_QGIS/", layer = "Final_Helsinki_Urban Atlas 2018_200m")
CPH_UA <- read_sf(dsn = "Copenhagen_Final_QGIS/", layer = "Final_Copenhagen_Urban Atlas 2018_200m")


# control data structure
colnames(HEL_UA)
colnames(CPH_UA)

# control classes
sf::st_drop_geometry(HEL_UA) |> select(code_2018, class_2018) |> unique() |> arrange(code_2018) |> print(n = 25) 
sf::st_drop_geometry(CPH_UA) |> select(code_2018, class_2018) |> unique() |> arrange(code_2018) |> print(n = 25) 

# ============================================
# 3) Data Reclassification
# ============================================

# Helsinki
HEL_UA_reclassified <- HEL_UA |> 
  mutate(type_2018 = case_when(code_2018 == 31000 ~ "Forest",
                               code_2018 == 14100 | code_2018 == 14200 | code_2018 == 32000 ~ "Greenspace",
                               T ~ "Other"))
# control
sf::st_drop_geometry(HEL_UA_reclassified) |> select(type_2018) |> unique()

# Copenhagen
CPH_UA_reclassified <- CPH_UA |> 
  mutate(type_2018 = case_when(code_2018 == 31000 ~ "Forest",
                               code_2018 == 14100 | code_2018 == 14200 | code_2018 == 32000 ~ "Greenspace",
                               T ~ "Other"))
# control
sf::st_drop_geometry(CPH_UA_reclassified) |> select(type_2018) |> unique()


# ============================================
# 4) Save reclassified data as .shp files
# ============================================

# Helsinki
if (!file.exists("Helsinki_Final_QGIS/Helsinki_Urban Atlas 2018_reclassified_200m.shp")) {
  sf::st_write(HEL_UA_reclassified, "Helsinki_Final_QGIS/Final_Helsinki_Urban Atlas 2018_reclassified_200m.shp")
}

# Copenhagen
if (!file.exists("Copenhagen_Final_QGIS/Copenhagen_Urban Atlas 2018_reclassified_200m.shp")) {
  sf::st_write(CPH_UA_reclassified, "Copenhagen_Final_QGIS/Final_Copenhagen_Urban Atlas 2018_reclassified_200m.shp")
}








