# ============================================ 
# MT_05_Grid Data Post-processing
# ============================================
#
# AUTHOR: Sam Huhnke, M.Sc. University of Helsinki
#
#
# This code is intended to post-process the grid based data to prepare it for analysis
# 
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

# Vector of dataset suffixes
cities <- c("CPH", "HEL")
resolution <- c("100", "175", "250", "375", "500")
content <- c("LC", "CCSV") # used to load all datasets


# TURN THIS INTO A LOOP to load all the dfs for CPH and HEL

# Copenhagen loading loop
for (x in resolution) {
    for (file in content) {
      
      # load file
      current <- read_sf(dsn = "Copenhagen_Final_QGIS/Grid_Outputs", layer = paste0("CPH_", x, "m", "_", file))
      
        # data handling
        if (file == "CCSV") {           
          final <- current |> 
            # remove all columns that are not needed
            select(-c(left, top, right, bottom, row_index, col_index)) |> 
            arrange(id)
        } else {
          final <- current |> 
            # remove all columns that are not needed
            select(-c(1:12), -c(left, top, right, bottom, row_index, col_index)) |> 
            select(id, area_m2, everything()) |> 
            arrange(id)
        }
      
      # assign file
      assign(
        paste0("CPH_", x, "m", "_", file),
        final)
    }
}

# Helsinki loading loop
for (x in resolution) {
  for (file in content) {
    
    # load file
    current <- read_sf(dsn = "Helsinki_Final_QGIS/Grid_Outputs", layer = paste0("HEL_", x, "m", "_", file))
    
    # data handling
    if (file == "CCSV") {
      final <- current |> 
        # remove all columns that are not needed
        select(-c(left, top, right, bottom, row_index, col_index)) |> 
        arrange(id)
    } else {
      final <- current |> 
        # remove all columns that are not needed
        select(-c(1:12), -c(left, top, right, bottom, row_index, col_index)) |> 
        select(id, area_m2, everything()) |> 
        arrange(id)
    }
    
    # assign file
    assign(
      paste0("HEL_", x, "m", "_", file),
      final)
  }
}

# ============================================
# 3) Pivoting LC into wide format
# ============================================

for (city in cities) {

for (x in resolution) {
  
  current_LC <- get(paste0(city, "_", x, "m", "_LC"))
  current_CCSV <- get(paste0(city, "_", x, "m",  "_CCSV"))
  current_res <- as.numeric(x)
  
  # combine land use classes within a single grid cell and calculate percent of area covered by each land use 
  Temp <- sf::st_drop_geometry(current_LC) |> 
    group_by(id, type_2018) |> 
    mutate(area_t_m2 = sum(area_t_m2),
           area_t_m2 = case_when(area_t_m2 > current_res*current_res ~ current_res*current_res, # adapting maximum area size to resolution
                                 T ~ area_t_m2),
           area_t_p = area_t_m2/area_m2 * 100, 
           area_t_p = round(area_t_p, digits = 2)) |> 
    unique()
  
  # pivot table wider -> this has less rows than the CCSV dataset because some cells don't have any landcover as per the UA 2018
  # (usually because they are in water)
  Wide <- Temp |> 
    select(-area_t_m2) |> 
    pivot_wider(names_from = type_2018, values_from = area_t_p) 
  
  # Combine into one dataset
  Final <- current_CCSV |> 
    left_join(Wide) |> 
    mutate(across(where(is.numeric), ~ replace_na(.x, 0)))
  
  # create landcover dominance column
  Final$Dominant_LC <- factor(
    ifelse(Final$Forest >= Final$Greenspace & Final$Forest >= Final$Other, "Forest",
           ifelse(Final$Greenspace >= Final$Other, "Greenspace", "Other"))
  )
  
  # create final dataset
  assign(
    paste0(city, "_", x, "m"),
    Final
  ) 
  
  # Save to CSV
  write.csv(sf::st_drop_geometry(Final), paste0("Final_data/CSVs/", city, "_", x, "m", ".csv"), row.names = FALSE)
  
  # save to GeoPackage
  sf::st_write(Final, paste0("Final_data/GPKGs/", city, "_", x, "m", ".gpkg"))
  
  # remove used data
  rm(list = c(paste0(city, "_", x, "m", "_LC"), paste0(city, "_", x, "m", "_CCSV")))
  
  }
  
}

