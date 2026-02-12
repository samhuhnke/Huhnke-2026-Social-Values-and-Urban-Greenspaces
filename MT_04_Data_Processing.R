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





# ============================================
# 7) Spatial analysis 
# ============================================

# Spatial Analysis: Spatially constrained permutation [MOCKUP FOR HEL_FOREST] ============
{
  # convert to sf
  HEL_sf <- geojson_sf(HEL_Forest$geojson)
  HEL_sf <- cbind(HEL_sf, HEL_Forest[ , !names(HEL_Forest) %in% "geojson"])
  class(HEL_sf)
  st_crs(HEL_sf) <- 4326      # GeoJSON is almost always WGS84
  HEL_sf <- st_transform(HEL_sf, 3067)
  
  # Define neighbors by establishing distance 
  coords <- st_coordinates(HEL_sf)
  
  # rn virtually useless
  nb <- dnearneigh(coords, 0, 50) # last digit = distance to neighbor
  
  # create spatial blocks (factor) - you need to define this yourself
  # e.g., divide points into clusters based on coordinates
  set.seed(42)
  HEL_sf$block <- factor(kmeans(coords, centers = 50)$cluster)
  
  # spatial permutation test
  perm_test <- independence_test(
    CanopyCover_mean ~ factor(SV_new) | block,  # <-- use 'block' in formula
    data = HEL_sf,
    distribution = approximate(nresample = 9999))  # updated argument
  
  perm_test
  
}

# testing with sf and linear models
{
  # libraries
  library(geojsonsf)
  library(sf)
  
  # add sf features to df
  HEL_sf <- HEL # re-assign df
  HEL_sf$geometry <- geojson_sfc(HEL$geojson) # add geometry column based on the geojson column
  HEL_sf <- st_as_sf(HEL_sf) #, crs = 3067) # turn into sf file. If needed update CRS. Current CRS is 4326 WGS 84
  HEL_sf$geojson <- NULL  # Remove the original text column
  
  
  # linear model to predict canopy cover based on social value and land cover type 
  HEL_modelled <- HEL_sf |> 
      dplyr::mutate(sv_model = lm(CanopyCover_mean ~ SV_new + type_2018, HEL_sf)$fitted.values) |> # not that + or * can be used to have interaction or no interaction
      dplyr::select(sv_model, CanopyCover_mean)
  
  # plot 
  plot(HEL_modelled)
}

# testing with spatial autocorrelation
{
  # libraries
  library(geojsonsf)
  library(sf)
  library(spdep)
  library(spData)
  
  # Moran's I tests - only suitable for continuous variables
  {
    ### Approach 1: Distance Matrix
    # create weigthed list object from point distance matrix
    dmat <- 1/sf::st_distance(HEL_sf)^2 |> 
      unclass() # unclassing because st_distance() otherwise returns a "unit" class object, which would affect later operations
    class(dmat)
    dmat
    diag(dmat) <- 0 # set matrix diagonal as 0
    dmat
    
    dmat_listw <- spdep::mat2listw(dmat, style = "W")
    dmat_listw$neighbours[[1]] 
    sum(dmat_listw$wights[[1]])
    
    # calculate moran's I
    moran <- spdep::morarn.test(HEL_sf$SV_new, dmat_listw)
    
    ### Approach 2: k-nearest neighbor
    # 
    knn_listw <- sf::st_coordinates(HEL_sf) %>% 
      spdep::knearneigh(k=10) %>% 
      spdep::knn2nb() %>% # create neighbour list
      spdep::nb2listw(style = "W") # create weights list from neighbour object
    knn_listw$neighbours[[1]]
    knn_listw$weights[[1]]
    
    # calculate moran's I
    spdep::moran.test(HEL_sf$CanopyCover_mean, knn_listw)
  }
  
  
  # Joint count statistics - suitable for categorical variables
  # NOTE: this tests whether AA co-occurences are more likely than expected due to random chance
  {
    ### Approach 2: k-nearest neighbor
    # 
    knn_listw <- sf::st_coordinates(HEL_sf) %>% 
      spdep::knearneigh(k=10) %>% 
      spdep::knn2nb() %>% # create neighbour list
      spdep::nb2listw(style = "W") # create weights list from neighbour object
    knn_listw$neighbours[[1]]
    knn_listw$weights[[1]]
    
    # Make sure SV_new is a factor
    HEL_sf$SV_new <- as.factor(HEL_sf$SV_new)
    
    # Example for one category:
    HEL_sf$SV_Personal <- as.numeric(HEL_sf$SV_new == 8)
    
    join_Personal <- joincount.test(
      factor(HEL_sf$SV_Personal),
      knn_listw
    )
    
    join_Personal
  }
  
}



