# ============================================ 
# MT_06_Spatial_Analysis
# ============================================

# AUTHOR: Sam Huhnke, M.Sc. University of Helsinki
#
#
# This code provides spatial analyses of relations between mapped social values and tree canopy cover for
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

library(MASS) # for section 7) + load before dplyr
library(mgcv) # for section 7) + load before nnet

library(tidyverse) # used for data handling
library(ggplot2) # used for plotting
library(ggpubr) # used for plotting Dunn's test results
library(gratia) # used to plot GAM with ggplot - can use & from patchwork instead of +

library(rstatix) # used to assess effect sizes
library(nnet) # used for multinomial logistic regression
library(ggeffects) # to visualize results of multinomial logistic regression

library(sf) # to use sf type files
library(geojsonsf) # to read geojson variable

library(spdep)
library(coin)

# ============================================
# 2) Load data
# ============================================

# Helsinki
HEL <- read.csv("CSVs/Helsinki_Final.csv", sep = ",")

# Copenhagen
CPH <- read.csv("CSVs/Copenhagen_Final.csv", sep = ",")

# ============================================
# 3) Turn into spatial file
# ============================================

# libraries
library(geojsonsf)

# Helsinki
{
  # add sf features to df
  HEL_sf <- HEL |> # re-assign df
    select(geojson, SV_new, CanopyCover_mean, type_2018)
  HEL_sf$geometry <- geojson_sfc(HEL$geojson) # add geometry column based on the geojson column
  HEL_sf <- st_as_sf(HEL_sf) #, crs = 3067) # turn into sf file. If needed update CRS. Current CRS is 4326 WGS 84
  HEL_sf$geojson <- NULL  # Remove the original text column
}

# Copenhagen
{
  # add sf features to df
  CPH_sf <- CPH |> # re-assign df
    select(geojson, SV_new, CanopyCover_mean, type_2018)
  CPH_sf$geometry <- geojson_sfc(CPH$geojson) # add geometry column based on the geojson column
  CPH_sf <- st_as_sf(CPH_sf) #, crs = 3067) # turn into sf file. If needed update CRS. Current CRS is 4326 WGS 84
  CPH_sf$geojson <- NULL  # Remove the original text column
}

# ============================================
# 4) Create Subsets
# ============================================

# Helsinki
HEL_sf_Forest <- HEL_sf |> filter(type_2018 == "Forest")
HEL_sf_Greenspace <- HEL_sf |> filter(type_2018 == "Greenspace")
HEL_sf_Other <- HEL_sf |> filter(type_2018 == "Other")

# Copenhagen
CPH_sf_Forest <- CPH_sf |> filter(type_2018 == "Forest")
CPH_sf_Greenspace <- CPH_sf |> filter(type_2018 == "Greenspace")
CPH_sf_Other <- CPH_sf |> filter(type_2018 == "Other")


# ============================================
# 5) Analysis Preparation
# ============================================

# Assign colors
{
  # Land Use type colors
  type_cols <- c("#008c00", "#8cdc00", "#959595" )
  
  # Social Value colors
  sv_cols <- c("#d45680", "#4d9955", "#a0afa0", "#009acb", "#006695", "#e69c24", "#ffeccc", "#8b7356")
  
}

# Assign labels
{
  # Land Use types
  type_labels <- c(
    "1" = "1: Forest",
    "2" = "2: Greenspace",
    "3" = "3: Other"
  )
  
  # Social values
  sv_labels <- c(
    "1" = "1: Relaxation",
    "2" = "2: Natural Values",
    "3" = "3: Aesthetics",
    "4" = "4: Physical Well-Being and \n  Outdoor Activity",
    "5" = "5: Social Interaction",
    "6" = "6: Heritage and \n  Community Values",
    "7" = "7: Spiritual Values",
    "8" = "8: Personal Identity"
  )
}


# ============================================
# 6) Spatial Analysis
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

