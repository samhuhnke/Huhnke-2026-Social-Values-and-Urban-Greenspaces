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

library(tidyverse) # used for data handling

library(ggplot2) # used for plotting
library(ggpubr) # used for plotting Dunn's test results
library(gratia) # used to plot GAM with ggplot - can use & from patchwork instead of +

library(rstatix) # used to assess effect sizes
library(nnet) # used for multinomial logistic regression
library(ggeffects) # to visualize results of multinomial logistic regression



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
library(geojsonsf) # to extract information from geojson column
library(sf) # to use sf type files

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


### 6.1) Spatial autocorrelation based on joint count statistics

# Helsinki
{
  # libraries
  library(geojsonsf)
  library(sf)
  library(spdep)
  
  # Joint count statistics for each subset
  {
    # Vector of dataset suffixes
    datasets <- c("Forest", "Greenspace", "Other")
    
    # Initialize a nested list to store all results
    joincount_results_types <- list()
    
    # Loop through each dataset
    for (dataset in datasets) {
      
      # Get the current dataset
      current_data <- get(paste0("HEL_sf_", dataset))
      
      # create weighted list
      knn_listw <- sf::st_coordinates(current_data) |>  
        spdep::knearneigh(k=10) |>  
        spdep::knn2nb() |>  # create neighbour list
        spdep::nb2listw(style = "W") # create weights list from neighbor object
      
      # Get unique social value categories
      sv_categories <- unique(current_data$SV_new)
      
      # Initialize a list for this dataset's results
      joincount_results_types[[dataset]] <- list()
      
      # Loop through each category
      for (i in sv_categories) {
        # Create presence-absence column
        current_data[[paste0("SV_", i)]] <- as.numeric(current_data$SV_new == i)
        
        # Calculate joint count statistic
        joincount_results_types[[dataset]][[paste0("SV_", i)]] <- joincount.test(
          factor(current_data[[paste0("SV_", i)]]),
          knn_listw
        )
      }
      
      # Save the modified dataset back
      assign(paste0("HEL_sf_", dataset), current_data)
    
    }
    
    # Access results for a specific dataset and category:
    joincount_results_types$Forest
    joincount_results_types$Greenspace
    joincount_results_types$Other
    joincount_results_types
    
  }
  
  # Plots for joint count
  {
    # Extract the [[1]] results (which appear to be the main join count tests)
    results_df <- data.frame(
      dataset = character(),
      category = character(),
      statistic = numeric(),
      p_value = numeric(),
      stringsAsFactors = FALSE
    )
    
    # loop to extract elements
    for (dataset in datasets) {
      for (category in names(joincount_results_types[[dataset]])) {
        result <- joincount_results_types[[dataset]][[category]]
        
        # Extract the first element [[1]] which contains the test results
        results_df <- rbind(results_df, data.frame(
          dataset = dataset,
          category = category,
          statistic = result[[2]]$statistic,
          p_value = result[[2]]$p.value
        ))
      }
    }
    
    # Create the grouped bar chart
    library(ggplot2)
    
    # Add significance indicator
    results_df$significant <- ifelse(results_df$p_value < 0.05, "Significant", "Not Significant")
    
    ggplot(results_df, aes(x = category, y = statistic, fill = dataset, alpha = significant)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = type_cols) +
      scale_alpha_manual(values = c("Significant" = 1, "Not Significant" = 0.3)) +
      theme_minimal() +
      labs(title = "Join Count Statistics by Social Value and Landscape Type",
           x = "Social Value Category",
           y = "Join Count Statistic",
           fill = "Landscape Type",
           alpha = "Significance (p < 0.05)")
  
  

}
  
  
}

# Copenhagen
{
  # Joint count statistics for each subset
  {
    # Vector of dataset suffixes
    datasets <- c("Forest", "Greenspace", "Other")
    
    # Initialize a nested list to store all results
    joincount_results_types <- list()
    
    # Loop through each dataset
    for (dataset in datasets) {
      
      # Get the current dataset
      current_data <- get(paste0("CPH_sf_", dataset))
      
      # create weighted list
      knn_listw <- sf::st_coordinates(current_data) |>  
        spdep::knearneigh(k=10) |>  
        spdep::knn2nb() |>  # create neighbour list
        spdep::nb2listw(style = "W") # create weights list from neighbor object
      
      # Get unique social value categories
      sv_categories <- unique(current_data$SV_new)
      
      # Initialize a list for this dataset's results
      joincount_results_types[[dataset]] <- list()
      
      # Loop through each category
      for (i in sv_categories) {
        # Create presence-absence column
        current_data[[paste0("SV_", i)]] <- as.numeric(current_data$SV_new == i)
        
        # Calculate joint count statistic
        joincount_results_types[[dataset]][[paste0("SV_", i)]] <- joincount.test(
          factor(current_data[[paste0("SV_", i)]]),
          knn_listw
        )
      }
      
      # Save the modified dataset back
      assign(paste0("CPH_sf_", dataset), current_data)
    }
    
    # Access results for a specific dataset and category:
    joincount_results_types$Forest
    joincount_results_types$Greenspace
    joincount_results_types$Other
    joincount_results_types
    
  }
  
  # Plots for joint count
  {
    # Extract the [[1]] results (which appear to be the main join count tests)
    results_df <- data.frame(
      dataset = character(),
      category = character(),
      statistic = numeric(),
      p_value = numeric(),
      stringsAsFactors = FALSE
    )
    
    # loop to extract elements
    for (dataset in datasets) {
      for (category in names(joincount_results_types[[dataset]])) {
        result <- joincount_results_types[[dataset]][[category]]
        
        # Extract the first element [[1]] which contains the test results
        results_df <- rbind(results_df, data.frame(
          dataset = dataset,
          category = category,
          statistic = result[[2]]$statistic,
          p_value = result[[2]]$p.value
        ))
      }
    }
    
    # Create the grouped bar chart
    library(ggplot2)
    
    # Add significance indicator
    results_df$significant <- ifelse(results_df$p_value < 0.05, "Significant", "Not Significant")
    
    ggplot(results_df, aes(x = category, y = statistic, fill = dataset, alpha = significant)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = type_cols) +
      scale_alpha_manual(values = c("Significant" = 1, "Not Significant" = 0.3)) +
      theme_minimal() +
      labs(title = "Join Count Statistics by Social Value and Landscape Type",
           x = "Social Value Category",
           y = "Join Count Statistic",
           fill = "Landscape Type",
           alpha = "Significance (p < 0.05)")
    
    
    
  }
  
}


### 6.2) Spatial autocorrelation




# ============================================
# 7) WIP Stuff
# ============================================

# Spatial Analysis: Spatially constrained permutation [MOCKUP FOR HEL_FOREST] ============
{
  # 
  
  library(MASS) # for section 7) + load before dplyr or call functions individually
  library(mgcv) # for section 7) + load before nnet or call functions individually
  
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

# Moran's I tests - only suitable for continuous variables
{
  ### Approach 1: Distance Matrix
  # NOTE: This takes forever because the dataset is so large.
  # NOTE: Also this takes into account all points at any given point, which for this analysis doesn't seem to be too sensible
  {
    # create weighted list object from point distance matrix
    dmat <- 1/sf::st_distance(HEL_sf)^2 |> 
      unclass() # unclassing because st_distance() otherwise returns a "unit" class object, which would affect later operations
    class(dmat)
    dmat
    diag(dmat) <- 0 # set matrix diagonal as 0
    dmat
    
    dmat_listw <- spdep::mat2listw(dmat, style = "W") # this creates the weighted list
    dmat_listw$neighbours[[1]] 
    sum(dmat_listw$wights[[1]])
    
    # calculate moran's I
    moran <- spdep::morarn.test(HEL_sf$SV_new, dmat_listw)
    
  }
  
  ### Approach 2: k-nearest neighbor
  # NOTE: THis is faster and more sensible!
  {
    knn_listw <- sf::st_coordinates(HEL_sf) |> 
      spdep::knearneigh(k=10) |>  
      spdep::knn2nb() |>  # create neighbour list
      spdep::nb2listw(style = "W") # create weights list from neighbour object
    knn_listw$neighbours[[1]]
    knn_listw$weights[[1]]
    
    # calculate moran's I
    spdep::moran.test(HEL_sf$CanopyCover_mean, knn_listw)
  }
  
}

# Moran's I on MLR residuals - This can test model adequacy: significant results = autocorrelation at play
# NOTE: All of this can be turned into a loop
{
  # prepare weighted list
  knn_listw <- sf::st_coordinates(HEL_sf) |>  
    spdep::knearneigh(k=10) |>  
    spdep::knn2nb() |>  # create neighbour list
    spdep::nb2listw(style = "W") # create weights list from neighbor object
  
  # load 
  library(nnet)
  
  # createt MLR
  MLR <- multinom(SV_new ~ CanopyCover_mean, data = HEL_sf)
  
  # Pearson residuals
  res <- residuals(MLR, type = "pearson")
  
  # Moran's on each variable
  
  # Initialize a list to store results
  moran_results <- list()
  
  # Loop through columns 1 to 4
  for (i in 1:8) {
    moran_results[[i]] <- moran.test(res[, i], knn_listw)
  }
  
  # Access results
  moran_results
  # or e.g.
  moran_results[[1]]
}

# Spatial Clustering by k means -> NO SPATIAL INFORMATION
{
    data <- sf::st_set_geometry(HEL_sf, NULL) |> select(CanopyCover_mean)
    
    HEL_sf
    data
    
    clusters <- kmeans(data, 8, iter.max = 100)
    
    clusters
    
    HEL_sf$cluster <- clusters$cluster
    plot(HEL_sf[,"cluster"])
    
    # plot
    plotdata <- tibble::add_column(data, cluster = clusters$cluster)
    
    # reshape the wide format data frame into a long format
    plotdata <- tidyr::gather(plotdata, variable, value, -cluster)
    
    # change type of cluster variable to factor for grouping
    plotdata$cluster <- factor(plotdata$cluster)
    
    # ggplot2
    library(ggplot2)
    ggplot(plotdata, aes(x = variable, y = value, fill = cluster)) + 
      geom_boxplot()
  }




