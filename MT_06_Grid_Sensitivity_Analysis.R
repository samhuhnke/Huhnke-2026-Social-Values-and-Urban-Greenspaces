# ============================================ 
# MT_05_Buffer_Sensitivity_Analysis (+ at different grid sizes)
# ============================================
#
# AUTHOR: Sam Huhnke, M.Sc. University of Helsinki
#
# This code contains all GLMs and GAMs used to analyze the data. 
# It further creates tables to check the general data structure and to assess dispersion
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
SVs <- c("1", "2", "3", "4", "5", "6", "7", "8")

# loading loop
for (city in cities) {
  for (x in resolution) {
    
    # load file
    current <- read.csv(paste0("Final_data/CSVs/", city, "_", x, "m", ".csv"))
    
    # assign file
    # assign file
    assign(
      paste0(city, "_", x, "m"),
      current)
  }
}


# ============================================
# 3) Data structure
# ============================================

# create empty table before loops
analysis_table <- data.frame(city = character(), resolution = character(),
                             cells = numeric(),
                             area_km2 = numeric(), 
                             area_rel = numeric(),
                             total_sv = numeric(),
                             sv_deficit = numeric(),
                             sv_def_rel = numeric(),
                             sv_density = numeric(),
                             canopy_mean = numeric(),
                             canopy_median = numeric(),
                             Other_avg = numeric(),
                             GS_avg = numeric(),
                             Forest_avg = numeric(),
                             Other_dom_n = numeric(),
                             GS_dom_n = numeric(),
                             Forest_dom_n = numeric(),
                             CC_mean_b50 = numeric(),
                             CC_median_b50 = numeric()
                             )


# Data Structure Analysis Loop
for (city in cities) {
  for (x in resolution) {
    
    # current file + resolution
    current <- get(paste0(city, "_", x, "m"))
    current_res <- as.numeric(x)
    
    # cell number
    cells <- nrow(current)
    
    # rows x cell_size = total_area -> total_area/1000000 = area in km2 
    area <- nrow(current) * current_res*current_res/1000000 
    
    # areas still have to be adapted to 100m buffered administrative boundary sizes
    area_rel <- if (city == "HEL") { area / 249 * 100 } else { area / 109 * 100 }
    
    # total social value count across all cells
    total_sv <- current |> select(n_total) |> sum()
    
    # social value deficit compared to total mapped pins for cities after clipping: CPH = 5806, HEL = 15241 [ = 100m buffer administrative boundary]
    sv_deficit <- if (city == "HEL") { total_sv - 16372 } else { total_sv - 5806 }
    
    # social value data loss in percent compared to original clipped data [ = 100m buffer administrative boundary]
    deficit_rel <- if (city == "HEL") { sv_deficit / 16372*100 } else { sv_deficit / 5806*100 }
    
    # average social value density per grid cell
    sv_density <- total_sv / nrow(current)
    
    # mean of mean canopy cover
    canopy_mean <- mean(current$CC_mean)
    
    # mean of median canopy 
    canopy_median <- mean(current$CC_median)
    
    # number of mean canopy values larger zero but smaller than 50
    CC_mean_b50 <- nrow(current |> filter(CC_mean > 0 & CC_mean < 50))
    
    # number of median canopy values larger zero but smaller than 50
    CC_median_b50 <- nrow(current |> filter(CC_median > 0 & CC_median < 50))
    
    # mean Other landcover per grid cell
    Other_avg <- mean(current$Other)
    
    # mean Greenspace landcover per gird cell
    GS_avg <- mean(current$Greenspace)
    
    # mean Forest landcover per grid cell
    Forest_avg <- mean(current$Forest)
    
    # number of cells with other dominance
    Other_dom <- nrow(current |> filter(Dominant_LC == "Other"))
    
    # number of cells with Greenspace dominance
    GS_dom <- nrow(current |> filter(Dominant_LC == "Greenspace"))
    
    # number of cellls with Forest dominance 
    Forest_dom <- nrow(current |> filter(Dominant_LC == "Forest"))
    
    
    # save outputs
   analysis_table <- rbind(analysis_table, 
                           data.frame(
                             city = city, 
                             resolution = paste0(x, "m"), 
                             cells = cells,
                             area_km2 = area,
                             area_rel = area_rel,
                             total_sv = total_sv,
                             sv_deficit = sv_deficit,
                             sv_loss_rel = deficit_rel,
                             sv_density = sv_density,
                             canopy_mean = canopy_mean,
                             canopy_median = canopy_median,
                             CC_mean_b50 = CC_mean_b50,
                             CC_median_b50 = CC_median_b50,
                             Other_avg = Other_avg,
                             GS_avg = GS_avg,
                             Forest_avg = Forest_avg,
                             Other_dom_n = Other_dom,
                             GS_dom_n = GS_dom,
                             Forest_dom_n = Forest_dom
                             )
                           )
   
  }
}


# ============================================
# 4) GLM Analysis and Dispersion Check
# ============================================

# libraries needed
{
  library(mgcv) # used for GLMs and GAMs
  library(patchwork)
  library(ggplot2)
  library(gratia) # used to plot GAM with ggplot - can use & from patchwork instead of +
}

# create empty table before loops to check dispersion in each dataset
dispersion_table <- data.frame(city = character(), resolution = character(),
                             dispersion = numeric() 
                             )

# Dispersion loop
for (city in cities) {
  for (x in resolution){
    
    # current file
    current <- get(paste0(city, "_", x, "m"))
    
    # Poisson regression GLM
    pois_mod <- glm(n_total ~ CC_mean, family = poisson(link = "log"), data = current)
    
    # dispersion
    dispersion <- sum(residuals(pois_mod, type = "pearson")^2) / df.residual(pois_mod)
    
    dispersion_table <- rbind(dispersion_table,
                              data.frame(
                                city = city,
                                resolution = paste0(x, "m"),
                                dispersion = dispersion
                              ))
  }
}
dispersion_table

# assign current file
file <- HEL_500m

# GLM: Negative Binomial
nb_mod <- MASS::glm.nb(n_total ~ CC_mean, data = file)
summary(nb_mod)

# ============================================
# 5) GAM Analysis - Total SV Count
# ============================================

# libraries needed - GAM
{
  library(mgcv) # used for GLMs and GAMs
  library(patchwork)
  library(ggplot2)
  library(gratia) # used to plot GAM with ggplot - can use & from patchwork instead of +
}

# libraries needed - Plot
{
  library(ggplot2)
  library(gratia)  # gratia::smooth_estimates() extracts GAM smooths as dataframes
}

# GAM 1: n_total ~ s(CC_mean)
for (city in cities) {
  for (x in resolution) {
    
    # current file 
    current <- get(paste0(city, "_", x, "m"))
    
    # GAM 1: Canopy Cover 
    gam_1 <- gam(n_total ~ s(CC_mean), family = nb(), data = current)
    
    # save GAM
    assign(
      paste0(city, "_", x, "m", "_", "GAM_1", "_", "total"),
      gam_1
    )

      
  }
}

# GAM 2: n_total ~ s(CC_mean) + Forest + Greenspace + Other
for (city in cities) {
  for (x in resolution) {
    
    # current file + resolution
    current <- get(paste0(city, "_", x, "m"))
    
    # GAM 1: Canopy Cover 
    gam_2 <- gam(n_total ~ s(CC_mean) + Forest + Greenspace + Other, family = nb(), data = current)
    
    # save GAM
    assign(
      paste0(city, "_", x, "m", "_", "GAM_2", "_", "total"),
      gam_2
    )
    
    
  }
}

# GAM 3: n_total ~ s(CC_mean) + Forest + Greenspace
for (city in cities) {
  for (x in resolution) {
    
    # current file + resolution
    current <- get(paste0(city, "_", x, "m"))
    
    # GAM 1: Canopy Cover 
    gam_3 <- gam(n_total ~ s(CC_mean) + Forest + Greenspace, family = nb(), data = current)
    
    # save GAM
    assign(
      paste0(city, "_", x, "m", "_", "GAM_3", "_", "total"),
      gam_3
    )
    
    
  }
}

# GAM 4: n_total ~ s(CC_mean, by = factor(Dominant_LC)) + factor(Dominant_LC)
for (city in cities) {
  for (x in resolution) {
    
    # current file + resolution
    current <- get(paste0(city, "_", x, "m"))
    
    # GAM 1: Canopy Cover 
    gam_4 <- gam(n_total ~ s(CC_mean, by = factor(Dominant_LC)) + factor(Dominant_LC), family = nb(), data = current)
    
    # save GAM
    assign(
      paste0(city, "_", x, "m", "_", "GAM_4", "_", "total"),
      gam_4
    )
    
    
  }
}

# plot - adjust city and GAM in this section!
{
  # choose all GAMs to be compared
  all_names <- ls()
  filtered <- all_names[grep("GAM_1_total", all_names)] # adjust GAM here!
  filtered <- filtered[grep("CPH", filtered)] # adjust city here!
  gam_list <- mget(filtered)
  
  # extract smooths
  smooth_df <- do.call(rbind, lapply(names(gam_list), function(name) {
    df <- smooth_estimates(gam_list[[name]])
    df$model <- name
    return(df)
  }))
  
  # extract resolution from model name
  smooth_df$resolution <- sub(".*_(\\d+)m_.*", "\\1", smooth_df$model)
  
  ggplot(smooth_df, aes(x = CC_mean, y = .estimate, colour = resolution, linetype = resolution)) +
    geom_line() +
    geom_ribbon(aes(ymin = .estimate - .se, ymax = .estimate + .se, fill = resolution),
                alpha = 0.1, colour = NA) +
    labs(colour = "Resolution (m)", fill = "Resolution (m)", linetype = "Resolution (m)",
         x = "Canopy Cover [%]", y = "Partial Effect (log scale)",
         title = "Copenhagen",
         subtitle = "Grid Sensitivity for: total SV count ~ mean Canopy Cover") +
    theme_minimal()
  
}

# summaries
summary(CPH_100m_GAM_1_total)

rm(list = ls()[grep("_total", ls())]) # this has to be adjusted!

# ============================================
# 6) GAM Analysis - Individual SV Count
# ============================================

# libraries needed - GAM
{
  library(mgcv) # used for GLMs and GAMs
  library(patchwork)
  library(ggplot2)
  library(gratia) # used to plot GAM with ggplot - can use & from patchwork instead of +
}

# libraries needed - Plot
{
  library(ggplot2)
  library(gratia)  # gratia::smooth_estimates() extracts GAM smooths as dataframes
}

# GAM 1: n_svX ~ s(CC_mean)
for (city in cities) {
  for (x in resolution) {
    
    # current file + resolution
    current <- get(paste0(city, "_", x, "m"))
    
    for (y in SVs)  {
      
      # name of columns: n_svX with X being a number between 1 and 8
      sv <- paste0("n_sv", y)
      
      # Build formulas as strings, then convert with as.formula()
      f1 <- as.formula(paste(sv, "~ s(CC_mean)"))

      # GAM 1: Canopy Cover 
      gam_1 <- gam(f1, family = nb(), data = current)
      
      # save GAM
      assign(
        paste0(city, "_", x, "m", "_", "GAM_1", "_", y),
        gam_1
      )
      
    }
  }
}

# GAM 2: n_svX ~ s(CC_mean) + Forest + Greenspace + Other
for (city in cities) {
  for (x in resolution) {
    
    # current file + resolution
    current <- get(paste0(city, "_", x, "m"))
    
    for (y in SVs)  {
      
      # name of columns: n_svX with X being a number between 1 and 8
      sv <- paste0("n_sv", y)
      
      # Build formulas as strings, then convert with as.formula()
      f2 <- as.formula(paste(sv, "~ s(CC_mean) + Forest + Greenspace + Other"))
      
      # GAM 1: Canopy Cover 
      gam_2 <- gam(f2, family = nb(), data = current)
      
      # save GAM
      assign(
        paste0(city, "_", x, "m", "_", "GAM_2", "_", y),
        gam_2
      )
      
    }
  }
}

# colors and labels
{
  # Social Value colors
  sv_cols <- c(
    "#900c3f",  # 1: Relaxation         
    "#0b5227",  # 2: Natural Values      
    "#182b55",  # 3: Aesthetics          
    "#f7c435",  # 4: Physical Well-Being  
    "#d94f21",  # 5: Social Interaction  
    "#9aab4b",  # 6: Heritage            
    "#5f4e94",  # 7: Spiritual Values    
    "#f0b6ad"   # 8: Personal Identity   
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

# plot - adjust data entry in this section!
{
  # choose all GAMs to be compared
  gam_list <- mget(ls()[grep("HEL_250m_GAM_1", ls())]) # this has to be adjusted!
  
  # prepare plot
  smooth_df <- do.call(rbind, lapply(names(gam_list), function(name) {
    df <- smooth_estimates(gam_list[[name]])
    df$model <- name
    return(df)
  }))
  
  # extract SV number from model name (the last character)
  smooth_df$sv_num <- sub(".*GAM_1_", "", smooth_df$model)
  
  ggplot(smooth_df, aes(x = CC_mean, y = .estimate, colour = sv_num, linetype = sv_num)) +
    geom_line() +
    #geom_ribbon(aes(ymin = .estimate - .se, ymax = .estimate + .se, fill = sv_num), 
     #           alpha = 0.05, colour = NA) +
    # scale_y_continuous(trans = "exp") +     # this transforms the estimate link-scale (= log scale) into a response scale (exp(log scale))
    scale_colour_manual(values = sv_cols, labels = sv_labels) +
    scale_fill_manual(values = sv_cols, labels = sv_labels) +
    scale_linetype_manual(values = c("1" = "solid", "2" = "solid", "3" = "solid",
                                     "4" = "solid", "5" = "dashed", "6" = "dashed",
                                     "7" = "dashed", "8" = "dashed"),
                          labels = sv_labels) +
    labs(colour = "Social Value", fill = "Social Value", linetype = "Social Value",
         x = "Canopy Cover [%]", y = "Partial Effect (log scale)",
         title = "Helsinki",
         subtitle = "GAM: total SV count ~ mean Canopy Cover; Resolution: 250m") +
    theme_minimal()
  
}

summary(CPH_250m_GAM_1_1)



# remove function after all plots are done
rm(list = ls()[grep("GAM", ls())]) # this has to be adjusted!
