# ============================================ 
# MT_06_Grid_Sensitivity_Analysis
# ============================================
#
# AUTHOR: Sam Huhnke, M.Sc. University of Helsinki
#
# This code contains the sensitivity analysis
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

# Colors
resolution_cols <- c(
  "100" = "#9E9E9E",  # medium grey
  "175" = "#7A7A7A",  # darker grey
  "250" = "#FF0000",  # red
  "375" = "#555555",  # dark grey
  "500" = "#2F2F2F"   # very dark grey
)

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
{
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
                               Other_top_q = numeric(),
                               GS_top_q = numeric(),
                               Forest_top_q = numeric(),
                               CC_mean_b50 = numeric(),
                               CC_median_b50 = numeric(),
                               n_sv1 = numeric(),n_sv2 = numeric(),n_sv3 = numeric(),n_sv4 = numeric(),n_sv5 = numeric(),n_sv6 = numeric(),n_sv7 = numeric(),n_sv8 = numeric()
  )
}

# Data Structure Analysis Loop
for (city in cities) {
  for (x in resolution) {
    
    # current file + resolution
    current <- get(paste0(city, "_", x, "m"))
    current_res <- as.numeric(x)
    
    # cell number
    cells_temp <- nrow(current)
    
    # rows x cell_size = total_area -> total_area/1000000 = area in km2 
    area_temp <- nrow(current) * current_res*current_res/1000000 
    
    # areas still have to be adapted to 100m buffered administrative boundary sizes
    area_rel_temp <- if (city == "HEL") { area_temp / 249 * 100 } else { area_temp / 109 * 100 }
    
    # total social value count across all cells
    total_sv_temp <- current |> select(n_total) |> sum()
    
    # social value deficit compared to total mapped pins for cities after clipping: CPH = 5806, HEL = 15241 [ = 100m buffer administrative boundary]
    sv_deficit_temp <- if (city == "HEL") { total_sv_temp - 16372 } else { total_sv_temp - 5806 }
    
    # social value data loss in percent compared to original clipped data [ = 100m buffer administrative boundary]
    deficit_rel_temp <- if (city == "HEL") { sv_deficit_temp / 16372*100 } else { sv_deficit_temp / 5806*100 }
    
    # average social value density per grid cell
    sv_density_temp <- total_sv_temp / nrow(current)
    
    # mean of mean canopy cover
    canopy_mean_temp <- mean(current$CC_mean)
    
    # mean of median canopy 
    canopy_median_temp <- mean(current$CC_median)
    
    # number of mean canopy values larger zero but smaller than 50
    CC_mean_b50_temp <- nrow(current |> filter(CC_mean > 0 & CC_mean < 50))
    
    # number of median canopy values larger zero but smaller than 50
    CC_median_b50_temp <- nrow(current |> filter(CC_median > 0 & CC_median < 50))
    
    # mean Other landcover per grid cell
    Other_avg_temp <- mean(current$Other)
    
    # mean Greenspace landcover per gird cell
    GS_avg_temp <- mean(current$Greenspace)
    
    # mean Forest landcover per grid cell
    Forest_avg_temp <- mean(current$Forest)
    
    # count of cells with dominance "Other"
    Other_d_temp <- nrow(current |> filter(Dominant_LC == "Other"))
    
    # count of cells with dominance "Greenspace"
    GS_d_temp <- nrow(current |> filter(Dominant_LC == "Greenspace"))
    
    # count of cells with dominance "Forest"
    Forest_d_temp <- nrow(current |> filter(Dominant_LC == "Forest"))
    
    # number of cells with other dominance
    Other_q_temp <- nrow(current |> filter(Other_quintile == 5))
    
    # number of cells with Greenspace dominance
    GS_q_temp <- nrow(current |> filter(Greenspace_quintile == 5))
    
    # number of cellls with Forest dominance 
    Forest_q_temp <- nrow(current |> filter(Forest_quintile == 5))
    
    # individual social value count across all cells
    sv1_temp <- current |> select(n_sv1) |> sum()
    sv2_temp <- current |> select(n_sv2) |> sum()
    sv3_temp <- current |> select(n_sv3) |> sum()
    sv4_temp <- current |> select(n_sv4) |> sum()
    sv5_temp <- current |> select(n_sv5) |> sum()
    sv6_temp <- current |> select(n_sv6) |> sum()
    sv7_temp <- current |> select(n_sv7) |> sum()
    sv8_temp <- current |> select(n_sv8) |> sum()
    
    
    # save outputs
    analysis_table <- rbind(analysis_table, 
                            data.frame(
                              city = city, 
                              resolution = paste0(x, "m"), 
                              cells = cells_temp,
                              area_km2 = area_temp,
                              area_rel = area_rel_temp,
                              total_sv = total_sv_temp,
                              sv_deficit = sv_deficit_temp,
                              sv_loss_rel = deficit_rel_temp,
                              sv_density = sv_density_temp,
                              canopy_mean = canopy_mean_temp,
                              canopy_median = canopy_median_temp,
                              CC_mean_b50 = CC_mean_b50_temp,
                              CC_median_b50 = CC_median_b50_temp,
                              Other_avg = Other_avg_temp,
                              GS_avg = GS_avg_temp,
                              Forest_avg = Forest_avg_temp,
                              Other_dom_n = Other_d_temp,
                              GS_dom_n = GS_d_temp,
                              Forest_dom_n = Forest_d_temp,
                              Other_top_q = Other_q_temp,
                              GS_top_q = GS_q_temp,
                              Forest_top_q = Forest_q_temp,
                              n_sv1 = sv1_temp, n_sv2 = sv2_temp, n_sv3 = sv3_temp, n_sv4 = sv4_temp, 
                              n_sv5 = sv5_temp, n_sv6 = sv6_temp, n_sv7 = sv7_temp, n_sv8 = sv8_temp
                            )
    )
    
    
    # remove everything that is not needed
    rm(list = ls()[grep("_temp", ls())]) # this has to be adjusted!
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

# plot - comparison - adjust city and GAM in this section!
{
  # choose all GAMs to be compared
  all_names <- ls()
  filtered <- all_names[grep("GAM_1_total", all_names)] # adjust GAM here!
  filtered <- filtered[grep("HEL", filtered)] # adjust city here!
  gam_list <- mget(filtered)
  
  # extract smooths
  smooth_df <- do.call(rbind, lapply(names(gam_list), function(name) {
    df <- smooth_estimates(gam_list[[name]])
    df$model <- name
    return(df)
  }))
  
  # extract resolution from model name
  smooth_df$resolution <- sub(".*_(\\d+)m_.*", "\\1", smooth_df$model)
  
  p1 <- ggplot(smooth_df, aes(x = CC_mean, y = .estimate, colour = resolution, linetype = resolution)) +
    geom_line() +
    geom_ribbon(aes(ymin = .estimate - .se, ymax = .estimate + .se, fill = resolution),
                alpha = 0.1, colour = NA) +
    scale_color_manual(values = resolution_cols) +
    scale_fill_manual(values = resolution_cols) +
    labs(colour = "Resolution (m)", fill = "Resolution (m)", linetype = "Resolution (m)",
         x = "Canopy Cover [%]", y = "Partial Effect (log scale)") +
    annotate("label", x = min(smooth_df$CC_mean), y = Inf,
             label = "Helsinki",
             hjust = 0, vjust = 1.2) +
    theme_minimal() +
    theme(legend.position = "none")
    theme(
      legend.position = c(0.98, 0.02),
      legend.justification = c(1, 0)
    )
  
  # adapt assignment to change plots
  p1 + p2
  
  ggsave("Sensitivity.png", width = 210, height = 120, units = "mm", dpi = 300)
  
}

# plot - single - adjust city and GAM in this section!
plot(CPH_250m_GAM_1_total)
plot(HEL_250m_GAM_1_total)

# summaries
summary(CPH_100m_GAM_1_total)

rm(list = ls()[grep("_total", ls())]) # this has to be adjusted!















