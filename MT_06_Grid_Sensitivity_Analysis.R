# ============================================ 
# MT_05_Buffer_Sensitivity_Analysis (+ at different grid sizes)
# ============================================
#
# AUTHOR: Sam Huhnke, M.Sc. University of Helsinki
#
#
# This code is intended to analyze the impact of grid size and clipping process on results
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
cities <- c("CPH") #, "HEL")
resolution <- c("100", "175", "250", "375", "500")

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

# # create empty table before loops
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
                             Forest_dom_n = numeric()
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
    area_rel <- if (city == "HEL") { area / 212 * 100 } else { area / 109 * 100 }
    
    # total social value count across all cells
    total_sv <- current |> select(n_total) |> sum()
    
    # social value deficit compared to total mapped pins for cities after clipping: CPH = 5806, HEL = 15241 [ = 100m buffer administrative boundary]
    sv_deficit <- if (city == "HEL") { total_sv - 15241 } else { total_sv - 5806 }
    
    # social value data loss in percent compared to original clipped data [ = 100m buffer administrative boundary]
    deficit_rel <- if (city == "HEL") { sv_deficit / 15241*100 } else { sv_deficit / 5806*100 }
    
    # average social value density per grid cell
    sv_density <- total_sv / nrow(current)
    
    # mean of mean canopy cover
    canopy_mean <- mean(current$CC_mean)
    
    # mean of median canopy 
    canopy_median <- mean(current$CC_median)
    
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
# 4) GLM + GAM Analysis
# ============================================

# assign current file
file <- CPH_175m


# libraries needed
{
  library(mgcv) # used for GLMs and GAMs
  library(patchwork)
  library(ggplot2)
  library(gratia) # used to plot GAM with ggplot - can use & from patchwork instead of +
}


# GLM: Poisson Regression
pois_mod <- glm(n_total ~ CC_mean, family = poisson(link = "log"), data = file)
summary(pois_mod)


# GLM: Dispersion check
dispersion <- sum(residuals(pois_mod, type = "pearson")^2) / df.residual(pois_mod)
dispersion # dispersion 283.085 >> 2, so switch to negative binomial!


# GLM: Negative Binomial
nb_mod <- MASS::glm.nb(n_total ~ CC_mean, data = file)
summary(nb_mod)


# GAM: Canopy Cover 
gam_mod_1 <- gam(n_total ~ s(CC_mean), family = nb(), data = file)
summary(gam_mod_1) #edf = 2.816 --> U-shaped
plot(gam_mod_1, rug = TRUE)


# GAM: Canopy Cover + all Landcovers
gam_mod_2 <- gam(n_total ~ s(CC_mean) + Forest + Greenspace + Other, family = nb(), data = file)
summary(gam_mod_2) 
plot(gam_mod_2, rug = TRUE)

# 
visreg::visreg(gam_mod_2, "Forest")
visreg::visreg(gam_mod_2, "Greenspace")
visreg::visreg(gam_mod_2, "Other")


# GAM: Canopy Cover + %Forest + %Greenspace --> landcover relative areas sum to one. by dropping %Other, Forest and Greenspace are shown in comparison to other
gam_mod_3 <- gam(n_total ~ s(CC_mean) + Forest + Greenspace, family = nb(), data = file)
summary(gam_mod_3) 
plot(gam_mod_3, rug = TRUE)


# GAM: Canopy Cover divided by dominant landcover type
gam_mod_4 <- gam(n_total ~ s(CC_mean, by = factor(Dominant_LC)) + factor(Dominant_LC), family = nb(), data = file)
summary(gam_mod_4)
plot(gam_mod_4, pages = 1, rug = TRUE)

# Plot: Frequency plots
ggplot(file, aes(x = CC_mean, y = n_total)) +
  geom_point() +
  geom_smooth(method = "loess", se = TRUE) +
  labs(
    x = "Canopy cover (%)",
    y = "Number of mapped social values",
    title = "Global frequency of mapped social values along canopy cover gradient"
  )

names(file)
str(file$Dominant_LC)
