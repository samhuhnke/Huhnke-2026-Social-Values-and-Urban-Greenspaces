# ============================================ 
# MT_07_250m_Analysis
# ============================================
#
# AUTHOR: Sam Huhnke, M.Sc. University of Helsinki
#
# This code contains all GAMs used to analyze the data. 
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

# libraries needed for data handling
library(tidyverse)

# libraries needed - GAM
library(mgcv) # used for GLMs and GAMs

# libraries needed - Plot
library(ggplot2)
library(gratia)  # gratia::smooth_estimates() extracts GAM smooths as dataframes
library(patchwork) # to produce combined plot

# ============================================
# 2) Load data
# ============================================

# Vector of dataset suffixes
cities <- c("CPH", "HEL")
resolution <- c("250")
SVs <- c("1", "2", "3", "4", "5", "6", "7", "8")
land_use <- c("Forest", "Greenspace", "Other")

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
  names(sv_cols) <- c("1", "2", "3", "4", "5", "6", "7", "8")
  
  
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
# 5) GAM 1 - Total SV Count
# ============================================

# GAM 1: n_total ~ s(CC_mean)
for (city in cities) {
  for (x in resolution) {
    
    # current file 
    current <- get(paste0(city, "_", x, "m"))
    
    # GAM 1: Canopy Cover 
    gam_1 <- gam(n_total ~ s(CC_mean), family = nb(), method = "REML", data = current)
    
    # save GAM
    assign(
      paste0(city, "_", x, "m", "_", "GAM_1"),
      gam_1
    )
    
    
  }
}

# GAM 1: plot
{
  # Extract smooth estimates for both cities and bind together
  sm_CPH <- smooth_estimates(CPH_250m_GAM_1) %>% mutate(City = "Copenhagen")
  sm_HEL <- smooth_estimates(HEL_250m_GAM_1) %>% mutate(City = "Helsinki")
  
  # bind together
  bind_rows(sm_CPH, sm_HEL) %>%
    ggplot(aes(x = CC_mean, y = .estimate, colour = City, fill = City)) +
    geom_ribbon(aes(ymin = .estimate - 1.96 * .se, ymax = .estimate + 1.96 * .se), alpha = 0.15, colour = NA) +
    geom_line(linewidth = 0.9) +
    labs(
      x      = "Canopy Cover [%]",
      y      = "Partial effect (log scale)",
      colour = "City",
      fill   = "City"
    ) +
    theme_bw(base_size = 12) +
    theme(
      legend.position = c(0.2, 0.75),
      legend.justification = c(1, 0)
    )
  
  ggsave("GAM1_combined.png", width = 210, height = 120, units = "mm", dpi = 300)
}

# GAM 1: summary
summary(HEL_250m_GAM_1)

# remove GAMs and temprorary dfs
rm(list = ls()[grep("_GAM_1|^sm|^gam", ls())])

# ============================================
# 6) GAM 2 - Individual SV Count 
# ============================================

# GAM 2: n_svX ~ s(CC_mean)
for (city in cities) {
  
  # current file + resolution
  current <- get(paste0(city, "_", "250", "m"))
  
  for (y in SVs)  {
    
    # name of columns: n_svX with X being a number between 1 and 8
    sv <- paste0("n_sv", y)
    
    # Build formulas as strings, then convert with as.formula()
    f1 <- as.formula(paste(sv, "~ s(CC_mean)"))
    
    # GAM 1: Canopy Cover 
    gam_2 <- gam(f1, family = nb(), method = "REML", data = current)
    
    # save GAM
    assign(
      paste0(city, "_", "250", "m", "_", "GAM_2", "_", y),
      gam_2
    )
    
  }
}

# GAM 2: plot - adjust data entry in this section!
{
  # choose all GAMs to be compared
  gam_list <- mget(ls()[grep("CPH_250m_GAM_2", ls())]) # this has to be adjusted!
  
  # prepare plot
  smooth_df <- do.call(rbind, lapply(names(gam_list), function(name) {
    df <- smooth_estimates(gam_list[[name]])
    df$model <- name
    return(df)
  }))
  
  # extract SV number from model name (the last character)
  smooth_df$sv_num <- sub(".*GAM_2_", "", smooth_df$model)
  
  p2 <- ggplot(smooth_df, aes(x = CC_mean, y = .estimate, colour = sv_num, linetype = sv_num)) +
    geom_line() +
    #geom_ribbon(aes(ymin = .estimate - .se, ymax = .estimate + .se, fill = sv_num), 
     #          alpha = 0.05, colour = NA) +
     #scale_y_continuous(trans = "exp") +     # this transforms the estimate link-scale (= log scale) into a response scale (exp(log scale))
    scale_colour_manual(values = sv_cols, labels = sv_labels) +
    scale_fill_manual(values = sv_cols, labels = sv_labels) +
    scale_linetype_manual(values = c("1" = "solid", "2" = "solid", "3" = "dashed",
                                     "4" = "solid", "5" = "dashed", "6" = "dashed",
                                     "7" = "dotted", "8" = "dashed"),
                          labels = sv_labels) +
    labs(colour = "Social Value", fill = "Social Value", linetype = "Social Value",
         x = "Canopy Cover [%]", y = "Partial Effect (log scale)") +
    theme_bw()
  
  p1 + p2 + 
    plot_layout(guides = "collect",
                axis_titles = "collect") & 
    theme(legend.position = "bottom",
          legend.title = element_blank()
          )
  
  ggsave("GAM2_combined.png", width = 210, height = 120, units = "mm", dpi = 300)
  
}

# GAM 2: summary
summary(HEL_250m_GAM_2_1)

# remove GAMs and temporary dfs
rm(list = ls()[grep("_GAM_2|^smooth|^gam", ls())])


# ============================================
# 7) GAM 3 - Quintile based approach 
# - how does the SV ~ CC relationship behave in different land-covers? -
# ============================================

# subset data based on quintiles
for (city in cities) {
  for (use in land_use) {
    
    current <- get(paste0(city, "_", "250", "m"))
    current_use <- paste0(use, "_", "quintile")
    
    if (use == "Forest") {
      Final <- current |> filter(Forest_quintile == 5)
    } else if (use == "Greenspace"){
      Final <- current |> filter(Greenspace_quintile == 5)
    } else {
      Final <- current |> filter(Other_quintile == 5)
    }
    
    assign(
      paste0(city, "_", "250", "m", "_", use),
      Final
    )
    
  }
  
}

# GAM 3: n_total ~ s(CC_mean)
for (city in cities) {
  for (use in land_use) {
    
    # current file 
    current <- get(paste0(city, "_", "250", "m", "_", use))
    
    # GAM 1: Canopy Cover 
    gam_3 <- gam(n_total ~ s(CC_mean), family = nb(), method = "REML", data = current)
    
    # save GAM
    assign(
      paste0(city, "_", "250", "m", "_", "GAM_3", "_", use),
      gam_3
    )
    
  }
}

# GAM 3: plots
{
  cities <- c("HEL") # this one needs to be adapted to it only takes one city at a time to produce the plot
  
  for (city in cities) {
    
    plot_data <- list()
    
    for (use in land_use) {
      
      # get current GAM
      current_gam <- get(paste0(city, "_250m_GAM_3_", use))
      
      # extract partial effect of s(CC_mean)
      plot_obj <- plot(current_gam, select = 1, se = TRUE)
      
      partial_df <- data.frame(
        CC_mean  = plot_obj[[1]]$x,
        fit      = plot_obj[[1]]$fit,
        se       = plot_obj[[1]]$se,
        LandUse  = use
      )
      
      partial_df$lower <- partial_df$fit - 1.96 * partial_df$se
      partial_df$upper <- partial_df$fit + 1.96 * partial_df$se
      
      plot_data[[use]] <- partial_df
    }
    
    plot_df <- do.call(rbind, plot_data)
    
    p <- ggplot(plot_df, aes(x = CC_mean, y = fit, color = LandUse, fill = LandUse, alpha = LandUse)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
      geom_line(linewidth = 1) +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, color = NA) +
      scale_color_manual(values = c("Forest"     = "#1b7837",
                                    "Greenspace" = "#762a83",
                                    "Other"      = "#e08214")) +
      scale_fill_manual(values  = c("Forest"     = "#1b7837",
                                    "Greenspace" = "#762a83",
                                    "Other"      = "#e08214")) +
      scale_alpha_manual(values = c("Forest" = 0.5,
                                    "Greenspace" = 1,
                                    "Other" = 0.5)) +
      labs(
        x     = "Canopy Cover (%)",
        y     = "Partial Effect (log scale)",
        color = "Land Use",
        fill  = "Land Use",
        alpha = "Land Use"
      ) +
      theme_bw() +
      theme(legend.position = "bottom"
      )
    
    assign(paste0(city, "_GAM_1_plot"), p)
    print(p)
  }
  
  p1 <- p
  
  p1 + p2 + 
    plot_layout(guides = "collect",
                axis_titles = "collect") & 
    theme(legend.position = "bottom",
          legend.title = element_blank()
    )
  
  ggsave("GAM3_v1_combined_confidence.png", width = 210, height = 120, units = "mm", dpi = 300)
  
}

# GAM 3: summary
summary(CPH_250m_GAM_3_Other)

# GAM 3: support data structure
{
  analysis_table_LU <- data.frame(city = character(), land_cover = character(),
                                  cells = numeric(),
                                  total_sv = numeric(),
                                  sv_density = numeric(),
                                  canopy_mean = numeric(),
                                  CC_mean_b50 = numeric(),
                                  Other_avg = numeric(),
                                  GS_avg = numeric(),
                                  Forest_avg = numeric(),
                                  n_sv1 = numeric(), n_sv2 = numeric(), n_sv3 = numeric(), n_sv4 = numeric(), 
                                  n_sv5 = numeric(), n_sv6 = numeric(), n_sv7 = numeric(), n_sv8 = numeric()
  )
  
  # Data Structure Analysis Loop
  for (city in cities) {
    for (x in resolution) {
      for (use in land_use) {
        
        # current file + resolution
        current <- get(paste0(city, "_", x, "m", "_", use))
        
        # cell number
        cells_temp <- nrow(current)
        
        # land-cover
        land_cover_temp <- use
        
        # total social value count across all cells
        total_sv_temp <- current |> select(n_total) |> sum()
        
        # average social value density per grid cell
        sv_density_temp <- total_sv_temp / nrow(current)
        
        # mean of mean canopy cover
        canopy_mean_temp <- mean(current$CC_mean)
        
        # number of mean canopy values larger zero but smaller than 50
        CC_mean_b50_temp <- nrow(current |> filter(CC_mean > 0 & CC_mean < 50))
        
        # mean Other landcover per grid cell
        Other_avg_temp <- mean(current$Other)
        
        # mean Greenspace landcover per gird cell
        GS_avg_temp <- mean(current$Greenspace)
        
        # mean Forest landcover per grid cell
        Forest_avg_temp <- mean(current$Forest)
        
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
        analysis_table_LU <- rbind(analysis_table_LU, 
                                   data.frame(
                                     city = city, 
                                     cells = cells_temp,
                                     land_cover = land_cover_temp,
                                     total_sv = total_sv_temp,
                                     sv_density = sv_density_temp,
                                     canopy_mean = canopy_mean_temp,
                                     CC_mean_b50 = CC_mean_b50_temp,
                                     Other_avg = Other_avg_temp,
                                     GS_avg = GS_avg_temp,
                                     Forest_avg = Forest_avg_temp,
                                     n_sv1 = sv1_temp, n_sv2 = sv2_temp, n_sv3 = sv3_temp, n_sv4 = sv4_temp, 
                                     n_sv5 = sv5_temp, n_sv6 = sv6_temp, n_sv7 = sv7_temp, n_sv8 = sv8_temp
                                   )
        )
        
        
        # remove everything that is not needed
        rm(list = ls()[grep("_temp", ls())]) # this has to be adjusted!
      }
      
    }
    
  }
}

# GAM 3: support plots
{
  # library
  library(tidytext)
  
  # Helsinki
  {
    label_df <- analysis_table_LU |>
      filter(city == "HEL") |>
      select(land_cover, total_sv) |>
      group_by(land_cover) |>
      summarise(total = sum(total_sv))
    
    p1 <- analysis_table_LU |> 
      filter(city == "HEL") |>
      select(land_cover, total_sv, 11:18) |>
      group_by(land_cover) |> 
      pivot_longer(cols = -c(land_cover, total_sv),    # fix: use c() for multiple exclusions
                   names_to = "variable", 
                   values_to = "count") |>
      mutate(variable = reorder_within(variable, -count, land_cover),
             sv_key = str_extract(as.character(variable), "\\d+")) |>
      ungroup() |>
      ggplot() +
      geom_col(aes(x = variable, y = count, fill = sv_key)) +
      scale_fill_manual(values = sv_cols, guide = "none") +
      scale_x_reordered(labels = function(x) sv_labels[str_extract(x, "\\d+")]) +
      labs(x = NULL, y = "Count") +
      geom_label(data = label_df, aes(x = Inf, y = Inf, label = paste("Total SVs =", total)),
                 hjust = 1.1, vjust = 1.5, inherit.aes = FALSE) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      facet_grid(~land_cover, scales = "free_x")                     # free_x needed
    
    p1 
  }
  
  # Copenhagen
  {
    label_df <- analysis_table_LU |>
      filter(city == "CPH") |>
      select(land_cover, total_sv) |>
      group_by(land_cover) |>
      summarise(total = sum(total_sv))
    
    p2 <- analysis_table_LU |> 
      filter(city == "CPH") |>
      select(land_cover, total_sv, 11:18) |>
      group_by(land_cover) |> 
      pivot_longer(cols = -c(land_cover, total_sv),    # fix: use c() for multiple exclusions
                   names_to = "variable", 
                   values_to = "count") |>
      mutate(variable = reorder_within(variable, -count, land_cover),
             sv_key = str_extract(as.character(variable), "\\d+")) |>
      ungroup() |>
      ggplot() +
      geom_col(aes(x = variable, y = count, fill = sv_key)) +
      scale_fill_manual(values = sv_cols, guide = "none") +
      scale_x_reordered(labels = function(x) sv_labels[str_extract(x, "\\d+")]) +
      labs(x = NULL, y = "Count") +
      geom_label(data = label_df, aes(x = Inf, y = Inf, label = paste("Total SVs =", total)),
                 hjust = 1.1, vjust = 1.5, inherit.aes = FALSE) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      facet_grid(~land_cover, scales = "free_x")                     # free_x needed
    
    p2
  }
  
  # patch them together
  p1 / p2
  
  # save plot
  ggsave("GAM3_Supplementary.png", width = 210, height = 210, units = "mm", dpi = 300)
  
}

# remove GAMs and temporary dfs
rm(list = ls()[grep("_GAM_3|^current|^plot|^Final|^partial|^gam", ls())])


# ============================================
# 8) GAM 4 - Land-cover based interaction: marginal canopy effect
# - how does the SV ~ CC relationship change with an increase of Forest/Greenspace compared to Other -
# ============================================

# GAM 4: n_total ~ s(CC_mean) + ti(CC_mean, Forest) + ti(CC_mean, Greenspace)
for (city in cities) {
  for (x in resolution) {
    
    # current file 
    current <- get(paste0(city, "_", x, "m"))
    
    # GAM 4: Canopy Cover + interaction terms 
    gam_4 <- gam(n_total ~ s(CC_mean) + ti(CC_mean, Forest) + ti(CC_mean, Greenspace), family = nb(), method = "REML", data = current)
    
    # save GAM
    assign(
      paste0(city, "_", "250", "m", "_", "GAM_4"),
      gam_4
    )
    
  }
}

# GAM 4: Plot
{
  vis.gam(HEL_250m_GAM_4, # change to HEL_ or CPH_
          view = c("CC_mean", "Greenspace"), # change to "Forest" or "Greenspace"
          plot.type = "contour") # change to "contour" for 2D or "persp" for 3D
  
}

# GAM 4: summary
summary(HEL_250m_GAM_4)

# remove GAMs and temporary dfs
rm(list = ls()[grep("^current|^plot|^gam", ls())])



# ============================================
# 9) GAM 5 - Land-cover based interaction: pure interaction effect 
# - how does the SV ~ CC relationship change with an increase of Forest/Greenspace compared to Other -
# ============================================

# GAM 5: n_total ~ ti(CC_mean) + ti(Forest) + ti(Greenspace) + ti(CC_mean, Forest) + ti(CC_mean, Greenspace)
for (city in cities) {
  for (x in resolution) {
    
    # current file 
    current <- get(paste0(city, "_", x, "m"))
    
    # GAM 4: Canopy Cover + interaction terms 
    gam_5 <- gam(n_total ~ ti(CC_mean) + ti(Forest) + ti(Greenspace) + 
                   ti(CC_mean, Forest) + ti(CC_mean, Greenspace),
                 family = nb(), method = "REML", data = current)
    
    # save GAM
    assign(
      paste0(city, "_", "250", "m", "_", "GAM_5"),
      gam_5
    )
    
  }
}

# GAM 5: Plot
{
  vis.gam(CPH_250m_GAM_5, # change to HEL_ or CPH_
          view = c("CC_mean", "Greenspace"), # change to "Forest" or "Greenspace"
          plot.type = "contour") # change to "contour" for 2D or "persp" for 3D
  
  # Main effects
  draw(CPH_250m_GAM_5, select = smooths(CPH_250m_GAM_5)[1:3])
  
  # Interactions
  draw(CPH_250m_GAM_5, select = smooths(CPH_250m_GAM_5)[4:5])
  
}

# GAM 5: summary
summary(CPH_250m_GAM_5)


# ============================================
# 10) GAM 6 - Land-cover based interaction: shifts in land-cover 
# - how does the SV ~ CC relationship change when shifting from one land-cover to another? -
# ============================================

# GAM 6: Create comparison columns - NAs if both land-covers are 0
for (city in cities) {
  for (x in resolution) {
    
    # current file
    current <- get(paste0(city, "_", x, "m"))
    
    # comparison columns
    current$For_vs_Oth <- current$Forest / (current$Forest + current$Other)
    current$Gsp_vs_Oth <- current$Greenspace / (current$Greenspace + current$Other)
    current$For_vs_Gsp <- current$Forest / (current$Forest + current$Greenspace)
    
    # re-assign
    assign(paste0(city, "_", x, "m"),
           current)
    
    }

}

HEL_250m$For_vs_Oth <- HEL_250m$Forest / ((HEL_250m$Forest + HEL_250m$Other))
HEL_250m$Gsp_vs_Oth <- HEL_250m$Greenspace / ((HEL_250m$Greenspace + HEL_250m$Other))
HEL_250m$For_vs_Gsp <- HEL_250m$Forest / ((HEL_250m$Forest + HEL_250m$Greenspace))

CPH_250m$For_vs_Oth <- CPH_250m$Forest / ((CPH_250m$Forest + CPH_250m$Other))
CPH_250m$Gsp_vs_Oth <- CPH_250m$Greenspace / ((CPH_250m$Greenspace + CPH_250m$Other))
CPH_250m$For_vs_Gsp <- CPH_250m$Forest / ((CPH_250m$Forest + CPH_250m$Greenspace))

# GAM 6: Forest vs Other
for (city in cities) {
  for (x in resolution) {
    
    # current file 
    current <- get(paste0(city, "_", x, "m"))
    
    # GAM 6: Canopy Cover + interaction terms 
    gam_6 <- gam(n_total ~ ti(CC_mean) + ti(For_vs_Oth) + ti(CC_mean, For_vs_Oth),
                 family = nb(), method = "REML", data = current)
    
    # save GAM
    assign(
      paste0(city, "_", "250", "m", "_", "GAM_6_FORvsOTH"),
      gam_6
    )
    
  }
}

# GAM 6: Forest vs Greenspace
for (city in cities) {
  for (x in resolution) {
    
    # current file 
    current <- get(paste0(city, "_", x, "m"))
    
    # GAM 6: Canopy Cover + interaction terms 
    gam_6 <- gam(n_total ~ ti(CC_mean) + ti(For_vs_Gsp) + ti(CC_mean, For_vs_Gsp),
                 family = nb(), method = "REML", data = current)
    
    # save GAM
    assign(
      paste0(city, "_", "250", "m", "_", "GAM_6_FORvsGSP"),
      gam_6
    )
    
  }
}

# GAM 6: Greenspace vs Other
for (city in cities) {
  for (x in resolution) {
    
    # current file 
    current <- get(paste0(city, "_", x, "m"))
    
    # GAM 6: Canopy Cover + interaction terms 
    gam_6 <- gam(n_total ~ ti(CC_mean) + ti(Gsp_vs_Oth) + ti(CC_mean, Gsp_vs_Oth),
                 family = nb(), method = "REML", data = current)
    
    # save GAM
    assign(
      paste0(city, "_", "250", "m", "_", "GAM_6_GSPvsOTH"),
      gam_6
    )
    
  }
}

# GAM 6: Plot
{
  vis.gam(HEL_250m_GAM_6, # change to HEL_ or CPH_
          view = c("CC_mean", "For_vs_Oth"), # change to "Forest" or "Greenspace"
          plot.type = "persp") # change to "contour" for 2D or "persp" for 3D
  
  # Main effects
  draw(HEL_250m_GAM_6_FORvsGS, select = smooths(HEL_250m_GAM_6_FORvsGS)[1:2])
  
  # Interactions
  draw(HEL_250m_GAM_6_FORvsOTH, select = smooths(HEL_250m_GAM_6_FORvsOTH)[3])
  draw(HEL_250m_GAM_6_FORvsGSP, select = smooths(HEL_250m_GAM_6_FORvsGSP)[3])
  draw(HEL_250m_GAM_6_GSPvsOTH, select = smooths(HEL_250m_GAM_6_GSPvsOTH)[3])
  
}

# GAM 6: summary
summary(CPH_250m_GAM_5)


# ============================================
# 11) Supporting Plots
# ============================================

# libraries
library(ggplot2)
library(patchwork)

# Histograms of canopy cover
{
  p1 <- HEL_250m |> 
    ggplot() +
    geom_histogram(aes(x = CC_mean), binwidth = 5, fill = "#6B7C5C", boundary = 0) +
    scale_x_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) +
    scale_y_continuous(breaks = seq(0, 1300, by = 100), limits = c(0, 1300)) +
    labs(x = "Mean Canopy Cover per cell [%]",
         y = "Number of cells") +
    annotate("label", x = Inf, y = Inf, label = "Total grid cells = 4,054",
             hjust = 1.1, vjust = 1.5) +
    theme_classic()
  
  p2 <- CPH_250m |> 
    ggplot() +
    geom_histogram(aes(x = CC_mean), binwidth = 5, fill = "#6B7C5C", boundary = 0) +
    scale_x_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) +
    scale_y_continuous(breaks = seq(0, 1300, by = 100), limits = c(0, 1300)) +
    labs(x = "Mean Canopy Cover per cell [%]",
         y = "Number of cells") +
    annotate("label", x = Inf, y = Inf, label = "Total grid cells = 1,753",
             hjust = 1.1, vjust = 1.5) +
    theme_classic()
  
  p1 + p2 + 
    plot_layout(guides = "collect",
                axis_titles = "collect")
  
  ggsave("Canoyp_Covers.png", width = 210, height = 120, units = "mm", dpi = 300)
}

# Histograms of total social values
{
  p1 <- HEL_250m |> 
    ggplot() +
    geom_histogram(aes(x = n_total), binwidth = 5, fill = "black", boundary = 0) +
    scale_x_continuous(breaks = seq(0, 220, by = 40), limits = c(0, 220)) +
    scale_y_continuous(breaks = seq(0, 3500, by = 500), limits = c(0, 3500)) +
    labs(x = "Social Values per cell [n]",
         y = "Number of cells") +
    annotate("label", x = Inf, y = Inf, label = "Total grid cells = 4,054",
             hjust = 1.1, vjust = 1.5) +
    theme_classic()
  
  p2 <- CPH_250m |> 
    ggplot() +
    geom_histogram(aes(x = n_total), binwidth = 5, fill = "black", boundary = 0) +
    scale_x_continuous(breaks = seq(0, 220, by = 40), limits = c(0, 220)) +
    scale_y_continuous(breaks = seq(0, 1600, by = 200), limits = c(0, 1600)) +
    labs(x = "Social Values per cell [n]",
         y = "Number of cells") +
    annotate("label", x = Inf, y = Inf, label = "Total grid cells = 1,753",
             hjust = 1.1, vjust = 1.5) +
    theme_classic()
  
  p1 + p2 + 
    plot_layout(guides = "collect",
                axis_titles = "collect")
  
  ggsave("Social_value_distribution.png", width = 210, height = 120, units = "mm", dpi = 300)
}

# Histograms of total social values - zoomed
{
  p1 <- HEL_250m |> 
    ggplot() +
    geom_histogram(aes(x = n_total), binwidth = 5, fill = "black", boundary = 0) +
    scale_x_continuous(breaks = seq(0, 200, by = 20), limits = c(0, 200)) +
    scale_y_continuous(breaks = seq(0, 3500, by = 500), limits = c(0, 3500)) +
    labs(x = "Social Values per cell [n]",
         y = "Number of cells") +
    annotate("label", x = Inf, y = Inf, label = "Total grid cells = 4,054",
             hjust = 1.1, vjust = 1.5) +
    theme_classic()
  
  p2 <- CPH_250m |> 
    ggplot() +
    geom_histogram(aes(x = n_total), binwidth = 5, fill = "black", boundary = 0) +
    scale_x_continuous(breaks = seq(0, 200, by = 20), limits = c(0, 200)) +
    scale_y_continuous(breaks = seq(0, 1600, by = 200), limits = c(0, 1600)) +
    labs(x = "Social Values per cell [n]",
         y = "Number of cells") +
    annotate("label", x = Inf, y = Inf, label = "Total grid cells = 1,753",
             hjust = 1.1, vjust = 1.5) +
    theme_classic()
  
  p1 + p2
  
  ggsave("Social_Values_Distribution_zoomed.png", width = 210, height = 120, units = "mm", dpi = 300)
}

# Barplots of social value counts
{
  #ordered
  p1 <- analysis_table |> filter(city == "HEL") |> #adjust this to either HEL or CPH
    select(c(20:27)) |> 
    pivot_longer(everything(), names_to = "variable", values_to = "count") |>
    mutate(sv_num = as.character(row_number()),
           sv_num = fct_reorder(sv_num, count, .desc = TRUE)) |>
    ggplot() +
    geom_col(aes(x = sv_num, y = count, fill = sv_num)) +
    scale_fill_manual(values = sv_cols, guide = "none") +
    scale_x_discrete(labels = sv_labels) +
    labs(x = NULL, y = "Count") +
    annotate("label", x = Inf, y = Inf, label = "Total social values = 16,297",
             hjust = 1.1, vjust = 1.5) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  p2 <- analysis_table |> filter(city == "CPH") |> #adjust this to either HEL or CPH
    select(c(20:27)) |> 
    pivot_longer(everything(), names_to = "variable", values_to = "count") |>
    mutate(sv_num = as.character(row_number()),
           sv_num = fct_reorder(sv_num, count, .desc = TRUE)) |>
    ggplot() +
    geom_col(aes(x = sv_num, y = count, fill = sv_num)) +
    scale_fill_manual(values = sv_cols, guide = "none") +
    scale_x_discrete(labels = sv_labels) +
    labs(x = NULL, y = "Count") +
    annotate("label", x = Inf, y = Inf, label = "Total social values = 5,764", # adjust this number to HEL: 16297 and CPH: 5764  
             hjust = 1.1, vjust = 1.5) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  p1 + p2 + 
    plot_layout(axis_titles = "collect")
  
  ggsave("SV_Bars.png", width = 210, height = 120, units = "mm", dpi = 300)
}

