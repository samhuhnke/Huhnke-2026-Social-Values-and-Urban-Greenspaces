# ============================================ 
# MT_05_Nonspatial_Analysis
# ============================================

# AUTHOR: Sam Huhnke, M.Sc. University of Helsinki
#
#
# This code provides non-spatial analyses of relations between mapped social values and tree canopy cover for
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
library(ggeffects) # to visualize results of multinomial logistic regression

library(rstatix) # used to assess effect sizes
library(nnet) # used for multinomial logistic regression

# ============================================
# 2) Load data
# ============================================

# Helsinki
HEL <- read.csv("CSVs/Helsinki_Final.csv", sep = ",")

# Copenhagen
CPH <- read.csv("CSVs/Copenhagen_Final.csv", sep = ",")


# ============================================
# 3) Create Subsets
# ============================================

# Helsinki
HEL_Forest <- HEL |> filter(type_2018 == "Forest")
HEL_Greenspace <- HEL |> filter(type_2018 == "Greenspace")
HEL_Other <- HEL |> filter(type_2018 == "Other")

# Copenhagen
CPH_Forest <- CPH |> filter(type_2018 == "Forest")
CPH_Greenspace <- CPH |> filter(type_2018 == "Greenspace")
CPH_Other <- CPH |> filter(type_2018 == "Other")


# ============================================
# 4) Analysis Preparation
# ============================================

# Assign colors
{
  # Land Use category colors
  category_cols <- c("#bf0000", "#959595", "#734d37", "#8cdc00", "#ffffa8", "#008c00", "#a6a6ff", "#80f2e6","#ccffcc")
  
  # Land Use type colors
  type_cols <- c("#008c00", "#8cdc00", "#959595" )
  
  # Social Value colors
  sv_cols <- c("#d45680", "#4d9955", "#a0afa0", "#009acb", "#006695", "#e69c24", "#ffeccc", "#8b7356")
  
}

# Assign labels
{
  # Land Use categories
  lu_labels <- c(
    "1" = "1: Urban fabric",
    "2" = "2: Industrial, Commercial, Public,\n Military, Private and Transport Units",
    "3" = "3: Mine, dump, construction",
    "4" = "4: Artificial green spaces",
    "5" = "5: Agricultural land",
    "6" = "6: Semi-/Natural green spaces",
    "7" = "7: Wetlands",
    "8" = "8: Waterbodies",
    "9" = "9: Natural open spaces"
  )
  
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
# 5) Exploratory Analysis (Point Counts & densities, histograms)
# ============================================

# Number of respondents per survey
{
  summary_table <- tibble(
    City = c("Helsinki", "Copenhagen"),
    # Number of total individual repsondents
    Total_Respond = c(
      HEL |> distinct(Respondent) |> nrow(),
      CPH |> distinct(Respondent) |> nrow()
    ),
    Mapped_SVs = c(
      HEL |> nrow(),
      CPH |> nrow()
    )
  )
  
  # View it
  summary_table
}

# Area sizes
{
  HEL |> select(type_area_km2) |> distinct() |> sum()
  CPH |> select(type_area_km2) |> distinct() |> sum()
}

# Point count & point density per type (i.e. forest, greenspace, other)
{
  HEL |> group_by(type_2018) |> count() |> 
    left_join(HEL |> select(type_2018, type_area_km2, type_area_per) |> unique(), by = "type_2018") |> 
    mutate(point_density_km2 = n/type_area_km2)
  
  CPH |> group_by(type_2018) |> count() |> 
    left_join(CPH |> select(type_2018, type_area_km2, type_area_per) |> unique(), by = "type_2018") |> 
    mutate(point_density_km2 = n/type_area_km2)
}

# Point count & point density by type and social value
{
  HEL |> group_by(type_2018, SV_new) |> count() |> 
    left_join(HEL |> select(type_2018, type_area_km2) |> unique(), by = "type_2018") |> 
    mutate(point_density_km2 = n/type_area_km2) |> 
    print(n = 25)
  
  CPH |> group_by(type_2018, SV_new) |> count() |> 
    left_join(CPH |> select(type_2018, type_area_km2) |> unique(), by = "type_2018") |> 
    mutate(point_density_km2 = n/type_area_km2) |> 
    print(n = 25)
}

# How is canopy cover distributed overall? 
{
  HEL |> ggplot(aes(x = CanopyCover_mean)) +
    geom_histogram()
  
  CPH |> ggplot(aes(x = CanopyCover_mean)) +
    geom_histogram()
}

# How is canopy cover distributed by land-use type?
{
  ## Helsinki all
  ggplot(HEL, aes(x = CanopyCover_mean, fill = as.factor(type_2018))) +
    geom_histogram(
      position = "identity",
      alpha = 0.7,
      bins = 30
    ) +
    scale_fill_manual(values = type_cols) +
    facet_wrap(~ type_2018)
  
  ## Copenhagen all
  ggplot(CPH, aes(x = CanopyCover_mean, fill = as.factor(type_2018))) +
    geom_histogram(
      position = "identity",
      alpha = 0.7,
      bins = 30
    ) +
    scale_fill_manual(values = type_cols) +
    facet_wrap(~ type_2018)
}

# Histograms by type and social value 
{
  # Helsinki
  HEL |> filter(type_2018 == "Forest") |> 
    ggplot(aes(x = CanopyCover_mean, fill = as.factor(SV_new))) + 
    geom_histogram(bins = 30) +
    labs(title = "Helsinki Forest") +
    scale_fill_manual(values = sv_cols,
                      labels = sv_labels,
                      name = "Social Values") +
    facet_wrap(~ SV_new)
  
  HEL |> filter(type_2018 == "Greenspace") |> 
    ggplot(aes(x = CanopyCover_mean, fill = as.factor(SV_new))) + 
    geom_histogram(bins = 30) +
    labs(title = "Helsinki Greenspace") +
    scale_fill_manual(values = sv_cols,
                      labels = sv_labels,
                      name = "Social Values") +
    facet_wrap(~ SV_new)
  
  HEL |> filter(type_2018 == "Other") |> 
    ggplot(aes(x = CanopyCover_mean, fill = as.factor(SV_new))) + 
    geom_histogram(bins = 30) +
    labs(title = "Helsinki Other") +
    scale_fill_manual(values = sv_cols,
                      labels = sv_labels,
                      name = "Social Values") +
    facet_wrap(~ SV_new)
  
  
  # Copenhagen
  CPH |> filter(type_2018 == "Forest") |> 
    ggplot(aes(x = CanopyCover_mean, fill = as.factor(SV_new))) + 
    geom_histogram(bins = 30) +
    labs(title = "Copenhagen Forest") +
    scale_fill_manual(values = sv_cols,
                      labels = sv_labels,
                      name = "Social Values") +
    facet_wrap(~ SV_new)
  
  CPH |> filter(type_2018 == "Greenspace") |> 
    ggplot(aes(x = CanopyCover_mean, fill = as.factor(SV_new))) + 
    geom_histogram(bins = 30) +
    labs(title = "Copenhagen Greenspace") +
    scale_fill_manual(values = sv_cols,
                      labels = sv_labels,
                      name = "Social Values") +
    facet_wrap(~ SV_new)
  
  CPH |> filter(type_2018 == "Other") |> 
    ggplot(aes(x = CanopyCover_mean, fill = as.factor(SV_new))) + 
    geom_histogram(bins = 30) +
    labs(title = "Copenhagen Other") +
    scale_fill_manual(values = sv_cols,
                      labels = sv_labels,
                      name = "Social Values") +
    facet_wrap(~ SV_new)
  
}

# Pie Charts for type-based land-use
{
  # load library
  library(patchwork)
  
  # extract category and area percent
  c1 <- CPH |> select(type_2018, type_area_per) |> unique()
  h1 <- HEL |> select(type_2018, type_area_per) |> unique()
  
  
  # Copenhagen
  p1 <- ggplot(c1, aes(x = "", y = type_area_per, fill = as.factor(type_2018))) +
    geom_col(width = 1, color = "black", linewidth = 0.4) +
    coord_polar("y") +
    theme_void() +
    labs(title = "Copenhagen LU",
         subtitle = "non-buffered boundaries data") +
    scale_fill_manual(
      values = type_cols,
      labels = type_labels,
      name = "Land use"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 8))
  
  # helsinki
  p2 <- ggplot(h1, aes(x = "", y = type_area_per, fill = as.factor(type_2018))) +
    geom_col(width = 1, color = "black", linewidth = 0.4) +
    coord_polar("y") +
    theme_void() +
    labs(title = "Helsinki LU",
         subtitle = "non-buffered boundaries data") +
    scale_fill_manual(
      values = type_cols,
      labels = type_labels,
      name = "Land use"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 8))
  
  # patchwork
  p1 + p2 + plot_layout(guides = "collect") &
    theme(legend.position = "right")
  
}

# Pie Charts for type-based point counts
{
  # load library
  library(patchwork)
  
  # create point count data
  HEL_points <- HEL |> group_by(type_2018) |> count() |> 
    left_join(HEL |> select(type_2018, type_area_km2, type_area_per) |> unique(), by = "type_2018") |> 
    mutate(point_density_km2 = n/type_area_km2)
  
  CPH_points <- CPH |> group_by(type_2018) |> count() |> 
    left_join(CPH |> select(type_2018, type_area_km2, type_area_per) |> unique(), by = "type_2018") |> 
    mutate(point_density_km2 = n/type_area_km2)
  
  # extract category and area percent
  h1 <- HEL_points |> select(type_2018, n) |> unique()
  c1 <- CPH_points |> select(type_2018, n) |> unique()
  
  # helsinki
  p1 <- ggplot(c1, aes(x = "", y = n, fill = as.factor(type_2018))) +
    geom_col(width = 1, color = "black", linewidth = 0.4) +
    coord_polar("y") +
    theme_void() +
    labs(title = "Copenhagen Point Counts",
         subtitle = "Absolute counts by land-use") +
    scale_fill_manual(
      values = type_cols,
      labels = type_labels,
      name = "Land use"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 8))
  
  # Copenhagen
  p2 <- ggplot(h1, aes(x = "", y = n, fill = as.factor(type_2018))) +
    geom_col(width = 1, color = "black", linewidth = 0.4) +
    coord_polar("y") +
    theme_void() +
    labs(title = "Helsinki Point Counts",
         subtitle = "Absolute counts by land-use") +
    scale_fill_manual(
      values = type_cols,
      labels = type_labels,
      name = "Land use"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 8))
  
  # patchwork
  p1 + p2 + plot_layout(guides = "collect") &
    theme(legend.position = "right")
}


# ============================================
# 6) Non-spatial analysis 
# ============================================

#### Helsinki

# Q1: Do mapped SV in general co-occur with canopy cover?
{
  # S1) - S7) with the inclusion of bin 2.5 (which includes 0s)
  {
    # S1) binned data
    HEL_binned <- HEL |> 
      mutate(
        canopy_bin = cut(
          CanopyCover_mean,
          breaks = seq(0, 100, by = 5),
          include.lowest = TRUE,
          labels = seq(2.5, 97.5, by = 5)
        )
      )
    
    # S2) counts of mapped points for each bin
    canopy_counts <- HEL_binned |> 
      count(canopy_bin) |> 
      mutate(canopy_mid = as.numeric(as.character(canopy_bin)))
    
    # S3) plot showing the relation
    ggplot(canopy_counts, aes(x = canopy_mid, y = n)) +
      geom_point() +
      geom_smooth(method = "loess", se = TRUE) +
      labs(
        x = "Canopy cover (%)",
        y = "Number of mapped social values",
        title = "Helsinki: Global frequency of mapped social values along canopy cover gradient"
      )
    
    # S4) Poisson Regression - Assumes: Var(n) = Mean(n)
    pois_mod <- glm(
      n ~ canopy_mid,
      family = poisson(link = "log"),
      data = canopy_counts
    )
    summary(pois_mod)
    
    exp(0.00164) # A 1% increase in canopy cover would lead to a 0.16% increase in counts of social values
    
    # S5) Check for dispersion
    dispersion <- sum(residuals(pois_mod, type = "pearson")^2) /
      df.residual(pois_mod)
    
    dispersion # dispersion 283.085 >> 2, so switch to negative binomial!
    
    # S6) Negative Binomial
    nb_mod <- glm.nb(n ~ canopy_mid, data = canopy_counts)
    summary(nb_mod)
    
    exp(0.001379) # A 1% increase in canopy cover would lead to a 0.14% increase in counts of social values, BUT p = 0.777, i.e. non-significant
    
    # S7) Generalized Additive Model (GAM)
    gam_mod <- gam(n ~ s(canopy_mid), family = nb(), data = canopy_counts)
    summary(gam_mod) #edf = 2.816 --> U-shaped
    
    plot(gam_mod, rug = TRUE)
    
    # replaces plot(gam_mod, ...)
    draw(gam_mod, rug = TRUE) &
      theme_minimal() &
      labs(title = "Helsinki Global GAM")
    
  }
  
  # S1) - S7) without bin 2.5 (which includes 0s)
  {
    # S1) binned data
    HEL_binned <- HEL |> 
      mutate(
        canopy_bin = cut(
          CanopyCover_mean,
          breaks = seq(0, 100, by = 5),
          include.lowest = TRUE,
          labels = seq(2.5, 97.5, by = 5)
        )
      )
    
    # S2) counts of mapped points for each 5% bin
    canopy_counts <- HEL_binned |> 
      count(canopy_bin) |> 
      mutate(canopy_mid = as.numeric(as.character(canopy_bin)))
    
    canopy_counts <- canopy_counts |> filter(canopy_bin != "2.5")
    
    # S3) plot showing the relation
    ggplot(canopy_counts, aes(x = canopy_mid, y = n)) +
      geom_point() +
      geom_smooth(method = "loess", se = TRUE) +
      labs(
        x = "Canopy cover (%)",
        y = "Number of mapped social values",
        title = "Frequency of mapped social values along canopy cover gradient"
      )
    
    # S4) Poisson Regression - Assumes: Var(n) = Mean(n)
    pois_mod <- glm(
      n ~ canopy_mid,
      family = poisson(link = "log"),
      data = canopy_counts
    )
    
    summary(pois_mod)
    
    # S5) Check for dispersion
    dispersion <- sum(residuals(pois_mod, type = "pearson")^2) /
      df.residual(pois_mod)
    dispersion # dispersion high so switch to negative binomial instead
    
    # S6) Negative Binomial
    nb_mod <- glm.nb(n ~ canopy_mid, data = canopy_counts)
    summary(nb_mod)
    
    # S7) Generalized Additive Model (GAM)
    gam_mod <- gam(n ~ s(canopy_mid), family = nb(), data = canopy_counts)
    summary(gam_mod)
  }
}

# Q2: Do mapped social values co-occur with canopy cover within land-use types? (LU types are forest, greenspace, and other)
{
  # S1) Binned data
  HEL_binned <- HEL |> 
    mutate(
      canopy_bin = cut(
        CanopyCover_mean,
        breaks = seq(0, 100, by = 5),
        include.lowest = TRUE,
        labels = seq(2.5, 97.5, by = 5)
      )
    )
  
  # S2) canopy counts by land-use
  canopy_counts_lu <- HEL_binned |> 
    group_by(type_2018, canopy_bin) |> 
    summarise(n = n(), .groups = "drop") |> 
    mutate(canopy_mid = as.numeric(as.character(canopy_bin)))
  
  # S3) GLMS - Question: Does land-use have a significant impact on the relation?
  
  # interaction model
  nb_lu_mod <- glm.nb(
    n ~ canopy_mid * type_2018,
    data = canopy_counts_lu
  )
  
  # no interaction model
  nb_no_interaction <- glm.nb(
    n ~ canopy_mid + type_2018,
    data = canopy_counts_lu
  )
  
  # ANOVA to compare both
  # NOTE: no interaction model still doesn't converge
  # NOTE: Hence in S4) this analysis will be repeated but the canopy counts are centered first
  anova(nb_no_interaction, nb_lu_mod, test = "Chisq") 
  
  # S4) center canopy counts + re-run models
  canopy_counts_lu <- canopy_counts_lu |> 
    mutate(canopy_c = scale(canopy_mid, center = TRUE, scale = FALSE))
  
  # interaction model
  nb_lu_mod <- glm.nb(
    n ~ canopy_mid * type_2018,
    data = canopy_counts_lu
  )
  
  summary(nb_lu_mod)
  
  # no interaction model
  nb_no_interaction <- glm.nb(
    n ~ canopy_mid + type_2018,
    data = canopy_counts_lu
  )
  
  # ANOVA to compare both
  # NOTE: no interaction model still doesn't converge
  # RESULTS: interaction significantly improves model perfomance (LR = 90.94), df = 2 (for greenspace and other, with forest as baseline), P = 0
  # RESULTS: Land-uses differ relationship between social value count and canopy cover
  anova(nb_no_interaction, nb_lu_mod, test = "Chisq")
  
  # S4.2) Alternative: GAM
  canopy_counts_lu$type_2018 <- as.factor(canopy_counts_lu$type_2018)
  
  gam_lu <- gam(
    n ~ s(canopy_mid, by = type_2018) + type_2018,
    family = nb(),
    data = canopy_counts_lu,
    method = "REML"
  )
  
  summary(gam_lu)
  
  # S5) Results of negative binomial GLM for each land-use
  # RESULTS: For forests, for each 1% increase in canopy cover, we see a 3% increase in social value counts
  # RESULTS: For greenspaces, for each 1% increase in canopy cover, we see a -1% decrease in social value counts
  # RESULTS: For other, for each 1% increase in canopy cover, we see a -4% decrease in social value counts
  by(canopy_counts_lu, canopy_counts_lu$type_2018, function(df) {
    summary(glm.nb(n ~ canopy_c, data = df))
  })
  exp(0.034582)
  -(1 - exp(-0.01031))
  -(1 - exp(-0.04206))
  
  
  # S6) Visualization
  ggplot(canopy_counts_lu,
         aes(x = canopy_mid, y = n, color = type_2018)) +
    geom_point() +
    geom_smooth(method = "glm.nb", se = TRUE) +
    scale_color_manual(values = type_cols,
                       labels = type_labels) +
    labs(
      x = "Canopy cover (%)",
      y = "Number of mapped social values",
      color = "Land-use",
      title = "Helsinki: Mapped social values by land-use"
    ) +
    theme_minimal()
  
  plot(gam_lu, pages = 1, rug = TRUE)
  
  # replaces plot(gam_mod, ...)
  draw(gam_lu, rug = TRUE) &
    theme_minimal() &
    labs(title = "Helsinki Land Cover GAM")
}

# Q3: Are there differences between social values?
{
  # S1) Global tests
  {
    kruskal.test(CanopyCover_mean ~ SV_new, data = HEL)
    
    kruskal_effsize(CanopyCover_mean ~ SV_new, data = HEL)
    
    # Dunn's Test --> uses same ranks as kruskal-wallis
    d_test <- dunn_test(HEL, CanopyCover_mean ~ SV_new, p.adjust.method = "bonferroni") # with bonferroni correction
    print(d_test, n = 30)
    
    # Pairwise Wilcoxon rank-sum test (or Mann-Withney-Wilcoxon) -> uses different ranks as Kruskal-Wallis
    pairwise.wilcox.test(HEL$CanopyCover_mean, HEL$SV_new,  p.adjust.method = "bonferroni") # with bonferroni correction
    
    # Boxplot simple
    boxplot(CanopyCover_mean ~ SV_new, data = HEL,
            xlab = "Social value category",
            ylab = "Canopy cover (%)",
            main = "Helsinki",
            col = sv_cols)
    
    # Compact Letter Display
    {
      library(multcompView)
      
      # Prepare p-values for the CLD function
      # NOTE: needs a named vector of adjusted p-values
      p_adj <- d_test$p.adj
      names(p_adj) <- paste0(d_test$group1, "-", d_test$group2)
      
      # Generate the letters
      cld <- multcompLetters(p_adj)
      cld_df <- data.frame(SV_new = names(cld$Letters), 
                           letters = cld$Letters)
      
      # CLD Plot
      ggplot(HEL, aes(x = factor(SV_new), y = CanopyCover_mean, fill = factor(SV_new))) + 
        geom_boxplot(outlier.alpha = 0.2) +
        theme_minimal() +
        # Add letters above boxes
        geom_text(data = cld_df, aes(x = SV_new, y = 105, label = letters), 
                  vjust = 0, size = 5, fontface = "bold", 
                  inherit.aes = FALSE) + # Critical to prevent looking for 'fill' in cld_df
        scale_fill_manual(values = sv_cols,
                          labels = sv_labels,
                          name = "Social Values") + # Legend title
        labs(title = "Helsinki: Canopy Cover by Social Value Category",
             subtitle = "GLOBAL DATA. Groups sharing a letter are not significantly different (Dunn's test with Bonferroni correction)",
             x = "Social Value Category",
             y = "Canopy Cover (%)")
    }
    
  }
  
  # S2) By Land-Use
  {
    # Forest
    {
      # Step 1) Exploratory Box-Plot
      boxplot(CanopyCover_mean ~ SV_new, data = HEL |> filter(type_2018 == "Forest"),
              xlab = "Social value category",
              ylab = "Mean canopy cover (%)",
              col = sv_cols)
      
      # Step 2) Kruskal-Wallis
      kruskal.test(CanopyCover_mean ~ factor(SV_new), data = HEL_Forest)
      
      # Step 3) Kruskal effect size
      kruskal_effsize(CanopyCover_mean ~ factor(SV_new), data = HEL_Forest)
      
      # Dunn's Test --> uses same ranks as kruskal-wallis
      d_test <- dunn_test(HEL_Forest, CanopyCover_mean ~ SV_new, p.adjust.method = "bonferroni") # with bonferroni correction
      print(d_test, n = 30)
      
      # Compact Letter Display
      {
        library(multcompView)
        
        # Prepare p-values for the CLD function
        # NOTE: needs a named vector of adjusted p-values
        p_adj <- d_test$p.adj
        names(p_adj) <- paste0(d_test$group1, "-", d_test$group2)
        
        # Generate the letters
        cld <- multcompLetters(p_adj)
        cld_df <- data.frame(SV_new = names(cld$Letters), 
                             letters = cld$Letters)
        
        # CLD Plot
        ggplot(HEL_Forest, aes(x = factor(SV_new), y = CanopyCover_mean, fill = factor(SV_new))) + 
          geom_boxplot(outlier.alpha = 0.2) +
          theme_minimal() +
          # Add letters above boxes
          geom_text(data = cld_df, aes(x = SV_new, y = 105, label = letters), 
                    vjust = 0, size = 5, fontface = "bold", 
                    inherit.aes = FALSE) + # Critical to prevent looking for 'fill' in cld_df
          scale_fill_manual(values = sv_cols,
                            labels = sv_labels,
                            name = "Social Values") + # Legend title
          labs(title = "Helsinki: Canopy Cover by Social Value Category",
               subtitle = "FOREST DATA. Groups sharing a letter are not significantly different (Dunn's test, Bonferroni)",
               x = "Social Value Category",
               y = "Canopy Cover (%)")
      }
      
      
      
    }
    
    # Greenspace
    {
      # Step 1) Exploratory Box-Plot
      boxplot(CanopyCover_mean ~ SV_new, data = HEL |> filter(type_2018 == "Greenspace"),
              xlab = "Social value category",
              ylab = "Mean canopy cover (%)",
              col = sv_cols)
      
      # Step 2) Kruskal-Wallis
      kruskal.test(CanopyCover_mean ~ factor(SV_new), data = HEL_Greenspace)
      
      # Step 3) Kruskal effect size
      kruskal_effsize(CanopyCover_mean ~ factor(SV_new), data = HEL_Greenspace)
      
      # Dunn's Test --> uses same ranks as kruskal-wallis
      d_test <- dunn_test(HEL_Greenspace, CanopyCover_mean ~ SV_new, p.adjust.method = "bonferroni") # with bonferroni correction
      print(d_test, n = 30)
      
      # Compact Letter Display
      {
        library(multcompView)
        
        # Prepare p-values for the CLD function
        # NOTE: needs a named vector of adjusted p-values
        p_adj <- d_test$p.adj
        names(p_adj) <- paste0(d_test$group1, "-", d_test$group2)
        
        # Generate the letters
        cld <- multcompLetters(p_adj)
        cld_df <- data.frame(SV_new = names(cld$Letters), 
                             letters = cld$Letters)
        
        # CLD Plot
        ggplot(HEL_Greenspace, aes(x = factor(SV_new), y = CanopyCover_mean, fill = factor(SV_new))) + 
          geom_boxplot(outlier.alpha = 0.2) +
          theme_minimal() +
          # Add letters above boxes
          geom_text(data = cld_df, aes(x = SV_new, y = 105, label = letters), 
                    vjust = 0, size = 5, fontface = "bold", 
                    inherit.aes = FALSE) + # Critical to prevent looking for 'fill' in cld_df
          scale_fill_manual(values = sv_cols,
                            labels = sv_labels,
                            name = "Social Values") + # Legend title
          labs(title = "Helsinki: Canopy Cover by Social Value Category",
               subtitle = "GREENSPACE DATA. Groups sharing a letter are not significantly different (Dunn's test, Bonferroni)",
               x = "Social Value Category",
               y = "Canopy Cover (%)")
      }
    }
    
    # Other
    {
      # Step 1) Exploratory Box-Plot
      boxplot(CanopyCover_mean ~ SV_new, data = HEL |> filter(type_2018 == "Other"),
              xlab = "Social value category",
              ylab = "Mean canopy cover (%)",
              col = sv_cols)
      
      # Step 2) Kruskal-Wallis
      kruskal.test(CanopyCover_mean ~ factor(SV_new), data = HEL_Other)
      
      # Step 3) Kruskal effect size
      kruskal_effsize(CanopyCover_mean ~ factor(SV_new), data = HEL_Other)
      
      # Dunn's Test --> uses same ranks as kruskal-wallis
      d_test <- dunn_test(HEL_Other, CanopyCover_mean ~ SV_new, p.adjust.method = "bonferroni") # with bonferroni correction
      print(d_test, n = 30)
      
      # Compact Letter Display
      {
        library(multcompView)
        
        # Prepare p-values for the CLD function
        # NOTE: needs a named vector of adjusted p-values
        p_adj <- d_test$p.adj
        names(p_adj) <- paste0(d_test$group1, "-", d_test$group2)
        
        # Generate the letters
        cld <- multcompLetters(p_adj)
        cld_df <- data.frame(SV_new = names(cld$Letters), 
                             letters = cld$Letters)
        
        # CLD Plot
        ggplot(HEL_Other, aes(x = factor(SV_new), y = CanopyCover_mean, fill = factor(SV_new))) + 
          geom_boxplot(outlier.alpha = 0.2) +
          theme_minimal() +
          # Add letters above boxes
          geom_text(data = cld_df, aes(x = SV_new, y = 105, label = letters), 
                    vjust = 0, size = 5, fontface = "bold", 
                    inherit.aes = FALSE) + # Critical to prevent looking for 'fill' in cld_df
          scale_fill_manual(values = sv_cols,
                            labels = sv_labels,
                            name = "Social Values") + # Legend title
          labs(title = "Helsinki: Canopy Cover by Social Value Category",
               subtitle = "OTHER DATA. Groups sharing a letter are not significantly different (Dunn's test, Bonferroni)",
               x = "Social Value Category",
               y = "Canopy Cover (%)")
      }
      
    }
    
    # Comparative plot
    {
      ggplot(HEL, aes(x = SV_new, y = CanopyCover_mean, fill = SV_new)) +
        geom_boxplot(outlier.shape = 21, outlier.size = 2, alpha = 0.8) +  
        facet_wrap(~type_2018) +
        scale_fill_manual(values = sv_cols,
                          labels = sv_labels) +                              
        labs(
          x = "Social values",
          y = "Canopy Cover (%)",
          fill = "Social value"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          strip.text = element_text(face = "bold"),                        
          legend.position = "right"                                         
        )
    }
    
    # Multinomial logistic regression (MLR)
    # NOTE: multinom() uses whichever category is first as the reference. To change the reference category use
    # NOTE: df$SV_new <- factor(df$SV_new, levels = c("reference", "c1", "c2", "and so on"))
    {
      # Individual MLRs
      MLR_Forest <- multinom(SV_new ~ CanopyCover_mean, data = HEL_Forest)
      MLR_Greenspace <- multinom(SV_new ~ CanopyCover_mean, data = HEL_Greenspace)
      MLR_Other <- multinom(SV_new ~ CanopyCover_mean, data = HEL_Other)
      
      summary(MLR_Forest) # Current reference category = 1
      summary(MLR_Greenspace) # Current reference category = 1
      summary(MLR_Other) # Current reference category = 1
      
      pred_Forest <- ggpredict(MLR_Forest, terms = "CanopyCover_mean")
      plot(pred_Forest) # visualization
      
      pred_Greenspace <- ggpredict(MLR_Greenspace, terms = "CanopyCover_mean")
      plot(pred_Greenspace) # visualization
      
      pred_Other <- ggpredict(MLR_Other, terms = "CanopyCover_mean")
      plot(pred_Other) # visualization
      
      
      # Comparative MLR across all land-uses
      MLR_HEL_all <- multinom(SV_new ~ CanopyCover_mean * type_2018, data = HEL)
      summary(MLR_HEL_all)
      
      pred_HEL_all <- ggpredict(MLR_HEL_all, terms = c("CanopyCover_mean [all]", "type_2018"))
      head(pred_HEL_all)
      names(pred_HEL_all)
      
      # simple plot divided into different social values
      plot(pred_HEL_all) 
      
      # plot divided into different land-uses
      pred_HEL_all$group <- factor(pred_HEL_all$group, levels = c("Forest", "Greenspace", "Other")) # to order factes
      
      ggplot(pred_HEL_all, aes(x = x, y = predicted, color = response.level, group = response.level)) +
        geom_line(linewidth = 1.2) +
        facet_wrap(~group) +  # this specifies the panels
        scale_color_manual(values = sv_cols,
                           labels = sv_labels) +
        labs(
          x = "Canopy Cover (%)",
          y = "Predicted Probability",
          color = "Social Value"
        ) +
        theme_minimal(base_size = 14)
    }
    
    
  }
}

# Q3: GAMS for differences in SVS
{
  # S1) Binned data
  HEL_binned <- HEL_Other |> # set input data to HEL_Forest, HEL_Greenspace, or HEL_Other
    mutate(
      canopy_bin = cut(
        CanopyCover_mean,
        breaks = seq(0, 100, by = 5),
        include.lowest = TRUE,
        labels = seq(2.5, 97.5, by = 5)
      )
    )
  
  # S2) canopy counts by land-use
  canopy_counts_lu <- HEL_binned |> 
    group_by(SV_new, canopy_bin) |> 
    summarise(n = n(), .groups = "drop") |> 
    mutate(canopy_mid = as.numeric(as.character(canopy_bin)))
  
  # interaction model
  gam_lu <- gam(
    n ~ s(canopy_mid, by = SV_new) + SV_new,
    family = nb(),
    data = canopy_counts_lu,
    method = "REML"
  )
  
  summary(gam_lu)
  
  # plot GAM
  plot(gam_lu, pages = 1, rug = TRUE)
  
  # This replaces plot(gam_lu, pages = 1, rug = TRUE)
  library(gratia)
  library(ggplot2)
  
  draw(gam_lu, rug = TRUE) &
    theme_minimal() &
    labs(title = "Helsinki SV GAM")
}


#### Copenhagen

# Q1: Do mapped SV in general co-occur with canopy cover?
{
  # S1) - S7) with the inclusion of bin 2.5 (which includes 0s)
  {
    # S1) binned data
    CPH_binned <- CPH |> 
      mutate(
        canopy_bin = cut(
          CanopyCover_mean,
          breaks = seq(0, 100, by = 5),
          include.lowest = TRUE,
          labels = seq(2.5, 97.5, by = 5)
        )
      )
    
    # S2) counts of mapped points for each bin
    canopy_counts <- CPH_binned |> 
      count(canopy_bin) |> 
      mutate(canopy_mid = as.numeric(as.character(canopy_bin)))
    
    # S3) plot showing the relation
    ggplot(canopy_counts, aes(x = canopy_mid, y = n)) +
      geom_point() +
      geom_smooth(method = "loess", se = TRUE) +
      labs(
        x = "Canopy cover (%)",
        y = "Number of mapped social values",
        title = "Copenhagen: Global frequency of mapped social values along canopy cover gradient"
      )
    
    # S4) Poisson Regression - Assumes: Var(n) = Mean(n)
    pois_mod <- glm(
      n ~ canopy_mid,
      family = poisson(link = "log"),
      data = canopy_counts
    )
    summary(pois_mod)
    
    -(1 - exp(-0.0257760)) # A 1% increase in canopy cover would lead to a -2.5% decrease in counts of social values
    
    # S5) Check for dispersion
    dispersion <- sum(residuals(pois_mod, type = "pearson")^2) /
      df.residual(pois_mod)
    
    dispersion # dispersion 255.37 >> 2, so switch to negative binomial!
    
    # S6) Negative Binomial
    nb_mod <- glm.nb(n ~ canopy_mid, data = canopy_counts)
    summary(nb_mod)
    
    -(1 - exp(0.018497)) # A 1% increase in canopy cover would lead to a -1.9% decrease in counts of social values
    
    # S7) Generalized Additive Model (GAM)
    gam_mod <- gam(n ~ s(canopy_mid), family = nb(), data = canopy_counts)
    summary(gam_mod) # edf = 8.375  -> "wiggly"; p = ***; 
    
    plot(gam_mod, rug = TRUE)
    
    # replaces plot(gam_mod, ...)
    draw(gam_mod, rug = TRUE) &
      theme_minimal() &
      labs(title = "Copenhagen Global GAM")
  }
  
  # S1) - S7) without of bin 2.5 (which includes 0s)
  {
    # S1) binned data
    CPH_binned <- CPH |> 
      mutate(
        canopy_bin = cut(
          CanopyCover_mean,
          breaks = seq(0, 100, by = 5),
          include.lowest = TRUE,
          labels = seq(2.5, 97.5, by = 5)
        )
      )
    
    # S2) counts of mapped points for each 5% bin
    canopy_counts <- CPH_binned |> 
      count(canopy_bin) |> 
      mutate(canopy_mid = as.numeric(as.character(canopy_bin)))
    
    canopy_counts <- canopy_counts |> filter(canopy_bin != "2.5")
    
    # S3) plot showing the relation
    ggplot(canopy_counts, aes(x = canopy_mid, y = n)) +
      geom_point() +
      geom_smooth(method = "loess", se = TRUE) +
      labs(
        x = "Canopy cover (%)",
        y = "Number of mapped social values",
        title = "Frequency of mapped social values along canopy cover gradient"
      )
    
    # S4) Poisson Regression - Assumes: Var(n) = Mean(n)
    pois_mod <- glm(
      n ~ canopy_mid,
      family = poisson(link = "log"),
      data = canopy_counts
    )
    
    summary(pois_mod)
    
    # S5) Check for dispersion
    dispersion <- sum(residuals(pois_mod, type = "pearson")^2) /
      df.residual(pois_mod)
    dispersion # dispersion high so switch to negative binomial instead
    
    # S6) Negative Binomial
    nb_mod <- glm.nb(n ~ canopy_mid, data = canopy_counts)
    summary(nb_mod)
    
    # S7) Generalized Additive Model (GAM)
    gam_mod <- gam(n ~ s(canopy_mid), family = nb(), data = canopy_counts)
    summary(gam_mod)
    
    plot(gam_mod, rug = TRUE)
  }
}

# Q2: Do mapped social values co-occur with canopy cover within land-use types? (LU types are forest, greenspace, and other)
{
  # S1) Binned data
  CPH_binned <- CPH |> 
    mutate(
      canopy_bin = cut(
        CanopyCover_mean,
        breaks = seq(0, 100, by = 5),
        include.lowest = TRUE,
        labels = seq(2.5, 97.5, by = 5)
      )
    )
  
  # S2) canopy counts by land-use
  canopy_counts_lu <- CPH_binned |> 
    group_by(type_2018, canopy_bin) |> 
    summarise(n = n(), .groups = "drop") |> 
    mutate(canopy_mid = as.numeric(as.character(canopy_bin)))
  
  
  # S3) GLMS - Question: Does land-use have a significant impact on the relation?
  # NOTE: This does not work, probably because there is too many missing bins for some of the land uses
  
  # interaction model
  nb_lu_mod <- glm.nb(
    n ~ canopy_mid * type_2018,
    data = canopy_counts_lu
  )
  
  # no interaction model
  nb_no_interaction <- glm.nb(
    n ~ canopy_mid + type_2018,
    data = canopy_counts_lu
  )
  
  # ANOVA to compare both
  anova(nb_no_interaction, nb_lu_mod, test = "Chisq") 
  
  # S4) center canopy counts + re-run models
  # NOTE: This does not work, probably because there is too many missing bins for some of the land uses
  canopy_counts_lu <- canopy_counts_lu |> 
    mutate(canopy_c = scale(canopy_mid, center = TRUE, scale = FALSE))
  
  # interaction model
  nb_lu_mod <- glm.nb(
    n ~ canopy_mid * type_2018,
    data = canopy_counts_lu
  )
  
  summary(nb_lu_mod)
  
  # no interaction model
  nb_no_interaction <- glm.nb(
    n ~ canopy_mid + type_2018,
    data = canopy_counts_lu
  )
  
  # ANOVA to compare both
  anova(nb_no_interaction, nb_lu_mod, test = "Chisq")
  
  # S4.2) Alternative: GAM
  canopy_counts_lu$type_2018 <- as.factor(canopy_counts_lu$type_2018)
  
  gam_lu <- gam(
    n ~ s(canopy_mid, by = type_2018) + type_2018,
    family = nb(),
    data = canopy_counts_lu,
    method = "REML"
  )
  
  summary(gam_lu)
  
  # S5) Results of negative binomial GLM for each land-use
  # NOTE: This consequently to the previous ones doesn't work
  by(canopy_counts_lu, canopy_counts_lu$type_2018, function(df) {
    summary(glm.nb(n ~ canopy_c, data = df))
  })
  
  
  # S6) Visualization
  ggplot(canopy_counts_lu,
         aes(x = canopy_mid, y = n, color = type_2018)) +
    geom_point() +
    geom_smooth(method = "glm.nb", se = TRUE) +
    scale_color_manual(values = type_cols,
                       labels = type_labels) +
    labs(
      x = "Canopy cover (%)",
      y = "Number of mapped social values",
      color = "Land-use",
      title = "Copenhagen: Mapped social values by land-use"
    ) +
    theme_minimal()
  
  plot(gam_lu, pages = 1, rug = TRUE)
  
  # replaces plot(gam_mod, ...)
  draw(gam_lu, rug = TRUE) &
    theme_minimal() &
    labs(title = "Copenhagen Land Cover GAM")
}

# Q3: Are there differences between social values?
{
  # S1) Global tests
  {
    kruskal.test(CanopyCover_mean ~ SV_new, data = CPH)
    
    kruskal_effsize(CanopyCover_mean ~ factor(SV_new), data = CPH)
    
    # Dunn's Test --> uses same ranks as kruskal-wallis
    d_test <- dunn_test(CPH, CanopyCover_mean ~ SV_new, p.adjust.method = "bonferroni") # with bonferroni correction
    print(d_test, n = 30)
    
    # Boxplot
    boxplot(CanopyCover_mean ~ SV_new, data = CPH,
            xlab = "Social value category",
            ylab = "Canopy cover (%)",
            main = "Copenhagen",
            col = sv_cols)
    
    # Compact Letter Display
    {
      library(multcompView)
      
      # Prepare p-values for the CLD function
      # NOTE: needs a named vector of adjusted p-values
      p_adj <- d_test$p.adj
      names(p_adj) <- paste0(d_test$group1, "-", d_test$group2)
      
      # Generate the letters
      cld <- multcompLetters(p_adj)
      cld_df <- data.frame(SV_new = names(cld$Letters), 
                           letters = cld$Letters)
      
      # CLD Plot
      ggplot(CPH, aes(x = factor(SV_new), y = CanopyCover_mean, fill = factor(SV_new))) + 
        geom_boxplot(outlier.alpha = 0.2) +
        theme_minimal() +
        # Add letters above boxes
        geom_text(data = cld_df, aes(x = SV_new, y = 105, label = letters), 
                  vjust = 0, size = 5, fontface = "bold", 
                  inherit.aes = FALSE) + # Critical to prevent looking for 'fill' in cld_df
        scale_fill_manual(values = sv_cols,
                          labels = sv_labels,
                          name = "Social Values") + # Legend title
        labs(title = "Copenhagen: Canopy Cover by Social Value Category",
             subtitle = "GLOBAL DATA. Groups sharing a letter are not significantly different (Dunn's test, Bonferroni)",
             x = "Social Value Category",
             y = "Canopy Cover (%)")
    }
    
  }
  
  # S2) By Land-Use
  {
    # Forest
    {
      # Step 1) Exploratory Box-Plot
      boxplot(CanopyCover_mean ~ SV_new, data = CPH |> filter(type_2018 == "Forest"),
              xlab = "Social value category",
              ylab = "Mean canopy cover (%)",
              col = sv_cols)
      
      # Step 2) Kruskal-Wallis
      kruskal.test(CanopyCover_mean ~ factor(SV_new), data = CPH_Forest)
      
      # Step 3) Kruskal effect size
      kruskal_effsize(CanopyCover_mean ~ factor(SV_new), data = CPH_Forest)
      
      # Dunn's Test --> uses same ranks as kruskal-wallis
      d_test <- dunn_test(CPH_Forest, CanopyCover_mean ~ SV_new, p.adjust.method = "bonferroni") # with bonferroni correction
      print(d_test, n = 30)
      
      # Compact Letter Display
      {
        library(multcompView)
        
        # Prepare p-values for the CLD function
        # NOTE: needs a named vector of adjusted p-values
        p_adj <- d_test$p.adj
        names(p_adj) <- paste0(d_test$group1, "-", d_test$group2)
        
        # Generate the letters
        cld <- multcompLetters(p_adj)
        cld_df <- data.frame(SV_new = names(cld$Letters), 
                             letters = cld$Letters)
        
        # CLD Plot
        ggplot(CPH_Forest, aes(x = factor(SV_new), y = CanopyCover_mean, fill = factor(SV_new))) + 
          geom_boxplot(outlier.alpha = 0.2) +
          theme_minimal() +
          # Add letters above boxes
          geom_text(data = cld_df, aes(x = SV_new, y = 105, label = letters), 
                    vjust = 0, size = 5, fontface = "bold", 
                    inherit.aes = FALSE) + # Critical to prevent looking for 'fill' in cld_df
          scale_fill_manual(values = sv_cols,
                            labels = sv_labels,
                            name = "Social Values") + # Legend title
          labs(title = "Copenhagen: Canopy Cover by Social Value Category",
               subtitle = "FOREST DATA. Groups sharing a letter are not significantly different (Dunn's test, Bonferroni)",
               x = "Social Value Category",
               y = "Canopy Cover (%)")
      }
    }
    
    # Greenspace
    {
      # Step 1) Exploratory Box-Plot
      boxplot(CanopyCover_mean ~ SV_new, data = CPH |> filter(type_2018 == "Greenspace"),
              xlab = "Social value category",
              ylab = "Mean canopy cover (%)",
              col = sv_cols)
      
      # Step 2) Kruskal-Wallis
      kruskal.test(CanopyCover_mean ~ factor(SV_new), data = CPH_Greenspace)
      
      # Step 3) Kruskal effect size
      kruskal_effsize(CanopyCover_mean ~ factor(SV_new), data = CPH_Greenspace)
      
      # Dunn's Test --> uses same ranks as kruskal-wallis
      d_test <- dunn_test(CPH_Greenspace, CanopyCover_mean ~ SV_new, p.adjust.method = "bonferroni") # with bonferroni correction
      print(d_test, n = 30)
      
      # Compact Letter Display
      {
        library(multcompView)
        
        # Prepare p-values for the CLD function
        # NOTE: needs a named vector of adjusted p-values
        p_adj <- d_test$p.adj
        names(p_adj) <- paste0(d_test$group1, "-", d_test$group2)
        
        # Generate the letters
        cld <- multcompLetters(p_adj)
        cld_df <- data.frame(SV_new = names(cld$Letters), 
                             letters = cld$Letters)
        
        # CLD Plot
        ggplot(CPH_Greenspace, aes(x = factor(SV_new), y = CanopyCover_mean, fill = factor(SV_new))) + 
          geom_boxplot(outlier.alpha = 0.2) +
          theme_minimal() +
          # Add letters above boxes
          geom_text(data = cld_df, aes(x = SV_new, y = 105, label = letters), 
                    vjust = 0, size = 5, fontface = "bold", 
                    inherit.aes = FALSE) + # Critical to prevent looking for 'fill' in cld_df
          scale_fill_manual(values = sv_cols,
                            labels = sv_labels,
                            name = "Social Values") + # Legend title
          labs(title = "Copenhagen: Canopy Cover by Social Value Category",
               subtitle = "GREENSPACE DATA. Groups sharing a letter are not significantly different (Dunn's test, Bonferroni)",
               x = "Social Value Category",
               y = "Canopy Cover (%)")
      }
    }
    
    # Other
    {
      # Step 1) Exploratory Box-Plot
      boxplot(CanopyCover_mean ~ SV_new, data = CPH |> filter(type_2018 == "Other"),
              xlab = "Social value category",
              ylab = "Mean canopy cover (%)",
              col = sv_cols)
      
      # Step 2) Kruskal-Wallis
      kruskal.test(CanopyCover_mean ~ factor(SV_new), data = CPH_Other)
      
      # Step 3) Kruskal effect size
      kruskal_effsize(CanopyCover_mean ~ factor(SV_new), data = CPH_Other)
      
      # Dunn's Test --> uses same ranks as kruskal-wallis
      d_test <- dunn_test(CPH_Other, CanopyCover_mean ~ SV_new, p.adjust.method = "bonferroni") # with bonferroni correction
      print(d_test, n = 30)
      
      # Compact Letter Display
      {
        library(multcompView)
        
        # Prepare p-values for the CLD function
        # NOTE: needs a named vector of adjusted p-values
        p_adj <- d_test$p.adj
        names(p_adj) <- paste0(d_test$group1, "-", d_test$group2)
        
        # Generate the letters
        cld <- multcompLetters(p_adj)
        cld_df <- data.frame(SV_new = names(cld$Letters), 
                             letters = cld$Letters)
        
        # CLD Plot
        ggplot(CPH_Other, aes(x = factor(SV_new), y = CanopyCover_mean, fill = factor(SV_new))) + 
          geom_boxplot(outlier.alpha = 0.2) +
          theme_minimal() +
          # Add letters above boxes
          geom_text(data = cld_df, aes(x = SV_new, y = 105, label = letters), 
                    vjust = 0, size = 5, fontface = "bold", 
                    inherit.aes = FALSE) + # Critical to prevent looking for 'fill' in cld_df
          scale_fill_manual(values = sv_cols,
                            labels = sv_labels,
                            name = "Social Values") + # Legend title
          labs(title = "Copenhagen: Canopy Cover by Social Value Category",
               subtitle = "OTHER DATA. Groups sharing a letter are not significantly different (Dunn's test, Bonferroni)",
               x = "Social Value Category",
               y = "Canopy Cover (%)")
      }
    }
    
    # Comparative plot
    {
      ggplot(CPH, aes(x = SV_new, y = CanopyCover_mean, fill = SV_new)) +
        geom_boxplot(outlier.shape = 21, outlier.size = 2, alpha = 0.8) +  
        facet_wrap(~type_2018) +
        scale_fill_manual(values = sv_cols,
                          labels = sv_labels) +                              
        labs(
          x = "Social values",
          y = "Canopy Cover (%)",
          fill = "Social value"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          strip.text = element_text(face = "bold"),                        
          legend.position = "right"                                         
        )
    }
    
    # Multinomial logistic regression (MLR)
    # NOTE: multinom() uses whichever category is first as the reference. To change the reference category use
    # NOTE: df$SV_new <- factor(df$SV_new, levels = c("reference", "c1", "c2", "and so on"))
    {
      # Individual MLRs
      MLR_Forest <- multinom(SV_new ~ CanopyCover_mean, data = CPH_Forest)
      MLR_Greenspace <- multinom(SV_new ~ CanopyCover_mean, data = CPH_Greenspace)
      MLR_Other <- multinom(SV_new ~ CanopyCover_mean, data = CPH_Other)
      
      summary(MLR_Forest) # Current reference category = 1
      summary(MLR_Greenspace) # Current reference category = 1
      summary(MLR_Other) # Current reference category = 1
      
      pred_Forest <- ggpredict(MLR_Forest, terms = "CanopyCover_mean")
      plot(pred_Forest) # visualization
      
      pred_Greenspace <- ggpredict(MLR_Greenspace, terms = "CanopyCover_mean")
      plot(pred_Greenspace) # visualization
      
      pred_Other <- ggpredict(MLR_Other, terms = "CanopyCover_mean")
      plot(pred_Other) # visualization
      
      
      # Comparative MLR across all land-uses
      MLR_CPH_all <- multinom(SV_new ~ CanopyCover_mean * type_2018, data = CPH)
      summary(MLR_CPH_all)
      
      pred_CPH_all <- ggpredict(MLR_CPH_all, terms = c("CanopyCover_mean [all]", "type_2018"))
      head(pred_CPH_all)
      names(pred_CPH_all)
      
      # simple plot divided into different social values
      plot(pred_CPH_all) 
      
      # plot divided into different land-uses
      pred_CPH_all$group <- factor(pred_CPH_all$group, levels = c("Forest", "Greenspace", "Other")) # to order factes
      
      ggplot(pred_CPH_all, aes(x = x, y = predicted, color = response.level, group = response.level)) +
        geom_line(linewidth = 1.2) +
        facet_wrap(~group) +  # this specifies the panels
        scale_color_manual(values = sv_cols,
                           labels = sv_labels) +
        labs(
          x = "Canopy Cover (%)",
          y = "Predicted Probability",
          color = "Social Value"
        ) +
        theme_minimal(base_size = 14)
    }
    
  }
  
}

# Q3: GAMS for differences in SVS
{
  # S1) Binned data
  CPH_binned <- CPH_Forest |> # set input data to HEL_Forest, HEL_Greenspace, or HEL_Other
    mutate(
      canopy_bin = cut(
        CanopyCover_mean,
        breaks = seq(0, 100, by = 5),
        include.lowest = TRUE,
        labels = seq(2.5, 97.5, by = 5)
      )
    )
  
  # S2) canopy counts by land-use
  canopy_counts_lu <- CPH_binned |> 
    group_by(SV_new, canopy_bin) |> 
    summarise(n = n(), .groups = "drop") |> 
    mutate(canopy_mid = as.numeric(as.character(canopy_bin)))
  
  # interaction model
  gam_lu <- gam(
    n ~ s(canopy_mid, by = SV_new) + SV_new,
    family = nb(),
    data = canopy_counts_lu,
    method = "REML"
  )
  
  summary(gam_lu)
  
  # plot GAM
  plot(gam_lu, pages = 1, rug = TRUE)
  
  # This replaces plot(gam_lu, pages = 1, rug = TRUE)
  draw(gam_lu, rug = TRUE) &
    theme_minimal() &
    labs(title = "Copenhagen SV GAM")
}


