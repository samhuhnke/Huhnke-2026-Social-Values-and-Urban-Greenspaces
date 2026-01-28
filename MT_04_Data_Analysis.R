# ============================================ 
# MT_04_Data_Analysis
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

library(MASS) # for section 7) + load first otherwise it maskes select from dplyr
library(tidyverse) # used for data handling
library(ggplot2) # used for plotting
library(rstatix) # used to assess effect sizes
library(nnet) # used for multinomial logistic regression
library(ggeffects) # to visualize results of multinomial logistic regression
library(sf)
library(geojsonsf) # to read geojson variable
library(spdep)
library(coin)
library(mgcv) # for section 7)

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
# 3) Data Pre-Processing
# NOTE: This section joins the area data with the PPGIS based on the code_2018 column.
# NOTE: Further this section filters out any unneccessary columns for further analysis.
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
    reframe(type_area_km2 = sum(area_km2, na.rm = T))
  
  # join type area into complete data set
  HEL <- HEL |> 
    left_join(HEL_type_area, by = "type_2018")
  
  # Turn SV_new into categorical value (or factor in R)
  HEL$SV_new <- factor(HEL$SV_new)
  
  # Create subsets
  HEL_Forest <- HEL |> filter(type_2018 == "Forest")
  HEL_Greenspace <- HEL |> filter(type_2018 == "Greenspace")
  HEL_Other <- HEL |> filter(type_2018 == "Other")

}

# Copenhagen
{
  CPH <- CPH_raw |> 
    # add area data
    left_join(CPH_areas, by = "code_2018") |> 
    # select relevant variables
    select(GeoJSON, Respondent, code_2018, category_2018, area_km2, area_percent, total_km2, SV_new, X_mean) |> 
    # rename canopy cover variable 
    rename(CanopyCover_mean = X_mean) |> 
    # further simplify classes into Greenspace, Forest, and Other
    mutate(type_2018 = case_when(category_2018 == 4 ~ "Greenspace",
                                 category_2018 == 6 ~ "Forest",
                                 category_2018 != 4 & category_2018 != 6 | is.na(category_2018) ~ "Other"))
  
  # calculate area per type
  CPH_type_area <- CPH |> 
    select(type_2018, area_km2) |> 
    group_by(type_2018) |> 
    unique() |> 
    reframe(type_area_km2 = sum(area_km2, na.rm = T))
  
  # join type area into complete data set
  CPH <- CPH |> 
    left_join(CPH_type_area, by = "type_2018")
  
  # Turn SV_new into categorical value (or factor in R)
  CPH$SV_new <- factor(CPH$SV_new)
  
  # Create subsets
  CPH_Forest <- CPH |> filter(type_2018 == "Forest")
  CPH_Greenspace <- CPH |> filter(type_2018 == "Greenspace")
  CPH_Other <- CPH |> filter(type_2018 == "Other")
}

# remove temporary layers
rm("CPH_type_area", "HEL_type_area")


# ============================================
# 4) Analysis Preparation
# ============================================

# Colors
{
  # Land Use category colors
  category_cols <- c("#bf0000", "#959595", "#734d37", "#8cdc00", "#ffffa8", "#008c00", "#a6a6ff", "#80f2e6","#ccffcc")
  
  # Land Use type colors
  type_cols <- c("#008c00", "#8cdc00", "#959595" )
  
  # Social Value colors
  sv_cols <- c("#d45680", "#4d9955", "#a0afa0", "#009acb", "#006695", "#e69c24", "#ffeccc", "#8b7356")
  
}

# Labels
{
  # Land Use categories
  lu_labels <- c(
    "1" = "1: Urban fabric",
    "2" = "2: Industrial, commercial, transport",
    "3" = "3: Mine, dump, construction",
    "4" = "4: Artificial green spaces",
    "5" = "5: Agricultural land",
    "6" = "6: Semi-/Natural green spaces",
    "7" = "7: Wetlands",
    "8" = "8: Waterbodies",
    "9" = "9: Natural open spaces"
  )
  
  # Social values
  sv_labels <- c(
    "1" = "1: Relaxation",
    "2" = "2: Natural Values",
    "3" = "3: Aesthetics",
    "4" = "4: Physical Well-Being and Outdoor Activity",
    "5" = "5: Social Interaction",
    "6" = "6: Heritage and Community Values",
    "7" = "7: Spiritual Values",
    "8" = "8: Personal Identity"
  )
}

# ============================================
# 5) General data analysis
# ============================================

# Metrics

# Point count & point density per type (i.e. forest, greenspace, other) 
{
  HEL |> group_by(type_2018) |> count() |> 
    left_join(HEL |> select(type_2018, type_area_km2) |> unique(), by = "type_2018") |> 
    mutate(point_density_km2 = n/type_area_km2)
  
  CPH |> group_by(type_2018) |> count() |> 
    left_join(CPH |> select(type_2018, type_area_km2) |> unique(), by = "type_2018") |> 
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


# ============================================
# 6) Social value data analysis
# ============================================

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

# Correlations between SV and canopy cover NOT divided by type

# MOCKUP FOR Helsinki ---------------------------------
# Approach 1: ANOVA
# Question: Does canopy cover differ between different social values?
{
  
  # ANOVA
  anova_model <- aov(CanopyCover_mean ~ SV_new, data = HEL)
  summary(anova_model)
  
  # Effect sizes
  eta_sq <- 837424 / (837424 + 13019985)
  eta_sq # Interpretation: about 6% of the variance in canopy cover is explained by social values
  
  # Follow-up Tukey test
  TukeyHSD(anova_model)
  
  # Box-Plot
  boxplot(CanopyCover_mean ~ SV_new, data = HEL,
          xlab = "Social value category",
          ylab = "Mean canopy cover (%)",
          col = sv_cols)
}

# Approach 2: Kruskal-Wallis --> Presumably this is the main approach for RQ1 ! 
# Question: Does canopy cover differ between different social values?
{
  # Boxplot
  boxplot(CanopyCover_mean ~ SV_new, data = HEL,
          xlab = "Social value category",
          ylab = "Mean canopy cover (%)",
          col = sv_cols)
  
  # Kruskal-Wallis
  kruskal.test(CanopyCover_mean ~ SV_new, data = HEL)
  
  # Follow-up Wilcox
  pairwise.wilcox.test(
    HEL$CanopyCover_mean,
    HEL$SV_new,
    p.adjust.method = "BH")
  
}

# Approach 3: Multinomial logistic regression
{
  LR_model <- multinom(SV_new ~ CanopyCover_mean, data = HEL)
  summary(LR_model)
  
  z <- summary(LR_model)$coefficients / summary(LR_model)$standard.errors
  p <- 2 * (1 - pnorm(abs(z)))
  p
  

  
  pred <- ggpredict(LR_model, terms = "CanopyCover_mean")
  plot(pred)
}


# Correlations between SV and canopy cover divided by type =================

# Helsinki Forest
{
  # Step 1) Exploratory Box-Plot
  boxplot(CanopyCover_mean ~ SV_new, data = HEL |> filter(type_2018 == "Forest"),
          xlab = "Social value category",
          ylab = "Mean canopy cover (%)",
          col = sv_cols)
  
  # Step 2) Kruskal-Wallis
  kruskal.test(CanopyCover_mean ~ factor(SV_new), data = HEL_Forest)
  
  # Step 3) Pairwise Wilcox
  pairwise.wilcox.test(
    HEL_Forest$CanopyCover_mean,
    HEL_Forest$SV_new,
    p.adjust.method = "BH")
  
  # Step 4) Effect Sizes
  kruskal_effsize(CanopyCover_mean ~ factor(SV_new), data = HEL_Forest)
  
  # Step 5) Model-based multinomial regression
  # NOTE: multinom() uses whichever category is first as the reference. To change the reference category use
  # NOTE: df$SV_new <- factor(df$SV_new, levels = c("reference", "c1", "c2", "and so on"))
  m <- multinom(SV_new ~ CanopyCover_mean, data = HEL_Forest)
  summary(m) # Current reference category = 1
  
  pred <- ggpredict(m, terms = "CanopyCover_mean")
  plot(pred) # visualization
  
}

# Helsinki Greenspace
{
  # Step 1) Exploratory Box-Plot
  boxplot(CanopyCover_mean ~ SV_new, data = HEL |> filter(type_2018 == "Greenspace"),
          xlab = "Social value category",
          ylab = "Mean canopy cover (%)",
          col = sv_cols)
  
  # Step 2) Kruskal-Wallis
  kruskal.test(CanopyCover_mean ~ factor(SV_new), data = HEL_Greenspace)
  
  # Step 3) Pairwise Wilcox
  pairwise.wilcox.test(
    HEL_Greenspace$CanopyCover_mean,
    HEL_Greenspace$SV_new,
    p.adjust.method = "BH")
  
  # Step 4) Effect Sizes
  kruskal_effsize(CanopyCover_mean ~ factor(SV_new), data = HEL_Greenspace)
  
  # Step 5) Model-based multinomial regression
  # NOTE: multinom() uses whichever category is first as the reference. To change the reference category use
  # NOTE: df$SV_new <- factor(df$SV_new, levels = c("reference", "c1", "c2", "and so on"))
  m <- multinom(SV_new ~ CanopyCover_mean, data = HEL_Greenspace)
  summary(m)
  
  pred <- ggpredict(m, terms = "CanopyCover_mean")
  plot(pred) # visualization
}

# Helsinki Other
{
  # Step 1) Exploratory Box-Plot
  boxplot(CanopyCover_mean ~ SV_new, data = HEL |> filter(type_2018 == "Other"),
          xlab = "Social value category",
          ylab = "Mean canopy cover (%)",
          col = sv_cols)
  
  # Step 2) Kruskal-Wallis
  kruskal.test(CanopyCover_mean ~ factor(SV_new), data = HEL_Other)
  
  # Step 3) Pairwise Wilcox
  pairwise.wilcox.test(
    HEL_Other$CanopyCover_mean,
    HEL_Other$SV_new,
    p.adjust.method = "BH")
  
  # Step 4) Effect Sizes
  kruskal_effsize(CanopyCover_mean ~ factor(SV_new), data = HEL_Other)
  
  # Step 5) Model-based multinomial regression
  # NOTE: multinom() uses whichever category is first as the reference. To change the reference category use
  # NOTE: df$SV_new <- factor(df$SV_new, levels = c("reference", "c1", "c2", "and so on"))
  m <- multinom(SV_new ~ CanopyCover_mean, data = HEL_Other)
  summary(m) # Current reference category = 1
  
  pred <- ggpredict(m, terms = "CanopyCover_mean")
  plot(pred) # visualization
}

# Copenhagen Forest
{
  # Step 1) Exploratory Box-Plot
  boxplot(CanopyCover_mean ~ SV_new, data = CPH |> filter(type_2018 == "Forest"),
          xlab = "Social value category",
          ylab = "Mean canopy cover (%)",
          col = sv_cols)
  
  # Step 2) Kruskal-Wallis
  kruskal.test(CanopyCover_mean ~ factor(SV_new), data = CPH_Forest)
  
  # Step 3) Pairwise Wilcox
  pairwise.wilcox.test(
    CPH_Forest$CanopyCover_mean,
    CPH_Forest$SV_new,
    p.adjust.method = "BH")
  
  # Step 4) Effect Sizes
  kruskal_effsize(CanopyCover_mean ~ factor(SV_new), data = CPH_Forest)
  
  # Step 5) Model-based multinomial regression
  # NOTE: multinom() uses whichever category is first as the reference. To change the reference category use
  # NOTE: df$SV_new <- factor(df$SV_new, levels = c("reference", "c1", "c2", "and so on"))
  m <- multinom(SV_new ~ CanopyCover_mean, data = CPH_Forest)
  summary(m) # Current reference category = 1
  
  pred <- ggpredict(m, terms = "CanopyCover_mean")
  plot(pred) # visualization
}

# Copenhagen Greenspace
{
  # Step 1) Exploratory Box-Plot
  boxplot(CanopyCover_mean ~ SV_new, data = CPH |> filter(type_2018 == "Greenspace"),
          xlab = "Social value category",
          ylab = "Mean canopy cover (%)",
          col = sv_cols)
  
  # Step 2) Kruskal-Wallis
  kruskal.test(CanopyCover_mean ~ factor(SV_new), data = CPH_Greenspace)
  
  # Step 3) Pairwise Wilcox
  pairwise.wilcox.test(
    CPH_Greenspace$CanopyCover_mean,
    CPH_Greenspace$SV_new,
    p.adjust.method = "BH")
  
  # Step 4) Effect Sizes
  kruskal_effsize(CanopyCover_mean ~ factor(SV_new), data = CPH_Greenspace)
  
  # Step 5) Model-based multinomial regression
  # NOTE: multinom() uses whichever category is first as the reference. To change the reference category use
  # NOTE: df$SV_new <- factor(df$SV_new, levels = c("reference", "c1", "c2", "and so on"))
  m <- multinom(SV_new ~ CanopyCover_mean, data = CPH_Greenspace)
  summary(m) # Current reference category = 1
  
  pred <- ggpredict(m, terms = "CanopyCover_mean")
  plot(pred) # visualization
}

# Copenhagen Other
{
  # Step 1) Exploratory Box-Plot
  boxplot(CanopyCover_mean ~ SV_new, data = CPH |> filter(type_2018 == "Other"),
          xlab = "Social value category",
          ylab = "Mean canopy cover (%)",
          col = sv_cols)
  
  # Step 2) Kruskal-Wallis
  kruskal.test(CanopyCover_mean ~ factor(SV_new), data = CPH_Other)
  
  # Step 3) Pairwise Wilcox
  pairwise.wilcox.test(
    CPH_Other$CanopyCover_mean,
    CPH_Other$SV_new,
    p.adjust.method = "BH")
  
  # Step 4) Effect Sizes
  kruskal_effsize(CanopyCover_mean ~ factor(SV_new), data = CPH_Other)
  
  # Step 5) Model-based multinomial regression
  # NOTE: multinom() uses whichever category is first as the reference. To change the reference category use
  # NOTE: df$SV_new <- factor(df$SV_new, levels = c("reference", "c1", "c2", "and so on"))
  m <- multinom(SV_new ~ CanopyCover_mean, data = CPH_Other)
  summary(m) # Current reference category = 1
  
  pred <- ggpredict(m, terms = "CanopyCover_mean")
  plot(pred) # visualization
}


# Step 6) Spatially constrained permutation [MOCKUP FOR HEL_FOREST] ============


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




# ============================================
# 7) Multilayered analysis TEST
# ============================================

# Q1: Do mapped SV in general co-occur with canopy cover? -----------------

# S2) - S7) with the inclusion of bin 2.5 (which includes 0s)
{
  # S1) binned data
  HEL_binned <- HEL %>%
    mutate(
      canopy_bin = cut(
        CanopyCover_mean,
        breaks = seq(0, 100, by = 5),
        include.lowest = TRUE,
        labels = seq(2.5, 97.5, by = 5)
      )
    )
  
  # S2) counts of mapped points for each bin
  canopy_counts <- HEL_binned %>%
    count(canopy_bin) %>%
    mutate(canopy_mid = as.numeric(as.character(canopy_bin)))
  
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
  summary(gam_mod)
}

# S2) - S7) without of bin 2.5 (which includes 0s)
{
  # S1) binned data
  HEL_binned <- HEL %>%
    mutate(
      canopy_bin = cut(
        CanopyCover_mean,
        breaks = seq(0, 100, by = 5),
        include.lowest = TRUE,
        labels = seq(2.5, 97.5, by = 5)
      )
    )
  
  # S2) counts of mapped points for each 5% bin
  canopy_counts <- HEL_binned %>%
    count(canopy_bin) %>%
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

# Q2: Do mapped social values co-occur with canopy cover within land-use types? (LU types are forest, greenspace, and other)

# 


