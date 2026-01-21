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

library(tidyverse)
library(ggplot2)

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
    select(Respondent, code_2018, category_2018, area_km2, area_percent, total_km2, SV_new, X_mean) |> 
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
}

# Copenhagen
{
  CPH <- CPH_raw |> 
    # add area data
    left_join(CPH_areas, by = "code_2018") |> 
    # select relevant variables
    select(Respondent, code_2018, category_2018, area_km2, area_percent, total_km2, SV_new, X_mean) |> 
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
HEL |> group_by(type_2018) |> count() |> 
  left_join(HEL |> select(type_2018, type_area_km2) |> unique(), by = "type_2018") |> 
  mutate(point_density_km2 = n/type_area_km2)

CPH |> group_by(type_2018) |> count() |> 
  left_join(CPH |> select(type_2018, type_area_km2) |> unique(), by = "type_2018") |> 
  mutate(point_density_km2 = n/type_area_km2)


# How is canopy cover distributed overall? 
HEL |> ggplot(aes(x = CanopyCover_mean)) +
  geom_histogram()

CPH |> ggplot(aes(x = CanopyCover_mean)) +
  geom_histogram()


# How is canopy cover distributed by land-use type?
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



# ============================================
# 6) Social value data analysis
# ============================================

# Histograms by type 
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

# Correlations between SV and canopy cover divided by type

# Helsinki ---------------------------------
# Approach 1: ANOVA
# Question: Does canopy cover differ between different social values?
{
  # Turn SV_new into categorical value (or factor in R)
  HEL$SV_new <- factor(HEL$SV_new)
  
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

# Approach 2: Kruskal-Wallis
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
library(nnet)

LR_model <- multinom(SV_new ~ CanopyCover_mean, data = HEL)
summary(LR_model)

z <- summary(LR_model)$coefficients / summary(LR_model)$standard.errors
p <- 2 * (1 - pnorm(abs(z)))
p


# Copenhagen ---------------------------------



