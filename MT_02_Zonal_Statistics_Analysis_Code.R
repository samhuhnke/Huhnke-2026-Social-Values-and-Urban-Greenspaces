# AUTHOR: Sam Huhnke, M.Sc. University of Helsinki
#
### ZONAL STATISTICS ANALYSIS
#
# This code provides non-spatial analyses of relations between mapped social values and tree canopy cover for
# Helsinki and Copenhagen. 
# 
# NOTE: The data used for this analysis is owned by the CO-CARBON research project. To gain access to the data,
# contact the CO-CARBON research project. Any use of the data and this code must be in accordance with the CO-CARBON 
# guidelines.


# Clear environment if needed
rm(list = ls(globalenv()))

# Set working directory to whatever needed
setwd("C:/Users/samhu/Desktop/HY/AAAA Master Thesis/DATA/ZONAL STATISTICS")

# Load necessary packages
library(tidyverse)
library(sf) 
library(ggplot2)

### Load data sets
# Helsinki
{
  HEL_10m <- read.csv("Helsinki/HEL_Zonal_Stats_10m.csv") |> 
    select(-c("Publicatio", "Submitted", "First.Acti", "Publicat_1", "Zoom", "Language", "Index")) |> 
    mutate(Buffer = "10m") 
  HEL_20m <- read.csv("Helsinki/HEL_Zonal_Stats_20m.csv") |> 
    select(-c("Publicatio", "Submitted", "First.Acti", "Publicat_1", "Zoom", "Language", "Index")) |>
    mutate(Buffer = "20m") 
  HEL_30m <- read.csv("Helsinki/HEL_Zonal_Stats_30m.csv") |> 
    select(-c("Publicatio", "Submitted", "First.Acti", "Publicat_1", "Zoom", "Language", "Index")) |>
    mutate(Buffer = "30m") 
  HEL_40m <- read.csv("Helsinki/HEL_Zonal_Stats_40m.csv") |> 
    select(-c("Publicatio", "Submitted", "First.Acti", "Publicat_1", "Zoom", "Language", "Index")) |>
    mutate(Buffer = "40m") 
  HEL_50m <- read.csv("Helsinki/HEL_Zonal_Stats_50m.csv") |> 
    select(-c("Publicatio", "Submitted", "First.Acti", "Publicat_1", "Zoom", "Language", "Index")) |>
    mutate(Buffer = "50m") 
}

# Copenhagen - TBD in QGIS
{
  CPH_10m <- read.csv("Copenhagen/CPH_Zonal_Stats_10m.csv") |> 
    select(-c("Submitted", "First.Acti", "Nature_typ", "Nature_t_1", "lat", "lon", "lat_jitter", "lon_jitter", "WKT_2", "GeoJSON_2", "Zoom")) |> 
    mutate(Buffer = "10m") 
  CPH_20m <- read.csv("Copenhagen/CPH_Zonal_Stats_20m.csv") |> 
    select(-c("Submitted", "First.Acti", "Nature_typ", "Nature_t_1", "lat", "lon", "lat_jitter", "lon_jitter", "WKT_2", "GeoJSON_2", "Zoom")) |> 
    mutate(Buffer = "20m") 
  CPH_30m <- read.csv("Copenhagen/CPH_Zonal_Stats_30m.csv") |> 
    select(-c("Submitted", "First.Acti", "Nature_typ", "Nature_t_1", "lat", "lon", "lat_jitter", "lon_jitter", "WKT_2", "GeoJSON_2", "Zoom")) |> 
    mutate(Buffer = "30m") 
  CPH_40m <- read.csv("Copenhagen/CPH_Zonal_Stats_40m.csv") |> 
    select(-c("Submitted", "First.Acti", "Nature_typ", "Nature_t_1", "lat", "lon", "lat_jitter", "lon_jitter", "WKT_2", "GeoJSON_2", "Zoom")) |> 
    mutate(Buffer = "40m") 
  CPH_50m <- read.csv("Copenhagen/CPH_Zonal_Stats_50m.csv") |> 
    select(-c("Submitted", "First.Acti", "Nature_typ", "Nature_t_1", "lat", "lon", "lat_jitter", "lon_jitter", "WKT_2", "GeoJSON_2", "Zoom")) |> 
    mutate(Buffer = "50m") 
}



# Prelim Plots for data exploration
HEL_50m |> 
  ggplot()+
  geom_histogram(aes(x=X_mean))


#histograms
{
  #HEL
  HEL_10m |> 
    filter(SV_new == 1) |> 
    ggplot(aes(x= X_mean)) + 
    geom_histogram() +
    ylim(0, 500)
  
  HEL_30m |> 
    filter(SV_new == 1) |> 
    ggplot(aes(x= X_mean)) + 
    geom_histogram() +
    ylim(0, 500)
  
  HEL_50m |> 
    filter(SV_new == 1) |> 
    ggplot(aes(x= X_mean)) + 
    geom_histogram() +
    ylim(0, 500) 
  
  #CPH
  CPH_10m |> 
    filter(SV_new == 1) |> 
    ggplot(aes(x= X_mean)) + 
    geom_histogram() 
  
  CPH_30m |> 
    filter(SV_new == 1) |> 
    ggplot(aes(x= X_mean)) + 
    geom_histogram() 
  
  CPH_50m |> 
    filter(SV_new == 1) |> 
    ggplot(aes(x= X_mean)) + 
    geom_histogram() 
}

#violin plots
{
  HEL_10m |> 
    ggplot(aes(y=X_mean, x= as.factor(SV_new)))+
    geom_violin(aes(fill = as.factor(SV_new)))
  
  HEL_30m |> 
    ggplot(aes(y=X_mean, x= as.factor(SV_new)))+
    geom_violin(aes(fill = as.factor(SV_new)))
  
  HEL_50m |> 
    ggplot(aes(y=X_mean, x= as.factor(SV_new)))+
    geom_violin(aes(fill = as.factor(SV_new)))
  
  #CPH
  CPH_10m |> 
    ggplot(aes(y=X_mean, x= as.factor(SV_new)))+
    geom_violin(aes(fill = as.factor(SV_new)))
  
  CPH_30m |> 
    ggplot(aes(y=X_mean, x= as.factor(SV_new)))+
    geom_violin(aes(fill = as.factor(SV_new)))
  
  CPH_50m |> 
    ggplot(aes(y=X_mean, x= as.factor(SV_new)))+
    geom_violin(aes(fill = as.factor(SV_new)))
}

#boxplots
{
  HEL_10m |> 
    ggplot(aes(y=X_mean, x= as.factor(SV_new)))+
    geom_boxplot(aes(fill = as.factor(SV_new)))
  
  HEL_30m |> 
    ggplot(aes(y=X_mean, x= as.factor(SV_new)))+
    geom_boxplot(aes(fill = as.factor(SV_new)))
  
  HEL_50m |> 
    ggplot(aes(y=X_mean, x= as.factor(SV_new)))+
    geom_boxplot(aes(fill = as.factor(SV_new)))
  
  #CPH
  CPH_10m |> 
    ggplot(aes(y=X_mean, x= as.factor(SV_new)))+
    geom_boxplot(aes(fill = as.factor(SV_new)))
  
  CPH_30m |> 
    ggplot(aes(y=X_mean, x= as.factor(SV_new)))+
    geom_boxplot(aes(fill = as.factor(SV_new)))
  
  CPH_50m |> 
    ggplot(aes(y=X_mean, x= as.factor(SV_new)))+
    geom_boxplot(aes(fill = as.factor(SV_new)))
}



