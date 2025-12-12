# ============================================ 
# MT_01_Data_Hermonization_Code
# ============================================
#
# AUTHOR: Sam Huhnke, M.Sc. University of Helsinki
#
#
# This code is intended to harmonize the Helsinki and Copenhagen CO-CARBON data sets. The harmonized data sets will 
# then be used for further spatial analyses in QGIS.
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
setwd("C:/Users/samhu/Desktop/Code Projects/Huhnke_2026/data/PPGIS Data")

# ============================================
# 1) Load necessary packages
# ============================================

library(tidyverse)
library(purrr) # for 5.2)

# ============================================
# 2) Load data
# ============================================
HEL_raw <- read.csv("Helsinki/Dataset_2_Spatial_important.csv", sep = ";")  # Helsinki PPGIS data
CPH_raw <- read.csv("Copenhagen/COCARBON_CPH_PlaceInNature.csv", sep = ",") # Copenhagen PPGIS data

# control data structure
colnames(HEL_raw)
colnames(CPH_raw)


# ============================================
# 3) Data Cleaning
# ============================================

# 3.1) Remove all rows that contain NAs for the social values column (= 'SV_type')
HEL_no_NA <- HEL_raw |> filter(!is.na(SV_type))

# 3.2) Reshape Create CPH_SV data to resemble HEL_SV format: from current wide format to long format
{
  # 3.2.I) temporarily save 'Other_open' answers to seperate df 
  # NOTE: this is done, to conserve the answers by later matching them based on ID and coordinates  
  
  CPH_SoVal_Other_Open <- CPH_raw |> 
    # create SoVal_other_open presence-absence column
    mutate(SoVal_other_open_P = case_when(SoVal_other_open == "" ~ 0, # absence
                                          SoVal_other_open != "" ~ 1 )) |> # presence
    select(Respondent.ID, lat, lon, SoVal_other_open_P, SoVal_other_open) # only select the relevant columns
  
  
  # 3.2.II) Create long table
  # NOTE: the resulting data should have one SV_type column
  # NOTE: as a result multiple social values will be assigned to each mapped pin. this leads to visual overlaps.
  # NOTE: as a solution I create a second coordinate column that slightly introduces noise to the coordinates
  # IMPORTANT: this is purely done for better visual interpretability and will not be used for later datanalyses!
  
  # set seed - for coordinate jitter
  # NOTE: still need to do that
  
  CPH_SV_long <- CPH_raw |> 
    # create SoVal_other_open presence-absence column
    mutate(SoVal_other_open_P = case_when(SoVal_other_open == "" ~ 0,
                                          SoVal_other_open != "" ~ 1 )) |> 
    # temporarily remove SoVal_other_open data
    # NOTE: this is done so I can filter for starts_with('SoVal') in the next step
    # NOTE: otherwise this column creates issues as it is not presence-absence but a character string instead
    select(-SoVal_other_open) |> 
    # remove activity values
    # NOTE: this is done to keep the table more manageable and because the 'Activity' values will not be used for analysis
    select(-starts_with("Activity")) |> 
    # pivot the table from wide to long format
    pivot_longer(
      cols = starts_with("SoVal"),  # IMPORTANT: this only works if SoVal_other_open was removed
      names_to = "SV_type",         # column for social value type
      values_to = "SV_presence"     # column to indicate presence of social value
    ) |> 
    # add ID for each social value to detach it from more complicated character name
    # NOTE: this is done for convenience
    mutate(SV_ID = case_when(SV_type == "SoVal_Mental.well.being" ~ 1,
                             SV_type == "SoVal_Sensorial.experience" ~ 2,
                             SV_type == "SoVal_Wildlife" ~ 3,
                             SV_type == "SoVal_Habitat" ~ 4,
                             SV_type == "SoVal_Aesthetics" ~ 5,
                             SV_type == "SoVal_Physical.well.being" ~ 6,
                             SV_type == "SoVal_Social" ~ 7,
                             SV_type == "SoVal_Community.importance" ~ 8,
                             SV_type == "SoVal_Spirituality" ~ 9,
                             SV_type == "SoVal_Self.identity" ~ 10,
                             SV_type == "SoVal_Morality" ~ 11,
                             SV_type == "SoVal_Justice" ~ 12,
                             SV_type == "SoVal_Provision" ~ 13,
                             SV_type == "SoVal_Regulation" ~ 14,
                             SV_type == "SoVal_Amenity...Access" ~ 15,
                             SV_type == "SoVal_Other" ~ 16,
                             #this one has to be tested 
                             # NOTE: Maybe this should also just be counted as 'Other' and turned into 'Other'
                             SV_type == "SoVal_other_open_P" ~ 17)) |> 
    # remove any rows only containing absent social values
    # NOTE: these were created by pivot_longer for every non-mapped SV for a given respondent
    filter(SV_presence != 0) |> 
    # recreate SoVal_other_open presence-absence column to then left_join() the given answer character string
    # NOTE: this is done to preserve the individual answers to the "Other" social value
    mutate(SoVal_other_open_P = case_when(SV_ID == 17 ~ 1,
                                          SV_ID != 17 ~ 0)) |> 
    # re-introduce answers for SoVal_other_open to the data 
    left_join(CPH_SoVal_Other_Open, by = c("Respondent.ID", "lat", "lon", "SoVal_other_open_P")) |> 
    # turn all empty rows in SoVal_other_open into NAs for potential future use
    # NOTE: If the social value is not 'Other_open', the previous left_join() matches empty spaces
    mutate(SoVal_other_open = case_when(SoVal_other_open == "" ~ NA,
                                        TRUE ~ SoVal_other_open))
  
  # 3.2.III) Introduce coordinate noise - REQUIRES: sf package
  
  # set seed - for coordinate jitter
  # NOTE: still need to do that
  
  # extract coordinates from WKT as they are one decimal more accurate than the lat and lon values
  
  CPH_SV_sf <- st_as_sf(CPH_SV_long, wkt = "WKT") # extract WKT values
  CPH_SV_coords <- st_coordinates(CPH_SV_sf)      # turn them into coordinates
  
  # 'jitter' coordinates - DECIMAL PLACES: 4 decimals ~ 11m, 5 decimals ~ 1.1m, 6 decimals ~ 0.11m
  CPH_SV_long <- CPH_SV_long |> 
    mutate(
      lat_jittered = CPH_SV_coords[,2] + rnorm(n(), mean = 0, sd = 0.00001), # introduce very tiny jitter on lat
      lon_jittered = CPH_SV_coords[,1] + rnorm(n(), mean = 0, sd = 0.00001), # introduce very tiny jitter on lat
      WKT_2 = sprintf("POINT (%.6f %.6f)", lon_jittered, lat_jittered),      # this is the jittered WKT column
      GeoJSON_2 = paste0('{"geometry":{"type":"Point","coordinates":[',      # this is the jittered GeoJSON column
                         lon_jittered, ',', lat_jittered, ']}}')
    )
  
  
  # remove temporary data sets
  # NOTE: any temporary datasets used to create CPH_SV_long is not needed any longer
  rm(list = c("CPH_SoVal_Other_Open", "CPH_SV_sf", "CPH_SV_coords"))
}


# ============================================
# 4) Data Harmonization
# ============================================

# -------------------------------------
# 4.1) Helsinki
# -------------------------------------

{
  # 4.1a) Reclassify HEL_no_NA social values into new categories
  # NOTE: see reclassification table
  HEL_SV_reclassified <- HEL_no_NA |> 
    mutate(SV_new = as.factor(case_when(SV_type == 0 ~ 1,
                                        SV_type == 1 ~ 2,
                                        SV_type == 3 ~ 3,
                                        SV_type == 4 | SV_type == 5 | SV_type == 6 ~ 4,
                                        SV_type == 7 ~ 5,
                                        SV_type == 8 | SV_type == 12 ~ 6,
                                        SV_type == 10 ~ 7,
                                        SV_type == 11 ~ 8,
                                        T ~ NA))) # set all other not reclassified categories to NA
  
  # 4.1b) remove NAs and unneeded (old) columns
  HEL_SV_reclassified_final <- HEL_SV_reclassified |> 
    select(-SV_type) |>    # remove old classification column
    filter(!is.na(SV_new)) # remove unused categories
  
  # 4.1c) remove temporary not needed data sets
  rm("HEL_SV_reclassified")  
}


# -------------------------------------
# 4.2) Copenhagen
# -------------------------------------

{
  # 4.2a) Reclassify HEL_no_NA social values into new categories
  # NOTE: see reclassification table
  CPH_SV_reclassified <- CPH_SV_long |> 
    mutate(SV_new = as.factor(case_when(SV_ID == 1 | SV_ID == 2 ~ 1, 
                                        SV_ID == 3 | SV_ID == 4 ~ 2,
                                        SV_ID == 5 ~ 3,
                                        SV_ID == 6 ~ 4,
                                        SV_ID == 7 ~ 5,
                                        SV_ID == 8 ~ 6,
                                        SV_ID == 9 ~ 7,
                                        SV_ID == 10 ~ 8,
                                        T ~ NA))) # set all other not reclassified categories to NA
  
  # 4.2b) remove NAs and unneeded (old) columns
  CPH_SV_reclassified_final <- CPH_SV_reclassified |> 
    select(-c("SV_type", "SV_presence", "SV_ID", "SoVal_other_open_P", "SoVal_other_open")) |> # remove old classification
    filter(!is.na(SV_new)) # remove unused categories
  
  # 4.2c) remove temporary not needed data sets
  rm(list = "CPH_SV_reclassified")
}


# ============================================
# 5) Data statistics
# ============================================

# 5.1) Data structure metrics
# NOTE: CPH_raw's differing format makes it's structure strongly deviate from the rest
{
  # Create summary table
  summary_table <- tibble(
    City = c("Helsinki", "Helsinki_no_NA", "Helsinki_reclassified", "Copenhagen", "Copenhagen_long", "Copenhagen_reclassified"),
    # Number of total individual repsondents
    Total_Respond = c(
      HEL_raw |> distinct(Respondent.ID) |> nrow(),
      HEL_no_NA |> distinct(Respondent.ID) |> nrow(),
      HEL_SV_reclassified_final |> distinct(Respondent.ID) |> nrow(),
      CPH_raw |> distinct(Respondent.ID) |> nrow(),
      CPH_SV_long  |> distinct(Respondent.ID) |> nrow(),
      CPH_SV_reclassified_final  |> distinct(Respondent.ID) |> nrow()
    ),
    # Number of respondents that mapped more than 1 point
    Respondents_Mult_Ent = c(
      HEL_raw |> count(Respondent.ID) |> filter(n > 1) |> nrow(),
      HEL_no_NA |> count(Respondent.ID) |> filter(n > 1) |> nrow(),
      HEL_SV_reclassified_final |> count(Respondent.ID) |> filter(n > 1) |> nrow(),
      CPH_raw |> count(Respondent.ID) |> filter(n > 1) |> nrow(),
      CPH_SV_long |> count(Respondent.ID) |> filter(n > 1) |> nrow(),
      CPH_SV_reclassified_final |> count(Respondent.ID) |> filter(n > 1) |> nrow()
    ),
    # Amount of entries by respondents with multiple entries
    Entries_Mult_Res = c(
      HEL_raw |> count(Respondent.ID) |> filter(n > 1) |> summarise(sum(n)) |> pull(),
      HEL_no_NA |> count(Respondent.ID) |> filter(n > 1) |> summarise(sum(n)) |> pull(),
      HEL_SV_reclassified_final |> count(Respondent.ID) |> filter(n > 1) |> summarise(sum(n)) |> pull(),
      CPH_raw |> count(Respondent.ID) |> filter(n > 1) |> summarise(sum(n)) |> pull(),
      CPH_SV_long |> count(Respondent.ID) |> filter(n > 1) |> summarise(sum(n)) |> pull(),
      CPH_SV_reclassified_final |> count(Respondent.ID) |> filter(n > 1) |> summarise(sum(n)) |> pull()
    ),
    # Number of respondents that mapped only 1 point = equal to amount of entries by respondents with one entry
    Respondents_Sing_Ent = c(
      HEL_raw |> count(Respondent.ID) |> filter(n == 1) |>  nrow(),
      HEL_no_NA |> count(Respondent.ID) |> filter(n == 1) |>  nrow(),
      HEL_SV_reclassified_final |> count(Respondent.ID) |> filter(n == 1) |>  nrow(),
      CPH_raw |> count(Respondent.ID) |> filter(n == 1) |> nrow(),
      CPH_SV_long |> count(Respondent.ID) |> filter(n == 1) |>  nrow(),
      CPH_SV_reclassified_final |> count(Respondent.ID) |> filter(n == 1) |>  nrow()
    ),
    # totally mapped pins
    # NOTE: CPH_raw creates 
    Total_points_mapped = c(
      HEL_raw |> nrow(),
      HEL_no_NA |> nrow(),
      HEL_SV_reclassified_final |> nrow(),
      CPH_raw |> nrow(),
      CPH_SV_long |>  nrow(),
      CPH_SV_reclassified_final |> nrow()
    ),
    # number of NAs in social values
    Total_NAs = c(
      HEL_raw |> filter(is.na(SV_type)) |> nrow(),
      HEL_no_NA |> filter(is.na(SV_type)) |> nrow(),
      HEL_SV_reclassified_final |> filter(is.na(SV_new)) |> nrow(),
      NA, # because the raw copenhagen data does not have a presence - absence column
      CPH_SV_long |> filter(is.na(SV_type)) |> nrow(),
      CPH_SV_reclassified_final |> filter(is.na(SV_new)) |> nrow()
    )
  )
  
  # View it
  summary_table
}

# 5.2) Counts for each individual social value
{
  # helsinki old classification
  HEL_SV_old <- HEL_no_NA |> group_by(SV_type) |> count() |> rename(SV_ID = SV_type) # rename Sv column to enable full_join() 
  
  # helsinki new classification
  HEL_SV_new <- HEL_SV_reclassified_final |> mutate(SV_new = as.numeric(SV_new)) |> group_by(SV_new) |> count() |> rename(SV_ID = SV_new) # rename SV column to enable full_join() 
  
  # copenhagen old classification
  CPH_SV_old <- CPH_SV_long |> group_by(SV_ID) |> count()
  
  # copenhagen new classification
  CPH_SV_new <- CPH_SV_reclassified_final |> mutate(SV_new = as.numeric(SV_new)) |> group_by(SV_new) |> count() |> rename(SV_ID = SV_new) # rename SV column

  # Create a list of each individual df
  dfs <- list(
    HEL_SV_old |> rename(HEL_old = n),
    HEL_SV_new |> rename(HEL_new = n),
    CPH_SV_old |> rename(CPH_old = n),
    CPH_SV_new |> rename(CPH_new = n)
  )

  # combine dfs into one df
  SV_counts <- reduce(dfs, full_join, by = "SV_ID") |>
    arrange(SV_ID)
  
  # remove temporary data sets
  rm(list = c("HEL_SV_old", "HEL_SV_new", "CPH_SV_old", "CPH_SV_new", "dfs"))
  
  # view
  SV_counts
}


# ============================================
# 6) Save harmonized data as .csv files
# ============================================

# 6.1) Helsinki
if (!file.exists("Helsinki/Helsinki_SV_reclassified.csv")) {
  write.csv(HEL_SV_reclassified_final, "Helsinki/Helsinki_SV_reclassified.csv")
}

# 6.2) Copenhagen
if (!file.exists("Copenhagen/Copenhagen_SV_reclassified.csv")) {
  write.csv(CPH_SV_reclassified_final, "Copenhagen/Copenhagen_SV_reclassified.csv")
}












