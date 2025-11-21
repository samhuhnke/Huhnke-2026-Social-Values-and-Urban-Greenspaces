# AUTHOR: Sam Huhnke, M.Sc. University of Helsinki
#
### DATA HARMONIZATION
#
# This code is intended to harmonize the Helsinki and Copenhagen CO-CARBON data sets. The harmonized data sets will 
# then be used for further spatial analyses in QGIS.
# 
# NOTE: The data used for this analysis is owned by the CO-CARBON research project. To gain access to the data,
# contact the CO-CARBON research project. Any use of the data and this code must be in accordance with the CO-CARBON 
# guidelines.



# Clear environment if needed
rm(list = ls(globalenv()))

# Set working directory to whatever needed
setwd("C:/Users/samhu/Desktop/Code Projects/Huhnke_2026/DATA/PPGIS Data")

# Load necessary packages
library(tidyverse)
library(sf) 
library(ggplot2)

# Load data sets
HEL_raw <- read.csv("Helsinki/Dataset_2_Spatial_important.csv", sep = ";")
CPH_raw <- read.csv("Copenhagen/COCARBON_CPH_PlaceInNature.csv", sep = ",")
colnames(HEL_raw)
colnames(CPH_raw)

# set seed - for coordinate jitter



#### Data wrangling #####

## Remove NAs from HEL_raw
{
  HEL_no_NA <- HEL_raw |> 
  filter(!is.na(SV_type))
}

## Create CPH_SV data with similar format as HEL_SV: data = each SV has own row
{
  # save other open file to later match in long table based on ID and position  
  CPH_SoVal_Other_Open <- CPH_raw |> 
    # create SoVal_other_open presence-absence column
    mutate(SoVal_other_open_P = case_when(SoVal_other_open == "" ~ 0,
                                          SoVal_other_open != "" ~ 1 )) |> 
    select(Respondent.ID, lat, lon, SoVal_other_open_P, SoVal_other_open)
  
  
  # create long table, so all the social values are listed in one column equal to Helsinki data set
  # ISSUE: multiple social values for each pin --> mapped for EXACT SAME location. 
  # POT. FIX: introduce very tiny amount of noise for one coordinate, so the points "jitter" on map and can still be identified
  CPH_SV_long <- CPH_raw |> 
    # create SoVal_other_open presence-absence column
    mutate(SoVal_other_open_P = case_when(SoVal_other_open == "" ~ 0,
                                          SoVal_other_open != "" ~ 1 )) |> 
    # temporarily remove SoVal_other_open data
    select(-SoVal_other_open) |> 
    # remove activity values to keep table easier managable 
    select(-starts_with("Activity")) |> 
    pivot_longer(
      cols = starts_with("SoVal"),  # or use col1:col10, starts_with("prefix"), etc.
      names_to = "SV_type",     # column for social value type
      values_to = "SV_presence"           # column to indicate presence of social value
    ) |> 
    # add ID for each social value to detach it from more complicated character name
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
                             SV_type == "SoVal_other_open_P" ~ 17)) |> 
    # remove any rows only containing absent SV --> these were created by the pivot for each respondent for every non-mapped SV
    filter(SV_presence != 0) |> 
    # recreate SoVal_other_open presence-absence column to later left_join the Other answers
    mutate(SoVal_other_open_P = case_when(SV_ID == 17 ~ 1,
                                          SV_ID != 17 ~ 0)) |> 
    # reintroduce answers for SoVal_other_open
    left_join(CPH_SoVal_Other_Open, by = c("Respondent.ID", "lat", "lon", "SoVal_other_open_P")) |> 
    # turn all empty rows in SoVal_other_open into NAs for potential future use
    mutate(SoVal_other_open = case_when(SoVal_other_open == "" ~ NA,
                                        TRUE ~ SoVal_other_open))
  
  ## introduce jitter for coordinates - REQUIRES: sf package
  
  # extract coordinates from WKT as they are one decimal more accurate than the lat and lon values
  CPH_SV_sf <- st_as_sf(CPH_SV_long, wkt = "WKT")
  CPH_SV_coords <- st_coordinates(CPH_SV_sf)
  
  # jitter coordinates - DECIMAL PLACES: 4 decimals ~ 11m, 5 decimals ~ 1.1m, 6 decimals ~ 0.11m
  CPH_SV_long <- CPH_SV_long |> 
    mutate(
      lat_jittered = CPH_SV_coords[,2] + rnorm(n(), mean = 0, sd = 0.00001), # introduced very tiny jitter, so spatial analysis remains accurate 
      lon_jittered = CPH_SV_coords[,1] + rnorm(n(), mean = 0, sd = 0.00001), # but jitter TOO SMALL FOR GOOD VISUAL INTERPRETATION
      WKT_2 = sprintf("POINT (%.6f %.6f)", lon_jittered, lat_jittered),
      GeoJSON_2 = paste0('{"geometry":{"type":"Point","coordinates":[', 
                         lon_jittered, ',', lat_jittered, ']}}')
    )
  
  
  # remove temporary data setss
  rm(list = c("CPH_SoVal_Other_Open", "CPH_SV_sf", "CPH_SV_coords"))
}

## Reclassify HEL_no_NA social values into new categories
{
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
  
  # remove NAs and unneeded (old) columns
  HEL_SV_reclassified_final <- HEL_SV_reclassified |> 
    select(-SV_type) |> 
    filter(!is.na(SV_new)) # remove not used categories
  
  # entries for each new class and remaining total entries
  HEL_SV_reclassified_final |> group_by(SV_new) |> count()
  HEL_SV_reclassified_final |> nrow()
  
  # remove temporary not needed data sets
  rm("HEL_SV_reclassified", "HEL_no_NA")
}

## Reclassify CPH_SV social values into new categories
{
  CPH_SV_reclassified <- CPH_SV_long |> 
    mutate(SV_new = as.factor(case_when(SV_ID == 1 | SV_ID == 2 ~ 1, 
                                        SV_ID == 3 | SV_ID == 4 ~ 2,
                                        SV_ID == 5 ~ 3,
                                        SV_ID == 6 ~ 4,
                                        SV_ID == 7 ~ 5,
                                        SV_ID == 8 ~ 6,
                                        SV_ID == 9 ~ 7,
                                        SV_ID == 10 ~ 8,
                                        T ~ NA
    )))
  # remove NAs and unneeded (old) columns
  CPH_SV_reclassified_final <- CPH_SV_reclassified |> 
    select(-c("SV_type", "SV_presence", "SV_ID", "SoVal_other_open_P", "SoVal_other_open")) |> 
    filter(!is.na(SV_new)) # remove not used categories
  
  # entries for each new class and remaining total entries
  CPH_SV_reclassified_final |> group_by(SV_new) |> count()
  CPH_SV_reclassified_final |> nrow() 
  
  # remove temporary not needed data sets
  rm(list = "CPH_SV_reclassified", "CPH_SV_long")
  
}

## Safe both datasets as .csv
# Helsinki
if (!file.exists("Helsinki/Helsinki_SV_reclassified.csv")) {
  write.csv(HEL_SV_reclassified_final, "Helsinki/Helsinki_SV_reclassified.csv")
}

# Copenhagen
if (!file.exists("Copenhagen/Copenhagen_SV_reclassified.csv")) {
write.csv(CPH_SV_reclassified_final, "Copenhagen/Copenhagen_SV_reclassified.csv")
}

#### General metrics #####
{
  # Create summary table
  summary_table <- tibble(
    City = c("Helsinki", "Copenhagen"),
    # Number of total individual repsondents
    Total_Respondents = c(
      HEL_raw |> distinct(Respondent.ID) |> nrow(),
      CPH_raw |> distinct(Respondent.ID) |> nrow()
    ),
    # Number of respondents that mapped more than 1 point
    Respondents_Multiple_Entries = c(
      HEL_raw |> count(Respondent.ID) |> filter(n > 1) |> nrow(),
      CPH_raw |> count(Respondent.ID) |> filter(n > 1) |> nrow()
    ),
    # Amount of entries by respondents with multiple entries
    Entries_Multiple_Respondents = c(
      HEL_raw |> count(Respondent.ID) |> filter(n > 1) |> summarise(sum(n)) |> pull(),
      CPH_raw |> count(Respondent.ID) |> filter(n > 1) |> summarise(sum(n)) |> pull()
    ),
    # Number of respondents that mapped only 1 point = equal to amount of entries by respondents with one entry
    Respondents_Single_Entry = c(
      HEL_raw |> count(Respondent.ID) |> filter(n == 1) |>  nrow(),
      CPH_raw |> count(Respondent.ID) |> filter(n == 1) |> nrow()
    ),
    # totally mapped SV without NAs
    Total_mapped_SV = c(
      HEL_raw |> nrow(),
      CPH_SV_long |> nrow()
    ),
    # number of NAs in socual values
    Total_NAs = c(
      HEL_raw |> filter(is.na(SV_type)) |> nrow(),
      CPH_SV_long |> filter(is.na(SV_type)) |> nrow()
    )
  )
  
  # View it
  summary_table
}

#### General metrics - no NAs #####
## General information on respondents and number of entries
{
  # create HEL_no_NA again 
  HEL_no_NA <- HEL_raw |> 
    filter(!is.na(SV_type))
  
  # Create summary table
  summary_table <- tibble(
    City = c("Helsinki", "Copenhagen"),
    # Number of total individual repsondents
    Total_Respondents = c(
      HEL_no_NA |> distinct(Respondent.ID) |> nrow(),
      CPH_raw |> distinct(Respondent.ID) |> nrow()
    ),
    # Number of respondents that mapped more than 1 point
    Respondents_Multiple_Entries = c(
      HEL_no_NA |> count(Respondent.ID) |> filter(n > 1) |> nrow(),
      CPH_raw |> count(Respondent.ID) |> filter(n > 1) |> nrow()
    ),
    # Amount of entries by respondents with multiple entries
    Entries_Multiple_Respondents = c(
      HEL_no_NA |> count(Respondent.ID) |> filter(n > 1) |> summarise(sum(n)) |> pull(),
      CPH_raw |> count(Respondent.ID) |> filter(n > 1) |> summarise(sum(n)) |> pull()
    ),
    # Number of respondents that mapped only 1 point = equal to amount of entries by respondents with one entry
    Respondents_Single_Entry = c(
      HEL_no_NA |> count(Respondent.ID) |> filter(n == 1) |>  nrow(),
      CPH_raw |> count(Respondent.ID) |> filter(n == 1) |> nrow()
    ),
    # totally mapped SV without NAs
    Total_mapped_SV = c(
      HEL_no_NA |> nrow(),
      CPH_SV_long |> nrow()
    ),
    # number of NAs in socual values
    Total_NAs = c(
      HEL_no_NA |> filter(is.na(SV_type)) |> nrow(),
      CPH_SV_long |> filter(is.na(SV_type)) |> nrow()
    )
  )
  
  # View it
  summary_table
}
## Number of entries for each social value category BEFORE RECLASSIFICATION
{
  # create temporary Helsinki SV count data set
  HEL_SV_n <- HEL_no_NA |> 
    group_by(SV_type) |> 
    count() |> 
    rename(SV_ID = SV_type) # rename Sv column to enable full_join() 
  
  # create temporary Copenhage SV count data set
  CPH_SV_n <- CPH_SV_long |> 
    group_by(SV_ID) |> 
    count()
  
  #combine into new table with both counts
  combined_SV <- full_join(
    HEL_SV_n |> rename(Helsinki = n),
    CPH_SV_n |> rename(Copenhagen = n),
    by = "SV_ID"
  ) |> 
    arrange(SV_ID)
  
  combined_SV
  
  # remove temporary data sets
  rm(list = c("HEL_SV_n", "CPH_SV_n"))
}






######### CO-OCCRRENCE - NOT RELEVANT #######################
## CPH SV CO-OCCURRENCE 
{
  # Create co-occurrence dataset for SV
  {
    CPH_SV_co <- CPH_SV_long |> 
      select(Respondent.ID, lat, lon, WKT, SV_ID)
    #left join SV again 
    CPH_SV_co <- CPH_SV_co |> 
      left_join(CPH_SV_co, by = c("Respondent.ID", "lat", "lon", "WKT"), relationship = "many-to-many")
  }
  
  # Absolute co-occurrence heat map
  {
    CPH_SV_co |>
      count(SV_ID.x, SV_ID.y) |>
      ggplot(aes(x = SV_ID.y, y = SV_ID.x, fill = n)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "white", high = "darkblue", na.value = "grey90") +
      scale_y_reverse(breaks = 1:17) +
      scale_x_continuous(position = "top", breaks = 1:17) +
      labs(
        title = "Co-occurrence Heatmap: SV_ID by SV_ID",
        x = "SV ID",
        y = "SV ID",
        fill = "Count"
      ) +
      theme_minimal() +
      theme(
        panel.grid = element_blank()
      )
    }
  
  # Correlation heat map
  {
    correlation_map <- CPH_SV_co |>
      group_by(SV_ID.x, SV_ID.y) |>
      summarise(
        count = n(),
        .groups = "drop"
      ) |>
      # Calculate expected frequency under independence
      left_join(
        CPH_SV_co |> count(SV_ID.x, name = "sv_total.x"),
        by = "SV_ID.x"
      ) |>
      left_join(
        CPH_SV_co |> count(SV_ID.y, name = "sv_total.y"),
        by = "SV_ID.y"
      ) |>
      mutate(
        total = nrow(CPH_SV_co),
        expected = (sv_total.x * sv_total.y) / total,
        residual = (count - expected) / sqrt(expected)  # Pearson residual
      ) |>
      ggplot(aes(x = SV_ID.y, y = SV_ID.x, fill = residual)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(
        low = "blue", 
        mid = "white", 
        high = "red",
        midpoint = 0,
        limits = c(-4, 4),  # Cap the scale: >2 statistically significant, >3 strong association, 4 and higher very strong association
        oob = scales::squish  # Values outside limits get squished to limits
      ) +
      scale_y_reverse(breaks = 1:17) +
      scale_x_continuous(position = "top", breaks = 1:17) +
      labs(
        title = "Association Strength: SV_ID by SV_ID",
        x = "SV ID",
        y = "SV ID",
        fill = "Pearson\nResidual"
      ) +
      theme_minimal() +
      theme(panel.grid = element_blank())
    
    correlation_map
  }
}
## CPH SV - ACT CO-OCCURRENCE
{
  # Row-based relative co-occurrence --> NOT REALLY TELLING, JUST HERE IN CASE SIMILAR CODE IS NEEDED
  {
    co_occurrence_perc <- CPH_SV_ACT |>
      count(SV_ID, Activity_ID) |>
      group_by(SV_ID) |>
      mutate(percentage = round(n / sum(n) * 100, 1)) |>
      select(-n) |> 
      pivot_wider(names_from = Activity_ID, values_from = percentage)
    
    co_occurrence_perc
  }
  
  # Absolute occurrence heat map
  {
    CPH_SV_ACT |>
      count(SV_ID, Activity_ID) |>
      ggplot(aes(x = Activity_ID, y = SV_ID, fill = n)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "white", high = "darkblue", na.value = "grey90") +
      scale_y_reverse(breaks = 0:17) +
      scale_x_continuous(position = "top", breaks = 1:22) +
      labs(
        title = "Co-occurrence Heatmap: SV_ID by Activity_ID",
        x = "Activity ID",
        y = "SV ID",
        fill = "Count"
      ) +
      theme_minimal() +
      theme(
        panel.grid = element_blank()
      )
  }
  
  # Correlation heat map
  {
    correlation_map <- CPH_SV_ACT |>
      group_by(SV_ID, Activity_ID) |>
      summarise(
        count = n(),
        .groups = "drop"
      ) |>
      # Calculate expected frequency under independence
      left_join(
        CPH_SV_ACT |> count(SV_ID, name = "sv_total"),
        by = "SV_ID"
      ) |>
      left_join(
        CPH_SV_ACT |> count(Activity_ID, name = "activity_total"),
        by = "Activity_ID"
      ) |>
      mutate(
        total = nrow(CPH_SV_ACT),
        expected = (sv_total * activity_total) / total,
        residual = (count - expected) / sqrt(expected)  # Pearson residual
      ) |>
      ggplot(aes(x = Activity_ID, y = SV_ID, fill = residual)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(
        low = "blue", 
        mid = "white", 
        high = "red",
        midpoint = 0,
        limits = c(-4, 4),  # Cap the scale: >2 statistically significant, >3 strong association, 4 and higher very strong association
        oob = scales::squish  # Values outside limits get squished to limits
      ) +
      scale_y_reverse(breaks = 0:17) +
      scale_x_continuous(position = "top", breaks = 1:22) +
      labs(
        title = "Association Strength: SV_ID by Activity_ID",
        x = "Activity ID",
        y = "SV ID",
        fill = "Pearson\nResidual"
      ) +
      theme_minimal() +
      theme(panel.grid = element_blank())
    
    correlation_map
  }

}
## HEL SV CO-OCCURRENCE - NEEDS DIFFERENT APPROACH BECAUSE MAPPING HAPPENED DIFFERENTLY 
## -> PINS ARE AT DIFFERENT LOCATIONS
{
  # Create co-occurrence dataset for SV
  {
    HEL_SV_co <- HEL_raw |> 
      select(Respondent.ID, SV_type) |> 
      filter(!is.na(SV_type)) #otherwise the correlation calculation introduces NAs
    #left join SV again 
    HEL_SV_co <- HEL_SV_co |> 
      left_join(HEL_SV_co, by = c("Respondent.ID"), relationship = "many-to-many")
    head(HEL_SV_co)
  }
  
  # Absolute co-occurrence heat map
  {
    HEL_SV_co |>
      count(SV_type.x, SV_type.y) |>
      ggplot(aes(x = SV_type.y, y = SV_type.x, fill = n)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "white", high = "darkblue", na.value = "grey90") +
      scale_y_reverse(breaks = -1:12) +
      scale_x_continuous(position = "top", breaks = -1:12) +
      labs(
        title = "Co-occurrence Heatmap: SV_ID by SV_ID",
        x = "SV ID",
        y = "SV ID",
        fill = "Count"
      ) +
      theme_minimal() +
      theme(
        panel.grid = element_blank()
      )
  }
  
  # Correlation heat map
  {
    correlation_map <- HEL_SV_co |>
      group_by(SV_type.x, SV_type.y) |>
      summarise(
        count = n(),
        .groups = "drop"
      ) |>
      # Calculate expected frequency under independence
      left_join(
        HEL_SV_co |> count(SV_type.x, name = "sv_total.x"),
        by = "SV_type.x"
      ) |>
      left_join(
        HEL_SV_co |> count(SV_type.y, name = "sv_total.y"),
        by = "SV_type.y"
      ) |>
      mutate(
        total = nrow(HEL_SV_co),
        expected = (as.numeric(sv_total.x) * as.numeric(sv_total.y)) / total,
        residual = (count - expected) / sqrt(expected)  # Pearson residual
      ) |>
      ggplot(aes(x = SV_type.y, y = SV_type.x, fill = residual)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(
        low = "blue", 
        mid = "white", 
        high = "red",
        midpoint = 0,
        limits = c(-4, 4),  # Cap the scale: >2 statistically significant, >3 strong association, 4 and higher very strong association
        oob = scales::squish  # Values outside limits get squished to limits
      ) +
      scale_y_reverse(breaks = -1:12) +
      scale_x_continuous(position = "top", breaks = -1:12) +
      labs(
        title = "Association Strength: SV_ID by SV_ID",
        x = "SV ID",
        y = "SV ID",
        fill = "Pearson\nResidual"
      ) +
      theme_minimal() +
      theme(panel.grid = element_blank())
    
    correlation_map
  }
}


######### CPH ACTIVITY DATA - NOT RELEVANT ###################
## Create CPH_ACT data with similar format as CPH_SV and HEL_SV: data = each ACT has own row
{
  # save other open file to later match in long table based on ID and position  
  CPH_Activity_Other_Open <- CPH_raw |> 
    # create Activity_other_open presence-absence column
    mutate(Activity_other_open_P = case_when(Activity_other_open == "" ~ 0,
                                             Activity_other_open != "" ~ 1 )) |> 
    select(Respondent.ID, lat, lon, Activity_other_open_P, Activity_other_open)
  
  
  head(CPH_Activity_Other_Open)
  
  # create long table, so all the social values are listed in one column equal to Helsinki data set
  # ISSUE: multiple social values for each pin --> mapped for EXACT SAME location. 
  # POT. FIX: introduce very tiny amount of noise for one coordinate, so the points "jitter" on map and can still be identified
  CPH_ACT_long <- CPH_raw |> 
    # create Activity_other_open presence-absence column
    mutate(Activity_other_open_P = case_when(Activity_other_open == "" ~ 0,
                                             Activity_other_open != "" ~ 1 )) |> 
    # temporarily remove Activity_other_open data
    select(-Activity_other_open) |> 
    # remove activity values to keep table easier managable 
    select(-starts_with("SoVal")) |> 
    pivot_longer(
      cols = starts_with("Activity"),  # or use col1:col10, starts_with("prefix"), etc.
      names_to = "Activity_type",     # column for social value type
      values_to = "Activity_presence"           # column to indicate presence of social value
    ) |> 
    # add ID for each activity value to detach it from more complicated character name
    mutate(Activity_ID = case_when(Activity_type == "Activity_Going.for.a.walk..less.than.3.hours." ~ 1,
                                   Activity_type == "Activity_Hiking..more.than.3.hours." ~ 2,
                                   Activity_type == "Activity_Walking.the.dog" ~ 3,
                                   Activity_type == "Activity_Meeting.people" ~ 4,
                                   Activity_type == "Activity_Doing.sports..e.g...Running..football.playing..golf." ~ 5,
                                   Activity_type == "Activity_Mountain.biking" ~ 6,
                                   Activity_type == "Activity_Cycling" ~ 7,
                                   Activity_type == "Activity_Playing..climbing.trees..building.sand.castles" ~ 8,
                                   Activity_type == "Activity_Swimming.and.other.water.sports" ~ 9,
                                   Activity_type == "Activity_Barbecuing..going.for.a.picnic" ~ 10,
                                   Activity_type == "Activity_Relaxing..sunbathing.and.reading" ~ 11,
                                   Activity_type == "Activity_Observing.nature.and.natural.photography" ~ 12,
                                   Activity_type == "Activity_Experiencing.and.connecting.with.nature" ~ 13,
                                   Activity_type == "Activity_Hunting.or.fishing" ~ 14,
                                   Activity_type == "Activity_Horseback.riding" ~ 15,
                                   Activity_type == "Activity_Gardening.and.growing.food" ~ 16,
                                   Activity_type == "Activity_Foraging..e.g...Mushrooms..flowers..berries." ~ 17,
                                   Activity_type == "Activity_For.work..I.work.here." ~ 18,
                                   Activity_type == "Activity_Taking.care.of.nature..e.g...volunteering.work..collecting.trash." ~ 19,
                                   Activity_type == "Activity_Spending.the.night.in.nature" ~ 20,
                                   Activity_type == "Activity_other" ~ 21,
                                   Activity_type == "Activity_other_open_P" ~ 22)) |> 
    # remove any rows only containing absent ACT --> these were created by the pivot for each respondent for every non-mapped SV
    filter(Activity_presence != 0) |> 
    # recreate Activity_other_open presence-absence column to later left_join the Other answers
    mutate(Activity_other_open_P = case_when(Activity_ID == 22 ~ 1,
                                             Activity_ID != 22 ~ 0)) |> 
    # reintroduce answers for Activity_other_open
    left_join(CPH_Activity_Other_Open, by = c("Respondent.ID", "lat", "lon", "Activity_other_open_P")) |> 
    # turn all empty rows in Activity_other_open into NAs for potential future use
    mutate(Activity_other_open = case_when(Activity_other_open == "" ~ NA,
                                           TRUE ~ Activity_other_open))
  
  
  ## introduce jitter for coordinates - REQUIRES: sf package - PROBABLY JUST CREATE COMBINED DF FOR SV AND ACT FOR
  ## LATER VISUAL INTERPRETATION
  
  # extract coordinates from WKT as they are one decimal more accurate than the lat and lon values
  CPH_ACT_sf <- st_as_sf(CPH_ACT_long, wkt = "WKT")
  CPH_ACT_coords <- st_coordinates(CPH_ACT_sf)
  
  # jitter coordinates - DECIMAL PLACES: 4 decimals ~ 11m, 5 decimals ~ 1.1m, 6 decimals ~ 0.11m
  CPH_ACT_long <- CPH_ACT_long |> 
    mutate(
      lat_jittered = CPH_ACT_coords[,2] + rnorm(n(), mean = 0, sd = 0.00001), # introduced very tiny jitter, so spatial analysis remains accurate 
      lon_jittered = CPH_ACT_coords[,1] + rnorm(n(), mean = 0, sd = 0.00001), # but jitter TOO SMALL FOR GOOD VISUAL INTERPRETATION
      WKT_2 = sprintf("POINT (%.6f %.6f)", lon_jittered, lat_jittered),
      GeoJSON_2 = paste0('{"geometry":{"type":"Point","coordinates":[', 
                         lon_jittered, ',', lat_jittered, ']}}')
    )
  
  # save as .csv
  #write.csv(CPH_ACT_long, "Copenhagen/CPH_ACT.csv")
  
  # remove temporary data setss
  rm(list = c("CPH_Activity_Other_Open", "CPH_ACT_sf", "CPH_ACT_coords"))
}
## Join CPH_SV and CPH_ACT together
{
  CPH_SV_ACT <- CPH_SV_long |> 
    left_join(CPH_ACT_long |> select(Respondent.ID, WKT, Activity_ID, Activity_other_open_P, Activity_other_open), 
              by = c("Respondent.ID", "WKT"), relationship = "many-to-many")
  
  # remove temporary not needed data sets
  rm("CPH_ACT_long")
}



















