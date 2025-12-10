# HUHNKE (2026) - Social Values and Urban Canopy Cover  

This work was produced has part of a Master's Thesis at the University of Helsinki. 
**Student**: Sam Huhnke. 
**Supervisors**: 1. Prof. Christopher Raymond, 
                 2. Dr. Oriol Antunez Garcia, 
                 3. Dr. Jussi Lampinnen.


## CONTENT

This repository contains the code used for the analysis of relations between social values and urban canopy cover. The social value data was established through a PPGIS survey as part of the *Co-Carbon project*. The specific data contains information on Copenhagen, DK, and Helsinki, FI. The urban canopy cover data is publically available data from the *Copernicus Landuse Monitoring*. 


## DATA ACCESSIBILITY

The PPGIS social value data is not publically available and can only be accessed after prior agreement with the *Co-Carbon project*, the *University of Copenhagen*, and the *University of Helsinki* (or official and entitled representatives of each entity). 

The urban canopy cover data can be downloaded from: INSERT LINK


## INSTRUCTION TO USE THE CODES

Each code has a number assigned to it. This number indicates the order in which the codes are designed to run. It is important to note that each individual code leads to outputs necessary for the next code. Hence it is important to run each code in sequence and to properly store and access each created data subset.

If you have any questions about using the code, feel free to contact me. Please note, that I myself am **NOT ENTITLED** to provide access to the PPGIS data.

### MT_01_Data_Harmonization_Code

This code is intended to harmonize the PPGIS data from Helsinki and Copenhagen. The original social values (13 for Helsinki, 17 for Copenhagen) are hereby reclassified into **8 new** (oftentimes broader) social value categories. Categories that could not sensibly be reclassified were omitted from the 8 new social value categories.

The result of this code are two reclassified .csv-files! 

### QGIS Analysis

Between MT_01 and MT_02 the data is processed in QGIS. The steps taken are:
1) Cropping of the data to the administrative boundaries for both Copenhagen and Helsinki (the crop entails a buffer of around 100m)
2) Cropping of the data to the land area (as this analysis is interested in the effect of canopy cover)
3) The creation of buffers around each pin. These buffer areas are used as a unit of analysis.

### MT_02_Zonal_Statistics_Code

This code is intended to explore the relationships between social values and canopy cover. The analysis gets progressively deeper and moves from assessing non-spatial relationships to spatial relationships. 
