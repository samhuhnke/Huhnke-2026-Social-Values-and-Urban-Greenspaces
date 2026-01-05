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

The urban canopy cover data can be downloaded from: https://wekeo.copernicus.eu/ 

Instructions on how to download data: https://help.wekeo.eu/en/articles/6416936-how-to-download-wekeo-data 


## INSTRUCTION TO USE THE CODES

Each code has a number assigned to it. This number indicates the order in which the codes are designed to run. It is important to note that each individual code leads to outputs necessary for the next code. Hence it is important to run each code in sequence and to properly store and access each created data subset.

If you have any questions about using the code, feel free to contact me. Please note, that I myself am **NOT ENTITLED** to provide access to the PPGIS data.

### MT_01_Data_Harmonization_Code

This code is intended to harmonize the PPGIS data from Helsinki and Copenhagen. The original social values (13 for Helsinki, 17 for Copenhagen) are hereby reclassified into **8 new** (oftentimes broader) social value categories. Categories that could not sensibly be reclassified were omitted from the 8 new social value categories.

The result of this code are two reclassified .csv-files! 


### MT_02_QGIS - WIP

#### Helsinki Pre-Processing
1. Gathering of CLM canopy cover data for Helsinki and NLS administrative boundaries and water bodies data
2. Extension of NLS water bodies data to cover the entire water area included in the Helsinki administrative boundaries.
3. Combine NLS water bodies with extension of water bodies and dissolve both to create one single water bodies element.
4. Subtract the water area from the administrative boundaries to only retain mainland administrative boundaries.
5. Visually identify any islands within the administrative boundaries of Helsinki that were lost during step 4. 
6. Combine mainland administrative boundaries with the identified islands to establish the entire land administrative boundaries
7. Buffer administrative boundaries with 100m buffer to account for imprecision in PPGIS mapping
8. Clip PPGIS data to the extent of the 100m-buffered administrative boundaries.
9. Buffer administrative boundaries with 200m buffer to account for the influence of canopy cover just outside the 100m buffer-zone on points inside the 100m buffer-zone
10. Clip CLM canopy cover data to the extent of the 200m-buffered administrative boundaries (+for the 100m-buffered boundaries as a backup)


#### Helsinki Land Cover Data
To identify the type of greenspace the CLM Urban Atlas (2018) data was used. The data can be downloaded at: https://land.copernicus.eu/en/products/urban-atlas/urban-atlas-2018#download

1. Download data and add to the QGIS
2. Clip Urban Atlas landcover data to the extent of the 200m-buffered administrative boundary
3. Spatial join PPGIS and Urban Atlas landcover data by using "join attributes by location --> are within --> one-to-one"


#### Helsinki Zonal Statistics - WIP
1. WIP

#### Copenhagen Pre-Processing
1. Gathering of CLM canopy cover data for Copenhagen and DigDag administrative boundaries
2. Extract Copenhagen and Fredriksberg administrative boundaries from DigDag data (Fredriksberg was included as it is surrounded by the Copenhagen municipality)
3. Dissolve Copenhagen and Fredriksberg administrative boundaries into one element
4. Buffer administrative boundaries with 100m buffer to account for imprecision in PPGIS mapping
5. Clip PPGIS data to the extent of the 100m-buffered administrative boundaries
6. Buffer administrative boundaries with 200m buffer to account for the influence of canopy cover just outside the 100m buffer-zone on points inside the 100m buffer-zone
7. Clip CLM canopy cover data to the extent of the 200m-buffered administrative boundaries (+for the the 100m-buffered boundaries as a backup)

#### Copenhagen Land Cover Data
To identify the type of greenspace the CLM Urban Atlas (2018) data was used. The data can be downloaded at: https://land.copernicus.eu/en/products/urban-atlas/urban-atlas-2018#download

1. Download data and add to the QGIS
2. Clip Urban Atlas Land Cover data to the extent of the 200m-buffered administrative boundary
3. Spatial join PPGIS and Urban Atlas landcover data by using "join attributes by location --> are within --> one-to-one"

#### Copenhagen Zonal Statistics - WIP
1. WIP

### MT_03_Zonal_Statistics_Code

This code is intended to explore the relationships between social values and canopy cover. The analysis gets progressively deeper and moves from assessing non-spatial relationships to spatial relationships. 
