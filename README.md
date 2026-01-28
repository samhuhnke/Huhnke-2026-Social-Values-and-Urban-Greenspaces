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

The Urban Atlas 2018 was used to identify different land-uses for both Copenhagen and Helsinki. The data and descriptions can be found under: https://land.copernicus.eu/en/products/urban-atlas/urban-atlas-2018 


## INSTRUCTION TO USE THE CODES

Each code has a number assigned to it. This number indicates the order in which the codes are designed to run. It is important to note that each individual code leads to outputs necessary for the next code. Hence it is important to run each code in sequence and to properly store and access each created data subset.

If you have any questions about using the code, feel free to contact me. Please note, that I myself am **NOT ENTITLED** to provide access to the PPGIS data.

-----------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------

### MT_01_Data_Harmonization_Code

This code is intended to harmonize the PPGIS data from Helsinki and Copenhagen. The original social values (13 for Helsinki, 17 for Copenhagen) are hereby reclassified into **8 new** (oftentimes broader) social value categories. Categories that could not sensibly be reclassified were omitted from the 8 new social value categories.

The result of this code are two reclassified .csv-files! 

-----------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------

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

NOTE: Step 3 does not match all PPGIS points with a land use and thus introduces 107 unmatched points with *code_2018 = NA*. Most of theses unmatched points are in the water, where there is no Urban Atlas data. Some amount of points are within the Viikki Bay area.

#### Helsinki Zonal Statistics
1. Establish 50m radial buffer zone around each point of the PPGIS (*Vector Geometry --> Buffer* + *Segments = 10*)
2. Calculate mean canopy cover based on 200m-buffered canopy cover (*Raster analysis --> Zonal Statistics*)
3. Export as .csv-file

-----------------------------------------------------------------------------------------------------

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

NOTE: Step 3 does not match all PPGIS points with a land use and thus introduces 6 unmatched points with *code_2018 = NA*. All of these points are located on two islands off the coast, both of which still fall within the administrative area of Copenhagen.

#### Copenhagen Zonal Statistics - WIP
1. Establish 50m radial buffer zone around each point of the PPGIS (*Vector Geometry --> Buffer* + *Segments = 10*)
2. Calculate mean canopy cover based on 200m-buffered canopy cover (*Raster analysis --> Zonal Statistics*)
3. Export as .csv-file


-----------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------

### MT_03_General_Area_Metrics

This code is intended to describe the land-uses for both Copenhagen and Helsinki as defined by the CLMS Urban Atlas 2018. This is done to enable later interpretation of the results as the prevalence of different land uses enables different uses by citizens. This code also includes a pie chart to visualize the different land uses prevalent.

Steps taken in QGIS:
1. Establish area for all land-use categories in QGIS using the *Open field calculator* and the command *$area*. This creates area estimates in m2 for each land-use polygon.
2. Export the data as a .csv-file. 

IMPORTANT: Note that the Urban Atlas 2018 data was clipped to the extend of the 200m-buffered administrative boundaries! The total area is hence 10-20% bigger than the pure administrative boundary and includes water bodies (AT LEAST AS OF RIGHT NOW: 19.01.2026)

Steps in R-Code:
1. Create reclassification tables --> new classification based on overarching categories of the Urban Atlas 2018 + additional 9th category for natural open spaces without vegetation!
2. Calculate total area per overarching category
3. Create dataset that contains code_2018, category_2018, and area values

-----------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------

### MT_04_Data_Analysis

This code is intended to explore the relationships between social values and canopy cover. The analysis gets progressively deeper and moves from assessing non-spatial relationships to spatial relationships. 

Section 6: Multilayered analysis process

1. Do mapped SV in general co-occur with differing levels of canopy cover? This assesses all social values on a global level without any subsets.

A: For Helsinki, no, but according to the GAM results it is reasonable to assume there is are patterns in the data not revealed on a global level. 
A: For Copenhagen, yes.

2. Do mapped SV co-occur with differing levels of canopy cover if the data is seperated by land-use into forest, greenspace, and other? This assesses whether there are relationships between social values and canopy cover within land-uses.

A: For Helsinki. Yes.
A: For Copenhagen. Yes.

3. Based on 2., are there differences between different social values? This assesses whether each social value reacts the same way or whether there are differences between social values. Here, the code first assesses social values individually before it compares them to each other.

A: For Helsinki. Yes.
A: For Copenhagen. Yes.


**Current: Assess spatial structure via spatial permutation**
