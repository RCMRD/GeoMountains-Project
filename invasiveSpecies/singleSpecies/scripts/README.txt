The file labelled "tiffToASC_OneFile.r" is an R script that tested the conversion of a single tiff file to asc format. The data used was called from "current_1960_1990_TIFF" folder and written to "current_1960_1990_ASC" folder under the data section. (The written data should then be deleted after running this process) 


The file labelled "tiffToASC_allFiles.r" is an R script that used a modified version of "1_tiffToASC_OneFile.r" to convert multiple tiff files to asc format. The data used was called from "current_1960_1990_TIFF" folder and written to "current_1960_1990_ASC" folder under the data section.


The file labelled "cleaningOccurrenceData_Acacia.r" is an R script that;
- Read in the combined occurrence data in shapefile format found under the folder labelled "Occurrence_data"
- Reconstructed the .xml file
- Subsetted the data to the select species and created a shapefile which was then stored as "acacia" under the "Occurrence_data" folder in the data section
- Created presence data and wrote the data to a CSV file labelled "acacia_raw" 
- Cleaned the data appropriately and wrote it to a CSV file labelled "acacia" under the data section.


The file labelled "cleaningOccurrenceData_Opuntia.r" is an R script that;
- Read in the combined occurrence data in shapefile format found under the folder labelled "Occurrence_data"
- Reconstructed the .xml file
- Subsetted the data to the select species and created a shapefile which was then stored as "opuntia" under the "Occurrence_data" folder in the data section
- Created presence data and wrote the data to a CSV file labelled "opuntia_raw" 
- Cleaned the data appropriately and wrote it to a CSV file labelled "opuntia" under the data section.


An initial file labelled "biomod2_video_single_species_modelling.r" was an R script that modified the initial script provided for BIOMOD by replacing deprecated packages & using updated libraries and parameters.
The text file labelled "README_1.0" shows the changes that were made to the "biomod2_video_single_species_modelling.r" script.


The file labelled "biomod2_single_species_modelling_1.0.r" is an R script that models Opuntia Invasive species. It first uses the provided 8 bioclimatic variables, then uses the mean Variable Importance to select 4 of the most important bioclimatic variables.


The file labelled "biomod2_single_species_modelling_1.1.r" is an R script that models Acacia Invasive species. It first uses the provided 8 bioclimatic variables, then uses the mean Variable Importance to select 4 of the most important bioclimatic variables.

The file labelled "biomod2_single_species_modelling_2.0.r" is an R script that models Opuntia Invasive species using biophysical variables only. It first uses the provided variables, writes asc files from the loaded TIFF files then uses the mean Variable Importance to select 3 of the most important biophysical variables.


The file labelled "biomod2_single_species_modelling_2.1.r" is an R script that models Acacia Invasive species using biophysical variables only. It first uses the provided variables, then uses the mean Variable Importance to select 3 of the most important biophysical variables.


The file labelled "biomod2_single_species_modelling_3.0.r" is an R script that models Opuntia Invasive species using both bioclimatic and biophysical variables. It first uses all the provided variables, then uses the mean Variable Importance to select 10 of the most important variables.


The file labelled "biomod2_single_species_modelling_3.1.r" is an R script that models Acacia Invasive species using both bioclimatic and biophysical variables. It first uses all the provided variables, then uses the mean Variable Importance to select 10 of the most important variables.