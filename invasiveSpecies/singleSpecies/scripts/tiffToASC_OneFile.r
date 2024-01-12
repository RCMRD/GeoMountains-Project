## Author: Anita Carolyne Orera
## Date: 2023-11-01
## Pre-requisites: Ensure you have a data folder, an outputs folder, a scripts folder and an empty working directory
## Required data are available at:
## https://github.com/RCMRD/GeoMountains-Project/tree/b4b8bd8958a415903983d7b2bccdc59ebba2587f/invasiveSpecies/singleSpecies/data


# install.packages("raster")
# install.packages("sf")
# install.packages("lwgeom")
# install.packages("mapview")
# install.packages("terra")

library(raster)
library(sf)
library(lwgeom)
library(mapview)
library(terra)

# Set your working directory to the folder containing the TIFF files
setwd("path/to/geoMountains")

# Specify the path to your shapefile for clipping
shapefile_path <- "data/conservancyArea"

# Read the shapefile using the sf package
clip_shape <- st_read(shapefile_path)

# Define the output directory for ASC files
output_dir <- "data/current_1960_1990_ASC"

# Create the output directory if it doesn't exist
if (!dir.exists(output_dir)) {
                                dir.create(output_dir, recursive = TRUE)
                             }

# Specify the TIFF file you want to process
tiff_file <- "data/current_1960_1990_TIFF/bio1.tif"

# Load the TIFF file using the raster package
tif_raster <- raster(tiff_file)

# Define the target CRS as EPSG:4326 (WGS 84)
target_crs <- CRS("+init=epsg:32637")

# Reproject the raster to EPSG:4326
tif_raster_reprojected <- projectRaster(tif_raster, crs = target_crs)

# --- You can add the clipping process, but ensure you rename accordingly
# ----- Clip the reprojected raster using the shapefile
# clipped_raster <- crop(tif_raster_reprojected, clip_shape)

# Convert the sf object to a raster mask
raster_mask <- rasterize(clip_shape, tif_raster_reprojected)

# Use the mask to extract the raster data
extracted_raster <- mask(tif_raster_reprojected, raster_mask)

# Do an initial visualization
mapview(extracted_raster) #Visualizes the asc raster
mapview(extracted_raster) + mapview(clip_shape) #Visualizes the asc raster and shapefile

# Get just the file name (without path and extension) of the TIFF file
base_name <- tools::file_path_sans_ext(basename(tiff_file))

# Define the output ASC file path with the desired prefix and folder
asc_file <- file.path("data/current_1960_1990_ASC", paste0("worldclim_KE_", base_name, ".asc"))

# Write the clipped raster data to ASC format using the raster package
writeRaster(extracted_raster, filename = asc_file, format = "ascii")

# Print a message to confirm the conversion
cat(paste("Extracted and converted", tiff_file, "to", asc_file, "\n"))

########################################################################################################
# Define the target CRS using the EPSG code (EPSG:32637 for WGS 84 / UTM zone 37N)
target_crs <- "+init=epsg:32637"

########################################################################################################
# Specify the ASC file to be read in to confirm our extraction & projection process was correct
asc_file <- "data/worldclim_KE/worldclim_KE_2050_BC26_Bio1.asc"

# Read the called ASC file
asc_raster <- raster(asc_file)

# Do a comparison visualization
mapview(asc_raster) #Visualizes the asc raster
mapview(asc_raster) + mapview(clip_shape) #Visualizes the asc raster and shapefile