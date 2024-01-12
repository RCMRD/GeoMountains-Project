# install.packages("raster")
# install.packages("sf")
# install.packages("lwgeom")
# install.packages("mapview")
# install.packages("terra")

library(raster)
library(sf)
library(mapview)

# Set your working directory to the folder containing the TIFF files
setwd("C:/Users/USER/Desktop/Anita2023/speciesModeling/changesSingleSpecies/geoMountains")

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

# Specify the directory containing TIFF files
tiff_dir <- "data/current_1960_1990_TIFF"

# List all TIFF files in the directory
tiff_files <- list.files(tiff_dir, pattern = "\\.tif$", full.names = TRUE)

# Define the target CRS as EPSG:4326 (WGS 84)
target_crs <- CRS("+init=epsg:32637")

# Function to process a single TIFF file
process_tiff_file <- function(tiff_file) {
                                            # Load the TIFF file using the raster package
                                            tif_raster <- raster(tiff_file)

                                            # Reproject the raster to EPSG:4326
                                            tif_raster_reprojected <- projectRaster(tif_raster, crs = target_crs)

                                            # Convert the sf object to a raster mask
                                            raster_mask <- rasterize(clip_shape, tif_raster_reprojected)

                                            # Use the mask to extract the raster data
                                            extracted_raster <- mask(tif_raster_reprojected, raster_mask)

                                            # Get just the file name (without path and extension) of the TIFF file
                                            base_name <- tools::file_path_sans_ext(basename(tiff_file))

                                            # Define the output ASC file path with the desired prefix and folder
                                            asc_file <- file.path(output_dir, paste0("worldclim_KE_", base_name, ".asc"))

                                            # Write the clipped raster data to ASC format using the raster package
                                            writeRaster(extracted_raster, filename = asc_file, format = "ascii", overwrite = TRUE)

                                            # Print a message to confirm the conversion
                                            cat(paste("Extracted and converted", tiff_file, "to", asc_file, "\n"))
                                         }

# Process all TIFF files in the directory
for (tiff_file in tiff_files) {
                                process_tiff_file(tiff_file)
                              }