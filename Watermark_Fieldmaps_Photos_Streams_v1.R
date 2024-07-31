# Title: Processing WQ Images from Fieldmaps Geodatabase
#
# Author: Amanda McCutcheon (amanda_l_mccutcheon@nps.gov)
# Date: May 23, 2024
#
# 1) Navigate to https://nps.maps.arcgis.com/home/item.html?id=20b20a767ffb4d608f292f5ba4619208. You may need to log in to nps.maps.arcgis.com.
# 2) Click "Export Data" in the menu on the right, and Select "Export to FGDB".
# 3) Give the file an appropriate title and export to a folder in your content. Click "Export". This may take several minutes.
# 4) Once exported, click "Download" and download the file, saving it in the "geodatabase" folder within this project.
# 5) It will download as a zip file. Extract the file by right clicking and selecting "Extract All". Save the extracted file to the "geodatabase" folder.
# 6) Rename the geodatabase to something like "YYYYMMDD_WQ_PACN_Field_Images.gdb"
# 7) Update the USER INPUT section in this script. 
# 8) Use the Source button above or highlight the whole script and press ctrl-Enter.
# 9) Contact Amanda if you get an error.
#
# NOTE: The first time you run this you will need to install the three packages listed.
# Removed the # from the next line to install the required packages.
# install.packages("here","dplyr","PACNAquaticWaterQuality")

library(PACNAquaticWaterQuality)
library(here)
library(dplyr)

#### USER INPUT ####
location <- here("geodatabase") # Enter the folder name where the gdb is stored.
name <- "20240731_WQ_PACN_Field_Images.gdb" # Enter the name of the gdb file.
layer <- "PACN_WQ_2024_Sampling_v05"# Enter the layer name - this should stay the same unless Mark updates the collection layer title.

# Enter Sampling Event Info
unit_code <- c("HALE")
loc_type <- "FW"
select_months <- as.vector(c(7,8,9))
folder <- "watermarked/20240722_HALE"

#------------------------------------------------------------------------------

#### RUN WATERMARK FUNCTION ####
# This should work as is. No need to update.

process_watermark_streams(gdb_name=name,gdb_location=location,gdb_layer=layer,
                     park = unit_code, loctype = loc_type,
                     select_months = select_months, output_folder = folder)
