# Title: Processing WQ Images from Fieldmaps Geodatabase
#
# Author: Amanda McCutcheon (amanda_l_mccutcheon@nps.gov)
# Date: May 23, 2024
#
# Update the USER INPUT section. 
# Then use the Source button above or highlight the whole script and press ctrl-Enter.
# Contact Amanda if you get an error.
#
# the first time you run this you will need to install the three packages listed.
# Removed the # from the next line to install the required packages.
# install.packages("here","dplyr","PACNAquaticWaterQuality")

library(PACNAquaticWaterQuality)
library(here)
library(dplyr)

#### USER INPUT ####
location <- here("geodatabase") # Enter the folder name where the gdb is stored.
name <- "8c01990f-faa9-47ea-a5d6-67c8cfdcd4f4.gdb" # Enter the name of the gdb file.
layer <- "PACN_2024_Water_Quality_Points_Photos"# Enter the layer name - this is whatever description you gave when downloading

# Enter Sampling Event Info
unit_code <- "KAHO"
loc_type <- "AP"
select_months <- as.vector(c(4,5,6))
folder <- "watermarked/20240509_KAHO_AP"

#------------------------------------------------------------------------------

#### RUN WATERMARK FUNCTION ####
# This should work as is. No need to update.

process_watermark_wq(gdb_name=name,gdb_location=location,gdb_layer=layer,
                     park = unit_code, loctype = loc_type,
                     select_months = select_months, output_folder = folder)
