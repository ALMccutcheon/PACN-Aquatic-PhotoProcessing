library(PACNAquaticWaterQuality)
library(here)
library(dplyr)

# Enter the folder name where the gdb is stored
location <- here("geodatabase")

# Enter the name of the geodatabase (replace my_geodatabase.gdb with correct name):
name <- "8c01990f-faa9-47ea-a5d6-67c8cfdcd4f4.gdb"


# Enter the layer name - this is whatever description you gave when downloading
layer <- "PACN_2024_Water_Quality_Points_Photos"

# Enter Sampling Event Info
unit_code <- "PUHE"
loc_type <- "BB"
select_months <- as.vector(c(4,5,6))
folder <- "watermark_test"


####

process_watermark_wq(gdb_name=name,gdb_location=location,gdb_layer=layer,
                     park = unit_code, loctype = loc_type,
                     select_months = select_months, output_folder = folder)
