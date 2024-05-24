library(pacnvegetation)

protocol <- "FTPC"
geodatabase <- "KAHO_test_photo_layer.gdb"
path <- "C:/Users/JJGross/OneDrive - DOI/Documents/ArcGIS/Projects/KAHO_test_photo_layer/"
layer <- "test"

# look at the table output to see if things are working correctly
look <- process_photos(AGOL_Layer = protocol,
                       gdb_name = geodatabase,
                       gdb_location = path,
                       gdb_layer = layer,
                       return_table = TRUE)

# run it
process_photos(AGOL_Layer = protocol,
               gdb_name = geodatabase,
               gdb_location = path,
               gdb_layer = layer,
               return_table = FALSE)
