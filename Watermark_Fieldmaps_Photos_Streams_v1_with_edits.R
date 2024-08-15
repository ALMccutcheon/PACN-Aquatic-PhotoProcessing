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
name <- "20240815_WQ_PACN_Field_Images_Old_Layer.gdb" # Enter the name of the gdb file.
layer <- "PACN_WQ_2024_Sampling_v05"# Enter the layer name - this should stay the same unless Mark updates the collection layer title.

# Enter Sampling Event Info
unit_code <- c("HALE")
loc_type <- "FW"
select_months <- as.vector(c(7,8,9))
folder <- "watermarked/20240722_HALE_new"

#------------------------------------------------------------------------------

#### RUN WATERMARK FUNCTION ####
# This should work as is. No need to update.

process_watermark_streams(gdb_name=name,gdb_location=location,gdb_layer=layer,
                     park = unit_code, loctype = loc_type,
                     select_months = select_months, output_folder = folder)

#------------------------------------------------------------------------------
# This section is edited to fix the date field after the layer needed to be republished.
# The Comments field contains the correct dates.

gdb_table_wq_edit <- function(gdb_name, gdb_location, gdb_layer){
  
  # Create path for gdb from location and name
  gdb_path <- paste0(gdb_location, "/", gdb_name)
  
  # Layer File with attributes
  attributes <- sf::read_sf(gdb_path, gdb_layer)
  
  # Attachments Table containing photos
  layer_attach <- paste0(gdb_layer,"__ATTACH")
  attachments <- sf::read_sf(gdb_path, layer_attach)
  
  
  # Join the layer data with the _attach table
  joined_table <- attachments %>%
    dplyr::left_join(attributes, by = c("REL_GLOBALID" = "GlobalID"))
  
  # Apply function to get coordinates for the joined_table
  df <- joined_table$SHAPE
  coords<- sf::st_coordinates(df)
  
  # Make a date_time column appropriate for file names
  joined_table <- joined_table %>%
    dplyr::mutate(date_time_photo = as.character(created_date)) %>%
    dplyr::mutate(date_time_file = lubridate::date(created_date))%>%
    dplyr::mutate(date_time_file = stringr::str_replace_all(date_time_file,"-",""))%>%
    cbind(coords)%>% #add the x,y,z coordinates
    dplyr::mutate(hash = stringr::str_c(date_time_file,station_id,photo_subject)) %>% #creates a field called hash which has the fields that will be in filename
    dplyr::group_by(hash) %>% 
    dplyr::mutate(duplication_id = seq(n())-1) %>% #checks for duplication of the filename hash field and add a sequence number for duplicates
    dplyr::ungroup ()%>%
    dplyr::mutate(tag = ifelse(duplication_id==0,"",paste0("_",duplication_id)), #replaces duplication id of zero with nothing
                  Location_Name = ifelse(is.na(Location_Name)," ",Location_Name)) #replace NA in Location_Name with a space
  
  return(joined_table)
}


watermark_streams_edit <- function (x, new_folder) 
{
  p.dt_photo <- x["date_time_photo"]
  p.title <- paste(x["unit_code"], "Stream Monitoring", sep = " ")
  p.direction <- x["photo_subject"]
  p.locname <- x["Location_Name"]
  p.transect <- x["transect"]
  p.type <- x["Location_Type"]
  p.site <- x["station_id"]
  p.user <- x["Editor"]
  p.tag <- x["tag"]
  p.filename <- paste(x["date_time_file"], p.site, p.transect, 
                      p.direction, sep = "_")
  p.filename.tag <- paste0(p.filename, p.tag)
  p.lat <- x$Y
  p.lat <- round(p.lat, 6)
  p.long <- x$X
  p.long <- round(p.long, 6)
  dir.create(here::here(new_folder), recursive = TRUE, showWarnings = FALSE)
  out.path <- here::here(new_folder)
  out.name <- file.path(out.path, paste0(p.filename.tag, ".jpg"))
  print(out.name)
  image.x <- x["DATA"] %>% purrr::pluck(1)
  img.x <- magick::image_read(image.x)
  img.x2 <- magick::image_orient(img.x)
  nw <- dplyr::case_when(p.type == "FW" ~ paste(p.title, p.locname, 
                                                sep = "\n"), TRUE ~ NA)
  img.x2 <- magick::image_annotate(img.x2, nw, size = 25, 
                                   gravity = "northwest", font = "Helvetica", color = "white", 
                                   strokecolor = "black", weight = 900)
  n <- paste(p.direction)
  img.x2 <- magick::image_annotate(img.x2, n, size = 25, gravity = "north", 
                                   font = "Helvetica", color = "white", strokecolor = "black", 
                                   weight = 900)
  ne <- dplyr::case_when(p.type == "FW" ~ paste(p.site, p.transect, 
                                                sep = "\n"), TRUE ~ NA)
  img.x2 <- magick::image_annotate(img.x2, ne, size = 25, 
                                   gravity = "northeast", font = "Helvetica", color = "white", 
                                   strokecolor = "black", weight = 900)
  sw <- paste(p.dt_photo)
  img.x2 <- magick::image_annotate(img.x2, sw, size = 25, 
                                   gravity = "southwest", font = "Helvetica", color = "white", 
                                   strokecolor = "black", weight = 900)
  se <- paste(p.lat, p.long, sep = "\n")
  img.x2 <- magick::image_annotate(img.x2, se, size = 25, 
                                   gravity = "southeast", font = "Helvetica", color = "white", 
                                   strokecolor = "black", weight = 900)
  magick::image_write(img.x2, path = out.name, format = "jpg")
}


process_watermark_streams_edit <- function (gdb_name, gdb_location, gdb_layer, park, loctype, 
                                            select_months = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 
                                            output_folder = "watermarked") 
{
  t <- gdb_table_wq_edit(gdb_name, gdb_location, gdb_layer)
  t_select <- t %>% dplyr::filter(unit_code == park, Location_Type == 
                                    loctype, transect != "WQ") %>% dplyr::mutate(file_month = lubridate::month(created_date)) %>% 
    dplyr::filter(file_month %in% select_months)
  apply(X = t_select, MARGIN = 1, FUN = watermark_streams_edit, 
        new_folder = output_folder)

}

# Run with Edits ----
process_watermark_streams_edit(gdb_name=name,gdb_location=location,gdb_layer=layer,
                          park = unit_code, loctype = loc_type,
                          select_months = select_months, output_folder = folder)

# Testing ----
t <- gdb_table_wq_edit(gdb_name=name, gdb_location=location, gdb_layer=layer)
t_select <- t %>% dplyr::filter(unit_code == unit_code, Location_Type == 
                                  loc_type, transect != "WQ") %>% dplyr::mutate(file_month = lubridate::month(created_date)) %>% 
  dplyr::filter(file_month %in% select_months)%>%
  dplyr::mutate(station_id_new = case_when(station_id=="THALE08_fw" ~ "FHALE08_fw",
                                       station_id=="THALE09_fw" ~ "FHALE09_fw",
                                       station_id=="THALE02_fw" ~ "FHALE07_fw",
                                       !station_id%in%c("THALE09_fw","THALE08_fw","THALE02_fw") ~ station_id))%>%
  dplyr::mutate(station_id=station_id_new)%>%
  dplyr::select(-station_id_new)

t_select$station_id_new

apply(X = t_select, MARGIN = 1, FUN = watermark_streams_edit, 
      new_folder = folder)
