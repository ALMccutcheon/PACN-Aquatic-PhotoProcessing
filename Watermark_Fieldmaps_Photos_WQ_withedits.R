#Adjustments needed for photos added manually to data collection layer online.
#Needed to adjust date for filename and watermark and to increase font size on watermark.

library(PACNAquaticWaterQuality)
library(here)
library(dplyr)

#### USER INPUT ####
location <- here("geodatabase") # Enter the folder name where the gdb is stored.
name <- "20240615_WQ_PACN_Field_Images.gdb" # Enter the name of the gdb file.
layer <- "PACN_2024_Water_Quality_Points_Photos"# Enter the layer name - this is whatever description you gave when downloading

# Enter Sampling Event Info
park_code <- "AMME"
loc_type <- "FW"
select_months <- as.vector(c(4,5,6))
folder <- "watermarked/20240615_AMME_FW_BB"

#------------------------------------------------------------------------------

table <- gdb_table_wq(gdb_name=name,gdb_location=location,gdb_layer=layer)
table_2 <- table %>%
  filter(unit_code==park_code,Location_Type==loc_type)

#--------------------------------------------------
# Made a few edits to the table for AMME. This can be deleted for other uses
brackish_sites <- c("FAMME01_fw","FAMME02_fw","FAMME03_fw","FAMME09_fw")
freshwater_sites <- c("FAMME04_fw","FAMME05_fw","FAMME06_fw","FAMME07_fw","FAMME08_fw")
table_2 <- table_2 %>%
  mutate(Location_Type=ifelse(station_id%in%brackish_sites,"BB","FW"),
         Location_Name=ifelse(station_id%in%brackish_sites,"Constructed Wetland","Natural Wetland"),
         station_id=ifelse(station_id%in%brackish_sites,stringr::str_replace(station_id,"fw","bb"),station_id),
         date_time_photo=as.character(as.POSIXct(date_time_photo,format = "%Y-%m-%d %H:%M:%S")+lubridate::hours(20)),
         date_time_file="20240616")

apply(X = table_2, MARGIN = 1, FUN = watermark_wq, new_folder = folder)
#---------------------------------------------------

# This function is edited to change photo date and increase the water mark size from photos added to AGOL online rather than through fieldmaps.

watermark_wq_edited <- function (x, new_folder) 
{
  p.dt_photo <- "2024-04-10"
  p.title <- paste(x["unit_code"], "Water Quality Monitoring", 
                   sep = " ")
  p.direction <- x["photo_subject"]
  p.locname <- x["Location_Name"]
  p.type <- x["Location_Type"]
  p.site <- x["station_id"]
  p.user <- x["Editor"]
  p.tag <- x["tag"]
  p.filename <- paste("20240410", p.site, "WQ", p.type, 
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
                                                sep = "\n"), p.type == "MR" ~ paste(p.title, p.locname, 
                                                                                    sep = "\n"), p.type == "AP" ~ paste(p.title), p.type == 
                           "BB" ~ paste(p.title, p.locname, sep = "\n"), TRUE ~ 
                           NA)
  img.x2 <- magick::image_annotate(img.x2, nw, size = 75, 
                                   gravity = "northwest", font = "Helvetica", color = "white", 
                                   strokecolor = "black", weight = 900)
  n <- paste(p.direction)
  img.x2 <- magick::image_annotate(img.x2, n, size = 75, gravity = "north", 
                                   font = "Helvetica", color = "white", strokecolor = "black", 
                                   weight = 900)
  ne <- dplyr::case_when(p.type == "FW" ~ paste(p.site), p.type == 
                           "MR" ~ paste(p.site), p.type == "AP" ~ paste(p.site, 
                                                                        p.locname, sep = "\n"), p.type == "BB" ~ paste(p.site), 
                         TRUE ~ NA)
  img.x2 <- magick::image_annotate(img.x2, ne, size = 75, 
                                   gravity = "northeast", font = "Helvetica", color = "white", 
                                   strokecolor = "black", weight = 900)
  sw <- paste(p.dt_photo)
  img.x2 <- magick::image_annotate(img.x2, sw, size = 75, 
                                   gravity = "southwest", font = "Helvetica", color = "white", 
                                   strokecolor = "black", weight = 900)
  se <- paste(p.lat, p.long, sep = "\n")
  img.x2 <- magick::image_annotate(img.x2, se, size = 75, 
                                   gravity = "southeast", font = "Helvetica", color = "white", 
                                   strokecolor = "black", weight = 900)
  magick::image_write(img.x2, path = out.name, format = "jpg")
}

#------------------------------------------------------------------------------
# This function uses the above function to watermark rather than the one written into the package

process_watermark_wq_edited<- function (gdb_name, gdb_location, gdb_layer, park, loctype, 
          select_months = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 
          output_folder = "watermarked") 
{
  t <- gdb_table_wq(gdb_name, gdb_location, gdb_layer)
  t_select <- t %>% dplyr::filter(unit_code == park, Location_Type == 
                                    loctype) %>% dplyr::mutate(file_month = lubridate::month(CreationDate)) %>% 
    dplyr::filter(file_month %in% select_months)
  apply(X = t_select, MARGIN = 1, FUN = watermark_wq_edited, new_folder = output_folder)
}
#------------------------------------------------------------------------------
#### RUN WATERMARK FUNCTION ####
# This should work as is. No need to update.

process_watermark_wq_edited(gdb_name=name,gdb_location=location,gdb_layer=layer,
                     park = unit_code, loctype = loc_type,
                     select_months = select_months, output_folder = folder)
