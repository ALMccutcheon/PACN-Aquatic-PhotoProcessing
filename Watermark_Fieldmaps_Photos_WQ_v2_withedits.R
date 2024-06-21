# Author: Amanda McCutcheon amanda_l_mccutcheon@nps.gov
# Date: May 23, 2024
# Adapted from a script by Jacob Gross jacob_gross@nps.gov (April 12, 2022)

# Install these packages if not already installed (if installed, skip this line)
install.packages(c("here", "tidyverse", "sf", "magick"))

# Load the packages:
library(here) # Helps working with relative file paths
library(dplyr) # numerous packages that make R programming more readable
library(sf) # simple features spatial package for R
library(magick) # photo editing packages (for watermarking photos)
library(stringr)

# Return current working folder
here()

# Enter the folder name where the gdb is stored
gdb_location <- here("geodatabase")

# Enter the name of the geodatabase (replace my_geodatabase.gdb with correct name):
gdb_name <- "20240615_WQ_PACN_Field_Images.gdb"


# Enter the layer name - this is whatever description you gave when downloading
gdb_layer <- "PACN_2024_Water_Quality_Points_Photos"# path to geodatabase:


# This function creates X, Y, Z columns from the sfc_point object in the data.frame
sfc_as_cols <- function(x, names = c("x","y")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  ret <- sf::st_coordinates(x)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}

gdb_table_wq <- function(gdb_name, gdb_location, gdb_layer){

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

lubridate::tz(joined_table$CreationDate)<-"Pacific/Honolulu"

# Make a date_time column appropriate for file names
joined_table <- joined_table %>%
  dplyr::mutate(CreationDate=case_when(unit_code%in%c("AMME","WAPA") ~ lubridate::with_tz(CreationDate,tzone="Pacific/Guam"),
                          unit_code%in%c("ALKA","KAHO","KALA","HALE","PUHE","PUHO","HAVO") ~ lubridate::with_tz(CreationDate,tzone="Pacific/Honolulu"),
                          unit_code%in%c("NPSA") ~ lubridate::with_tz(CreationDate,tzone="Pacific/Samoa")))%>%
  dplyr::mutate(date_time_photo = as.character(CreationDate)) %>%
  dplyr::mutate(date_time_file = lubridate::date(CreationDate))%>%
  dplyr::mutate(date_time_file = str_replace_all(date_time_file,"-",""))%>%
  cbind(coords)%>% #add the x,y,z coordinates
  dplyr::mutate(hash = str_c(date_time_file,station_id,photo_subject)) %>% #creates a field called hash which has the fields that will be in filename
  dplyr::group_by(hash) %>% 
  dplyr::mutate(duplication_id = seq(n())-1) %>% #checks for duplication of the filename hash field and add a sequence number for duplicates
  dplyr::ungroup ()%>%
  dplyr::mutate(tag = ifelse(duplication_id==0,"",paste0("_",duplication_id)), #replaces duplication id of zero with nothing
         Location_Name = ifelse(is.na(Location_Name)," ",Location_Name)) #replace NA in Location_Name with a space

return(joined_table)
}

t <- gdb_table_wq(gdb_name, gdb_location, gdb_layer)


lubridate::tz(t$CreationDate)<-"Pacific/Honolulu"

lubridate::tz(t$CreationDate)

t$CreationDate

lubridate::with_tz(t["CreationDate"],tzone="Pacific/Guam")
##########################################################################

# Create a R function to apply to each photo (i.e. each row of the joined table)
watermark <- function(x, new_folder) {
  # Get watermarking info from the table (x)
  p.dt_photo <- x["date_time_photo"]
  p.title<-paste(x["unit_code"],"Water Quality Monitoring",sep=" ")
  p.direction<- x["photo_subject"]
  p.locname<-x["Location_Name"]
  p.type<-x["Location_Type"]
  p.site <- x["station_id"]
  p.user <- x["Editor"]
  p.tag <- x["tag"]
  p.filename<-paste(x["date_time_file"],p.site,"WQ",p.type,p.direction,sep="_")
  p.filename.tag<-paste0(p.filename,p.tag) #added tag separately because it already includes the "_"
  p.lat <- x$Y
  p.lat <- round(p.lat,6)
  p.long <- x$X
  p.long <- round(p.long,6)

  # Create paths and folders to save each photo
  dir.create(here(new_folder), recursive = TRUE, showWarnings = FALSE )
  out.path <- here(new_folder)
  out.name <- file.path(out.path, paste0(p.filename.tag,".jpg"))
  print(out.name)

  # Load photo
  image.x <- x["DATA"] %>%
    purrr::pluck(1)


  img.x <- magick::image_read(image.x)

  # Apply auto-orientation "image_orient()" which tries to infer the correct orientation
  #' from the Exif data.
  img.x2 <- magick::image_orient(img.x)


  # ---- Watermark photo -----

  # northwest corner
  nw <- dplyr::case_when(p.type=="FW" ~ paste(p.title,p.locname,sep="\n"),
                  p.type=="MR" ~ paste(p.title,p.locname,sep="\n"),
                  p.type=="AP" ~ paste(p.title),
                  p.type=="BB" ~ paste(p.title,p.locname,sep="\n"),
                  TRUE ~ NA)
  img.x2 <- magick::image_annotate(img.x2, nw,
                          size = 25,
                          gravity = "northwest",
                          font = "Helvetica",
                          color = "white",
                          strokecolor = "black",
                          weight = 900)
  # top center
  n <- paste(p.direction)
  img.x2 <- magick::image_annotate(img.x2, n,
                           size = 25,
                           gravity = "north",
                           font = "Helvetica",
                           color = "white",
                           strokecolor = "black",
                           weight = 900)
  
  # northeast corner
  ne <- dplyr::case_when(p.type=="FW" ~ paste(p.site),
                  p.type=="MR" ~ paste(p.site),
                  p.type=="AP" ~ paste(p.site,p.locname,sep="\n"),
                  p.type=="BB" ~ paste(p.site),
                  TRUE ~ NA)
  img.x2 <- magick::image_annotate(img.x2, ne,
                          size = 25,
                          gravity = "northeast",
                          font = "Helvetica",
                          color = "white",
                          strokecolor = "black",
                          weight = 900)
  # southwest corner
  sw <- paste(p.dt_photo)
  img.x2 <- magick::image_annotate(img.x2, sw,
                          size = 25,
                          gravity = "southwest",
                          font = "Helvetica",
                          color = "white",
                          strokecolor = "black",
                          weight = 900)
  
  # southeast corner
  se <- paste(p.lat,p.long,sep="\n")
  img.x2 <- magick::image_annotate(img.x2, se,
                           size = 25,
                           gravity = "southeast",
                           font = "Helvetica",
                           color = "white",
                           strokecolor = "black",
                           weight = 900)

  # Save photo
  magick::image_write(img.x2, path = out.name, format = "jpg")

}

# Run the function above on the "joined_table"
joined_table_select <- t%>%
  dplyr::filter(unit_code=="PUHO")
apply(X = joined_table_select, MARGIN = 1, FUN = watermark, new_folder = "watermarked")
# open "watermarked" folder in working path to see results

