# Author: Jacob Gross jacob_gross@nps.gov
# Date: April 12th 2022

# Install these packages if not already installed (if installed, skip this line)
install.packages(c("here", "tidyverse", "sf", "magick"))

# Load the packages:
library(here) # Helps working with relative file paths
library(tidyverse) # numerous packages that make R programming more readable
library(sf) # simple features spatial package for R
library(magick) # photo editing packages (for watermarking photos)

# Return current working folder
here()

# Create a folder called "geodatabase" for the geodatabase
dir.create(here("geodatabase"))
gdb_folder <- here("geodatabase")

# Now go to file explorer and move or copy the geodatabase (.gdb)
# to the "geodatabase" folder just created:
gdb_folder
# The geodatabase needs to contain both the layer file and
# the attachments table (__ATTACH) containing the photos

# Enter the name of the geodatabase (replace my_geodatabase.gdb with correct name):
gdb <- "8c01990f-faa9-47ea-a5d6-67c8cfdcd4f4.gdb"

# path to geodatabase:
gdb_path <- here(gdb_folder, gdb)

# display layers inside geodatabase:
st_layers(gdb_path)

# Photos are stored in the "__ATTACH table" and will have geometry_type == NA
# Copy and paste both the layer name and the _ATTACH table below
# (replace my_layer and my_layer__ATTACH with correct name):
gdb_layer_name <- "PACN_2024_Water_Quality_Points_Photos"
gdb_attach_name <- "PACN_2024_Water_Quality_Points_Photos__ATTACH"

# read the layer:
gdb_layer <- sf::read_sf(gdb_path, gdb_layer_name)
gdb_layer

# read the layer__ATTACH table:
gdb_attach <- sf::read_sf(gdb_path, gdb_attach_name)
gdb_attach

#test<-sf::read_sf(gdb_path)
#test["SHAPE"]

# The "DATA" Column in the _attach table contains the raw binary (BLOB) for each photo
# This is what the first raw photo in the table looks like:
gdb_attach$DATA[[1]]

# Note that the class of the DATA column (containing the BLOB) is a list
class(gdb_attach$DATA)
str(gdb_attach$DATA)

# (optional) If you tell R that the field is a BLOB, then you can view table in Rstudio
# gdb_attach$DATA <- blob::as_blob(gdb_attach$DATA)
# ... However, if using automation the apply() function doesn't like blobs
# so make sure to change column back to a list before feeding into apply()
# gdb_attach$DATA <- as.list(gdb_attach$DATA)

# also note that the objects within the list are recognized as "raw" by R
gdb_attach$DATA[[1]]
class(gdb_attach$DATA[[1]])
str(gdb_attach$DATA[[1]])
here()
# select the first photo to use as demo
test_photo <- unlist(gdb_attach$DATA[[1]])
str(test_photo)

# Test watermarking capabilities:
# this is the test photo:
str(test_photo)

# read the photo using "magick" package function "image_read()"

# Load photo
img <- image_read(test_photo)
img
print(image_attributes(img))


# ---- Watermark photo -----

# create labels in the corner of photos. Examples with text options:

# northwest corner
nw1 <- "PARK UNIT"
nw2 <- "sampling frame"
nw <- paste(nw1, nw2, sep = "\n")
img <- image_annotate(img, nw,
                        size = 25,
                        gravity = "northwest",
                        font = "Helvetica",
                        color = "white",
                        strokecolor = "black",
                        weight = 900)
img

# northeast corner
ne <- paste("PROTOCOL", "subject", sep = "\n")
img <- image_annotate(img, ne,
                        size = 25,
                        gravity = "northeast",
                        font = "Helvetica",
                        color = "white",
                        strokecolor = "black",
                        weight = 900)
# southwest corner
sw <- paste("YYYYMMDD")
img <- image_annotate(img, sw,
                        size = 25,
                        gravity = "southwest",
                        font = "Helvetica",
                        color = "white",
                        strokecolor = "black",
                        weight = 900)

# Save photo
image_write(img, path = here("watermarked_demo.jpg"), format = "jpg")
# Open "watermarked_demo.jpg" to see watermarked photo



# - Automation -----------------------------

# To automate process, create a function that
# pulls information straight from the layer file

# First step is to "join" or "relate" the layer data with the _attach table
joined_table <- gdb_attach %>%
  left_join(gdb_layer, by = c("REL_GLOBALID" = "GlobalID"))
head(joined_table)

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

# Apply function to get coordinates for the joined_table
df <- joined_table$SHAPE
coords<- sf::st_coordinates(df)

# Make a date_time column appropriate for file names
joined_table <- joined_table %>%
  mutate(date_time_photo = as.character(CreationDate)) %>%
  mutate(date_time_file = lubridate::date(CreationDate))%>%
  mutate(date_time_file = str_replace_all(date_time_file,"-",""))%>%
  cbind(coords) #add the x,y,z coordinates

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
  p.dt_file<-paste(x["date_time_file"],p.site,"WQ",p.type,p.direction,sep="_")
  p.filename <- x["station_id"]
  p.lat <- x$Y
  p.lat <- round(p.lat,6)
  p.long <- x$X
  p.long <- round(p.long,6)

  # Create paths and folders to save each photo
  dir.create(here(new_folder), recursive = TRUE, showWarnings = FALSE )
  out.path <- here(new_folder)
  out.name <- file.path(out.path, paste0(p.dt_file,".jpg"))
  print(out.name)

  # Load photo
  image.x <- x["DATA"] %>%
    purrr::pluck(1)


  img.x <- image_read(image.x)

  # Apply auto-orientation "image_orient()" which tries to infer the correct orientation
  #' from the Exif data.
  img.x2 <- magick::image_orient(img.x)


  # ---- Watermark photo -----

  # northwest corner
  nw <- case_when(p.type=="FW" ~ paste(p.title,p.locname,sep="\n"),
                  p.type=="MR" ~ paste(p.title,p.locname,sep="\n"),
                  p.type=="AP" ~ paste(p.title),
                  p.type=="BB" ~ paste(p.title,p.locname,sep="\n"),
                  TRUE ~ NA)
  img.x2 <- image_annotate(img.x2, nw,
                          size = 25,
                          gravity = "northwest",
                          font = "Helvetica",
                          color = "white",
                          strokecolor = "black",
                          weight = 900)
  # top center
  n <- paste(p.direction)
  img.x2 <- image_annotate(img.x2, n,
                           size = 25,
                           gravity = "north",
                           font = "Helvetica",
                           color = "white",
                           strokecolor = "black",
                           weight = 900)
  
  # northeast corner
  ne <- case_when(p.type=="FW" ~ paste(p.site),
                  p.type=="MR" ~ paste(p.site),
                  p.type=="AP" ~ paste(p.site,p.locname,sep="\n"),
                  p.type=="BB" ~ paste(p.site),
                  TRUE ~ NA)
  img.x2 <- image_annotate(img.x2, ne,
                          size = 25,
                          gravity = "northeast",
                          font = "Helvetica",
                          color = "white",
                          strokecolor = "black",
                          weight = 900)
  # southwest corner
  sw <- paste(p.dt_photo)
  img.x2 <- image_annotate(img.x2, sw,
                          size = 25,
                          gravity = "southwest",
                          font = "Helvetica",
                          color = "white",
                          strokecolor = "black",
                          weight = 900)
  
  # southeast corner
  se <- paste(p.lat,p.long,sep="\n")
  img.x2 <- image_annotate(img.x2, se,
                           size = 25,
                           gravity = "southeast",
                           font = "Helvetica",
                           color = "white",
                           strokecolor = "black",
                           weight = 900)

  # Save photo
  image_write(img.x2, path = out.name, format = "jpg")

}

# Run the function above on the "joined_table"
joined_table_select <- joined_table%>%filter(unit_code=="KALA")
apply(X = joined_table_select, MARGIN = 1, FUN = watermark, new_folder = "watermarked")
# open "watermarked" folder in working path to see results

