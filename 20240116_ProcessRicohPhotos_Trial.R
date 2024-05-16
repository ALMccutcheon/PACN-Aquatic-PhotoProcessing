# Libraries
library(here)
library(tidyverse)
library(magick)
library(exiftoolr)
install_exiftool()

# Working directory
setwd(here("testphotos"))

# Import photos
file.names<-list.files(pattern="*.JPG")

img <- image_read(file.names)

t <- exif_call(args=c("-s","Park"),file.names[1])
metadata <- exif_read(file.names,tags=c("Park","Direction","Site","Photographer","PoolName","GPSLatitude","GPSLongitude","CreateDate","GPSDestBearing"))
img_ann <- image_annotate(img,metadata[1,2],size = 250,
                gravity = "northwest",
                font = "Helvetica",
                color = "white",
                strokecolor = "black",
                weight = 9000)

image_write(img_ann, path = here("Out.jpg"), format = "jpg")
