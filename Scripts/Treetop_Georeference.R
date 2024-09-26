library(tidyverse)
library(sf)
library(spatial)
library(rgdal)
library(raster)
library(lidR)
library(sp)
library(nngeo)
library(future)

dir = "Q:\\SNC\\Data\\Big_Tree_GCA\\"
name <- "Big_Tree"
csv_path = paste0(dir, "Census_Data\\#4_bigtree_gca_masterfile.csv")

trees <- paste0("Site_Info\\03_GIS\\shapefiles")

#shapefiles for the below trees were done in a GIS off high res imagery
first_first = st_read(paste0(dir, trees, "\\first_first.shp")) %>% 
  st_geometry()

first_last = st_read(paste0(dir, trees, "\\first_last.shp")) %>% 
  st_geometry()

# column spacing in m 
c_space = 2

# row spacing in m 
r_space = 2

# read in the CSV
csv_B = read.csv(csv_path) %>% 
  dplyr::select(treeID, row, col, ht12, c12)
  
csv_B
# produce a grid using row and column numbers, to be rotated
grid = csv_B %>% 
  mutate(
    x_coord = st_coordinates(first_first)[1] + (col * c_space) - c_space,
    y_coord = st_coordinates(first_first)[2] + (row * r_space) - c_space)

# to sf object
grid_sf = st_as_sf(grid, coords = c("x_coord", "y_coord"), crs = 26910)

# get the coordinates of the first-last
first_last_sf = grid_sf %>% 
  filter(row == 1) %>%
  filter(col == max(.$col)) %>% 
  st_geometry()

# define a FUNCTION which rotates coords for a given angle
rot = function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)

# get the angle of rotation between the true (first, last) and
# the current position of (first, last)
a = st_coordinates(first_last - first_first)
b = st_coordinates(first_last_sf - first_first)

theta = acos( sum(a*b) / ( sqrt(sum(a * a)) * sqrt(sum(b * b)) ) )

# rotate all points 
grid_rot = (st_geometry(grid_sf) - first_first) * rot((theta)) + first_first

# in order to merge the new coordinates and the census info,
# copy the sf object created from the master csv
grid_rot_sf = grid_sf
# then replace its geometry with the rotated coords
st_geometry(grid_rot_sf) = grid_rot
# and set its CRS to the correct one
st_crs(grid_rot_sf) = 26910

#save rotated grid
st_write(obj = grid_rot_sf,
         dsn = paste0(dir, trees, "\\", name, "_grid_manual_bpg.shp"),
         append = FALSE,
         crs = 26910)

plot(grid_rot_sf)


