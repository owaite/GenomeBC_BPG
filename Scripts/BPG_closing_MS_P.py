# -*- coding: utf-8 -*-
"""
Created on Fri Apr 19 13:04:42 2024

@author: jaking
"""

import os, Metashape
# select project variables- name, date, chosen calibration panel numbers, site perimeter shape location

name = "Big_Tree_GCA"
date = "2024_08_27"
# enter chosen calibration panel numbers
cal_pan_1 = "2"
cal_pan_2 = "1002"

#path to site perimeter shapefile in WGS84
site_perim = "Q:\\SNC\\Data\\Big_Tree_GCA\\Site_Info\\03_GIS\\shapefiles\\big_tree_15m_buf_WGS84.shp"

# confirm project path
path = "Q:\\SNC\\Data"

################################## no change necessary from here

# Get the current document
doc = Metashape.app.document
# Get the current chunk
chunk = doc.chunk

# MS_P ortho export path
ms_P_ortho_path = path + "\\" + name + "\\" + "Flights" + "\\" + date + "\\2_Inputs\\metashape\\3_MS_ORTHO\\" + name + "_" + date + "_Calp", cal_pan_1,"p", cal_pan_2, "_MS_P_clip.tif"

###############################################################################
# Manually check the raster palette values, rebuild ortho if needed
###############################################################################


# setting raster transform values and disabling transform
#  normalize the 16 bit pixel values
chunk.raster_transform.formula = ["B1/32768", "B2/32768", "B3/32768","B4/32768", "B5/32768", "B6/32768","B7/32768", "B8/32768", "B9/32768","B10/32768","B11/32768" ]
chunk.raster_transform.enabled = False


#import the boundary
chunk.importShapes(
            path= site_perim,
            boundary_type=Metashape.Shape.OuterBoundary,
            format=Metashape.ShapesFormat.ShapesFormatSHP)
# setting up the projection
local_crs = Metashape.CoordinateSystem("EPSG::26910")
proj = Metashape.OrthoProjection()
proj.crs=local_crs
# Export raster with index values, alpha=False, and adjust settings for BigTIFF
compression = Metashape.ImageCompression()
compression.tiff_big = True
# export raster with index values, alpha= false, BigTIFF enabled
#################warning this won't overwrite existing and won't give warnings
chunk.exportRaster(ms_P_ortho_path, raster_transform=Metashape.RasterTransformValue,
                   image_format=Metashape.ImageFormatTIFF, projection=proj, clip_to_boundary=True,
                   save_alpha=False, image_compression=compression)
doc.save()