# -*- coding: utf-8 -*-
"""
Created on Fri Apr 19 12:59:06 2024

@author: jaking
"""
import os, Metashape
# select project variables, name, date, chosen calibration panel numbers, site perimeter 
name = "Big_Tree_GCA"
date = "2024_04_30"
# enter chosen calibration panel numbers
cal_pan_1 = "9"
cal_pan_2 = "1085"

site_perim = "Q:\\SNC\\Data\\Big_Tree_GCA\\Site_Info\\03_GIS\\shapefiles\\big_tree_15m_buf_WGS84.shp"
# confirm project path
path = "Q:\\SNC\\Data"

################################## no change necessary from here
# MS ortho export path
ms_ortho_path = path + "\\" + name + "\\" + "Flights" + "\\" + date + "\\2_Inputs\\metashape\\3_MS_ORTHO\\" + name + "_" + date + "_Calp", cal_pan_1,"p", cal_pan_2, "_MS_clip.tif"
# Get the current document
doc = Metashape.app.document
# Get the current chunk
chunk = doc.chunk

###############################################################################
# Manually check the raster palette values, rebuild ortho if needed
###############################################################################
# setting raster transform values and disabling transform
#  normalize the 16 bit pixel values
chunk.raster_transform.formula = ["B1/32768", "B2/32768", "B3/32768","B4/32768", "B5/32768", "B6/32768","B7/32768", "B8/32768", "B9/32768","B10/32768" ]
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
#################warning this won't overwrite existing and won't give warnings that it hasn't 
chunk.exportRaster(ms_ortho_path, raster_transform=Metashape.RasterTransformValue,
                   image_format=Metashape.ImageFormatTIFF, projection=proj, clip_to_boundary=True,
                   save_alpha=False, image_compression=compression)
doc.save()