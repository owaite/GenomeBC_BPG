# -*- coding: utf-8 -*-
"""
Created on Thu Aug 11 11:24:26 2022

@author: jaking
"""

import os, Metashape
import glob


# the location of all photos for the Agisoft psx, select site, check path, enter date YYYY_MM_DD
#name = "Fdc_JR_East_GCA"
name = "Hillcrest_GCA"
#name = "Big_Tree_GCA"
path = "Q:\\SNC\\Data"
date = "2024_08_28"
# make sure that subfolders with the name of this project exist
 
# Create folder structure specific to current project file setup
if not os.path.exists(path + "\\" + name + "\\" + "Flights" + "\\" + date + "\\2_Inputs\\metashape\\1_TILES\\"):
     os.makedirs(path + "\\" + name + "\\" + "Flights" + "\\" + date + "\\2_Inputs\\metashape\\1_TILES\\")

if not os.path.exists(path + "\\" + name + "\\" + "Flights" + "\\" + date + "\\2_Inputs\\metashape\\2_ORTHO\\"):
    os.makedirs(path + "\\" + name + "\\" + "Flights" + "\\" + date + "\\2_Inputs\\metashape\\2_ORTHO\\")

if not os.path.exists(path + "\\" + name + "\\" + "Flights" + "\\" + date +"\\2_Inputs\\metashape\\3_MS_ORTHO\\"):
    os.makedirs(path + "\\" + name + "\\" + "Flights" + "\\" + date +"\\2_Inputs\\metashape\\3_MS_ORTHO\\")    
# P1 path    
rgb_P1_path = path + "\\" + name + "\\Flights\\" + date + "\\1_Data\\P1\\*\\*.JPG"  
#micasense mx path
ms_path = path + "\\" + name + "\\Flights" + "\\" + date + "\\1_Data"+ "\\Micasense\\*\\*\\*\\*"
#micasense Pan chromatic path
ms_P_path = path + "\\" + name + "\\Flights" + "\\" + date + "\\1_Data"+ "\\Micasense_P\\*\\*\\*\\*"
#path to save project
psx_path = path + "\\" + name + "\\" + "Flights" + "\\" + date + "\\2_Inputs\\metashape\\" + name + "_" + date +".psx"
################################################
#setting up the document 
##############################################
doc = Metashape.app.document
doc.save(psx_path)
################################################
# Adding the RGB_P1 chunk, load photos with EXIF
################################################
chunk = doc.addChunk()
chunk.label = "RGB_P1"
photo_list = glob.glob(rgb_P1_path)
# add the photos and load the exif data
chunk.addPhotos(photo_list)
chunk.loadReferenceExif(load_rotation=True, load_accuracy=True)
################################################
# Adding the MS_Mica chunk, loading photos and locating reflectance panels
################################################
chunk = doc.addChunk()
chunk.label = "MS_Mica"
photo_list = glob.glob(ms_path)
#cloud_path = site_cloud_path + name + "_" + color + ".xyz"
# add the photos and load the exif data
chunk.addPhotos(photo_list)
chunk.loadReferenceExif(load_rotation=True, load_accuracy=True)
# locate reflectance panels
chunk.locateReflectancePanels()
doc.save()
################################################
# Adding the MS_Mica_P chunk, loading photos and locating reflectance panels
################################################
chunk = doc.addChunk()
chunk.label = "MS_Mica_P"
photo_list = glob.glob(ms_P_path)
# add the photos and load the exif data
chunk.addPhotos(photo_list)
chunk.loadReferenceExif(load_rotation=True, load_accuracy=True)
# locate reflectance panels
chunk.locateReflectancePanels()
doc.save()
##############################################
# Manually disable irrelevant cameras now  #
##############################################