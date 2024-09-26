library(exifr)

################################################################################

#1. Read and write exif data of all Micasense imagery

################################################################################
dir <- "directory to folder holding folders representing each flight aquisition date" #change to match your data
MS_folder_name <- "MicaSense" #change to match the name of your folder holding the folders of imagery from the MicaSense camera
band_length <-  10 # 10 bands for MicaSense RedEdge-MX Dual, 11 for panchromatic MX Dual system

#Loops through bands
for (j in 1:band_length){ # bands 1 through 10 
  pics = list.files(paste0(dir, date,"\\1_Data\\",MS_folder_name,"\\"), #path the original data that will NOT be written over
                    pattern = c(paste0("IMG_...._", j, ".tif"), paste0("IMG_...._", j, "_", ".tif")), recursive = TRUE, 
                    full.names = TRUE) # list of directories to all images
  
  if(!dir.exists(paste0(dir,date,"\\1_Data\\",MS_folder_name,"\\CSV\\"))){ #creating CSV folders
    dir.create(paste0(dir,date,"\\1_Data\\",MS_folder_name,"\\CSV\\"))}
  
  mask_images <- list.files(paste0(dir,date,"\\2_Inputs\\metashape\\MASKS\\",MS_folder_name,"\\")) #path to folder with masks that are exported from metashape 
  mask_img_names <- gsub("_mask\\.png$", "", mask_images) # removes the suffix "_mask.png" from each element in the 'mask_images' list, ie : IMG_0027_1_mask.png becomes : IMG_0027_1 
  print(paste0("example of mask name: ",mask_img_names[[1]])) #print to ensure your naming is correct, the list of mask_img_names will be used to identify panel images in the micasense image 
  # Loops through images in each band
  for (i in 1:length(pics)){ #for each image in MicaSense_Cleaned_save, one at a time
    pic = pics[i]
    pic_root <- gsub(".*/(IMG_.*)(\\.tif)$", "\\1", pic)# The pattern captures the filename starting with "IMG_" and ending with ".tif".Ie: IMG_0027_1.tiff becomes IMG_0027_1
    print(pic_root)
    if (substr(pic_root,11,11) == "0"){ # Distinguishes between _1 (band 1) and _10 (band 10), ie IMG_0027_1 verse IMG_0027_10
      band = substr(pic_root,10,11) # for _10
    } else {
      band = substr(pic_root,10,10) # for _1, _2, _3, ... _9 (bands 1-9)
    } 
    img_exif = exifr::read_exif(pic) # read the XMP data 
    print(paste0(date, " band ", j, " ", i, "/", length(pics))) # keep track of progress
    
    # Creating df with same column names as exif data
    if (i == 1){ # If it's the first image, make new df from XMP data 
      exif_df = as.data.frame(img_exif)%>%
        mutate(panel_flag = ifelse( gsub(".*/(IMG_.*)(\\.tif)$", "\\1", SourceFile) %in% mask_img_names, 1, 0)) # Give a value of 1 if the pic root name is in the list of mask root names and is therefore a calibration panel, give 0 otherwise (ie not a panel) 
    } else {   # Add each new image exif to dataframe
      exif_df = merge(exif_df, img_exif, by = intersect(names(exif_df), names(img_exif)), all = TRUE)%>%
        mutate(panel_flag = ifelse( gsub(".*/(IMG_.*)(\\.tif)$", "\\1", SourceFile) %in% mask_img_names, 1, 0)) # Give a value of 1 if the pic root name is in the list of mask root names and is therefore a calibration panel, give 0 otherwise (ie not a panel) 
    }
  }
  saveRDS(exif_df, paste0(dir,date, "\\1_Data\\",MS_folder_name,"\\CSV\\XMP_data_", date, "_", j, ".rds")) # save output for each band, set path
  #binding all bands into one dataframe
  if (j == 1){
    df_full = exif_df
  }else{
    # Add missing columns to xmp_all and fill with NA values
    df_full= rbind(df_full, exif_df)
  }
  saveRDS(df_full, paste0(dir,date,"\\1_Data\\",MS_folder_name,"\\CSV\\XMP_data_", date,"_AllBands.rds")) #.rds file containing the original exif data if all images for the flight
} 

#read back in exif data for all bands
xmp_all <- readRDS(paste0(dir,date,"\\1_Data\\",MS_folder_name,"\\CSV\\XMP_data_", date,"_AllBands.rds"))


################################################################################

#2. Calculate Solar Angle

################################################################################

#create columns needed in later steps and calculate solar angle
xmp_all_filtered <- xmp_all %>%
  mutate(Date_time = ymd_hms(DateTimeOriginal),
         BandName_Wavelength = paste0(BandName, "_", CentralWavelength),
         Date = as.Date(Date_time),
         Time = format(Date_time, format = "%H:%M:%S"),
         img_name = str_split(FileName, "\\.tif", simplify = TRUE)[, 1],
         img_root = sub("_(\\d+)$", "", img_name)) %>%
  drop_na(DateTimeOriginal) %>% 
  dplyr::mutate(
    site_avg_lat = median(GPSLatitude, na.rm = TRUE),
    site_avg_long = median(GPSLongitude, na.rm = TRUE),
    solar_angle = photobiology::sun_zenith_angle(time = ymd_hms(DateTimeOriginal),
                                                 geocode = tibble::tibble(lon = unique(site_avg_long),
                                                                          lat = unique(site_avg_lat),
                                                                          address = "Greenwich")))

# check for missing/corrupted imagery 
missing_imgs <- xmp_all[is.na(as.Date(xmp_all$CreateDate, format = "%Y:%m:%d"))]$FileName
missing_imgs_roots <- sub("_[^_]*$", "", missing_imgs)

xmp_all_filtered <- xmp_all_filtered %>% #removing images with missing date information
  filter(!FileName %in% c(missing_imgs))

################################################################################

#3. DLS1: Calculate Sun Sensor Angles (SSA) with DLS1 method

################################################################################

compute_sun_angle <- function(SolarElevation, SolarAzimuth, Roll, Pitch, Yaw) {
  ori <- c(0, 0, -1)
  SolarElevation <- as.numeric(SolarElevation)
  SolarAzimuth <- as.numeric(SolarAzimuth)
  Roll <- as.numeric(Roll)
  Pitch <- as.numeric(Pitch)
  Yaw <- as.numeric(Yaw)
  
  elements <- c(cos(SolarAzimuth) * cos(SolarElevation),
                sin(SolarAzimuth) * cos(SolarElevation),
                -sin(SolarElevation))
  
  nSun <- t(matrix(elements, ncol = 3))
  
  c1 <- cos(-Yaw)
  s1 <- sin(-Yaw)
  c2 <- cos(-Pitch)
  s2 <- sin(-Pitch)
  c3 <- cos(-Roll)
  s3 <- sin(-Roll)
  
  Ryaw <- matrix(c(c1, s1, 0, -s1, c1, 0, 0, 0, 1), ncol = 3, byrow = TRUE)
  Rpitch <- matrix(c(c2, 0, -s2, 0, 1, 0, s2, 0, c2), ncol = 3, byrow = TRUE)
  Rroll <- matrix(c(1, 0, 0, 0, c3, s3, 0, -s3, c3), ncol = 3, byrow = TRUE)
  
  R_sensor <- Ryaw %*% Rpitch %*% Rroll
  nSensor <- R_sensor %*% ori
  
  angle <- acos(sum(nSun * nSensor))
  return(angle)
}

# create columnas for the corrected sun sensor angles (in degrees and radians)
SSA_xmp_all_filtered <- xmp_all_filtered %>%
  rowwise() %>% 
  mutate(SunSensorAngle_DLS1_rad = compute_sun_angle(SolarElevation, SolarAzimuth, Roll, Pitch, Yaw),
         SunSensorAngle_DLS1_deg = SunSensorAngle_DLS1_rad * 180 / pi)

saveRDS(SSA_xmp_all_filtered,paste0(dir,date,"\\1_Data\\",MS_folder_name,"\\CSV\\XMP_", date,"_with_SSA.rds")) #Save XMP rds file with sun sensor angle added

################################################################################

#4. DLS2: Calculate Sun Sensor Angles (SSA) from exif data

################################################################################

SSA_xmp_all_filtered$SunSensorAngle_DLS2_rad <- sapply(xmp_all_filtered$EstimatedDirectLightVector, function(vec) acos(-1 * as.numeric(vec[[3]])))
SSA_xmp_all_filtered$SunSensorAngle_DLS2_deg <- as.numeric(SSA_xmp_all_filtered$SunSensorAngle_DLS2_rad) / pi * 180

################################################################################

#5.Plot and compare DLS1 verse DLS2 SSA values

################################################################################

xmp_all_ssa = SSA_xmp_all_filtered %>%
  # Converting from radians to degrees
  mutate(Yaw_deg = as.numeric(Yaw)*180/pi,
         Roll_deg = as.numeric(Roll)*180/pi,
         Pitch_deg = as.numeric(Pitch)*180/pi) %>% 
  # Grouping images by Date and band
  group_by(Date, BandName) %>% 
  arrange(ymd_hms(DateTimeOriginal)) %>% # Converting DateTimeOriginal to a Date and Time object and arranging in order 
  mutate(GPSLatitude_plot = scale(as.numeric(GPSLatitude)), # These are for clean ggplotting, no other reason to scale
         GPSLongitude_plot = scale(as.numeric(GPSLongitude)),
         cos_SSA = cos(SunSensorAngle_DLS1_rad),
         Irradiance = as.numeric(Irradiance),
         Date2 = ymd_hms(DateTimeOriginal)) # this is the date/time we will use moving forward 

(SSA_plot <- xmp_all_ssa %>%
    ggplot(aes(Date_time)) +
    geom_line(aes(y = SunSensorAngle_DLS2_deg, color = "DLS2"), linewidth = 1) +
    geom_line(aes(y = SunSensorAngle_DLS1_deg, color = "DLS1"), linewidth = 1) +
    geom_vline(data = subset(xmp_all_ssa, panel_flag == 1), aes(xintercept = as.numeric(Date_time)), color = "grey", alpha = 0.3) +
    geom_line(aes(y = solar_angle), color = "black", linewidth = 1, alpha = 1) +
    geom_smooth(
      aes(y = SunSensorAngle_DLS2_deg),
      method = "loess",
      se = FALSE,
      color = "blue",
      linetype = "dashed",
      size = 1
    ) +
    # Moving average for DLS1
    geom_smooth(
      aes(y = SunSensorAngle_DLS1_deg),
      method = "loess",
      se = FALSE,
      color = "red",
      linetype = "dashed",
      size = 1
    ) +
    scale_x_datetime(date_breaks = "10 min", date_labels = "%H:%M") +
    labs(x = "Time", y = "Sun Sensor Angle", title = paste0("SSA, Site: ",site_to_plot,", Date: ", date_to_plot, ", Camera: ", camera_to_plot))+
    labs(subtitle = "Grey vertical lines are calibration panel images, black hoirzontal line is the solar angle (calculated from lat, long, and flight time)", size = 10) + 
    scale_color_manual(values = c("DLS2" = "blue",
                                  "DLS1" = "red"),
                       labels = c("DLS2" = "DLS2 (from metadata)", 
                                  "DLS1" = "DLS1 (calculated)"),
                       name = "Sun Sensor Angle Comparison")+
    facet_wrap(. ~BandName_Wavelength, 
               scales = "free_x", ncol = 2)+ # this will plot each band as its own plot as a check for all data
    theme_bw()
)

################################################################################

#6. Estimating percent scattered irradiance and scattered/direct irradiance ratio using rolling regressions of linear relationship Ir = direct Ir*cos(SSA) + scattered Ir, only keeping logical models

################################################################################

# via a regression of Irradiance on cos_SSA over a specified time window (30s here)
mod_frame = xmp_all_ssa %>%
  drop_na(Date2) %>% 
  drop_na(cos_SSA) %>% 
  drop_na(Irradiance) %>% 
  # Fit a rolling regression
  # for each image, fit a linear model of all images (of the same band) within 30 seconds of the image
  tidyfit::regress(Irradiance ~ cos_SSA, m("lm"),
                   .cv = "sliding_index", .cv_args = list(lookback = lubridate::seconds(30), index = "Date2"),
                   .force_cv = TRUE, .return_slices = TRUE)

# df : summary of models, adding R sqaured and Dates
df = mod_frame %>% 
  # Get a summary of each model and extract the r squared value
  mutate(R2 = map(model_object, function(obj) summary(obj)$adj.r.squared)) %>% 
  # Extract the slope and intercept
  coef() %>% 
  unnest(model_info) %>% 
  mutate(Date2 = ymd_hms(slice_id)) 

# df_params : adding slope (direct irradiance) and y-intercept (scatterd irradiance) values
df_params = df %>%
  dplyr::select(Date:estimate, Date2) %>% 
  # we will have to go from long, with 2 observations per model, to wide
  pivot_wider(names_from = term, values_from = estimate, values_fn = {first}) %>% 
  dplyr::rename("Intercept" = `(Intercept)`,
                "Slope" = "cos_SSA")

# Cleaning up the df
df_p = df %>%
  filter(term == "cos_SSA") %>% 
  dplyr::select(Date:model, R2, p.value, Date2)

# Joining model info, parameter (slope, y intercept) info and XMP data with sun sensor angle and using the linear relationship 
# (spectral_irr = direct_irr * cos(SSA) + scattered_irr) to create % scattered and scattered/direct ratios
df_filtered = df_params %>% 
  left_join(df_p) %>% 
  left_join(xmp_all_ssa) %>% 
  mutate(percent_scattered = Intercept / (Slope + Intercept),
         dir_diff = Intercept/Slope)

#Eliminate models with poor fits:
df_to_use = df_filtered %>% 
  mutate(R2 = as.numeric(R2)) %>% 
  filter(R2 > .4 
         & Slope > 0 & Intercept > 0) %>% 
  group_by(Date) %>% 
  mutate(mean_scattered = mean(percent_scattered),
         dir_diff_ratio = mean(dir_diff))

#save out dataframe as an rds
saveRDS(df_to_use,paste0(dir,date,"\\1_Data\\",MS_folder_name,"\\CSV\\",date,"_rolling_regression_filteredModels_used_plot1.rds"))  #SET PATH to save the rds to 


#Plotting % scattered per model:
(RR_params <- df_to_use %>%
    group_by(Date, BandName) %>% 
    ggplot(aes(x = Date2, y = percent_scattered, color = R2)) +
    geom_point(data = df_filtered, color = "grey") +
    geom_hline(yintercept = 1, linetype = 2) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_hline(aes(yintercept = mean_scattered), color = "red4", linewidth = 1) +
    scale_y_continuous(breaks = seq(0, 1, .2), limits = c(0,1)) +
    ggnewscale::new_scale_color() +
    labs(title = paste0("Site: ",site_to_plot,", Date: ", date_to_plot, ", Camera: ", camera_to_plot),
         x = "Time (UTC)")+
    theme_bw(base_size = 16) +
    facet_wrap(. ~ Date, 
               scales = "free"))

#Plotting linear relationships of each model run:
(linear_plots <- df_to_use %>%
    filter(BandName == "Blue") %>% 
    ggplot(aes(x = cos_SSA, y = Irradiance, color = R2)) +
    geom_point(data = filter(df_filtered, BandName == "Blue"), color = "grey30", alpha = .4) +
    geom_point(data = filter(df_filtered, BandName == "Blue" & R2 > .4), aes(color = as.numeric(R2))) +
    geom_smooth(method = "lm", se = FALSE, aes(group = BandName)) +
    lims(x = c(0, 1),
         y = c(0, max(df_to_use$Irradiance))) +
    geom_abline(aes(slope = Slope, intercept = Intercept, color = R2), alpha = .3) +
    labs(title = paste0("Site: ",site_to_plot,", Date: ", date_to_plot, ", Camera: ", camera_to_plot))+
    theme_bw(base_size = 16) +
    scale_color_viridis_c() +
    facet_wrap(. ~ Date, 
               scales = "free_y"))

#Plotting spatial locations of images kept:
(photos_kept <- df_to_use %>%
    filter(GPSLongitude != 0 & GPSLatitude != 0) %>% # Filter out imgs with GPSLongitude and GPSLatitude of zero, 
    # this is rare and in my experience were corrupted imgs where the XMP could not be properly read
    ggplot(aes(x = GPSLongitude, y = GPSLatitude, color = SunSensorAngle_DLS1_deg)) +
    geom_point(data = df_filtered, color = "grey60") +
    geom_point(size = 3) +
    labs(title = paste0("Site: ",site_to_plot,", Date: ", date_to_plot, ", Camera: ", camera_to_plot))+
    theme_bw() +
    scale_color_viridis_c() +
    facet_wrap(. ~ Date, scales = "free"))

#Calculating scattered/direct component ratios:
(ratios = df_to_use %>% 
    dplyr::select(Date, mean_scattered, dir_diff_ratio,
                  GPSLatitude, GPSLongitude, Date2) %>% 
    group_by(Date) %>% 
    mutate(Lat_mean = mean(GPSLatitude),
           Long_mean = mean(GPSLongitude),
           Date_mean = mean(Date2)) %>% 
    distinct(Date, mean_scattered, dir_diff_ratio, Lat_mean, Long_mean, Date_mean))

saveRDS(ratios,paste0(dir,date,"\\1_Data\\",MS_folder_name,"\\CSV\\",date,"_ratios.rds"))  #SET PATH to save the rds to 

# Print the values to use in the calibration as a check (does this ratio looks reasonable?)
round(ratios$dir_diff_ratio, 2)

################################################################################

#7. Computing Horizontal Irradiance

################################################################################

fresnel_transmission = function(phi, n1, n2, polarization) {
  f1 = cos(phi)
  f2 = sqrt(1 - (n1 / n2 * sin(phi))^2)
  Rs = ((n1 * f1 - n2 * f2) / (n1 * f1 + n2 * f2))^2
  Rp = ((n1 * f2 - n2 * f1) / (n1 * f2 + n2 * f1))^2
  T = 1 - polarization[1] * Rs - polarization[2] * Rp
  T = pmin(pmax(T, 0), 1)  # Clamp the value between 0 and 1
  return(T)
}

multilayer_transmission = function(phi, n, polarization) {
  T = 1.0
  phi_eff = phi
  for (i in 1:(length(n) - 1)) {
    n1 = n[i]
    n2 = n[i + 1]
    phi_eff = asin(sin(phi_eff) / n1)
    T = T * fresnel_transmission(phi_eff, n1, n2, polarization)
  }
  return(T)
}

# Defining the fresnel_correction function 
fresnel_correction = function(x) {
  
  Irradiance = x$Irradiance
  SunSensorAngle_DLS1_rad = x$SunSensorAngle_DLS1_rad
  n1=1.000277
  n2=1.38
  polarization=c(0.5, 0.5)
  
  # Convert sun-sensor angle from radians to degrees
  SunSensorAngle_DLS1_deg <- SunSensorAngle_DLS1_rad * (180 / pi)
  
  # Perform the multilayer Fresnel correction
  Fresnel <- multilayer_transmission(SunSensorAngle_DLS1_rad, c(n1, n2), polarization)
  return(Fresnel)
}

# Computing horizontal irradiance
xmp_corrected = xmp_all_ssa  %>% 
  group_by(Date, BandName) %>% 
  nest(data = c(Irradiance, SunSensorAngle_DLS1_rad)) %>% # Creates a nested df where each group is stored as a list-column named data, containing the variables Irradiance and SunSensorAngle_DLS1_rad
  mutate(Fresnel = as.numeric(map(.x = data, .f = fresnel_correction))) %>% # Applies the fresnel_correction function to each group of nested data
  unnest(data) %>% #unnesting
  # Joining the ratios
  left_join(ratios, by = "Date") %>% 
  mutate(SensorIrradiance = as.numeric(SpectralIrradiance) / Fresnel, # irradiance adjusted for some reflected light from the DLS diffuser
         DirectIrradiance_new = SensorIrradiance / (dir_diff_ratio + cos(as.numeric(SunSensorAngle_DLS1_rad))), # adjusted for sun angle, 
         HorizontalIrradiance_new = DirectIrradiance_new * (dir_diff_ratio + sin(as.numeric(SolarElevation))), 
         ScatteredIrradiance_new = HorizontalIrradiance_new - DirectIrradiance_new)
saveRDS(xmp_corrected,paste0(dir,date,"\\1_Data\\",MS_folder_name,"\\CSV\\",date,"_xmp_corrected.rds"))  #SET PATH to save the rds to


# Plotting Corrected Irradiance Values

(Sensor_irr <- xmp_corrected %>% 
    filter(BandName == "Blue") %>% 
    ggplot(aes(x = Date2)) +
    geom_point(aes(y = SensorIrradiance, color = "Sensor Irradiance"),size = 1,show.legend = TRUE) +
    geom_point(aes(y = HorizontalIrradiance_new, color = "Horizontal_DLS1"), size = 1, show.legend = TRUE) +
    geom_point(aes(y = DirectIrradiance_new, color = "Direct_DLS1"), size = 1, show.legend = TRUE) +
    geom_point(aes(y = ScatteredIrradiance_new, color = "Scattered_DLS1"), size = 1, show.legend = TRUE) +
    geom_hline(yintercept = 0) +
    scale_color_manual(values = c("Sensor Irradiance"= "black", "Horizontal_DLS1" = "red", "Direct_DLS1" = "orange", "Scattered_DLS1" = "purple")) +
    #lims(y = c(50, 150)) +
    labs(y = "Irradiance", title = paste0("Site: ",site_to_plot,", Date: ", date_to_plot, ", Camera: ", camera_to_plot))+
    theme_bw() +
    facet_wrap(. ~ Date, scales = "free",
               ncol = 3) +
    labs(color = "Irradiance Type"))

################################################################################

#8. Plotting Corrected Irradiance vs. Uncorrected

################################################################################

# & calculating DLS1 and DLS2 based %scattered ir and scattered/direct ratios

avg_ratio_data <- xmp_corrected %>%
  filter(!is.na(BandName))%>%
  group_by(BandName) %>%
  summarize(median_ScatteredDirectRatio_DLS2 = median(as.numeric(ScatteredIrradiance) /as.numeric(DirectIrradiance), na.rm = TRUE),
            median_ScatteredDirectRatio_DLS1_calc = median(as.numeric(ScatteredIrradiance_new) /as.numeric(DirectIrradiance_new), na.rm = TRUE),
            median_percent_scat_DLS2 = median(100*as.numeric(ScatteredIrradiance)/(as.numeric(ScatteredIrradiance)+as.numeric(DirectIrradiance))),
            median_percent_scat_DLS1 = median(100*as.numeric(ScatteredIrradiance_new)/(as.numeric(ScatteredIrradiance_new)+as.numeric(DirectIrradiance_new))),
            max_Date_time = max(Date_time),
            max_dir = max(DirectIrradiance, na.rm = TRUE)) %>%
  ungroup()%>%
  mutate(
    Scattered_To_Direct_Ratio_DLS2 = as.character(round(median_ScatteredDirectRatio_DLS2,2)),
    Scattered_To_Direct_Ratio_DLS1 = as.character(round(median_ScatteredDirectRatio_DLS1_calc,2)),
    Percent_Scat_DLS2 = as.character(round(median_percent_scat_DLS2,2)),
    Percent_Scat_DLS1 = as.character(round(median_percent_scat_DLS1,2)),
    x = max_Date_time,
    y = max_dir,
  )%>%
  dplyr::select(c(Scattered_To_Direct_Ratio_DLS2,Scattered_To_Direct_Ratio_DLS1,Percent_Scat_DLS2,Percent_Scat_DLS1, x, y, BandName, camera))

unique(xmp_corrected$CenterWavelength) #make sure all wavebands are present

saveRDS(avg_ratio_data,paste0(dir,date,"\\1_Data\\",MS_folder_name ,"\\CSV\\",date,"_avg_ratio_data.rds"))  #SET PATH to save the rds to

data = avg_ratio_data %>% filter(BandName %in% c("Blue")) # only need to look at one band

(Dir_scat_irr_plot <- xmp_corrected %>%
    filter(BandName %in% c("Blue")) %>%
    mutate(DirectIrradiance = as.numeric(DirectIrradiance),
           ScatteredIrradiance = as.numeric(ScatteredIrradiance),
           Irradiance = as.numeric(Irradiance),
           SpectralIrradiance = as.numeric(SpectralIrradiance),
           HorizontalIrradiance = as.numeric(HorizontalIrradiance),
           # Date_time = ymd_hms(CreateDate),
           Time = format(Date_time, format = "%H:%M:%S"),
           #scaled 
           DirectIrradiance_scaled = as.numeric(DirectIrradiance)*0.01,
           ScatteredIrradiance_scaled = as.numeric(ScatteredIrradiance)*0.01,
           SpectralIrradiance_scaled = as.numeric(SpectralIrradiance)*0.01) %>%
    
    ggplot(aes(Date_time)) +
    geom_line(aes(y = DirectIrradiance, color = "Direct"), linewidth = 1) +
    geom_line(aes(y = DirectIrradiance_new, color = "Direct_DLS1"), linewidth = 1, linetype = "dashed") +
    
    geom_line(aes(y = ScatteredIrradiance, color = "Scattered"), linewidth = 1) +
    geom_line(aes(y = ScatteredIrradiance_new, color = "Scattered_DLS1"), linewidth = 1,linetype = "dashed") +
    
    geom_line(aes(y = HorizontalIrradiance, color = "Horizontal"),linewidth = 1) +
    geom_line(aes(y = HorizontalIrradiance_new, color = "Horizontal_DLS1"),linewidth = 1, linetype = "dashed") +
    
    geom_line(aes(y = Irradiance, color = "Irradiance"), linewidth = 1) +
    geom_line(aes(y = HorizontalIrradiance, color = "Horizontal"),linewidth = 1) +
    geom_line(aes(y = SpectralIrradiance, color = "Spectral"), linewidth = 1.5, linetype = "dotted") +
    labs(x = "Time (UTC)", y = "Irradiance", title = paste0("DLS1 (corrected) vs. DLS2 (metadata) Irradiance, Site: ",site_to_plot,", 
                                                      \nDate: ", date_to_plot,", Camera: ", camera_to_plot,
                                                            "\nScattered_To_Direct_Ratio_DLS1:", data$Scattered_To_Direct_Ratio_DLS1,
                                                            "\nScattered_To_Direct_Ratio_DLS2:", data$Scattered_To_Direct_Ratio_DLS2,
                                                            "\nPercent_Scat_DLS1:", data$Percent_Scat_DLS1,
                                                            "\nPercent_Scat_DLS2:", data$Percent_Scat_DLS2
    )) +
    scale_x_datetime(date_breaks = "10 min", date_labels = "%H:%M") +
    scale_color_manual(values = c("Direct" = "blue", 
                                  "Direct_DLS1" = "lightblue",
                                  "Scattered" = "black",
                                  "Scattered_DLS1" = "grey",
                                  "Horizontal" = "red",
                                  "Horizontal_DLS1" = "orange",
                                  "Spectral" = "forestgreen",
                                  "Irradiance" ="purple"
    ),
    name = "Irradiance (W/m2/nm)")+
    facet_grid(BandName ~ camera, scales = "free")+
    theme_bw()
)

################################################################################

#9. Overwriting exif data of MicaSense imagery 

################################################################################

# Writing over exif data with corrected SSA, horizontal irradiance, direct irradiance, and scattered irradiance

corrected_directory <- paste0(dir,date,"\\1_Data\\",MS_folder_name,"_cor\\") #i.e. this folder is called MicaSense_cor and is a copied version of the MicaSense folder, we will be correcting the exif data of images in the MicaSense_cor folder and leaving the MicaSense folder untouched with the original exif data
original_directory <- paste0(dir,date,"\\1_Data\\",MS_folder_name,"\\") # path the folder of micasense images

xmp_corrected = readRDS(paste0(dir, date, "\\", MS_folder_name,"\\CSV\\",date,"_xmp_corrected.rds")) %>% #path to corrected xmp data .rds file
  mutate(SourceFile = str_replace(SourceFile,original_directory, corrected_directory), # Changing source file name from the original directory to the corrected_directory. This will result in imagery in the corrected direcotry only to be edited
         TargetFile = SourceFile) #the TargetFiles are the path names to the files to be edited

# As vectors
(img_list = xmp_corrected$FileName)
(targets = xmp_corrected$TargetFile)
(SSA = xmp_corrected$SunSensorAngle_DLS1_rad)

(horirrorig = xmp_corrected$HorizontalIrradiance)
(horirr = xmp_corrected$HorizontalIrradiance_new)
(dirirr = xmp_corrected$DirectIrradiance_new)
(scairr = xmp_corrected$ScatteredIrradiance_new)

targets[1] #checking its the right imgs

for (i in seq_along(targets)) {
  # given a micasense config file, overwrite tags with computed values
  # using exiftool_call from the exifr package
  call = paste0("-config G:/GitHub/GenomeBC_BPG/MicaSense/MicaSense.config", # SET PATH to the config file, this file is on the PARSER GitHub in the same folder as this R script
                " -overwrite_original_in_place",
                " -SunSensorAngle=", SSA[i],
                " -HorizontalIrradiance=", horirr[i],
                " -HorizontalIrradianceDLS2=", horirrorig[i],
                " -DirectIrradiance=", dirirr[i],
                " -ScatteredIrradiance=", scairr[i], " ",
                targets[i])
  
  exiftool_call(call, quiet = TRUE)
  print(paste0(i, "/", length(targets), " updated img:",img_list[i] ))
}
