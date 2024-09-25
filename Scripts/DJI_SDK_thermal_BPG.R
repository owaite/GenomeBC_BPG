
#packages:
library(exiftoolr)
library(dplyr)
library(lubridate)
library(OpenImageR)
library(raster)
library(doParallel)
library(timechange)
library(data.table)
library(stringr)


################################################################################

#Step 0: Copying h20T images to sole folder

################################################################################

# Setting the site and flight date to convert thermal imagery
flight_dates <-  c("2023_05_10") # Here only one date is shown however this can be a list of however many dates you would like to iterate over
site <- "site name here" 
merged <- "Merged_T" #name of the 'merged' folder that will contain all the h20T images from the multiple folders ouput by the H20T

for(i in 1:length(flight_dates)){
  data_date <- flight_dates[i]
  print(data_date)
  dir <- paste0("I:\\PARSER_Ext\\",site,"\\Flights\\", data_date, "\\1_Data\\H20T")
  #create folders
  if (!dir.exists(paste0(dir, "\\",merged))) {
    dir.create(paste0(dir, "\\",merged),recursive = TRUE)
  }
  if (!dir.exists(paste0(dir, "\\",merged,"\\Temperature_rasters"))) {
    dir.create(paste0(dir, "\\",merged,"\\Temperature_rasters"),recursive = TRUE)
  }
  if (!dir.exists(paste0(dir, "\\",merged,"\\Temperature_rasters_EXIF"))) {
    dir.create(paste0(dir, "\\",merged,"\\Temperature_rasters_EXIF"),recursive = TRUE)
  }
  
  list_dir <- list.files(dir, full.names = TRUE,pattern = c("DJI.+-H20T")) #selecting folders in the H20T folder that have DJI and end in -H20T, this is how we named our folders, change this pattern to match your folders. You should be selecting all folders with H20T imagery ending in _T for that flight
  print(list_dir) #to check only correct directories are being read
  
  for (d in 1:length(list_dir)){
    folder_dir <- list_dir[d] #calling each directly separately
    T_files <- list.files(folder_dir, pattern = "_T\\.JPG$") #selecting all JPEGs ending in _T 
    file.copy(from = paste0(folder_dir,"\\", T_files),
              to = paste0(dir,"\\",merged,"\\", T_files), overwrite = FALSE) #copying h20T images ending in _T (aka unprocessed thermal images) into the thermal only folder for ease in DJI SDK step
  }
}

################################################################################

#STEP 1: Weather Data: Humidity, Ambient Temp

################################################################################
#getting image capture date from exif data of the first image in the H20T folder
data_date <- flight_dates[1]
time_file_list <- list.files(paste0(dir,"\\",merged), pattern = "_T\\.JPG$")

#Retrieving the time the FIRST H20T thermal image was taken
(img_1 <- time_file_list[1]) #find the first thermal image and print the name
start_exif_flight_date <- data.frame(exif_read(paste0(dir,"\\",merged,"\\",img_1),
                                               tags = "CreateDate",
                                               quiet = TRUE)) #find the time the image was taken/written from the CreateDate tag in the exif data
(start_flight_date_time <- start_exif_flight_date$CreateDate) #Start of the flight

#Retrieving the time the LAST H20T thermal image was taken
(img_last <- tail(time_file_list, n=1))#End flight time
end_flight_date_time <- data.frame(exif_read(paste0(dir,"\\",merged,"\\",img_last),
                                             tags = "CreateDate",
                                             quiet = TRUE))
(end_flight_date_time <- end_flight_date_time$CreateDate)#End of the flight


(flight_date <- as.Date(start_exif_flight_date$CreateDate, '%Y:%m:%d %H:%M:%S')) #the flight date as a date object
(flight_date_filt <- as.Date(flight_date, format = '%Y-%m-%d')) #flight date in the format of 2024-03-10, YYYY-mm-dd, this ill be used in the next step to filter weather data

# Load in weather data
cfs_weather <- "path to weather data" #loading in HOBO weather station data
cfs_weather$Date <- as.Date(cfs_weather$Date, format = "%m/%d/%y") #converting date in the format m/d/Y to a date object written as YYY-mm-dd

start_time_avg <- "10:00:00" #Based off of start_flight_date_time of 10:15am
stop_time_avg <- "12:00:00" #Based off of end_flight_date_time of 11:30am

#Looking at time in 15 min intervals
#setting up weights
# This method uses either 0.25 (15min), 0.5 (30min), 0.75 (45min) or 1 (1 hr) as weights
# ie for a flight that started at 10:15 and ended at 11:30, the start weight would be 0.75 (for 10am data), mid weight would be 1 (for 11am data), and end would be 0.5 (for 12pm data)
#round down for start and up for end ie start of 10:06 would be 10 and end of 11:36 would be 11:45

#Ie: Flight starts at 10:15am and ends at 11:30am
start_weight <- 0.75 # 75% of 10am
mid_weight <- 1 # 100% of  11am 
end_weight <- 0.5 # 50% of 12pm

sum_weight <- sum(start_weight,
                  mid_weight,#comment out the mid_weight if no mid weight
                  end_weight) 

#creating df with weighted average air temperature and humidity values
(cfs_weather_time_avg <- cfs_weather%>%
    filter(Date == flight_date_filt ) %>%
    filter(Time <= stop_time_avg & Time >= start_time_avg)%>% #filtering for values taken between the defined start (10am) and end (12pm) times
    mutate(Average_hum = (start_weight*RH[1]
                          + mid_weight*RH[2] #comment out this line if no mid_weight and just below index of [3] to [2]
                          + end_weight*RH[3])/sum_weight,
           Average_air_temp = (start_weight*Temperature[1] 
                               + mid_weight*Temperature[2]#comment out this line if no mid_weight and just below index of [3] to [2]
                               + end_weight*Temperature[3])/sum_weight))

#check values 
(avg_hum <- cfs_weather_time_avg$Average_hum)
(avg_temp <- cfs_weather_time_avg$Average_air_temp)
(date <- as.Date(cfs_weather_time_avg$Date, '%d-%m-%Y'))

#creating a data frame  with weighted averaged values
Canoe_flight_table <- data.frame(date, avg_hum, avg_temp)
Canoe_flight_table <- Canoe_flight_table %>% distinct()#removing duplicate rows

#resetting index in table to go from 1-N
rownames(Canoe_flight_table) <- NULL

#setting variables for weighted average of humidity and ambient temperature, these variables will be used as inputs into DJI's thermal anlysis tool SDK to convert rasters to temperature
flight_humidity <- Canoe_flight_table[Canoe_flight_table$date == flight_date, "avg_hum"]
flight_humidity

flight_amb_temp <- Canoe_flight_table[Canoe_flight_table$date == flight_date, "avg_temp"]
flight_amb_temp

################################################################################

#STEP 2: DJI SDK in Visual Studio : convert to temp in C : 

################################################################################
#converting to single band temperature rasters using DJI thermal's SDK
#setting parameters:
(flight_amb_temp) # already set above, () to print out the value in the console to ensure it is the correct one
(flight_humidity) # already set above
distance <- 25 # max distance that can be set, default is set to 5m
emissivity <- 0.98 # approximate value for trees, default is set to 1

################################################################################
#Editing .bat file for DJI SDK tool and running in terminal
################################################################################
omp_dir <- "C:/Users/owaite/Documents/Scripts/DJI_SDK_TemperatureConversion_Script" #change to the directory where you saved the original .bat script from the above GitHub link to

file.copy(from = paste0(omp_dir,"\\", "DJI_SDK_loop.bat"), #copy the original omp.bat file into the Merged_T folder
          to = paste0(dir,"\\",merged,"\\","DJI_SDK_loop.bat"), overwrite = FALSE)

bat_old <- file(paste0(dir,"\\",merged,"\\","DJI_SDK_loop.bat")) #opening .bat file to edit
bat_new <- readLines(bat_old) #reading in lines of .bat file to edit and saving them out
close(bat_old)

#replacing values
# !! comment out mid_weight line if necessary 
bat_new <- gsub("hum_change",flight_humidity, bat_new)
bat_new <- gsub("dist_change",distance, bat_new)
bat_new <- gsub("emis_change",emissivity,bat_new)
bat_new <- gsub("reflec_change",flight_amb_temp,bat_new)
bat_new <- gsub("start_time_avg",start_time_avg, bat_new)
bat_new <- gsub("stop_time_avg",stop_time_avg,bat_new)
bat_new <- gsub("start_weight", start_weight, bat_new)
bat_new <- gsub("mid_weight", mid_weight, bat_new) #if no mid_weight, comment this line out
bat_new <- gsub("end_weight", end_weight, bat_new)

#Write the new file
fileConn <- file(paste0(dir,"\\",merged,"\\DJI_SDK_loop_updated.bat"))
writeLines(bat_new, fileConn)
close(fileConn)

#running the .bat updated file in windows terminal
shell.exec(paste0(dir,"\\",merged,"\\DJI_SDK_loop_updated.bat"))

#OUPUT: a "DJI_SDK_raw" folder of raw images with temperature in binary in the /Merged_T/ folder 

################################################################################

#STEP 3: Looping over DJI_SDK_raw folders from above and converting into single band tifs
#See this link for more on reading binary and converting to a raster: https://community.rstudio.com/t/solution-to-reading-raw-images-into-r/45435

################################################################################

dir_raw <- paste0(dir,"\\",merged,"\\DJI_SDK_raw\\")
raw_list <- list.files(dir_raw) #check this is a list of the images saved out from the SDK step above
start.time <- Sys.time()#to log how long it takes for code to run 

for (i in 1:length(raw_list)){#loop that only writes out a folder with the Temperature rasters
  wh <- c(640, 512)
  print("new")
  str_m <- raw_list[i]
  #getting name of .raw file but without the .raw extension so it can be saved as a tiff with the same name as the .raw (which has the same name as the original H20T images) so they can be swapped after alignment in metashape
  name <- substr(str_m,1,nchar(str_m)-4)
  print(name)
  #If else below allows you to rerun this code from where it left off if R aborts the session
  test_exists <- paste0(dir,"\\",merged,"\\Temperature_rasters_EXIF\\", name,".tiff")
  if (file.exists(test_exists)){
    print("exists")
  } else {
    #divide by 2 because 16 means 2 bit integers (?)
    #print(paste0(dir_raw, str_m))
    
    file.info(paste0(dir_raw, str_m))$size/2 
    prod(wh) #getting product of width and height and checking it is correct
    v <- readBin(paste0(dir_raw,str_m), what = "integer",  #getting numerical values
                 n = prod(wh), size = 2,  
                 signed = TRUE, endian = "little")
    
    v_temp <- v/10 #temp in deg Celsius, DJI requires we divide by 10 here for Celsius
    range(v_temp) #check the values
    matrix <- matrix(v_temp, wh[1], wh[2])[wh[1]:1,] # creating a matrix of the proper size
    mat_rotated = rotateFixed(matrix, 90)#rotate the matrix by 90 to get correct orientation of image, requires library OpenImageR
    #to view image
    #image(matrix(v_temp, wh[1], wh[2])[wh[1]:1,], useRaster = TRUE, col = grey.colors(256))
    rast <- raster(mat_rotated) #creating a raster 
    
    #matching original image to measure.raw file
    dir_h20t <- paste0(dir,"\\",merged,"\\") #original h20T images:
    str_h20t <- paste0(substr(str_m, 1, nchar(str_m)-3),"JPG") #since names are the same, here we call the name of the raw file -.raw and add .JPG
    r_h20t <- raster(paste0(dir_h20t,str_h20t)) #reading in jpg h20T raster
    r_h20t_path <- paste0(dir_h20t,str_h20t) #for later exiftool
    print(r_h20t_path) #checking the path
    
    #need to add these before exif because exif command used won't overwrite already written ones
    extent(rast) <- extent(r_h20t)# the extent is bound by the resolution that you have assigned to it - so must change extent before resolution
    res(rast) <- res(r_h20t)
    #crs(rast) <- crs(r_h20t)
    print(paste0(dir,"\\",merged,"\\Temperature_rasters\\", name, ".tiff"))
    writeRaster(rast, paste0(dir,"\\",merged,"\\Temperature_rasters\\", name,".tiff"), overwrite = TRUE) #writing out temperature raster, that does not have exif data attached
    r_temperature_path <- paste0(dir,"\\",merged,"\\Temperature_rasters\\", name,".tiff")
    
    #Use EXIFTOOL in terminal to add all remaining/missing exif data from the original H20T images to the new single band temperature TIFFS
    out_dir <- paste0(dir,"\\",merged,"\\Temperature_rasters_EXIF\\", name,".tiff")
    shell(paste0("exiftool -wm cg -tagsfromfile ",r_h20t_path," -all:all ",r_temperature_path, " -o ",out_dir)) #shell lets you run windows terminal cmds from R  
  }
  percent <- i/length(raw_list)*100
  print(paste0("percent complete: ", round(percent, 3))) #gives you the % of photos done to give an idea of code speed/progress
}
end.time <- Sys.time()
(time.taken <- round(end.time - start.time,2)) #time taken for the script to run


################################################################################
#(EXTRA) Post conversion: making a folder with RGB images that have the same name as converted IR images so that only matching RGB images are grabbed and both RGB and IR can be loaded in metashape as a multicamera system for co-alignment
################################################################################

therm_dates <- c("2022_10_05") # date of interest

for(x in 1:length(therm_dates)){
  date_therm <- therm_dates[x]
  print(date_therm)
  
  dir <- paste0("I:\\PARSER_Ext\\Fdc_PR_Canoe\\Flights\\",date_therm,"\\1_Data\\H20T") #directory to h20T imagery
  list_dir <- list.files(dir, full.names = TRUE,pattern = c("DJI.+-H20T")) #folders containing orignal imagery
  print(list_dir)
  
  # GETTING ALL RGB IMAGES
  for (d in 1:length(list_dir)){
    #calling each directly separately
    folder_dir <- list_dir[d]
    if(d == 1){
      files_W <- list.files(folder_dir, pattern = "_W.JPG$")
    }else{
      files_W_add <- list.files(folder_dir, pattern = "_W.JPG$")
      files_W <- append(files_W,files_W_add)
    }
  }
  
  dir_IR <- paste0("I:\\PARSER_Ext\\Fdc_PR_Canoe\\Flights\\",date_therm,"\\1_Data\\H20T\\Merged_T\\Temperature_rasters_EXIF_DST")
  
  #list of thermal files
  files_T <- list.files(dir_IR, pattern = "_T.tiff$")
  
  
  # Function to extract the core name from a file path
  get_core_name <- function(file_path) {
    basename(file_path) %>%
      gsub("_[TW]$", "", .)
  }
  
  #creating a new folder for the metahspae test
  merged <- "Merged_T"
  
  if (!dir.exists(paste0(dir, "\\",merged,"\\RGB_&_IR_EXIF_DST"))) {
    dir.create(paste0(dir, "\\",merged,"\\RGB_&_IR_EXIF_DST"),recursive = TRUE)
  }
  
  dir_new <- paste0(dir, "\\",merged,"\\RGB_&_IR_EXIF_DST")
  
  for (d in 1:length(list_dir)){
    #calling each directly separately
    print(d)
    folder_dir_W <- list_dir[d]
    files_W <- list.files(folder_dir_W, pattern = "_W.JPG$")
    
    for (i in 1:length(files_T)) {
      core_name_T <- substr(files_T[i], 1, nchar(files_T[i]) - 6)
      matching_file_W <- files_W[str_detect(files_W, core_name_T)]
      if(length(matching_file_W)>=1){
        #copying over files
        file.copy(from = paste0(folder_dir_W,"\\", matching_file_W),
                  to = paste0(dir_new,"\\", matching_file_W), overwrite = FALSE)
        file.copy(from = paste0(dir_IR,"\\", files_T[i]),
                  to = paste0(dir_new,"\\", files_T[i]), overwrite = FALSE)
      }
    }
  }
  
}

