
library(exiftoolr)
library(dplyr)
library(lubridate)

install.packages("OpenImageR")
library(OpenImageR)
library(raster)
library(doParallel)

#install.packages("timechange")
library(timechange)

#install.packages("data.table")
library(data.table)

library(stringr)


# Read in CSV of weather data (ensure adjustments for daylight savings have been made if necessary) 
df <- read.csv("D:\\Fdc_PR_Canoe\\Site_Info\\Powell_River_Canoe2_All_Data_2023_02_07_PST_1_DaylightSavings_mar16_nov6_2022.csv")
df$Date <- as.Date(df$Date, '%m/%d/%y') #comes out as year-0m-0d

dates_DST <- c(unique(df$Date))
dates_DST
dates_list <- as.list(dates_DST)
dates_list

Canoe_df <- df

#Before running this code you need to have the original .bat file for step 2, the directory I keep it in is: omp_dir <- "C:/Users/owaite/Documents/Scripts/DJI_SDK_TemperatureConversion_Script" (line 224)

# Also need to change directories on lines: 28-32
# change location to weather data on line: 105
# line 108 and 109 we get the time of the first and last image, which will help us decide what times we will use to average humidity and temperature values as well as which weights to use this step is not automated, the user must input the correct time bounds (line 108 and 109) so a sentence has been left uncommented so that the code cant accidentally run through without checking those times The first few times you run the averaging it is a good idea to check the values of temp and humidity (lines 163 & 166) by calculated the weighted average by hand to make sure the code is doing what you expect make sure in lines ~120-137 all mid_weights are commented out if there are none, ie looking at 11am-12pm so only dealing with two data points and not three.



#### ERROR in DJI SDK, DJI is working on this problem
ERROR: call dirp_set_measurement_params failed
ERROR: call prv_isp_config failed
Test done with return code -6

######################
# AFTER THERMAL CONVERSION
#Step 00: making a folder with RGB images that have the same name as converted IR images so that only matching RGB images are grabbed and both RGB and IR can be loaded in metassshape as a multicamera system

######################
therm_dates <- c("2022_10_05")

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

########################################################################################################################################
#Step 0: making folders and copying h20T images to sole folder

########################################################################################################################################
#dir <- Data directory, folder where thermal and thermal_SDK_DJI_raw are held
#"Fdc_PR_Canoe", Cw_PR_Rainbow_SiteA
site <- "Fdc_PR_Canoe"
flight_dates_all <- list.files(paste0("I:\\PARSER_Ext\\",site,"\\Flights\\"))
flight_dates_all <- as.Date(flight_dates_all, "%Y_%m_%d")
flight_dates_all
flight_dates_DayST <- flight_dates_all[flight_dates_all %in% df$Date]
#chanign format to have it match the file format of year_m_d
flight_dates_DayST <- str_replace_all(flight_dates_DayST, "-", "_")
flight_dates_DayST
#name of the 'merged' folder that will contain all the h20T images from multiple folders, this was necessary when I was running tests but to stay consistent I would leave it set to 'Merged_T'

## Dates DONE : up to 2022_10_20
#merged_T folders done: 2022_11_19 
## 2022_11_28 - Mica only, no thermal for: 2023_01_26, 
## NEED TO FIND THERMAL DATA FOR 2023_04_26

dates <- c("2023_05_10")#, "2023_05_24")#"2023_02_17", "2023_03_22", "2023_04_05"
# library(filesstrings)

flight_dates_DayST <- dates
flight_dates_DayST

merged <- "Merged_T"

for(i in 1:length(flight_dates_DayST)){
  data_date <- flight_dates_DayST[i]
  print(data_date)
  dir <- paste0("I:\\PARSER_Ext\\",site,"\\Flights\\", data_date, "\\1_Data\\H20T")
  dir
  # #creating a folder to move non Daylight savings data to
  # if (!dir.exists(paste0(dir, "\\",merged,"\\No_DST"))) {
  #   dir.create(paste0(dir, "\\",merged,"\\No_DST"),recursive = TRUE)
  # }
  #moving non DST data to No_DST
  # file.rename(paste0(dir, "\\",merged,"\\DJI_SDK_raw\\"), paste0(dir, "\\",merged,"\\No_DST\\DJI_SDK_raw\\"))
  # file.rename(paste0(dir, "\\",merged,"\\Temperature_rasters\\"), paste0(dir, "\\",merged,"\\No_DST\\Temperature_rasters\\"))
  # file.rename(paste0(dir, "\\",merged,"\\Temperature_rasters_EXIF\\"), paste0(dir, "\\",merged,"\\No_DST\\Temperature_rasters_EXIF\\"))
  # file.move(paste0(dir, "\\",merged,"\\DJI_SDK_loop.bat"), paste0(dir, "\\",merged,"\\No_DST\\"))
  # file.move(paste0(dir, "\\",merged,"\\DJI_SDK_loop_updated.bat"), paste0(dir, "\\",merged,"\\No_DST\\"))
  
  if (!dir.exists(paste0(dir, "\\",merged,"\\Temperature_rasters_DST"))) {
    dir.create(paste0(dir, "\\",merged,"\\Temperature_rasters_DST"),recursive = TRUE)
  }
  
  if (!dir.exists(paste0(dir, "\\",merged,"\\Temperature_rasters_EXIF_DST"))) {
    dir.create(paste0(dir, "\\",merged,"\\Temperature_rasters_EXIF_DST"),recursive = TRUE)
  }
  
  #copying h20T images ending in _T (aka unprocessed thermal images) into the thermal only folder for ease in DJI SDK step
  #getting folders in the H20T folder that have DJI and end in -H20T, may need to check that all folders are named this way
  list_dir <- list.files(dir, full.names = TRUE,pattern = c("DJI.+-H20T"))
  print(list_dir)
  
  for (d in 1:length(list_dir)){
    #calling each directly seperately
    folder_dir <- list_dir[d]
    #getting allJPEGs ending in _T in that directory
    T_files <- list.files(folder_dir, pattern = "_T\\.JPG$")
    #print(T_files)
    file.copy(from = paste0(folder_dir,"\\", T_files),
              to = paste0(dir,"\\",merged,"\\", T_files), overwrite = FALSE)
  }
  
}
#no need to create merged folders, since they already exist for some


#################################################################################
#Converting to DST -- did it in excel
# Load necessary libraries
library(lubridate)
library(dplyr)

# Read the CSV file in GMT-8 timezone
data <- read.csv("D:\\Fdc_PR_Canoe\\Site_Info\\Powell_River_Canoe2_All_Data_2023_05_29_10_33_47_PDT_DaylightSavings.csv", stringsAsFactors = FALSE)

# Convert Date column to Date type
# data$Date <- mdy(data$Date)
# 
# data$Time <- as.POSIXct(data$Time, format = "%H:%M", tz = "America/Vancouver")
# # 
# # Combine Date and Time columns to create DateTime column in PST
# data$DateTime_PST <- with_tz(as.POSIXct(paste(data$Date, format(data$Time, "%H:%M")), format = "%Y-%m-%d %H:%M", tz = "America/Los_Angeles"), tzone = "America/Los_Angeles")
# 
# # Convert DateTime_PST to the appropriate timezone (PDT) for British Columbia, Canada
# data$DateTime <- with_tz(data$DateTime_PST, tzone = "America/Vancouver")
# 
# # Remove the original Date, Time, and DateTime_PST columns
# data <- dplyr::select(data, -c(Date, Time, DateTime_PST))

###
# # Convert Time column to POSIXct type
# data$Time <- as.POSIXct(data$Time, format = "%H:%M", tz = "PST")
# 
# # Convert Date and Time to appropriate timezone (GMT-8/GMT-7 with DST - British Columbia, Canada)
# data$DateTime <- with_tz(as.POSIXct(paste(data$Date, format(data$Time, "%H:%M")), format = "%Y-%m-%d %H:%M", tz = "PST"), tzone = "America/Vancouver")
# 
# # Remove the original Date and Time columns
# data <- select(data, -c(Date, Time))
# 
# # View the resulting data frame
# print(data)

################################################################################

################################################################################

#STEP 1: Weather Data: Humidity, Ambient Temp

################################################################################
##
#getting image capture date from exif data of the first image in the H20T folder
#"2022_04_23" "2022_05_08" "2022_05_27" "2022_06_08" "2022_06_23" "2022_07_06" "2022_07_22" "2022_07_27" "2022_08_05" "2022_08_24" "2022_09_08" "2022_09_20" "2022_10_05" "2022_10_20"
# 2022_11_19, 2023_02_17,"2023_03_22", "2023_04_05", "2023_05_10")#, "2023_05_24
# flight_dates_DayST
data_date <- "2023_05_24"
dir <- paste0("I:\\PARSER_Ext\\",site, "\\Flights\\", data_date, "\\1_Data\\H20T")

time_file_list <- list.files(paste0(dir,"\\",merged), pattern = "_T\\.JPG$")
img_1 <- time_file_list[1]
img_1
start_exif_flight_date <- data.frame(exif_read(paste0(dir,"\\",merged,"\\",img_1),
                                               tags = "CreateDate",
                                               quiet = TRUE))

#getting flight date for the h20T image (and therefore the flight) that will be used to get the humidity and amb temperature for that flight
flight_date <- as.Date(start_exif_flight_date$CreateDate, '%Y:%m:%d %H:%M:%S')
flight_date

#Get start flight time
start_flight_date_time <- start_exif_flight_date$CreateDate
start_flight_date_time

#Get end flight time
img_last <- tail(time_file_list, n=1) #n=1
#img_last

end_flight_date_time <- data.frame(exif_read(paste0(dir,"\\",merged,"\\",img_last),
                                             tags = "CreateDate",
                                             quiet = TRUE))

end_flight_date_time <- end_flight_date_time$CreateDate
end_flight_date_time

### RUN UP TO HERE THEM LOOK AT START AND STOP TIMES TO INFORM NEXT PART

ENTER start and stop times from above, ie if start was 11:15 put 11:00:00 and if end was 1:23 put 14:00:00 (2pm) as the end
start_time_avg <- "10:00:00" #Based off of start_flight_date_time from above
stop_time_avg <- "13:00:00" #Based off of end_flight_date_time from above

#Looking at time in 15 min intervals
#setting up weights

These weights will change with each iteration - left uncommented so we cant accidently run the script without changing this part
#if flight was from 10-11, then start and end weight swould be 1 and the mid weight can be commented out
# I will be usually either 0.25 (15min), 0.5 (30min), 0.75 (45min) or 1 (1 hr) as weights
# ie for a flight that started at 10:15 and ended at 11:30, the start weight would be 0.75 (for 10am data), mid weight would be 1 (for 11am data), and end would be 0.5 (for 12pm data)
#round down for start and up for end ie start of 10:06 would be 10 and end of 11:36 would be 11:45

new_flight_date <- as.Date(flight_date, format = '%Y-%m-%d')
new_flight_date

#loading in CFS HOBO weather data
cfs_weather <- data

colnames(data)
#cfs_weather

new_column_names <- gsub("\\.\\..*", "", colnames(cfs_weather))
colnames(cfs_weather) <- new_column_names

cfs_weather$Date <- as.Date(cfs_weather$Date, format = "%m/%d/%y")

# For 2 time points ----------------------------------------------------------------------------
start_weight <- 1
end_weight <- 0.75

sum_weight <- sum(start_weight,
                  end_weight) 

#creating df with columns that average humidity per date
cfs_weather_time_avg <- cfs_weather%>%
  group_by(Date)%>%
  filter(Date == new_flight_date ) %>%
  filter(Time <= stop_time_avg & Time >= start_time_avg)%>% #filtering only for humidity values taken between 10am and 12pm
  mutate( Average_hum = (start_weight*RH[1]
                         + end_weight*RH[2])/sum_weight,
          
          Average_air_temp = (start_weight*Temperature[1]
                              + end_weight*Temperature[2])/sum_weight)

cfs_weather_time_avg
# For 3 time points ----------------------------------------------------------------------------
start_flight_date_time
end_flight_date_time
#Ie: Flight starts at 10:15am and ends at 11:30
start_weight <- 0.25 # 75% of 10am
mid_weight <- 1 # 100% of  11am 
end_weight <- 0.25 # 50% of 12pm

sum_weight <- sum(start_weight,
                  mid_weight,
                  end_weight) 


#creating df with columns that average humidity per date
cfs_weather_time_avg <- cfs_weather%>%
  group_by(Date)%>%
  filter(Date == new_flight_date ) %>%
  filter(Time <= stop_time_avg & Time >= start_time_avg)%>% #filtering only for humidity values taken between 10am and 12pm
  mutate( Average_hum = (start_weight*RH[1]
                         + mid_weight*RH[2] #comment out this line if no mid_weight
                         + end_weight*RH[3])/sum_weight,
          
          Average_air_temp = (start_weight*Temperature[1]
                              + mid_weight*Temperature[2]#comment out this line if no mid_weight
                              + end_weight*Temperature[3])/sum_weight)
cfs_weather_time_avg
#####
#checking dimension
dim(cfs_weather_time_avg)
#checking values (for all dates)
avg_hum <- cfs_weather_time_avg$Average_hum
avg_hum
avg_temp <- cfs_weather_time_avg$Average_air_temp
avg_temp
date <- as.Date(cfs_weather_time_avg$Date, '%d-%m-%Y')
date

#creating a data frame  with unique averaged values (from 11am-12pm) humidity and dates
Canoe_flight_table <- data.frame(
  date,
  avg_hum,
  avg_temp
)
dim(Canoe_flight_table)
#removing duplicated values (ie time is 11 and 12, and both have the same averaged humidity and amp temp measurments)
Canoe_flight_table <- unique(Canoe_flight_table)

#resetting index in table to go from 1-N
rownames(Canoe_flight_table) <- NULL
dim(Canoe_flight_table)
Canoe_flight_table

#getting averaged humidity (11am-12pm) from CFS logger for flight date
flight_humidity <- Canoe_flight_table[Canoe_flight_table$date == flight_date, "avg_hum"]
flight_humidity

flight_amb_temp <- Canoe_flight_table[Canoe_flight_table$date == flight_date, "avg_temp"]
flight_amb_temp

################################################################################

#STEP 2: DJI SDK in Visual Studio : convert to temp in C : 

################################################################################

#getting temp using DJI SDK
#below section is old directions to open powershell and run script in the shell but now an .bat file with be written into the specified directory that does this, im just keeping it in the code in case we need it again
#####
#Just needed to download DJI SDK and most recent Visual Studio
#Open windows powershell:C:\Users\owaite\AppData\Roaming\Microsoft\Windows\Start Menu\Programs\Windows PowerShell (or can look in the start menu)

#change directory to folder with dji_irp.exe : cd "C:\dji_thermal_sdk_v1.3_20220517\utility\bin\windows\release_x64"
#below is code used in windows powershell for a single image (path: C:\Users\owaite\Downloads\dji_thermal_sdk_v1.3_20220517\utility\bin\windows\release_x64)
#.\dji_irp.exe -s "G:\Cw_PR_Rainbow\2022_05_07\1_Data\H20\Cw_PR_Rainbow_plotA_2022_05_07_80_65_90_1.2\DJI_202205071532_008_cedarAh20t\DJI_20220507153545_0001_T.JPG" -a measure -o measure.raw
#code to convert entire folder :
#./dji_irp_omp.exe -s G:\Cw_PR_Rainbow\2022_05_07\1_Data\H20\Cw_PR_Rainbow_plotA_2022_05_07_80_65_90_1point2\DJI_202205071532_008_cedarAh20t_VS_DJI_SDK_directory_test\Thermal_H20T\ -a measure -o G:\Cw_PR_Rainbow\2022_05_07\1_Data\H20\Cw_PR_Rainbow_plotA_2022_05_07_80_65_90_1point2\DJI_202205071532_008_cedarAh20t_VS_DJI_SDK_directory_test\Thermal_DJI_SDK_raw\measure_p
#####

#
#Parameters we need to confirm
distance <- 25
distance#Using max distance of 25m meter since we are flying higher (~40m away from object for h20T depending on site), See DJI quote #55 here https://forum.dji.com/forum.php?mod=viewthread&tid=230321&extra=&page=2
flight_humidity #from HOBO for Canoe site

Reflection : reflection of target is currently set to 23 range is -40 to 500
^ Reflected Temperature: https://dl.djicdn.com/downloads/dji_dtat/20220630/DJI+Thermal+Analysis+Tool+3_User+Guide_en.pdf the surface of the target that is measured could reflect the energy radiated by the surrounding objects. This reflected energy could be picked up by the camera along with the radiation, which could cause an error in the temperature reading. If there are no objects with extreme high or low temperatures nearby, set this parameter as the ambient temperature. Reflected temperature configurations could affect the measurement result, and the bigger the difference between the reading and the ambient temperature, the bigger the impact. see ambient temp calculate above from cfs data, also loaded below flight_amb_temp

emissivity <- 0.98
emissivity
#default emissivity is set to 1
# trees in general are 0.98: http://www.treethermography.it/infrared_radiation.htm
#another source that used 0.98 for conifers: https://www.sciencedirect.com/science/article/pii/S0168192317303301
#another course using 0.98 for conifers (Pine in this case) : https://www.sciencedirect.com/science/article/pii/S003442579600123X 
#to calculate the true averaged emissivity value, we would need to know the true temeprature : https://reader.elsevier.com/reader/sd/pii/0168192395023135?token=B6C189C4F8E67AFEE720AC4F9E6755B03D88B74588FB5629439ACC70360D4782DEDCA3BED6740F24C5E2A72E4F606713&originRegion=us-east-1&originCreation=20221024174023

#Nicholas and I agreed to use 0.98 for the emissivity value ##########################################################################################################

########
#Editing .bat file for DJI SDK tool and running in terminal
########

#copy omp.bat file into the /Merged_T folder
omp_dir <- "C:/Users/owaite/Documents/Scripts/DJI_SDK_TemperatureConversion_Script"

file.copy(from = paste0(omp_dir,"\\", "DJI_SDK_loop.bat"),
          to = paste0(dir,"\\",merged,"\\","DJI_SDK_loop.bat"), overwrite = FALSE)

#modifying .bat file info: https://stackoverflow.com/questions/46415031/r-automatically-modify-batch-files
#opening .bat file to edit
bat_old <- file(paste0(dir,"\\",merged,"\\","DJI_SDK_loop.bat"))
#reading in lines of .bat file to edit and saving them out
bat_new <- readLines(bat_old)
close(bat_old)


#replacing values
#gsub(search_term, replacement_term, string_searched, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
comment out mid_weight line if necessary
bat_new <- gsub("hum_change",flight_humidity, bat_new)
bat_new <- gsub("dist_change",distance, bat_new)
bat_new <- gsub("emis_change",emissivity,bat_new)
bat_new <- gsub("reflec_change",flight_amb_temp,bat_new)
bat_new <- gsub("start_time_avg",start_time_avg, bat_new)
bat_new <- gsub("stop_time_avg",stop_time_avg,bat_new)
bat_new <- gsub("start_weight", start_weight, bat_new)
bat_new <- gsub("mid_weight", mid_weight, bat_new) #if no mid_weight, comment this line out
bat_new <- gsub("end_weight", end_weight, bat_new)

#Write new file
fileConn <- file(paste0(dir,"\\",merged,"\\DJI_SDK_loop_updated.bat"))
writeLines(bat_new, fileConn)
close(fileConn)

#dir_shell <- paste0("I:/PARSER_Ext/",site, "/Flights/", data_date,"/1_Data/H20T")
#changing directory in windows to the Merged_T folder with the _T images
#cd /d I:\PARSER_Ext\Fdc_PR_Canoe\Flights\2022_08_24\1_Data\H20T\Merged_T
#shell(paste0("cd /d ",dir_shell,"/",merged,"/"))

#running the .bat updated file in windows terminal
shell.exec(paste0(dir,"\\",merged,"\\DJI_SDK_loop_updated.bat"))

#OUPUT: folder of new raw images with temp in binary in the /Merged_T/DJI_SDK_raw folder 

####below chunk is the old way I was trying
#dir_shell <- paste0("I:/PARSER_Ext/",site, "/Flights/", date,"/1_Data/H20T")
#changing directory in windows to the Merged_T folder with the _T images
#cd /d I:\PARSER_Ext\Fdc_PR_Canoe\Flights\2022_08_24\1_Data\H20T\Merged_T
#shell(paste0("cd /d",dir_shell,"/Merged_T/"))
#shell("omp")
##### end of old way


###########################################################################################################################################################################

#STEP 3: LOOPING OVER FOLDERS THAT ALREADY HAVE DJI_SDK_RAW FOLDERS:
#Read binary and convert to a raster: https://community.rstudio.com/t/solution-to-reading-raw-images-into-r/45435

###########################################################################################################################################################################
#library(OpenImageR)
#library(raster)

#site_names <- c("Cw_PR_Rainbow_SiteA",
"Cw_PR_Rainbow_SiteB")

site_names <- c("Fdc_PR_Canoe")

#looping through all flights per site

### leaving out 08_24 since SDK error

flight_dates_list <- c("2022_11_19", "2023_02_17","2023_03_22", "2023_04_05", "2023_05_10", "2023_05_24")
#c("2022_04_23", "2022_05_08" ,"2022_05_27", "2022_06_08", "2022_06_23", "2022_07_06" ,"2022_07_22" ,"2022_07_27" ,"2022_08_05", "2022_09_08", "2022_09_20" ,"2022_10_05", "2022_10_20")

for(i in 1:length(flight_dates_list)){
  data_date <- flight_dates_list[i]
  dir <- paste0("I:\\PARSER_Ext\\",site,"\\Flights\\", data_date, "\\1_Data\\H20T")
  print(dir)
  
  dir_raw <- paste0(dir,"\\",merged,"\\DJI_SDK_raw\\")
  raw_list <- list.files(dir_raw)
  length(raw_list)
  
  #seeing how long it takes for code to run 
  start.time <- Sys.time()
  
  #loop that only writes out a folder with the Temperature rasters
  for (i in 1:length(raw_list)){
    #for (i in 1:3){
    wh <- c(640, 512)
    print("new")
    #str_m <- paste0("measure_p_",i-1,".raw") #i-1 because .raw images start at zero not 1
    str_m <- raw_list[i]
    print(str_m)
    
    #getting name of .raw file but without the .raw extension so it can be saved as a tiff with the same name as the .raw (which has the same name as the original H20T images) so they can be swapped after alignment in metashape
    name <- substr(str_m,1,nchar(str_m)-4)
    print(name)
    
    #If else below allows you to rerun this code from where it left off if R aborts the session
    test_exists <- paste0(dir,"\\",merged,"\\Temperature_rasters_EXIF_DST\\", name,".tiff")
    
    if (file.exists(test_exists)){
      print("exists")
    } else {
      #divide by 2 because 16 means 2 bit integers (?)
      #print(paste0(dir_raw, str_m))
      
      file.info(paste0(dir_raw, str_m))$size/2 
      #getting product of width and height and checking it is correct
      prod(wh)
      #getting numerical values
      v <- readBin(paste0(dir_raw,str_m), what = "integer", 
                   n = prod(wh), size = 2,  
                   signed = TRUE, endian = "little")
      
      #temp in deg celcius, DJI requires we divide by 10 here for degrees in celcius
      v_temp <- v/10
      ## check the values
      range(v_temp)
      ## creating a matrix of the proper size
      matrix <- matrix(v_temp, wh[1], wh[2])[wh[1]:1,] 
      #matrix
      #needs library OpenImageR
      #rotate the matrix by 90 to get correct orientation of image
      mat_rotated = rotateFixed(matrix, 90)
      #to view image
      #image(matrix(v_temp, wh[1], wh[2])[wh[1]:1,], useRaster = TRUE, col = grey.colors(256))
      rast <- raster(mat_rotated) #creating a raster 
      #plot(rast)
      #matching original image to measure.raw file
      #original h20T images:
      dir_h20t <- paste0(dir,"\\",merged,"\\")
      #h20t_list <- list.files(dir_h20t, pattern = "_T\\.JPG$")
      
      #since names are the same, here we call the name of the raw file -.raw and add .JPG
      str_h20t <- paste0(substr(str_m, 1, nchar(str_m)-3),"JPG")
      
      #reading in jpg h20T raster
      r_h20t <- raster(paste0(dir_h20t,str_h20t))
      
      r_h20t_path <- paste0(dir_h20t,str_h20t) #for later exiftool
      print(r_h20t_path)
      
      # the extent is bound by the resolution that you have assigned to it - so must change extent then resolution: https://stackoverflow.com/questions/36019332/copy-metadata-between-two-raster-objects-r/
      #need to add these before exif because exif command used wont over write already written ones
      extent(rast) <- extent(r_h20t)
      res(rast) <- res(r_h20t)
      #crs(rast) <- crs(r_h20t)
      
      #writing out the itterator
      print(i)
      
      print(paste0(dir,"\\",merged,"\\Temperature_rasters_DST\\", name, ".tiff"))
      writeRaster(rast, paste0(dir,"\\",merged,"\\Temperature_rasters_DST\\", name,".tiff"), overwrite = TRUE)
      r_temperature_path <- paste0(dir,"\\",merged,"\\Temperature_rasters_DST\\", name,".tiff")
      
      #STEP 3: Use EXIFTOOL in terminal to add all remaining/missing metadata from the original H20T images to the new single band TIFFS
      out_dir <- paste0(dir,"\\",merged,"\\Temperature_rasters_EXIF_DST\\", name,".tiff")
      #shell lets you run windows terminal cmds from R
      shell(paste0("exiftool -wm cg -tagsfromfile ",r_h20t_path," -all:all ",r_temperature_path, " -o ",out_dir))
      #output are temperature tiffs with same metadata as original h20T images
    }
    percent <- i/length(raw_list)*100
    print(paste0("percent complete: ", round(percent, 3)))
  }
  
  end.time <- Sys.time()
  time.taken <- round(end.time - start.time,2)
  print(time.taken)
}


###########################################################################################################################################################################

#STEP 3: Read binary and convert to a raster: https://community.rstudio.com/t/solution-to-reading-raw-images-into-r/45435

###########################################################################################################################################################################
#library(OpenImageR)
#library(raster)

dir_raw <- paste0(dir,"\\",merged,"\\DJI_SDK_raw\\")
raw_list <- list.files(dir_raw)
length(raw_list)



#seeing how long it takes for code to run 
start.time <- Sys.time()

#loop that only writes out a folder with the Temperature rasters
for (i in 1:length(raw_list)){
  #for (i in 1:3){
  wh <- c(640, 512)
  print("new")
  #str_m <- paste0("measure_p_",i-1,".raw") #i-1 because .raw images start at zero not 1
  str_m <- raw_list[i]
  print(str_m)
  
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
    #getting product of width and height and checking it is correct
    prod(wh)
    #getting numerical values
    v <- readBin(paste0(dir_raw,str_m), what = "integer", 
                 n = prod(wh), size = 2,  
                 signed = TRUE, endian = "little")
    
    #temp in deg celcius, DJI requires we divide by 10 here for degrees in celcius
    v_temp <- v/10
    ## check the values
    range(v_temp)
    ## creating a matrix of the proper size
    matrix <- matrix(v_temp, wh[1], wh[2])[wh[1]:1,] 
    #matrix
    #needs library OpenImageR
    #rotate the matrix by 90 to get correct orientation of image
    mat_rotated = rotateFixed(matrix, 90)
    #to view image
    #image(matrix(v_temp, wh[1], wh[2])[wh[1]:1,], useRaster = TRUE, col = grey.colors(256))
    rast <- raster(mat_rotated) #creating a raster 
    #plot(rast)
    #matching original image to measure.raw file
    #original h20T images:
    dir_h20t <- paste0(dir,"\\",merged,"\\")
    #h20t_list <- list.files(dir_h20t, pattern = "_T\\.JPG$")
    
    #since names are the same, here we call the name of the raw file -.raw and add .JPG
    str_h20t <- paste0(substr(str_m, 1, nchar(str_m)-3),"JPG")
    
    #reading in jpg h20T raster
    r_h20t <- raster(paste0(dir_h20t,str_h20t))
    
    r_h20t_path <- paste0(dir_h20t,str_h20t) #for later exiftool
    print(r_h20t_path)
    
    # the extent is bound by the resolution that you have assigned to it - so must change extent then resolution: https://stackoverflow.com/questions/36019332/copy-metadata-between-two-raster-objects-r/
    #need to add these before exif because exif command used wont over write already written ones
    extent(rast) <- extent(r_h20t)
    res(rast) <- res(r_h20t)
    #crs(rast) <- crs(r_h20t)
    
    #writing out the itterator
    print(i)
    
    print(paste0(dir,"\\",merged,"\\Temperature_rasters\\", name, ".tiff"))
    writeRaster(rast, paste0(dir,"\\",merged,"\\Temperature_rasters\\", name,".tiff"), overwrite = TRUE)
    r_temperature_path <- paste0(dir,"\\",merged,"\\Temperature_rasters\\", name,".tiff")
    
    #STEP 3: Use EXIFTOOL in terminal to add all remaining/missing metadata from the original H20T images to the new single band TIFFS
    out_dir <- paste0(dir,"\\",merged,"\\Temperature_rasters_EXIF\\", name,".tiff")
    #shell lets you run windows terminal cmds from R
    shell(paste0("exiftool -wm cg -tagsfromfile ",r_h20t_path," -all:all ",r_temperature_path, " -o ",out_dir))
    #output are temperature tiffs with same metadata as original h20T images
  }
  percent <- i/length(raw_list)*100
  print(paste0("percent complete: ", round(percent, 3)))
}

end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken


#----Cleaning up for presentation-----------------------------

#divide by 2 because 16 means 2 bit integers (?)

#Dimensions of .jpg raster
wh <- c(640, 512) #Can check with: file.info(paste0(dir_raw, str_m))$size/2 

str_m <- "name of img.jpg"

#Isolating name of .raw file so the end product tiff can be named the same
name <- substr(str_m,1,nchar(str_m)-4)

#Retrieving numerical values from binary
v <- readBin(paste0(dir_raw,str_m), what = "integer", 
             n = prod(wh), size = 2,  
             signed = TRUE, endian = "little")

#Temp in Celsius, DJI requires we divide by 10 
v_temp <- v/10

#Check values
range(v_temp)

#Create a matrix with the proper size
matrix <- matrix(v_temp, wh[1], wh[2])[wh[1]:1,] 

#Rotate matrix by 90 deg to get correct orientation 
mat_rotated = rotateFixed(matrix, 90)

#Check and view image
image(matrix(v_temp, wh[1], wh[2])[wh[1]:1,], useRaster = TRUE, col = grey.colors(256))

rast_temp <- raster(mat_rotated) #creating a raster 

#Matching original .jpg image to .raw file
#Call the core name of the raw file without the .raw and add .JPG
str_h20t <- paste0(substr(str_m, 1, nchar(str_m)-3),"JPG")

#Reading in .jpg raster
r_h20t <- raster(paste0("direcotry to .jpgs",str_h20t))

#Adding extent and res before copying over the JPG exif 
#because exif command won't overwrite already written attributes
extent(rast) <- extent(r_h20t)
res(rast) <- res(r_h20t)

#path to the .jpg image
r_h20t_path <- paste0(dir_h20t,str_h20t) #for later exiftool
print(r_h20t_path)

#writing out the temperature raster (not necessary - I do it as a check)
writeRaster(rast, paste0(dir,"\\Temperature_rasters", name,".tiff"), overwrite = TRUE)
r_temperature_path <- paste0(dir,"\\Temperature_rasters\\", name,".tiff")

#STEP 3: Use EXIFTOOL in terminal to add all remaining/missing metadata from the original H20T JPGs to the new single band TIFFS
out_dir <- paste0(dir,"\\Temperature_rasters_EXIF\\", name,".tiff")
#shell lets you run windows terminal cmds from R
shell(paste0("exiftool -wm cg -tagsfromfile ",r_h20t_path," -all:all ",r_temperature_path, " -o ",out_dir))
#output are temperature tiffs with same metadata as original h20T JPGs


#############

bat_old <- file(paste0(dir,"DJI_SDK_loop.bat"))

#Reading in lines of .bat file to edit and closing the file
bat_new <- readLines(bat_old)
close(bat_old)

#Replacing placeholders in the original bat file 
bat_new <- gsub("hum_change",flight_humidity, bat_new)
bat_new <- gsub("dist_change",distance, bat_new)
bat_new <- gsub("emis_change",emissivity,bat_new)
bat_new <- gsub("reflec_change",flight_amb_temp,bat_new)
bat_new <- gsub("start_time_avg",start_time_avg, bat_new)
bat_new <- gsub("stop_time_avg",stop_time_avg,bat_new)
bat_new <- gsub("start_weight", start_weight, bat_new)
bat_new <- gsub("end_weight", end_weight, bat_new)

#Write new file
fileConn <- file(paste0(dir,"\\DJI_SDK_loop_updated.bat"))
writeLines(bat_new, fileConn)
close(fileConn)

#Running the .bat updated file in windows terminal
shell.exec(paste0(dir,"\\",merged,"\\DJI_SDK_loop_updated.bat"))




