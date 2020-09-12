### Psychophys HRV data wrangling and tidying code for SIBS
## Colin M. Bosma

## INSTRUCTIONS
## -----------------------------------------------------------------------------

# Beginning with the "LOAD PACKAGES" section, run each block of code one at a time and examine the output in the console
# Select lines of code, then press Ctrl + Enter keys to run (on windows) or click run at the top of this window. 
# Excel needs to NOT be open while running this script. 
# Be careful not to edit the code unless you are very confident. One small change can make it not work. 
# Make sure all file directories are correct and there are no typos in the file names

## TROUBLE SHOOTING ISSUES
## -----------------------------------------------------------------------------

## TYPICAL SCENARIO 1
# If the get_df() function runs and there is an error, 
# check in the console the last loaded file (the function prints the file path of each file),
# and look at the data files in the folder it was about to read in - 
# Look for typos in the file names.

## TYPICAL SCENARIO 2
# Sometimes the get_df() function will run but the values are shifted to the right for a few participants
# This typically means there is an issue with the affiliated excel spread sheet
# Open up the spread sheet and see what is going on. 
# Sometimes there aren't enough rows or there is an "NaN" value. 
# Delete "NaN" values or anything that is not a number in the RSA row. 
# If the ID or condition is missing, you just need to find the excel file and enter the info in the correct cells.

## If you have gone through these and you can't tell what is wrong, see if the issue can be fixed by some edits in excel after exporting the data
## If the data doesn't load or looks totally screwed up after trying these things, then contact Colin :)

## (OPTIONAL) REFERENCES - Do not need to run unless you want to access the references
## -----------------------------------------------------------------------------

## NOTES
  # If you delete the "#" before "browseURL" and run the code, it will open the website
  # The websites have documentation on the code used in this script. 

# Tidyverse website 
# browseURL("http://tidyverse.org")

# documentation for readxl pakage. Able to specify worksheets and columns
# browseURL("https://github.com/tidyverse/readxl")

# tidyxl package documentation - import and manipulate awkward excel files
# browseURL("https://cran.r-project.org/web/packages/tidyxl/index.html")

## STEP 1
## LOAD PACKAGES
## -----------------------------------------------------------------------------

library(tidyverse) # This is the meta package for data wrangling
library(readxl) # For working with excel files; make sure it is the most recent version (v1.0.0)
library(haven) # for exporting data frame to an SPSS data file (.sav)
library(magrittr) # library for bind_cols() function to combine data frames

## STEP 2
## CREATE FUNCTION FOR PULLING ID, GROUP, AND RSA FROM SPREADSHEETS
## -----------------------------------------------------------------------------

get_df <- function(data_path, file_match){
  list.files(
  path = data_path,
  pattern = file_match,
  recursive = TRUE,
  full.names = TRUE
  ) %>%
  map_df( ~ {
      
      print(.x) # This will print each file the function works on 
    
      temp_df <- read_excel(.x, col_names = FALSE) # temp reads files into a data frame
      
      meta_dat <- temp_df %>% # pulls ID, group
        filter(.[[1]] == "File Name") %>% 
        select(c(3:4))
      
      response <- temp_df %>% # pulls RSA values
        filter(.[[1]] == "RSA") %>% 
        select(-1)
      
     bind_cols(meta_dat, response) %>% # creates new df with pulled values 
       set_colnames(paste0("X", 1:ncol(.)))
  })
}


## STEP 3
## BASELINE: LOAD EXCEL SPREADSHEETS, EXTRACT VALUES, AND CREATE DATAFRAME
## -----------------------------------------------------------------------------

# extracting values and creating data frame using the get_df() function
  # The data_path argument should point to where the participant folders with the cleaned physio data is located
  # The file_match argument should match the end of the file names for baseline HRV and have the "$" at the end
baseline_df <- get_df(
  data_path = "C:/Users/Mindware/Desktop/SIBS Mindware Data", 
  file_match = "HRVBaselineoutput.xlsx$"
    )

# Print new data frame
  # Check that there are values for each row; there should be 9 columns
print(baseline_df) 

# Create variable names vector
var_names <- c("id", "group", "baseline_min1", "baseline_min2",
				"baseline_min3", "baseline_min4", "baseline_min5",
				"baseline_min6", "baseline_min7")

# Add variable names to new data frame
colnames(baseline_df) <- var_names

names(baseline_df) # check names

head(baseline_df, 2) # check that variable names were assigned correctly

# Open up a tab with the data frame. Review it and make sure the data look correct. 
View(baseline_df)

## STEP 4
## MOOD INUCTION: LOAD EXCEL SPREADSHEETS, EXTRACT VALUES, AND CREATE DATA FRAME
## -----------------------------------------------------------------------------

# Extracting RSA values and creating a data frame
os_df <- get_df(
  data_path = "C:/Users/Mindware/Desktop/SIBS Mindware Data", 
  file_match = "HRVOstracismoutput.xlsx$"
    )

print(os_df)

# Add variable names 
var_names <- c("id", "group", "os_min1", "os_min2", "os_min3",
				"os_min4")
			   
colnames(os_df) <- var_names

names(os_df) # check names

head(os_df, 2)  # check that variable names were assigned correctly

# Open up a tab with the data frame. Review it and make sure the data look correct. 
View(os_df)

## STEP 5
## RECOVERY: LOAD EXCEL SPREADSHEETS, EXTRACT VALUES, AND CREATE DATA FRAME
## -----------------------------------------------------------------------------

# Extracting RSA values and creating a data frame
recovery_df <- get_df(
  data_path = "C:/Users/Mindware/Desktop/SIBS Mindware Data", 
  file_match = "HRVRecoveryoutput.xlsx$"
    )

print(recovery_df) # quick check

# Add variable names 
var_names <- c("id", "group", "recovery_min1", "recovery_min2",
				"recovery_min3", "recovery_min4", "recovery_min5",
				"recovery_min6", "recovery_min7")
			   
colnames(recovery_df) <- var_names

names(recovery_df) # check names
           
head(recovery_df, 2) # check that variable names were assigned correctly        

# Open up a tab with the data frame. Review it and make sure the data look correct. 
View(recovery_df)


## STEP 6
## COMBINE DATA FRAMES 
## -----------------------------------------------------------------------------

# Amend basline data frame with os data frame by id
RSA_temp <- dplyr::full_join(baseline_df, os_df,
			by = c("id", "group"))

# Amend with recovery data frame by id
RSA_df <- dplyr::full_join(RSA_temp, recovery_df,
			by = c("id", "group"))


## STEP 7
## CHECKING DATA FRAME
## -----------------------------------------------------------------------------

# Print head data frame
head(RSA_df) 

# Can cross reference number of observations and number of variables
# Do the number of rows and columns make sense?
dim(RSA_df) 

# Structure and variable types (e.g., numeric, factor); 
# if they are not the right type, can change in SPSS later
str(RSA_df)

# View the data frame 
# IMPORTANT: Review data frame that is produced for issues
View(RSA_df) 

## STEP 8
## EXPORT DATA
## -----------------------------------------------------------------------------

# Exporting data frame to SPSS data file (.sav) for collaborating

# check current working directory
# This shoudl print the lowest level of the computer's director, which is typically the user name
getwd() 
# Set new working directory to save the SPSS file (.sav) where we want it
  # The directory here is set to save to a folder on Computer 4
setwd("C:/Users/Mindware/Desktop/SIBS Mindware Data/SIBS SPSS Data Files") 

# SPSS data frame
write_sav(RSA_df, "HRVdata_date_.sav")

# .csv file for excel 
write_csv(RSA_df, "SIBS_HRV_data.csv")

# SPSS data file should be ready to go!

# Write .csv for using this data frame to merge with other data sets using R



