


### Psychophys HRV data wrangling and tidying code for SIBS
## Colin M. Bosma

## NOTES FOR RESEARCH ASSISTANTS
## -----------------------------------------------------------------------------

# Excel needs to not be open while running this code. 
# Do not edit code without contacting Colin first. 
# Select lines of code, then press Ctrl + Enter keys to run 
# Make sure all directories are correct and there are no typos in the file names


## REFERENCES
## -----------------------------------------------------------------------------


# Tidyverse website 
# browseURL("http://tidyverse.org")

# documentation for readxl pakage. Able to specify worksheets and columns
# browseURL("https://github.com/tidyverse/readxl")

# tidyxl package documentation - import and manipulate awkward excel files
# browseURL("https://cran.r-project.org/web/packages/tidyxl/index.html")


## LOAD PACKAGES
## -----------------------------------------------------------------------------

library(tidyverse) # This is the meta package for data wrangling
library(readxl) # make sure it is the most recent version (v1.0.0)
library(psych) # used for 'describe' function 
library(haven) # for exporting data frame to an SPSS data file (.sav)
library(magrittr) # library for bind_cols() function

sessionInfo() # tells you what you have loaded in your workspace

## CREATE FUNCTION FOR PULLING ID, GROUP, CONDITION, AND RSA FROM SPREADSHEETS
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
    
      temp_df <- read_excel(.x, col_names = FALSE) # temp reads files into df
      
      meta_dat <- temp_df %>% # pulls ID, group, and condition
        filter(.[[1]] == "File Name") %>% 
        select(c(3:4))
      
      response <- temp_df %>% # pulls RSA values
        filter(.[[1]] == "RSA") %>% 
        select(-1)
      
     bind_cols(meta_dat, response) %>% # creates new df with pulled values 
       set_colnames(paste0("X", 1:ncol(.)))
  })
}

## BASELINE: LOAD EXCEL SPREADSHEETS, EXTRACT VALUES, AND CREATE DATAFRAME
## -----------------------------------------------------------------------------

# extracting values and creating dataframe using get_df() function
baseline_df <- get_df(
  data_path = "C:/Users/Mindware/Desktop/SIBS Mindware Data", 
  file_match = "HRVBaselineoutput.xlsx$"
    )

print(baseline_df) # quick check

# Add variable names 
var_names <- c("id", "group", "baseline_min1", "baseline_min2",
				"baseline_min3", "baseline_min4", "baseline_min5",
				"baseline_min6", "baseline_min7")
			   
colnames(baseline_df) <- var_names

names(baseline_df) # check names

head(baseline_df, 2) # check that variable names were assigned correctly

View(baseline_df)


## MI: LOAD EXCEL SPREADSHEETS, EXTRACT VALUES, AND CREATE DATAFRAME
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

View(os_df)

## RECOVERY: LOAD EXCEL SPREADSHEETS, EXTRACT VALUES, AND CREATE DATAFRAME
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

View(recovery_df)

## COMBINE DATA FRAMES 
## -----------------------------------------------------------------------------

# Amend basline data frame with os data frame by id
RSA_temp <- dplyr::full_join(baseline_df, os_df,
			by = c("id", "group"))

# Amend with recovery data frame by id
RSA_df <- dplyr::full_join(RSA_temp, recovery_df,
			by = c("id", "group"))


# Print head data frame
head(RSA_df, 2) 
dim(RSA_df) # Can cross reference # of observations and # of variables

View(RSA_df) # Take a look at the data frame

## COMPUTING NEW VARIABLES
## -----------------------------------------------------------------------------

# Save all variable names that should be numeric to a vector
cols_num <- c("baseline_min1", "baseline_min2", "baseline_min3", "baseline_min4",
			  "baseline_min5", "baseline_min6", "baseline_min7",
			  "os_min1", "os_min2", "os_min3", "os_min4",
			  "recovery_min1", "recovery_min2", "recovery_min3",
			  "recovery_min4", "recovery_min5", "recovery_min6",
			  "recovery_min7")

# convert to numeric
RSA_df[cols_num] <- sapply(RSA_df[cols_num], as.numeric) 

str(RSA_df) # check that RSA variables were converted to numeric

# Compute mean RSA at baseline for last 5 minutes (min5-9)
RSA_df$baseline_meanRSA <- rowMeans(RSA_df[c("baseline_min1",
                                              "baseline_min2",
                                              "baseline_min3",
                                              "baseline_min4",
                                              "baseline_min5",
                                              "baseline_min6",
                                              "baseline_min7")])
       
# Compute mean RSA at mi for last 4 minutes (min3-7)                                       
RSA_df$os_meanRSA <- rowMeans(RSA_df[c("os_min1",
                                        "os_min2",
                                        "os_min3",
                                        "os_min4")])

# Compute mean RSA at recovery for last 5 minutes (min5-9)                                       
RSA_df$recovery_meanRSA <- rowMeans(RSA_df[c("recovery_min1",
                                             "recovery_min2",
                                             "recovery_min3",
                                             "recovery_min4",
                                             "recovery_min5",
                                             "recovery_min6",
                                             "recovery_min7")])

# Let's quickly check our work
tail(names(RSA_df), 3) # check to see if variables were created 
head(RSA_df$baseline_meanRSA, 5) 
head(RSA_df$os_meanRSA, 4) 
head(RSA_df$recovery_meanRSA, 5) 


## CHECKING DATA FRAME
## -----------------------------------------------------------------------------

# Structure and variable types
str(RSA_df)


# View the data frame 
# IMPORTANT: Review data table that is produced for issues
View(RSA_df) 


## EXPORT DATA
## -----------------------------------------------------------------------------

# Exporting data frame to SPSS data file (.sav) for collaborating

# check current directory
getwd() 
# Set new working directory for saving .sav file
setwd("C:/Users/Mindware/Desktop/SIBS Mindware Data/SIBS SPSS Data Files") 

# SPSS data frame
write_sav(RSA_df, "HRVdata_date_.sav")

# .csv file for excel 
write_csv(RSA_df, "SIBS_HRV_data.csv")

# SPSS data file should be ready to go!


# Write .csv for using this data frame to merge with other data sets using R



