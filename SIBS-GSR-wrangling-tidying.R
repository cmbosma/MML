### Psychophys GSR data wrangling and tidying code for SIBS
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

# Packages needed
packages <- c("tidyverse",
              "readxl",
              "psych",
              "haven",
              "magrittr")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Load packages into workspace
invisible(lapply(packages, library, character.only = TRUE))

# old way of loading packages
#library(tidyverse) # This is the meta package for data wrangling
#library(readxl) # make sure it is the most recent version (v1.0.0)
#library(psych) # used for 'describe' function 
#library(haven) # for exporting data frame to an SPSS data file (.sav)
#library(magrittr) # library for bind_cols() function

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
      
      response1 <- temp_df %>% # pulls RSA values
        filter(.[[1]] == "Tonic SCL") %>% 
        select(-1)
      
      response2 <- temp_df %>% 
        filter(.[[1]] == "Mean SC") %>%
        select(-1)
      
     bind_cols(meta_dat, response1, response2) %>% # creates new df with pulled values 
       set_colnames(paste0("X", 1:ncol(.)))
  })
}

## BASELINE: LOAD EXCEL SPREADSHEETS, EXTRACT VALUES, AND CREATE DATAFRAME
## -----------------------------------------------------------------------------

# extracting EDA values and creating dataframe using get_df() function
baseline_df <- get_df(
  data_path = "C:/Users/Mindware/Desktop/SIBS Mindware Data", 
  file_match = "EDABaselineoutput.xlsx$"
    )

print(baseline_df) # quick check

# Add variable names 
var_names <- c("id", "condition",
               "baseline_scl_min1", "baseline_scl_min2","baseline_scl_min3", "baseline_scl_min4", "baseline_scl_min5","baseline_scl_min6", "baseline_scl_min7",
               "baseline_meansc_min1", "baseline_meansc_min2","baseline_meansc_min3", "baseline_meansc_min4", "baseline_meansc_min5","baseline_meansc_min6", "baseline_meansc_min7")
			   
colnames(baseline_df) <- var_names

names(baseline_df) # check names

head(baseline_df, 10) # check that variable names were assigned correctly

View(baseline_df)


## INCLUSION: LOAD EXCEL SPREADSHEETS, EXTRACT VALUES, AND CREATE DATAFRAME
## -----------------------------------------------------------------------------

# Extracting EDA values and creating a data frame
inclusion_df <- get_df(
  data_path = "C:/Users/Mindware/Desktop/SIBS Mindware Data", 
  file_match = "EDAInclusionmoutput.xlsx$"
    )

print(inclusion_df)

# Add variable names 
var_names <- c("id", "condition",
               "inc_scl_min1", "inc_scl_min2",
               "inc_meansc_min1", "inc_meansc_min2")
			   
colnames(inclusion_df) <- var_names

names(inclusion_df) # check names

head(inclusion_df, 10)  # check that variable names were assigned correctly

View(inclusion_df)

## EXCLUSION: LOAD EXCEL SPREADSHEETS, EXTRACT VALUES, AND CREATE DATAFRAME
## -----------------------------------------------------------------------------

# Extracting EDA values and creating a data frame
exclusion_df <- get_df(
  data_path = "C:/Users/Mindware/Desktop/SIBS Mindware Data", 
  file_match = "EDAExclusionoutput.xlsx$"
    )

print(recovery_df) # quick check

# Add variable names 
var_names <- c("id", "condition",
               "exc_scl_min1", "exc_scl_min2", "exc_scl_min3", "exc_scl_min4", 
               "exc_meansc_min1", "exc_meansc_min2", "exc_meansc_min3", "exc_meansc_min4")
			   
colnames(recovery_df) <- var_names

names(exclusion_df) # check names
           
head(exclusion_df, 2) # check that variable names were assigned correctly        

View(exclusion_df)

## COMBINE DATA FRAMES 
## -----------------------------------------------------------------------------

# Amend basline data frame with inclusion data frame by id and condition
EDA_temp <- dplyr::full_join(baseline_df, inclusion_df,
			by = c("id", "condition"))

# Amend with recovery data frame by id
EDA_df <- dplyr::full_join(EDA_temp, exclusion_df,
			by = c("id", "condition"))


# Print head data frame
head(EDA_df, 2) 
dim(EDA_df) # Can cross reference # of observations and # of variables

View(EDA_df) # Take a look at the data frame

## COMPUTING NEW VARIABLES
## -----------------------------------------------------------------------------

# Save all variable names that should be numeric to a vector
cols_num <- c( "baseline_scl_min1", "baseline_scl_min2","baseline_scl_min3", "baseline_scl_min4", "baseline_scl_min5","baseline_scl_min6", "baseline_scl_min7",
               "baseline_meansc_min1", "baseline_meansc_min2","baseline_meansc_min3", "baseline_meansc_min4", "baseline_meansc_min5","baseline_meansc_min6", "baseline_meansc_min7",
               
               "inc_scl_min1", "inc_scl_min2",
               "inc_meansc_min1", "inc_meansc_min2",
               
               "exc_scl_min1", "exc_scl_min2", "exc_scl_min3", "exc_scl_min4", 
               "exc_meansc_min1", "exc_meansc_min2", "exc_meansc_min3", "exc_meansc_min4")

# convert to numeric
EDA_df[cols_num] <- sapply(EDA_df[cols_num], as.numeric) 

str(EDA_df) # check that RSA variables were converted to numeric

## REMOVING PILOT DATA FROM THE DATA FRAM
## -----------------------------------------------------------------------------

# Note: All participants before SIBS086 are part of the pilot phase of SIBS

# using dplyr
EDA_df %>% slice(1:n) 

# base r using indexing
EDA_df <- EDA_df[-c(1:n), ] # enter number row for last row to be deleted


## CHECKING DATA FRAME
## -----------------------------------------------------------------------------

# Structure and variable types
str(EDA_df)


# View the data frame 
# IMPORTANT: Review data table that is produced for issues
View(EDA_df) 


## EXPORT DATA
## -----------------------------------------------------------------------------

# Exporting data frame to SPSS data file (.sav) for collaborating

# check current directory
getwd() 
# Set new working directory for saving .sav file
setwd("C:/Users/Mindware/Desktop/SIBS Mindware Data/SIBS SPSS Data Files") 

# SPSS data frame
write_sav(EDA_df, "SIBS_EDA_date_.sav") # SPSS data file should be ready to go!

# .csv file for excel 
write_csv(EDA_df, "SIBS_EDA_data.csv") # Write .csv for using this data frame to merge with other data sets using R







