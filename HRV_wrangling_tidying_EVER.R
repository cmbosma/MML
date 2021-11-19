
### Psychophys HRV data wrangling and tidying code for the EVER Study
## Colin M. Bosma

## NOTES FOR RESEARCH ASSISTANTS
## -----------------------------------------------------------------------------

# Excel needs to not be open while running this code.
# Do not edit code without contacting Colin first.
# Select lines of code, then press Ctrl + Enter keys to run
# Make sure all directories are correct and there are no typos in the file names


## Step 1: LOAD PACKAGES
## -----------------------------------------------------------------------------

library(tidyverse) # multiple useful libraries 
library(readxl) # make sure it is the most recent version (v1.0.0)
library(haven) # for exporting data frame to an SPSS data file (.sav)
library(magrittr) # library for bind_cols() function


## Step 2: CREATE FUNCTION FOR PULLING ID AND RSA VALUES FROM SPREADSHEETS
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
      
      meta_dat <- temp_df %>% # pulls ID from row with "File Name" and column 3
        filter(.[[1]] == "File Name") %>%
        select(c(2)) 
      
      response <- temp_df %>% # pulls RSA values from RSA row
        filter(.[[1]] == "RSA") %>%
        select(-1)
      
      bind_cols(meta_dat, response) %>% # creates new data frame with pulled values
        set_colnames(paste0("X", 1:ncol(.)))
    })
}

# "get_df" should show up in the environment tab (top right of the window) as a function

## Step 3a: NEUTRAL: LOAD EXCEL SPREADSHEETS, EXTRACT VALUES, AND CREATE DATAFRAME
## -----------------------------------------------------------------------------

# extracting values and creating dataframe using get_df() function
neutral_df <- get_df(
  data_path = "/Users/colinbosma/Downloads/EVER Cleaned HRV Data", # directory with parent folder containing all AAE mindware data
  file_match = "Neutral Vid Output.xlsx$" # tells R to look for files with "Baselineoutput.xlsx" at the end of the file name
)

# Create an ID variable from the file name, drop file name and extra RSA column with "NaN" 
neutral_df <- neutral_df %>% 
  dplyr::mutate(id = str_extract(X1, "\\d\\d\\d")) %>%
  mutate(id = paste0("EVER", id)) %>%
  select(id, everything()) %>%
  select(-c(X1, X5))


head(neutral_df) # quick check


# Create variable names and save them to a vector
var_names <- c("id", "neutral_min1", "neutral_min2","neutral_min3")
               
# Adding variable names to the baseline_df data frame
colnames(neutral_df) <- var_names

# check names
names(neutral_df) 

# check that variable names were assigned correctly
head(neutral_df, 2) 


## Step 3b: Mood Induction (MI): LOAD EXCEL SPREADSHEETS, EXTRACT VALUES, AND CREATE DATA FRAME
## -----------------------------------------------------------------------------

# extracting values and creating dataframe using get_df() function
mi_df <- get_df(
  data_path = "/Users/colinbosma/Downloads/EVER Cleaned HRV Data",
  file_match = "Mood Vid Output.xlsx$"
)

# Create an ID variable from the file name, drop file name and extra RSA column with "NaN" 
mi_df <- mi_df %>% 
  dplyr::mutate(id = str_extract(X1, "\\d\\d\\d")) %>%
  mutate(id = paste0("EVER", id)) %>%
  select(id, everything()) %>%
  select(-X1)


head(mi_df) # quick check

# Create variable names and save them to a vector
var_names <- c("id", "mi_min1", "mi_min2", "mi_min3") # Notice how the names go to the last column with an RSA value

# Adding variable names to the mi_df data frame
colnames(mi_df) <- var_names

# check names
names(mi_df) 

# check that variable names were assigned correctly
head(mi_df, 2)  # check that variable names were assigned correctly (i.e., ID has participant IDs)



## Step 3c: RECOVERY: LOAD EXCEL SPREADSHEETS, EXTRACT VALUES, AND CREATE DATAFRAME
## -----------------------------------------------------------------------------

# Extracting RSA values and creating a data frame
recovery_df <- get_df(
  data_path = "/Users/colinbosma/Downloads/EVER Cleaned HRV Data",
  file_match = "Recovery Output.xlsx$"
)

# Create an ID variable from the file name, drop file name and extra RSA column with "NaN" 
recovery_df <- recovery_df %>% 
  dplyr::mutate(id = str_extract(X1, "\\d\\d\\d")) %>%
  mutate(id = paste0("EVER", id)) %>%
  select(id, everything()) %>%
  select(-c(X1, X7))


head(recovery_df) # quick check

# Create variable names and save them to a vector
var_names <- c("id", "recovery_min1", "recovery_min2",
               "recovery_min3", "recovery_min4", "recovery_min5")

# Adding variable names to the recovery_df data frame
colnames(recovery_df) <- var_names

# check names
names(recovery_df) 

# check that variable names were assigned correctly
head(recovery_df, 2) 


## Step 4: COMBINE DATA FRAMES
## -----------------------------------------------------------------------------

# Amend baseline data frame with mi data frame by id
RSA_temp <- dplyr::full_join(neutral_df, mi_df,
                             by = c("id"))

# Amend temp data frame above with recovery data frame by id to create full data frame
RSA_df <- dplyr::full_join(RSA_temp, recovery_df,
                           by = c("id"))


# Print head data frame
head(RSA_df, 2)
dim(RSA_df) # Can cross reference # of observations and # of variables


## Step 5: CHANGING VARIABLE TYPES (THIS IS OPTIONAL BUT MAY AS WELL! - CAN SKIP TO STEP 7)
## -----------------------------------------------------------------------------

# Save all variable names that should be numeric to a vector
cols_num <- c("neutral_min1", "neutral_min2","neutral_min3",
              "mi_min1", "mi_min2", "mi_min3",
              "recovery_min1", "recovery_min2", "recovery_min3",
              "recovery_min4", "recovery_min5")

# convert to numeric
RSA_df[cols_num] <- sapply(RSA_df[cols_num], as.numeric)



## Step 6: CHECKING DATA FRAME
## -----------------------------------------------------------------------------

# Structure and variable types

## Tidyverse approach
RSA_df %>% glimpse()

## Base R approach 
str(RSA_df)

# View the data frame
View(RSA_df) # Review data table in new tab for issues, such as values not pulled in or shifted over

## Step 7: EXPORT DATA
## -----------------------------------------------------------------------------

# Exporting data frame to SPSS data file (.sav) for collaborating


# Set new working directory for saving .sav file 
# Copy and paste desired directory where it says "insert-directory" below
setwd("insert-directory")
getwd() # doulbe check that it worked

# Saving new data frame to a .sav file type  - can change file name to include the date
write_sav(RSA_df, "HRVdata.sav")

# Saving new data frame to a .csv file type
write_csv(RSA_df, "HRVdata.csv")

## Okay, both a .sav and .csv file should be saved in the folder you specified and are ready to use!


## REFERENCES
## -----------------------------------------------------------------------------

# Note: skip running this section unless you want to check out the documentation

# Tidyverse website
browseURL("http://tidyverse.org")

# documentation for readxl pakage. Able to specify worksheets and columns
browseURL("https://github.com/tidyverse/readxl")

# tidyxl package documentation - import and manipulate awkward excel files
browseURL("https://cran.r-project.org/web/packages/tidyxl/index.html")
