### Psychophys GSR data wrangling and tidying code for SIBS
## Colin M. Bosma

## GENERAL NOTES (IMPORTANT FOR RUNNING SCRIPT)
## -----------------------------------------------------------------------------

# This script works after making a copy of the SIBS Mindware DATa folder and deleting the pilot cases (everyone before SIBS086)
# - This one done because of indexing issues that earlier cases create when scraping. This script addresses indexing issues post SIBS086
# - consider writing code that removes these cases at each point where you are scraping data (can't just do it at the end)

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



## ---------------------------------------------------------------------------##
##                     PART2: PULLING TOTAL SCL VALUES                        ##
## ---------------------------------------------------------------------------##

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

      meta_dat <- temp_df %>% # pulls participant ID
        filter(.[[1]] == "File Name") %>%
        select(c(3))

      response <- temp_df %>%
        filter(.[[1]] == "Tonic SCL") %>%
        select(-1)

     bind_cols(meta_dat, response) %>% # creates new df with pulled values
       set_colnames(paste0("X", 1:ncol(.)))
  })
}

## BASELINE: LOAD EXCEL SPREADSHEETS, EXTRACT VALUES, AND CREATE DATAFRAME
## -----------------------------------------------------------------------------

# extracting EDA values and creating dataframe using get_df() function
baseline_df <- get_df(
  data_path = "/Users/colinbosma/Desktop/SIBS APC Data 02.12.20",
  file_match = "EDABaselineoutput.xlsx$"
    )

print(baseline_df) # quick check

# Add variable names
var_names <- c("id",
               "baseline_scl_min1", "baseline_scl_min2","baseline_scl_min3",
               "baseline_scl_min4", "baseline_scl_min5","baseline_scl_min6",
               "baseline_scl_min7")

colnames(baseline_df) <- var_names

names(baseline_df) # check names

head(baseline_df, 10) # check that variable names were assigned correctly

View(baseline_df)


## INCLUSION: LOAD EXCEL SPREADSHEETS, EXTRACT VALUES, AND CREATE DATAFRAME
## -----------------------------------------------------------------------------

# Extracting EDA values and creating a data frame
inclusion_df <- get_df(
  data_path = "/Users/colinbosma/Desktop/SIBS APC Data 02.12.20",
  file_match = "EDAInclusionoutput.xlsx$"
    )

print(inclusion_df)

# Note: Because some spread sheets have a condition entered in the fourth column (i.e., negative/positive),
#       an extra, empty column is created and needs to be removed.

inclusion_df <- inclusion_df %>%
  select(X1, X2, X3)

# Variable names
var_names <- c("id",
               "inc_scl_min1", "inc_scl_min2")

# Assign variable names to df
colnames(inclusion_df) <- var_names

names(inclusion_df) # check names

head(inclusion_df, 10)  # check that variable names were assigned correctly

View(inclusion_df)

## EXCLUSION: LOAD EXCEL SPREADSHEETS, EXTRACT VALUES, AND CREATE DATAFRAME
## -----------------------------------------------------------------------------

# Extracting EDA values and creating a data frame
exclusion_df <- get_df(
  data_path = "/Users/colinbosma/Desktop/SIBS APC Data 02.12.20",
  file_match = "EDAExclusionoutput.xlsx$"
    )

print(exclusion_df) # quick check

# Add variable names
var_names <- c("id",
               "exc_scl_min1", "exc_scl_min2", "exc_scl_min3", "exc_scl_min4")

colnames(exclusion_df) <- var_names

names(exclusion_df) # check names

head(exclusion_df, 2) # check that variable names were assigned correctly

View(exclusion_df)

## COMBINE DATA FRAMES
## -----------------------------------------------------------------------------

# Amend basline data frame with inclusion data frame by id and condition
EDA_scl_temp <- dplyr::full_join(baseline_df, inclusion_df,
			by = c("id"))

# Amend with recovery data frame by id
EDA_scl_df <- dplyr::full_join(EDA_scl_temp, exclusion_df,
			by = c("id"))


# Print head data frame
head(EDA_scl_df, 2)
dim(EDA_scl_df) # Can cross reference # of observations and # of variables

View(EDA_scl_df) # Take a look at the data frame

## COMPUTING NEW VARIABLES
## -----------------------------------------------------------------------------

# Save all variable names that should be numeric to a vector
cols_num <- c( "baseline_scl_min1", "baseline_scl_min2","baseline_scl_min3", "baseline_scl_min4", "baseline_scl_min5","baseline_scl_min6", "baseline_scl_min7",

               "inc_scl_min1", "inc_scl_min2",

               "exc_scl_min1", "exc_scl_min2", "exc_scl_min3", "exc_scl_min4")

# convert to numeric
EDA_scl_df[cols_num] <- sapply(EDA_scl_df[cols_num], as.numeric)

str(EDA_scl_df) # check that Total SCL variables were converted to numeric





## ---------------------------------------------------------------------------##
##                     PART2: PULLING MEAN SC VALUES                          ##
## ---------------------------------------------------------------------------##


## UPDATE get_df FUNCTION FOR PULLING ID AND MEAN SC FROM SPREADSHEETS
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

      meta_dat <- temp_df %>% # pulls participant ID
        filter(.[[1]] == "File Name") %>%
        select(c(3))

      response <- temp_df %>%
        filter(.[[1]] == "Mean SC") %>%
        select(-1)

      bind_cols(meta_dat, response) %>% # creates new df with pulled values
        set_colnames(paste0("X", 1:ncol(.)))
    })
}

## BASELINE: LOAD EXCEL SPREADSHEETS, EXTRACT VALUES, AND CREATE DATAFRAME
## -----------------------------------------------------------------------------

# extracting EDA values and creating dataframe using get_df() function
baseline_df <- get_df(
  data_path = "/Users/colinbosma/Desktop/SIBS APC Data 02.12.20",
  file_match = "EDABaselineoutput.xlsx$"
)

print(baseline_df) # quick check

# Add variable names
var_names <- c("id",
               "baseline_meansc_min1", "baseline_meansc_min2","baseline_meansc_min3",
               "baseline_meansc_min4", "baseline_meansc_min5","baseline_meansc_min6",
               "baseline_meansc_min7")

colnames(baseline_df) <- var_names

names(baseline_df) # check names

head(baseline_df, 10) # check that variable names were assigned correctly

View(baseline_df)


## INCLUSION: LOAD EXCEL SPREADSHEETS, EXTRACT VALUES, AND CREATE DATAFRAME
## -----------------------------------------------------------------------------

# Extracting EDA values and creating a data frame
inclusion_df <- get_df(
  data_path = "/Users/colinbosma/Desktop/SIBS APC Data 02.12.20",
  file_match = "EDAInclusionoutput.xlsx$"
)

print(inclusion_df)

# Note: Because some spread sheets have a condition entered in the fourth column (i.e., negative/positive),
#       an extra, empty column is created and needs to be removed.

inclusion_df <- inclusion_df %>%
  select(X1, X2, X3)

# Variable names
var_names <- c("id",
               "inc_meansc_min1", "inc_meansc_min2")

# Assign variable names to df
colnames(inclusion_df) <- var_names

names(inclusion_df) # check names

head(inclusion_df, 10)  # check that variable names were assigned correctly

View(inclusion_df)

## EXCLUSION: LOAD EXCEL SPREADSHEETS, EXTRACT VALUES, AND CREATE DATAFRAME
## -----------------------------------------------------------------------------

# Extracting EDA values and creating a data frame
exclusion_df <- get_df(
  data_path = "/Users/colinbosma/Desktop/SIBS APC Data 02.12.20",
  file_match = "EDAExclusionoutput.xlsx$"
)

print(exclusion_df) # quick check

# Add variable names
var_names <- c("id",
               "exc_meansc_min1", "exc_meansc_min2", "exc_meansc_min3", "exc_meansc_min4")

colnames(exclusion_df) <- var_names

names(exclusion_df) # check names

head(exclusion_df, 2) # check that variable names were assigned correctly

View(exclusion_df)

## COMBINE DATA FRAMES
## -----------------------------------------------------------------------------

# Amend basline data frame with inclusion data frame by id and condition
EDA_meansc_temp <- dplyr::full_join(baseline_df, inclusion_df,
                                 by = c("id"))

# Amend with recovery data frame by id
EDA_meansc_df <- dplyr::full_join(EDA_meansc_temp, exclusion_df,
                           by = c("id"))


# Print head data frame
head(EDA_meansc_df, 2)
dim(EDA_meansc_df) # Can cross reference # of observations and # of variables

View(EDA_meansc_df) # Take a look at the data frame

## COMPUTING NEW VARIABLES
## -----------------------------------------------------------------------------

# Save all variable names that should be numeric to a vector
cols_num <- c("baseline_meansc_min1", "baseline_meansc_min2","baseline_meansc_min3", "baseline_meansc_min4", "baseline_meansc_min5","baseline_meansc_min6", "baseline_meansc_min7",

              "inc_meansc_min1", "inc_meansc_min2",

              "exc_meansc_min1", "exc_meansc_min2", "exc_meansc_min3", "exc_meansc_min4")

# convert to numeric
EDA_meansc_df[cols_num] <- sapply(EDA_meansc_df[cols_num], as.numeric)

str(EDA_meansc_df) # check that RSA variables were converted to numeric


## REMOVING PILOT DATA FROM THE DATA FRAME (use if you switch to whole dataset - put at beginning)
## -----------------------------------------------------------------------------

# Note: All participants before SIBS086 are part of the pilot phase of SIBS

# using dplyr
#EDA_df %>% slice(1:n)

# base r using indexing
#EDA_df <- EDA_df[-c(1:n), ] # enter number row for last row to be deleted


## ---------------------------------------------------------------------------##
##                     PART3: COMBINING BOTH AND EXPORTING                    ##
## ---------------------------------------------------------------------------##

## COMBINE TOTAL SCL AND MEAN SC DATA FRAMES INTO ONE
## -----------------------------------------------------------------------------

EDA_df <- dplyr::full_join(EDA_scl_df, EDA_meansc_df,
                                 by = c("id"))

# Let's check all of our hard work!
View(EDA_df)

## EXPORT DATA
## -----------------------------------------------------------------------------

# Exporting data frame to SPSS data file (.sav) for collaborating

# check current directory
getwd()
# Set new working directory for saving .sav file
setwd("/Users/colinbosma/Desktop")

# SPSS data frame
write_sav(EDA_df, "SIBS_EDA_021220.sav") # SPSS data file should be ready to go!

EDA_df[is.na(EDA_df)] <- " " # replace al "NA"s with a blank cell

# .csv file for excel
write_csv(EDA_df, "SIBS_EDA_021220.csv") # Write .csv for using this data frame to merge with other data sets using R
