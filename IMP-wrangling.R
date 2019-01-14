## MINDWARE IMPEDANCE WRANGLING AND TIDYING SCRIPT
## -----------------------------------------------------------------------------

## IMPORTANT NOTES BEFORE RUNNING THE SCRIPT
# Excel needs to be closed while running this code.
# Do not edit code without contacting Colin first.
# Select all code, then press Ctrl + Enter keys to run
# Make sure all directories are correct and there-
# are no typos in the file names


## REFERENCES
## -----------------------------------------------------------------------------

# Tidyverse website
browseURL("http://tidyverse.org")

# documentation for readxl pakage. Able to specify worksheets and columns
browseURL("https://github.com/tidyverse/readxl")

# tidyxl package documentation - import and manipulate awkward excel files
browseURL("https://cran.r-project.org/web/packages/tidyxl/index.html")


## LOAD PACKAGES
## -----------------------------------------------------------------------------

library(tidyverse)
library(readxl) # make sure it is the most recent version (v1.0.0)
library(psych) # used for 'describe' function
library(haven) # for exporting data frame to an SPSS data file (.sav)
library(magrittr) # library for bind_cols() function

sessionInfo() # tells you what you have loaded in your workspace

## FUNCTION FOR PULLING ID, GROUP, CONDITION, SV, CO, AND PEP FROM SPREADSHEETS
## -----------------------------------------------------------------------------

get_df <- function(data_path, file_match){
  list.files(
    path = data_path,
    pattern = file_match,
    recursive = TRUE,
    full.names = TRUE
  ) %>%
  map_df( ~ {
    print(.x)
    temp_df <- read_excel(.x, col_names = FALSE)
    meta_dat <- temp_df %>%
      filter(.[[1]] == "File Name") %>%
      select(c(3:5))

    response1 <- temp_df %>%
      filter(.[[1]] == "CO") %>%
      select(-1)

    response2 <- temp_df %>%
      filter(.[[1]] == "PEP") %>%
      select(-1)

    response3 <- temp_df %>%
      filter(.[[1]] == "Heart Rate") %>%
      select(-1)

    bind_cols(meta_dat, response1, response2, response3) %>%
      set_colnames(paste0("X", 1:ncol(.)))
    })
}


## BASELINE: LOAD EXCEL SPREADSHEETS, EXTRACT VALUES, AND CREATE DATAFRAME
## -----------------------------------------------------------------------------

# extracting values and creating dataframe using get_df() function
baseline_IMP_df <- get_df(
  data_path = "C:/Users/Mindware/Desktop/AAE Mindware Data",
  file_match = "IMPBaselineoutput.xlsx$"
  )

View(baseline_IMP_df) # quick check

# Add variable names

var_names <- c("id", "group", "condition",
               "base_co_min1", "base_co_min2", "base_co_min3", "base_co_min4",
               "base_co_min5", "base_co_min6", "base_co_min7", "base_co_min8",
               "base_co_min9", "base_co_min10",
               "base_pep_min1", "base_pep_min2", "base_pep_min3",
               "base_pep_min4", "base_pep_min5", "base_pep_min6",
               "base_pep_min7", "base_pep_min8", "base_pep_min9",
               "base_pep_min10",
               "base_hr_min1", "base_hr_min2", "base_hr_min3", "base_hr_min4",
               "base_hr_min5", "base_hr_min6", "base_hr_min7", "base_hr_min8",
               "base_hr_min9", "base_hr_min10")

colnames(baseline_IMP_df) <- var_names
names(baseline_IMP_df) # check col_names
head(baseline_IMP_df, 2) # check that the variable names were assigned correctly




## MI: LOAD EXCEL SPREADSHEETS, EXTRACT VALUES, AND CREATE DATAFRAME
## -----------------------------------------------------------------------------


# extracting values and creating dataframe using get_df() function
mi_IMP_df <- get_df(
  data_path = "C:/Users/Mindware/Desktop/AAE Mindware Data",
  file_match = "IMPMIoutput.xlsx$"
)

View(mi_IMP_df) # quickly check

# Add variable names
var_names <- c("id", "group", "condition",
               "mi_co_min1", "mi_co_min2", "mi_co_min3", "mi_co_min4",
               "mi_co_min5", "mi_co_min6", "mi_co_min7", "mi_co_min8",
               "mi_pep_min1", "mi_pep_min2", "mi_pep_min3",
               "mi_pep_min4", "mi_pep_min5", "mi_pep_min6", "mi_pep_min7",
               "mi_pep_min8",
               "mi_hr_min1", "mi_hr_min2", "mi_hr_min3", "mi_hr_min4",
               "mi_hr_min5", "mi_hr_min6", "mi_hr_min7", "mi_hr_min8")

colnames(mi_IMP_df) <- var_names
names(mi_IMP_df)
head(mi_IMP_df, 2)


## RECOVERY: LOAD EXCEL SPREADSHEETS, EXTRACT VALUES, AND CREATE DATAFRAME
## -----------------------------------------------------------------------------

recovery_IMP_df <- get_df(
  data_path = "C:/Users/Mindware/Desktop/AAE Mindware Data",
  file_match = "IMPRecoveryoutput.xlsx$"
)

print(recovery_IMP_df)

# Add variable names
var_names <- c("id", "group", "condition",
               "recovery_co_min1", "recovery_co_min2", "recovery_co_min3", "recovery_co_min4",
               "recovery_co_min5", "recovery_co_min6", "recovery_co_min7", "recovery_co_min8",
               "recovery_co_min9", "recovery_co_min10",
               "recovery_pep_min1", "recovery_pep_min2", "recovery_pep_min3",
               "recovery_pep_min4", "recovery_pep_min5", "recovery_pep_min6",
               "recovery_pep_min7", "recovery_pep_min8", "recovery_pep_min9",
               "recovery_pep_min10",
               "recovery_hr_min1", "recovery_hr_min2", "recovery_hr_min3", "recovery_hr_min4",
               "recovery_hr_min5", "recovery_hr_min6", "recovery_hr_min7", "recovery_hr_min8",
               "recovery_hr_min9", "recovery_hr_min10")

colnames(recovery_IMP_df) <- var_names
names(recovery_IMP_df)
head(recovery_IMP_df, 2)


## COMBINE DATA FRAMES
## -----------------------------------------------------------------------------


# Amend basline data frame with mi data frame by id
IMP_temp <- dplyr::full_join(baseline_IMP_df, mi_IMP_df,
			by = c("id", "group", "condition"))

# Amend with recovery data frame by id
IMP_df <- dplyr::full_join(IMP_temp, recovery_IMP_df,
			by = c("id", "group", "condition"))

# Print head data frame
head(IMP_df, 2)
dim(IMP_df) # Can cross reference # of observations and # of variables


## CONVERT VARIABLES FROM CHARACTOR TO NUMERIC
## -----------------------------------------------------------------------------

# save all variables that should be numeric to one vector
cols_num <- c("base_co_min1", "base_co_min2", "base_co_min3", "base_co_min4",
"base_co_min5", "base_co_min6", "base_co_min7", "base_co_min8",
"base_co_min9", "base_co_min10",
"base_pep_min1", "base_pep_min2", "base_pep_min3",
"base_pep_min4", "base_pep_min5", "base_pep_min6",
"base_pep_min7", "base_pep_min8", "base_pep_min9",
"base_pep_min10",
"base_hr_min1", "base_hr_min2", "base_hr_min3", "base_hr_min4",
"base_hr_min5", "base_hr_min6", "base_hr_min7", "base_hr_min8",
"base_hr_min9", "base_hr_min10",

"mi_co_min1", "mi_co_min2", "mi_co_min3", "mi_co_min4",
"mi_co_min5", "mi_co_min6", "mi_co_min7", "mi_co_min8",
"mi_pep_min1", "mi_pep_min2", "mi_pep_min3",
"mi_pep_min4", "mi_pep_min5", "mi_pep_min6", "mi_pep_min7",
"mi_pep_min8",
"mi_hr_min1", "mi_hr_min2", "mi_hr_min3", "mi_hr_min4",
"mi_hr_min5", "mi_hr_min6", "mi_hr_min7", "mi_hr_min8",

"recovery_co_min1", "recovery_co_min2", "recovery_co_min3", "recovery_co_min4",
"recovery_co_min5", "recovery_co_min6", "recovery_co_min7", "recovery_co_min8",
"recovery_co_min9", "recovery_co_min10",
"recovery_pep_min1", "recovery_pep_min2", "recovery_pep_min3",
"recovery_pep_min4", "recovery_pep_min5", "recovery_pep_min6",
"recovery_pep_min7", "recovery_pep_min8", "recovery_pep_min9",
"recovery_pep_min10",
"recovery_hr_min1", "recovery_hr_min2", "recovery_hr_min3", "recovery_hr_min4",
"recovery_hr_min5", "recovery_hr_min6", "recovery_hr_min7", "recovery_hr_min8",
"recovery_hr_min9", "recovery_hr_min10"
)

# convert to numeric all at once
IMP_df[cols_num] <- sapply(IMP_df[cols_num], as.numeric)

str(IMP_df) # check our work

## CHECKING DATA FRAME
## -----------------------------------------------------------------------------

# Structure and variable types
str(IMP_df)

# Summary of data frame
describe(IMP_df)

# View the data frame
View(IMP_df) # Review data table that is produced for issues

## EXPORT DATA
## -----------------------------------------------------------------------------

# Exporting data frame to SPSS data file (.sav) for collaborating

# check current directory
getwd()
# Set new working directory for saving .sav file
# setwd("insert-directory")
setwd("C:/Users/Mindware/Desktop")
write_sav(IMP_df, "IMPdata_date.sav") # SPSS data file should be ready to go!

# Write .csv for using this data frame to merge with other data sets using R
write_csv(IMP_df, "IMPdata_date.csv")
