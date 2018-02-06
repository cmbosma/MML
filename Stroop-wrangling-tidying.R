## Stroop data - wrangling and tidying
## Colin M. Bosma


## LOAD LIBRARIES
## ----------------------------------------------------------------------------
library(tidyverse)
library(haven)


## SET WORKING DIRECTORY AND IMPORT DATA
## ----------------------------------------------------------------------------

# Set working directory
seted("insert-path")

# Import data
sdata <- read_csv("filename.csv")
View(sdata)


## RESTRUCTURE DATA FROM LONG TO WIDE FRORMAT
## ----------------------------------------------------------------------------
names(sdata) # view names of variables
sdata_wide <- spread(sdata, key = Block, value = ESTrialStim.RT)
View(sdata_wide)


## RENAMING 
## -----------------------------------------------------------------------------

# Note: Consider dropping practice blocks using data.frame()

# Rename experimental condition variable to "condition"
colnames(sdata_wide)[colnames(sdata_wide)=="ExperimentName"] <- "condition"

# Rename "Subject" to "id" - important for merging data frames
colnames(sdata_wide)[colnames(sdata_wide)=="Subject"] <- "id"

# adding "AAE" to participant ID
names(sdata_wide)

sdata_wide$id[1:48] <- gsub("^", "AAE0", sdata_wide$id[1:48])
sdata_wide$id[49:94] <- gsub("^", "AAE", sdata_wide$id[49:94])

sdata_wide$id # check your work

# Adding "block" to block variable names 
names(sdata_wide) <- gsub("^", "block", names(sdata_wide[28:73])) # change index

# Rename data entries for duplicate condition names
sdata_wide$condition[sdata_wide$condition=="1Neutral ES -  Neutral MI"] <- 
"Neutral ES - Neutral MI" 
# Note extra space in the incorrect name
sdata_wide$condition
[sdata_wide$condition=="1Negative ES - Neutral MI"] <- 
"Negative ES - Neutral MI"
sdata_wide$condition[sdata_wide$condition=="1Negative ES - Sad MI"] <- 
"Negative ES - Sad MI"
sdata_wide$condition[sdata_wide$condition=="1Neutral ES - Sad MI"] <- 
"Neutral ES - Sad MI"

print(sdata_wide$condition)


## EXPORT DATA
## ----------------------------------------------------------------------------

# Exporting data frame to SPSS data file (.sav)

# Set new directory for exporting and sharing
setwd("new_path") 

write_sav(sdata_wide, "StroopData_date.sav")

# SPSS data file should be ready to go!
