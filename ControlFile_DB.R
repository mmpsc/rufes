# Clear workspace
rm(list = ls())

# Install packages ----
if(!require(pacman)) install.packages("pacman")
library(pacman)

p_load(here, tidyverse, magrittr, readxl, XLConnect, svDialogs,
       lubridate, RODBC, odbc, DBI)

# Set parameters ------
# SPECIFY YEAR
year <- 2013

# OPTIONAL: reset threshold for percent differences and database path
threshold <- 1e-4
db_path <- "//ADAMS/Data Sets/DB_Projects/AccessDBs/RuFEs/RuFEsR.accde"

# SPECIFY CHECK
check <- "DBEs"
# CatchAbove, CatchBelow, Passage, Escapement, DBEs, CatchRollups, PassageRollups
# for pre-2003 years CatchAbove, CatchBelow, and Passage are checked together so just select 1

gc() 

# Set working directory -----
if(check == "CatchRollups" | check == "PassageRollups"){
  setwd(paste0(here(), "/", year, "/Rollups"))
} else {
  setwd(paste0(here(), "/", year)) 
}

# Source the relevant checking code and run ----
if(year < 2003 & 
   (check == "CatchBelow" | check == "CatchAbove" | check == "Passage")){
  source(paste0(here(), "/R Source Files/RuFEsCheck_DB_Pre2003CatchPassage.R"))
} else if(year < 2003 & check == "Escapement"){
  source(paste0(here(), "/R Source Files/RuFEsCheck_DB_Pre2003Escapement.R"))
} else if(check == "CatchAbove"){
  source(paste0(here(), "/R Source Files/RuFEsCheck_DB_AboveMission.R"))
} else if (check == "CatchBelow"){
  source(paste0(here(), "/R Source Files/RuFEsCheck_DB_BelowMission.R"))
} else if (check == "Passage"){
  source(paste0(here(), "/R Source Files/RuFEsCheck_DB_Passage.R"))
} else if (check == "Escapement"){
  source(paste0(here(), "/R Source Files/RuFEsCheck_DB_Escapement.R"))
} else if (check == "DBEs"){
  source(paste0(here(), "/R Source Files/RuFEsCheck_DB_DBE.R"))
} else if (check == "CatchRollups"){
  source(paste0(here(), "/R Source Files/RuFEsCheck_CatchRollups.R"))
} else if (check == "PassageRollups"){
  source(paste0(here(), "/R Source Files/RuFEsCheck_PassageRollups.R"))
}

# Write results to summary workbook -----
source(paste0(here(), "/R Source Files/SaveResults.R"))


#########################################################################
# If there are errors in the catch checks ----
source(paste0(here(), "/R Source Files/CategoryComparisonFunction.R"))
# column names must have quotes around them
if(year < 2003){
  CategoryComparisonChecks(dat = datI, datRF = datRF, # datIfra
                           Stock = "I_Stock", 
                           Area2 = "I_Area2", 
                           Type2 = "I_Type2", 
                           User2 = "I_User2", 
                           Gear  = "I_Gear")
} else {
  # column names must have quotes around them
  CategoryComparisonChecks(dat = datCR, datRF = datRF, 
                           Stock = "CR_Stock", 
                           Area2 = "CR_Area2", 
                           Type2 = "CR_Type2", 
                           User2 = "CR_User2",
                           Gear  = "CR_Gear")
}

sort(unique(datI$I_Stock))
sort(unique(datCR$CR_Stock))
sort(unique(datRF$RF_Stock))

setdiff(datCR$CR_Stock, datRF$RF_Stock)
setdiff(datI$I_Stock, datRF$RF_Stock)
setdiff(datRF$RF_User, datCR$CR_User2)
setdiff(datRF$RF_Stock, datCR$CR_Stock)

# dev.off()

unlink(paste0(here(), "/", year, 
              "/RuFEsQAQC_Summary_", year, ".xlsx"), recursive = TRUE)



