######################################################
# Code to check RuFEs DBEs for years before 2003

# Created: Serena Wong, May 2021
#######################################################

# Clear workspace
rm(list = ls())

# Install packages
if(!require(pacman)) install.packages("pacman")
library(pacman)

p_load(here, tidyverse, magrittr, readxl, xlsx)

# SPECIFY YEAR
year <- 2010

# Set working directory
# "C:/Users/wong/OneDrive - Pacific Salmon Commission/Documents/RuFEs/QAQC/"
setwd(paste0(here(), "/", year))
dir.create("Summaries", showWarnings = FALSE)

# Import data ----
# Catch above mission and passage data exported from RuFEs database
datRFabove <- readxl::read_excel(paste0(year, "ruqDataSource_DailyCatchAbove_MA-RSAStocksByPop.xlsx"),
                                 sheet = "ruqDataSource_DailyCatch")
datRFpass <- readxl::read_excel(paste0(getwd(), "/Rollups/",
                                       year, "ruqDataSource_DailyPassage_DNAGrpByPop.xlsx"),
                                sheet = "ruqDataSource_DailyPassage")

# DBE and escapement data exported from RuFEs database
datDBE <- readxl::read_excel(paste0(year, "ruqDataSource_AnnualEscapement_GroupedExportByPop.xlsx"),
                             sheet = "ruqDataSource_AnnualEscapement_")

# Common data 
cmtStocks <- readxl::read_excel(paste0(here(), "/cmtStocks.xlsx"), sheet = "Sheet1")
# management groups
cmtMG <- readxl::read_excel(paste0(here(), "/cmtManagementGroups.xlsx"), sheet = "Sheet1")
# management group details
cmtMGD <- readxl::read_excel(paste0(here(), "/cmtManagementGroupsDetails.xlsx"), sheet = "Sheet1")


# DBE data exported from RuFEs for an old SEF project 
# use to check changes/see differences
# datSEF <- readxl::read_excel(paste0(here(), "/ruqDataSource_DailyCatch_SEFOutputFINAL_Incl2015.xlsx"),
#                              sheet = "ruqDataSource_DailyCatch_SEFOut")

# Organize Common data ----
# we'll use the common data to combine the catch above and the passage data
# so that you don't have to download another file from RuFEs and can just
# use the passage data from the rollup checks (DNA grp by population)
cmtStocks %<>% 
  dplyr::rename(cmtStocksID = cmtStocksId)
cmtMG_MA_RSA <- cmtMG %>% filter(Source == "MA-RSA Stocks") 
cmtMG_DNA <- cmtMG %>% filter(Source == "DNA Group")

cmt_MA_RSA <- left_join(cmtMG_MA_RSA, cmtMGD, by = c("cmtManagementGroupsID", "Source"))
cmt_MA_RSA <- full_join(cmt_MA_RSA, cmtStocks, by = "cmtStocksID")
cmtSub_MA_RSA <- cmt_MA_RSA %>% 
  dplyr::select(c(cmtManagementGroupsID, cmtStocksID, GroupName, 
                  Stream, DFOCode)) %>% 
  rename("MA-RSA Stocks" = GroupName, 
         cmtManagementGroupsID_MA_RSA = cmtManagementGroupsID)

cmt_DNA <- left_join(cmtMG_DNA, cmtMGD, by = c("cmtManagementGroupsID", "Source"))
cmt_DNA <- full_join(cmt_DNA, cmtStocks, by = "cmtStocksID")
cmtSub_DNA <- cmt_DNA %>% 
  dplyr::select(c(cmtManagementGroupsID, cmtStocksID, GroupName, 
                  Stream, DFOCode)) %>% 
  rename(DNAgrp = GroupName, 
         cmtManagementGroupsID_DNA = cmtManagementGroupsID)

datCommon <- full_join(cmtSub_DNA, cmtSub_MA_RSA, 
                       by = c("Stream", "DFOCode", "cmtStocksID"))

# Organize RuFEs data -----
datRFabove %<>% 
  dplyr::select(Year, Stream, GroupName, SumOfStockCatch) %>% 
  dplyr::rename(RF_CatchAbove = SumOfStockCatch, 
                "MA-RSA Stocks" = GroupName) %>%
  na.omit()

datRFpass %<>% 
  dplyr::select(Year, Population, GroupName, SumOfStockCatch) %>% 
  dplyr::rename(Stream = Population,
                RF_Passage = SumOfStockCatch, 
                DNAgrp = GroupName) %>% 
  na.omit()

# join catch above mission and passage data together
datRFabove2 <- left_join(datRFabove, datCommon, by = c("Stream", "MA-RSA Stocks"))
datRFpass2 <-  left_join(datRFpass, datCommon, by = c("Stream", "DNAgrp"))
datRF <- full_join(datRFabove2, datRFpass2, 
                   by = c("Year", "Stream", "cmtManagementGroupsID_DNA", 
                          "cmtManagementGroupsID_MA_RSA", "cmtStocksID",
                          "DFOCode", "MA-RSA Stocks", "DNAgrp"))

# Organize DBE/Escapement data -----
datDBE %<>% 
  rowwise() %>% 
  mutate(RF_Escapement = sum(SumOffemales, SumOfmales, na.rm = TRUE)) %>% 
  select(c(Year, GroupName, # SpawningDistrict, "Watershed Group Name", 
           StockName, SumOfDBE, RF_Escapement)) %>% 
  rename(c(# "WatershedGrpName" = "Watershed Group Name", 
           "Stream" = StockName, 
           "RF_DBE" = SumOfDBE, 
           "MA-RSA Stocks" = GroupName))

# Make full dataframe ----
dat <- full_join(datDBE, datRF, by = c("Year", "MA-RSA Stocks", "Stream"))

# Calculate DBEs in R -----
dat %<>% 
  select(c(Year, # SpawningDistrict, WatershedGrpName, 
           "MA-RSA Stocks", 
           DNAgrp, Stream, RF_Escapement, RF_Passage, RF_CatchAbove, RF_DBE)) %>% 
  # do it this way so that if passage or catch = NA it still calculates a DBE
  mutate(calDBE = sum(RF_Escapement, -1*RF_Passage, RF_CatchAbove, na.rm = TRUE)) %>% 
  # get difference between the DBE calculated in RuFEs and the one calcualted here
  mutate(diffDBE = RF_DBE - calDBE, 
         # percent difference
         pDiffDBE = (abs(RF_DBE - calDBE)/
                       ((RF_DBE + calDBE)/2)*100),
         # calDBE is the DBE calculated here in R while RF_DBE is the DBE calculated in RuFEs
         # flag errors
         # (mismatches between RuFEs DBE and the one calculated here)
         pErrors = ifelse(is.na(RF_Escapement), 
                          pErrors <- "no Esc",
                          ifelse(is.na(pDiffDBE) & 
                                   RF_DBE == 0 & calDBE == 0,
                                 pErrors <- "no error",
                                 # can change threshold
                                 ifelse(abs(pDiffDBE) > 1E-4,
                                        pErrors <- "error", 
                                        pErrors <- "no error"))))

# get dataframe of entries with errors
datError <- dat %>% filter(pErrors == "error")

# Group data by MA-RSA group & spawning district ----
# can also check this against the group export from RuFEs (NOT by Population)
datGrpd <- dat %>% 
  # have to rename to get the group_by function to work
  rename(MAstocks = "MA-RSA Stocks") %>% 
  group_by(# SpawningDistrict, 
           MAstocks) %>% 
  summarize(RF_Passage = sum(RF_Passage, na.rm = TRUE),
            RF_CatchAbove = sum(RF_CatchAbove, na.rm = TRUE), 
            RF_Escapement = sum(RF_Escapement, na.rm = TRUE),
            RF_DBE = sum(RF_DBE, na.rm = TRUE), 
            calDBE = sum(calDBE, na.rm = TRUE)) %>% 
  mutate(diffDBE = RF_DBE - calDBE, 
         # percent difference
         pDiffDBE = (abs(RF_DBE - calDBE)/
                       ((RF_DBE + calDBE)/2)*100),
         # flag errors (mismatches between RuFEs DBE and the one calculated here)
         pErrors = # ifelse(is.na(SpawningDistrict), 
                          # pErrors <- NA, 
                          ifelse(is.na(pDiffDBE) & 
                                   RF_DBE == 0 & calDBE == 0,
                                 pErrors <- "no error",
                                 # can change threshold
                                 ifelse(abs(pDiffDBE) > 1E-4,
                                        pErrors <- "error", 
                                        pErrors <- "no error"))) # )


# Compare with SEF ----

datSEF %<>% 
  filter(Year == year) %>% 
  select(c(GroupName, Year, MissionCatch, FreshwaterCatchAM, TotalEscapement, TotalDBE,
           TotalPSE))

# Summaries ----
print(paste("Total number of DBE errors:", nrow(datError)))

# save summary table
datGrpd <- as.data.frame(datGrpd)
datLong <- as.data.frame(dat)
datError <- as.data.frame(datError)
datSEF <- as.data.frame(datSEF)

filename <- paste0(here(), "/", year, "/Summaries/DBESummaryTable_", Sys.Date(),".xlsx")

write.xlsx(datGrpd, filename, row.names = FALSE, sheetName = "Grouped")
write.xlsx(datLong, filename, row.names = FALSE, sheetName = "ByStream", append = TRUE)
write.xlsx(datError, filename, row.names = FALSE, sheetName = "Errors", append = TRUE)
write.xlsx(datSEF, filename, row.names = FALSE, sheetName = "SEF", append = TRUE)



