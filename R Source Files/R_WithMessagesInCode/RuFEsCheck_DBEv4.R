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
datEsc <- readxl::read_excel(paste0(year, 
                                    "ruqDataSource_AnnualEscapement_GroupedExportByPop.xlsx"),
                             sheet = "ruqDataSource_AnnualEscapement_")

datDBE <- readxl::read_excel(paste0(here(), "/", "rutDataSource_DBE.xlsx"),
                             sheet = "Sheet1")


# Common data 
cmtStocks <- readxl::read_excel(paste0(here(), "/cmtStocks.xlsx"), sheet = "Sheet1")
# management groups
cmtMG <- readxl::read_excel(paste0(here(), "/cmtManagementGroups.xlsx"), sheet = "Sheet1")
# management group details
cmtMGD <- readxl::read_excel(paste0(here(), "/cmtManagementGroupsDetails.xlsx"), sheet = "Sheet1")


# DBE data exported from RuFEs for an old SEF project 
# use to check changes/see differences
datSEF <- readxl::read_excel(paste0(here(), "/ruqDataSource_DailyCatch_SEFOutputFINAL_Incl2015.xlsx"),
                              sheet = "ruqDataSource_DailyCatch_SEFOut")

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
datDBE <- left_join(datDBE, datCommon, by = "DFOCode")

datDBE %<>% 
  filter(Year == year) %>% 
  rowwise() %>% 
  mutate(RF_Escapement = sum(as.numeric(malesESC), as.numeric(femalesESC), na.rm = TRUE),
         RFesc_Passage = as.numeric(Passage),
         RFesc_CatchAbove = as.numeric(Above), 
         RF_DBE = DBE, 
         RF_DBEbrood = DBE_brood) %>% 
  select(c(Year, DFOCode, Stream, RF_Escapement, RFesc_Passage, 
           RFesc_CatchAbove, RF_DBE, RF_DBEbrood, 
           cmtManagementGroupsID_DNA,
           cmtStocksID, DNAgrp, Stream,                 
           cmtManagementGroupsID_MA_RSA, `MA-RSA Stocks`)) %>% 
  ungroup()

datEsc %<>%
  rowwise() %>% 
  mutate(RF_EscBrood = sum(SumOffemales_brood, SumOfmales_brood, na.rm = TRUE)) %>% 
  rename(Stream = StockName, 
         `MA-RSA Stocks` = GroupName, 
         WatershedGrpName = `Watershed Group Name`,
         ) %>% 
  select(c(Year, SpawningDistrict, WatershedGrpName, 
           `MA-RSA Stocks`, Stream, RF_EscBrood)) %>% 
  ungroup()

datEsc <- left_join(datEsc, datCommon, by = c("Stream", 'MA-RSA Stocks'))
datDBEfull <- full_join(datEsc, datDBE, by = c("Year", "MA-RSA Stocks", "Stream", 
                                               "cmtManagementGroupsID_DNA", 
                                               "cmtStocksID", "DFOCode", 
                                               "cmtManagementGroupsID_MA_RSA", "DNAgrp"))

# Make full dataframe ----
dat <- full_join(datDBEfull, datRF, by = c("Year", "MA-RSA Stocks", "Stream", 
                                           "cmtManagementGroupsID_DNA", 
                                           "cmtStocksID", "DFOCode", 
                                           "cmtManagementGroupsID_MA_RSA", "DNAgrp"))

# Calculate DBEs in R -----
dat %<>% 
  mutate(RF_Passage2 = ifelse(is.na(RF_Passage), 
                              0, 
                              RF_Passage), 
         RF_CatchAbove2 = ifelse(is.na( RF_CatchAbove), 
                                 0, 
                                 RF_CatchAbove)) %>% 
  select(c(Year, DFOCode, Stream, `MA-RSA Stocks`, DNAgrp, RF_Escapement, RF_EscBrood,
           RF_Passage2, RFesc_Passage, RFesc_CatchAbove, RF_CatchAbove2, 
           RF_DBE, RF_DBEbrood)) %>% 
  rename(RF_Passage = RF_Passage2, 
         RF_CatchAbove = RF_CatchAbove2) %>% 
  # do it this way so that if passage or catch = NA it still calculates a DBE
  rowwise() %>% 
  mutate(diffPsg = RFesc_Passage - RF_Passage, 
         diffCatchA = RFesc_CatchAbove - RF_CatchAbove,
         calDBE = sum(RF_Escapement, -1*RF_Passage, RF_CatchAbove, na.rm = TRUE),
         calDBE_brood = sum(sum(RF_EscBrood, RF_Escapement, na.rm = TRUE), 
                            -1*RF_Passage, RF_CatchAbove, na.rm = TRUE)) %>% 
  # brood DBE is only relevant for stocks where broodstock is taken (Cultus)
  mutate(calDBE_brood2 = ifelse(DFOCode == 7001, 
                                calDBE_brood, 
                                calDBE)) %>% 
  # get difference between the DBE calculated in RuFEs and the one calcualted here
  mutate(diffDBE = RF_DBE - calDBE, 
         diffDBE_brood = RF_DBEbrood - calDBE_brood2, 
         # percent difference
         pDiffDBE = (abs(RF_DBE - calDBE)/
                       ((RF_DBE + calDBE)/2)*100),
         pDiffDBE_brood = (abs(RF_DBEbrood - calDBE_brood2)/
                             ((RF_DBEbrood + calDBE_brood2)/2)*100),
         # calDBE is the DBE calculated here in R while RF_DBE is the DBE calculated in RuFEs
         # flag errors (mismatches between RuFEs DBE and the one calculated here)
         Errors = ifelse((is.na(pDiffDBE) & RF_DBE == 0 & calDBE == 0) &
                           (is.na(pDiffDBE_brood) & RF_DBEbrood == 0 & calDBE_brood2 == 0),
                         "no error",
                         # can change threshold
                         ifelse(abs(pDiffDBE) < 1E-4 & abs(pDiffDBE_brood) < 1E-4,
                                "no error", 
                                "error"))) %>% 
  ungroup()

# get dataframe of entries with errors
datError <- dat %>% filter(Errors == "error")

# Group data by MA-RSA group & spawning district ----
# can also check this against the group export from RuFEs (NOT by Population)
datGrpd <- dat %>% 
  select(-calDBE_brood) %>% 
  rename(calDBE_brood = calDBE_brood2) %>% 
  group_by(`MA-RSA Stocks`) %>% 
  summarize_at(vars(RF_Escapement:calDBE_brood), sum, na.rm = TRUE) %>% 
  mutate(diffDBE = RF_DBE - calDBE, 
         diffDBE_brood = RF_DBEbrood - calDBE_brood, 
         # percent difference
         pDiffDBE = (abs(RF_DBE - calDBE)/
                       ((RF_DBE + calDBE)/2)*100),
         pDiffDBE_brood = (abs(RF_DBEbrood - calDBE_brood)/
                             ((RF_DBEbrood + calDBE_brood)/2)*100),
         # calDBE is the DBE calculated here in R while RF_DBE is the DBE calculated in RuFEs
         # flag errors (mismatches between RuFEs DBE and the one calculated here)
         Errors = ifelse((is.na(pDiffDBE) & RF_DBE == 0 & calDBE == 0) &
                           (is.na(pDiffDBE_brood) & RF_DBEbrood == 0 & calDBE_brood == 0),
                         "no error",
                         # can change threshold
                         ifelse(abs(pDiffDBE) < 1E-4 & abs(pDiffDBE_brood) < 1E-4,
                                "no error", 
                                "error"))) 

# Compare with SEF ----

if(year %in% datSEF$Year){
datSEF %<>% 
  filter(Year == year) %>% 
  select(c(GroupName, Year, MissionCatch, FreshwaterCatchAM, TotalEscapement, TotalDBE)) %>% 
  rename(`MA-RSA Stocks` = GroupName)

datGrpdSub <- datGrpd %>% 
  select(`MA-RSA Stocks`, RF_Escapement, RF_Passage, RF_CatchAbove, RF_DBE, calDBE) %>% 
  mutate_if(is.numeric, round, 0)
datSEF <- full_join(datGrpdSub, datSEF, by = "MA-RSA Stocks")

datSEF %<>%
  select(c(Year, `MA-RSA Stocks`, RF_Escapement, TotalEscapement, 
           RF_Passage, MissionCatch, RF_CatchAbove, FreshwaterCatchAM, 
           RF_DBE, calDBE, TotalDBE)) %>% 
  rename(SEF_Escapement = TotalEscapement, 
         SEF_Passage = MissionCatch, 
         SEF_CatchAbove = FreshwaterCatchAM, 
         CalcualtedDBE = calDBE, 
         SEF_DBE = TotalDBE)
} else {
  datSEF <- as.data.frame(as.matrix(nrow = 0, ncol = 0))
}

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

