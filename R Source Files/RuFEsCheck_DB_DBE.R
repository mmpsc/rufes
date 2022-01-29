######################################################
# Code to check RuFEs DBEs for years before 2003

# Created: Serena Wong, May 2021
#######################################################
# Functions ----
funcLoadRuFEs_CommonData <- function(dbPath){
  # set up driver info and database path
  # library(RODBC)
  driverInfo <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
  path <- paste0(driverInfo, "DBQ=", dbPath)
  
  # establish connection
  channel <- RODBC::odbcDriverConnect(path)
  on.exit(odbcClose(channel))
  
  # extract table of interest as dataframe
  cmtStocks <- sqlQuery(channel, 
                        paste0("SELECT * 
                          FROM [cmtStocks];"))   
  cmtManagementGroups <- sqlQuery(channel, 
                                  paste0("SELECT * 
                          FROM [cmtManagementGroups];"))
  cmtManagementGroupsDetails <- sqlQuery(channel, 
                                         paste0("SELECT * 
                          FROM [cmtManagementGroupsDetails];"))
  
  # assign dataframe to environment
  assign("cmtStocks", cmtStocks, envir = .GlobalEnv)
  assign("cmtMG", cmtManagementGroups, envir = .GlobalEnv)
  assign("cmtMGD", cmtManagementGroupsDetails, envir = .GlobalEnv)
}

funcLoadRuFEs_ByPop <- function(dbPath,
                                     Yr, 
                                     Table,
                                     drop_vars = c()){
  # set up driver info and database path
  # library(RODBC)
  driverInfo <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
  path <- paste0(driverInfo, "DBQ=", dbPath)
  
  # establish connection
  channel <- RODBC::odbcDriverConnect(path)
  on.exit(odbcClose(channel))
  
  # retrieve all variables names from table tbl
  tbl_vars <- sqlColumns(channel, Table)["COLUMN_NAME"]
  # exclude variables based on input parameters
  tbl_vars <- subset(tbl_vars, !(tbl_vars$COLUMN_NAME %in% drop_vars))
  # add brackets to each variable (ie. [variable]) to maintin ACCESS syntax
  tbl_vars$COLUMN_NAME <- paste0("[", tbl_vars$COLUMN_NAME, "]")
  # transform dataframe column into string separated by comma
  cols <- paste0(tbl_vars[1:nrow(tbl_vars), ], collapse = ",")
  
  # extract table of interest as dataframe
  
  sqlQuery(channel, paste0("SELECT ", cols,  
                           "FROM [", Table, "]", 
                           "WHERE Year = ", Yr, ";"))
  
}

funcLoadRuFEs_GroupByPop <- function(dbPath, Grouping, Yr, abMission){
  # set up driver info and database path
  driverInfo <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
  path <- paste0(driverInfo, "DBQ=", dbPath)
  
  # establish connection
  channel <- RODBC::odbcDriverConnect(path)
  
  on.exit(odbcClose(channel))
  
  sqlQuery(channel, paste0("SELECT Sum(rutDailyCatchByStream.StockCatch) AS SumOfStockCatch,
                                  cmtManagementGroups.GroupName, 
                                  rutDailyCatchByStream.abMission,
                                  rutDailyCatchByStream.Year,
                                  cmtStocks.DFOCode, 
                                  cmtStocks.Stream
                            FROM ([rutDailyCatchByStream] 
                            LEFT JOIN (
                              SELECT cmtStocks.*, 
                                     cmtManagementGroups.GroupName, 
                                     cmtManagementGroups.Source
                              FROM cmtManagementGroups 
                              INNER JOIN ([cmtStocks] 
                                    INNER JOIN [cmtManagementGroupsDetails] 
                                    ON cmtStocks.cmtStocksId = cmtManagementGroupsDetails.cmtStocksID) 
                              ON cmtManagementGroups.cmtManagementGroupsID = cmtManagementGroupsDetails.cmtManagementGroupsID
                              WHERE ((cmtManagementGroups.Source) = '", Grouping,"'))
                           cmt ON rutDailyCatchByStream.DFOCode = cmt.DFOCode)
                            WHERE ((rutDailyCatchByStream.Year) = ", Yr, " AND
                           rutDailyCatchByStream.abMission = '", abMission, "')
                           GROUP BY cmtManagementGroups.GroupName, 
                                    rutDailyCatchByStream.abMission,
                                    cmtStocks.DFOCode, 
                                    cmtStocks.Stream,
                                    rutDailyCatchByStream.Year;"))
  
}

# Import data ----
# Catch above mission and passage data exported from RuFEs database
datRFabove <- funcLoadRuFEs_GroupByPop(dbPath = db_path, Grouping = "MA-RSA Stocks", 
                                  Yr = year, abMission = "a")

datRFpass <- funcLoadRuFEs_GroupByPop(dbPath = db_path, Grouping = "DNA Group", 
                                 Yr = year, abMission = "m")

# DBE and escapement data exported from RuFEs database
datEsc <- funcLoadRuFEs_ByPop(dbPath = db_path,
                                             Yr = year, 
                                             Table = "rutAnnualEscapement")

datDBE <- funcLoadRuFEs_ByPop(dbPath = db_path,
                                   Yr = year, 
                                   Table = "rutDataSource_DBE") 

# Broodstock
datBrood <- readxl::read_excel(paste0(here(), "/Data/Broodstock.xlsx"), sheet = "Sheet1")
# datBrood <- readxl::read_excel(paste0("//ADAMS/Data Sets/DB_Projects/AccessDBs/RuFEs/QAQC/Data/Broodstock.xlsx"), 
#                               sheet = "Sheet1", skip = 1)


# Common data 
funcLoadRuFEs_CommonData(dbPath = db_path)


# DBE data exported from RuFEs for an old SEF project 
# use to check changes/see differences
datSEF <- readxl::read_excel(paste0(here(), 
                                    "/Data/ruqDataSource_DailyCatch_SEFOutputFINAL_Incl2015.xlsx"),
                             sheet = "ruqDataSource_DailyCatch_SEFOut")
# datSEF <- readxl::read_excel(paste0("//ADAMS/Data Sets/DB_Projects/AccessDBs/RuFEs/QAQC/Data/ruqDataSource_DailyCatch_SEFOutputFINAL_Incl2015.xlsx"), 
#                               sheet = "ruqDataSource_DailyCatch_SEFOut")

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
  dplyr::select(Year, Stream, GroupName, SumOfStockCatch, DFOCode) %>% 
  dplyr::rename(RF_CatchAbove = SumOfStockCatch, 
                "MA-RSA Stocks" = GroupName) %>%
  na.omit()

datRFpass %<>% 
  dplyr::select(Year, Stream, GroupName, SumOfStockCatch, DFOCode) %>% 
  dplyr::rename(RF_Passage = SumOfStockCatch, 
                DNAgrp = GroupName) %>% 
  na.omit()

# join catch above mission and passage data together
datRFabove2 <- left_join(datRFabove, datCommon, by = c("Stream", "MA-RSA Stocks", "DFOCode"))
datRFpass2 <-  left_join(datRFpass, datCommon, by = c("Stream", "DNAgrp", "DFOCode"))
datRF <- full_join(datRFabove2, datRFpass2, 
                   by = c("Year", "Stream", "cmtManagementGroupsID_DNA", 
                          "cmtManagementGroupsID_MA_RSA", "cmtStocksID",
                          "DFOCode", "MA-RSA Stocks", "DNAgrp"))

# Organize Broodstock data ----

datBrood %<>%
  mutate(DFOCode = 7001) %>% # DFO code for Cultus Lake
  select(c(Year, DFOCode, TotalAdult)) %>% 
  rename(TotalBroodstockAdults = TotalAdult)

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

if(length(which(duplicated(datDBE$DFOCode))) >= 1){
  datDBE %<>% 
    group_by(DFOCode) %>% 
    slice_max(RF_Escapement) %>% 
    ungroup()
}

datEsc %<>%
  filter(!is.na(`Watershed Group Name`)) %>% 
  rowwise() %>% 
  mutate(RF_EscBrood = sum(females_brood, males_brood, na.rm = TRUE),
         RF_Esc = sum(females, males, na.rm = TRUE)) %>% 
  rename(Stream = `Stock Name(stream)`, 
         DFOCode = `Stock Group`,
         WatershedGrpName = `Watershed Group Name`) %>% 
  select(c(Year, WatershedGrpName, 
          Stream, DFOCode, RF_Esc, RF_EscBrood)) %>% 
  ungroup()

datEsc <- left_join(datEsc, datCommon, by = c("Stream", 'DFOCode'))
datDBEfull <- full_join(datEsc, datDBE, by = c("Year", "MA-RSA Stocks", "Stream", 
                                               "cmtManagementGroupsID_DNA", 
                                               "cmtStocksID", "DFOCode", 
                                               "cmtManagementGroupsID_MA_RSA", "DNAgrp"))

# Make full dataframe ----
dat <- full_join(datDBEfull, datRF, by = c("Year", "MA-RSA Stocks", "Stream", 
                                           "cmtManagementGroupsID_DNA", 
                                           "cmtStocksID", "DFOCode", 
                                           "cmtManagementGroupsID_MA_RSA", "DNAgrp"))
dat <- left_join(dat, datBrood, by = c("Year", "DFOCode")) 

# Calculate DBEs in R -----
dat %<>% 
  mutate(RF_Passage2 = ifelse(is.na(RF_Passage), 
                              0, 
                              RF_Passage), 
         RF_CatchAbove2 = ifelse(is.na( RF_CatchAbove), 
                                 0, 
                                 RF_CatchAbove)) %>% 
  select(c(Year, DFOCode, Stream, `MA-RSA Stocks`, DNAgrp, RF_Escapement, 
           RF_Esc, RF_EscBrood, TotalBroodstockAdults,
           RF_Passage2, RFesc_Passage, RFesc_CatchAbove, RF_CatchAbove2, 
           RF_DBE, RF_DBEbrood)) %>% 
  rename(RF_Passage = RF_Passage2, 
         RF_CatchAbove = RF_CatchAbove2) %>% 
  # do it this way so that if passage or catch = NA it still calculates a DBE
  rowwise() %>% 
  mutate(diffPsg = RFesc_Passage - RF_Passage, 
         diffCatchA = RFesc_CatchAbove - RF_CatchAbove,
         calDBE = sum(RF_Escapement, -1*RF_Passage, RF_CatchAbove, na.rm = TRUE),
         calDBE_brood = sum(sum(TotalBroodstockAdults, RF_Escapement, na.rm = TRUE), 
                            -1*RF_Passage, RF_CatchAbove, na.rm = TRUE)) %>% 
  # brood DBE is only relevant for stocks where broodstock is taken (Cultus)
  mutate(calDBE_brood2 = ifelse(DFOCode == 7001, 
                                calDBE_brood, 
                                calDBE)) %>% 
  # get difference between the DBE calculated in RuFEs and the one calculated here
  rowwise() %>% 
  mutate(diffDBE = RF_DBE - calDBE, 
         diffDBE_brood = RF_DBEbrood - calDBE_brood2, 
         # percent difference
         pDiffDBE = (abs(RF_DBE - calDBE)/
                       ((RF_DBE + calDBE)/2)*100),
         pDiffDBE_brood = (abs(RF_DBEbrood - calDBE_brood2)/
                             ((RF_DBEbrood + calDBE_brood2)/2)*100),
         MissingBroodstock =  ifelse(is.na(TotalBroodstockAdults), 
                                     "no error", 
                                     ifelse(round(abs(diffDBE_brood), 0) == TotalBroodstockAdults,
                                            "missing broodstock",
                                            ifelse((TotalBroodstockAdults == RF_EscBrood | 
                                                      (is.na(TotalBroodstockAdults)) & 
                                                      (RF_EscBrood == 0 | is.na(RF_EscBrood))), 
                                                   "no error", 
                                                   "missing broodstock"))),
         # calDBE is the DBE calculated here in R while RF_DBE is the DBE calculated in RuFEs
         # flag errors (mismatches between RuFEs DBE and the one calculated here)
         Errors = ifelse((is.na(pDiffDBE) & RF_DBE == 0 & calDBE == 0) &
                           (is.na(pDiffDBE_brood) & RF_DBEbrood == 0 & calDBE_brood2 == 0),
                         "no error",
                         # can change threshold
                         ifelse(abs(pDiffDBE) < 1E-4 & abs(pDiffDBE_brood) < 1E-4,
                                "no error", 
                                ifelse((RF_DBE < 1e-10 & calDBE == 0 & 
                                          RF_DBEbrood < 1e-10 & calDBE_brood2 == 0),
                                       "no error",
                                       "error")))) %>% 
  select(-calDBE_brood) %>% 
  rename(calDBE_brood = calDBE_brood2) %>% 
  ungroup()

# get dataframe of entries with errors
datError <- dat %>% filter(Errors == "error" | MissingBroodstock == "missing broodstock")

if(nrow(datError) >= 1){
  datErrorSave <- datError
} else {
  # to clear out old error messages when you re-save summary info
  datErrorSave <- as.data.frame(t(rep(NA, ncol(datError))))
  colnames(datErrorSave) <- colnames(datError)
}

# Group data by MA-RSA group & spawning district ----
# can also check this against the group export from RuFEs (NOT by Population)
datGrpd <- dat %>% 
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
           CalculatedDBE = calDBE, 
           SEF_DBE = TotalDBE) %>% 
    mutate(SEF_DBE = as.numeric(SEF_DBE)) %>% 
    rowwise() %>% 
    mutate(`diff (cal - SEF)` = CalculatedDBE - SEF_DBE) %>% 
    mutate(Year = ifelse(is.na(year),
                         Year <- Year,
                         Year <- year))
  
} else {
  datSEF <- as.data.frame(matrix(nrow = 0, ncol = 0))
}

# Summaries ----
dlg_message(paste("Total number of DBE errors:", nrow(datError)), type = "ok")

# save summary table
datGrpd <- as.data.frame(datGrpd)
datLong <- as.data.frame(dat)
datError <- as.data.frame(datErrorSave)
datSEF <- as.data.frame(datSEF)

# filename <- paste0(here(), "/", year, "/Summaries/DBESummaryTable_", Sys.Date(),".xlsx")
# 
# write.xlsx(datGrpd, filename, row.names = FALSE, sheetName = "Grouped")
# write.xlsx(datLong, filename, row.names = FALSE, sheetName = "ByStream", append = TRUE)
# write.xlsx(datError, filename, row.names = FALSE, sheetName = "Errors", append = TRUE)
# write.xlsx(datSEF, filename, row.names = FALSE, sheetName = "SEF", append = TRUE)
