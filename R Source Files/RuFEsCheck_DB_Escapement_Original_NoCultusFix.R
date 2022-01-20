#######################################################
# Code to check RuFEs catch/passage/escapement data

# Created: Serena Wong, February 2021
#
# OBJECT DEFINITIONS: 
#     * data with the datCR prefix are all from the CR100 excel files (data loaded into
#       RuFEs)
#     * data with the datRF prefix are all data downloaded from RuFEs 
#       (above = catch above Mission, below = catch below Mission, pass = Mission passage)
#     * data with the cmt prefix is all data from the Common database and used for 
#       stock translation
#     * datEsc is the escapement data downloaded from RuFEs
#     * datCheck has checks for whether the values for catch/passage between the CR100 
#       data and RuFEs match in terms of both absolute and percent differences
#     * datCatchEsc, datPropEsc, and datPropEscCatch all give some sense of the data available for each 
#       group (eg. number of streams in a stock group that have escapement data, which DNA 
#       groups have catch above/below and/or passage data)
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

funcLoadRuFEs_ByCat <- function(dbPath, Grouping, Yr, abMission){
  # set up driver info and database path
  driverInfo <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
  path <- paste0(driverInfo, "DBQ=", dbPath)
  
  # establish connection
  channel <- RODBC::odbcDriverConnect(path)
  
  on.exit(odbcClose(channel))
  
  sqlQuery(channel, paste0("SELECT Sum(rutDailyCatchByStream.StockCatch) AS SumOfStockCatch,
                                  cmtManagementGroups.GroupName, 
                                  rutDailyCatchByStream.abMission,
                                  rutDailyCatchByStream.FisheryType,
                                  rutDailyCatchByStream.FisheryUser,
                                  rutDailyCatchByStream.Area,
                                  rutDailyCatchByStream.GearType,
                                  rutDailyCatchByStream.DataType,
                                  rutDailyCatchByStream.Year,
                                   rutDailyCatchByStream.FName,
                                  rutDailyCatchByStream.CatchDate
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
                                    rutDailyCatchByStream.Year,
                                    rutDailyCatchByStream.FisheryType,
                                    rutDailyCatchByStream.FisheryUser,
                                    rutDailyCatchByStream.Area,
                                    rutDailyCatchByStream.GearType,
                                    rutDailyCatchByStream.FName,
                                    rutDailyCatchByStream.DataType,
                                    rutDailyCatchByStream.CatchDate;"))
  
}

# Import data ----
# Data sheets from CR100 DataOut files
if(year < 2015){
  datCRbelow <- readxl::read_excel(
    paste0("//ADAMS/Racial/CR100_Sockeye/", year, "/CR_Sock100/CR_2BelowMission_DataOut.xlsm"), 
    sheet = "DbData", skip = 4)
  datCRabove <- readxl::read_excel(
    paste0("//ADAMS/Racial/CR100_Sockeye/", year, "/CR_Sock100/CR_5AboveMission_DataOut.xlsm"), 
    sheet = "DbData", skip = 4)
  datCRpass <- readxl::read_excel(
    paste0("//ADAMS/Racial/CR100_Sockeye/", year, "/CR_Sock100/CR_4Passage_DataOut.xlsm"), 
    sheet = "DbData", skip = 4)
} else {
  datCRbelow <- readxl::read_excel(
    paste0("//ADAMS/Racial/", year, "/CR_Sock100/CR_2BelowMission_DataOut.xlsm"), 
    sheet = "DbData", skip = 4)
  datCRabove <- readxl::read_excel(
    paste0("//ADAMS/Racial/", year, "/CR_Sock100/CR_5AboveMission_DataOut.xlsm"), 
    sheet = "DbData", skip = 4)
  datCRpass <- readxl::read_excel(
    paste0("//ADAMS/Racial/", year, "/CR_Sock100/CR_4Passage_DataOut.xlsm"), 
    sheet = "DbData", skip = 4)
}

# Catch and passage data exported from RuFEs database
datRFbelow <- funcLoadRuFEs_ByCat(dbPath = db_path, Grouping = "DNA Group", 
                                  Yr = year, abMission = "b") %>% 
  group_by(Year, GroupName) %>% 
  summarise(RF_CatchBelow = sum(SumOfStockCatch))

datRFabove <- funcLoadRuFEs_ByCat(dbPath = db_path, Grouping = "DNA Group", 
                                  Yr = year, abMission = "b") %>% 
  group_by(Year, GroupName) %>% 
  summarise(RF_CatchAbove = sum(SumOfStockCatch))

datRFpass <- funcLoadRuFEs_ByCat(dbPath = db_path, Grouping = "DNA Group", 
                                 Yr = year, abMission = "b") %>% 
  group_by(Year, GroupName) %>% 
  summarise(RF_Passage = sum(SumOfStockCatch))

# Escapement data exported from RuFEs database
datEsc <- funcLoadRuFEs_ByPop(dbPath = db_path,
                              Yr = year, 
                              Table = "rutAnnualEscapement")

# Common data 
funcLoadRuFEs_CommonData(dbPath = db_path)

# Organize Common data ----
cmtStocks %<>% 
  dplyr::rename("cmtStocksID" = "cmtStocksId")
cmtMG %<>% 
  filter(Source == "DNA Group")
cmt <- left_join(cmtMG, cmtMGD, by = c("cmtManagementGroupsID", "Source"))
cmt <- full_join(cmt, cmtStocks, by = "cmtStocksID")
cmtSub <- cmt %>% 
  dplyr::select(c("cmtManagementGroupsID", "cmtStocksID", "GroupName", 
                  "GroupAbbr", "Stream", "DFOCode")) %>% 
  tidyr::drop_na(DFOCode)

# Organize catch and pass data ----
# catch below Mission
datCRbelow %<>% 
  dplyr::group_by(Stock, MgGrp, stkgrp) %>% 
  summarize(CatchBelow = sum(StkCat, na.rm = TRUE)) %>% 
  rename(c(GroupName = Stock, 
           StkGrp = stkgrp))

# catch above Mission
datCRabove %<>% 
  dplyr::group_by(Stock, MgGrp, stkgrp) %>% 
  summarize(CatchAbove = sum(StkCat, na.rm = TRUE)) %>% 
  rename(c(GroupName = Stock, 
           StkGrp = stkgrp))

# passage at Mission
datCRpass %<>% 
  dplyr::group_by(Stock, MgGrp, stkgrp) %>% 
  summarize(Passage = sum(StkCat, na.rm = TRUE)) %>% 
  rename(c(GroupName = Stock, 
           StkGrp = stkgrp))

# join all CR100 catch and passage data together
datCR <- full_join(datCRabove, datCRbelow, by = c("MgGrp", "StkGrp", "GroupName"))
datCR <- full_join(datCR, datCRpass, by = c("MgGrp", "StkGrp", "GroupName"))

datCR %<>% 
  mutate_at(c('CatchAbove', "CatchBelow", "Passage"), as.numeric) %>% 
  dplyr::rename("CR_MgGrp" = "MgGrp")

# Organize RuFEs data -----
datRF <- full_join(datRFbelow, datRFabove, by = c("GroupName", "Year"))
datRF <- full_join(datRF, datRFpass, by = c("GroupName", "Year"))

# Organize escapement data -----
datEscSub <- datEsc %>% 
  dplyr::rename(c("Stream" = "Stock Name(stream)", 
                  "Esc_MgGrp" = "Timing Group Codes_Timing Group",
                  "DFOCode" = "Stock Group")) %>% 
  dplyr::select(c("Stream", "Year", "Watershed Group Name", "Esc_MgGrp",
                  "DFOCode", "Total")) %>% 
  mutate(MgGrp = recode(Esc_MgGrp, 
                        "Early Stuart" = "EStu",
                        "Early Summer" = "ESum",
                        "Summer" = "Summ"))

# Checks ----
## check for differences between CR and RF
# setdiff(datRF$GroupName, datCR$GroupName) # in RF that aren't in CR, should be 0
# setdiff(datCR$GroupName, datRF$GroupName)
## check for differences between CR and cmt
# setdiff(cmtSub$GroupName, datCR$GroupName) # in cmt that aren't in CR
# setdiff(datCR$GroupName, cmtSub$GroupName) # should be 0
## check for differences between escapement and cmt
# setdiff(cmtSub$Stream, datEscSub$Stream) # in cmt that aren't in escapement
# setdiff(datEscSub$Stream, cmtSub$Stream) # should be 0

# Check CR vs. RuFEs -----
datCheck <- full_join(datCR, datRF, by = "GroupName")
datCheck %<>% 
  # percent difference
  mutate(pDiffA = (abs(CatchAbove - RF_CatchAbove)/
                     ((CatchAbove + RF_CatchAbove)/2)*100), 
         pDiffB = (abs(CatchBelow - RF_CatchBelow)/
                     ((CatchBelow + RF_CatchBelow)/2)*100),
         pDiffP = (abs(Passage - RF_Passage)/
                     ((Passage + RF_Passage)/2)*100)) %>% 
  # absolute difference
  mutate(DiffA = abs(CatchAbove - RF_CatchAbove), 
         DiffB = abs(CatchBelow - RF_CatchBelow),
         DiffP = abs(Passage - RF_Passage), 
         # errors
         pErrorsA =   ifelse(is.na(pDiffA) & 
                               (CatchAbove == 0 | 
                                  is.na(CatchAbove)),
                             pErrorsA <- "no error",
                             ifelse(pDiffA > threshold,
                                    pErrorsA <- "error", 
                                    pErrorsA <- "no error")), 
         pErrorsB = ifelse(is.na(pDiffB) & 
                             (CatchBelow == 0 | 
                                is.na(CatchBelow)),
                           pErrorsB <- "no error",
                           ifelse(pDiffB > threshold,
                                  pErrorsB <- "error", 
                                  pErrorsB <- "no error")), 
         pErrorsP = ifelse(is.na(pDiffP) & 
                             (Passage == 0 | 
                                is.na(Passage)),
                           pErrorsP <- "no error",
                           ifelse(pDiffP > threshold,
                                  pErrorsP <- "error", 
                                  pErrorsP <- "no error")),
         pOverallErrors = ifelse(pErrorsA == "error" | 
                                   pErrorsB == "error" |
                                   pErrorsP == "error", 
                                 pOverallErrors <- "error", 
                                 pOverallErrors <- "no error"))

datCheckError <- datCheck %>% 
  filter(pOverallErrors == "error") %>% 
  dplyr::select(CR_MgGrp, StkGrp, GroupName, 
                CatchAbove, RF_CatchAbove, 
                CatchBelow, RF_CatchBelow, 
                Passage, RF_Passage, 
                DiffA, DiffB, DiffP, 
                pDiffA, pDiffB, pDiffP,
                pErrorsA, pErrorsB, pErrorsP, pOverallErrors)

# Join data together ----
datCatchEsc <- full_join(datCR, cmtSub, by = "GroupName")
datCatchEsc <- full_join(datCatchEsc, datEscSub, 
                         by = c("Stream", "DFOCode")) # MgGrp

# which(duplicated(datCatchEsc$DFOCode)) # should be zero
# which(duplicated(datCatchEsc$cmtStocksID)) # should be zero

# Escapement data types -----
datCatchEsc$EscType <- with(datCatchEsc, ifelse(
  Total == 0, 
  EscType <- 0, 
  ifelse(Total > 0,
         EscType <- ">0", 
         ifelse(is.na(Total),
                EscType <- "NA",
                EscType <- "unknown"))))  

# which(datCatchEsc$EscType == "unknown") # should be zero, this would flag negatives

datPropEsc <- datCatchEsc[complete.cases(datCatchEsc$GroupName), ] # this is just duplicating the dataframe currently
datPropEsc %<>%
  dplyr::select("CR_MgGrp", "Esc_MgGrp", "Watershed Group Name", "StkGrp", "GroupName", "GroupAbbr", 
                "cmtManagementGroupsID", "Stream", "cmtStocksID", "DFOCode", 
                "CatchBelow", "CatchAbove", "Passage", "Total", 
                "EscType") %>% 
  group_by(GroupName, .drop = FALSE) %>% 
  # number of streams in each DNA group etc 
  dplyr::summarize(NumStreams = n_distinct(DFOCode), # same as length(unique())
                   NumNA = sum(is.na(Total)),
                   NumZero = sum(Total == 0, na.rm = TRUE),
                   NumVal = sum(Total > 0, na.rm = TRUE)) %>% 
  group_by(GroupName, .drop = FALSE) %>% 
  dplyr::mutate(Sum = sum(NumNA, NumZero, NumVal), # should equal NumStreams
                Diff = NumStreams - Sum, # this flags is there is an issue (NumStreams should be total of NumNA, NumZero and NumVal)
                pNA = NumNA/NumStreams, 
                pZero = NumZero/NumStreams,
                pVal = NumVal/NumStreams #, 
                # pSum = (pVal + pZero + pNA)
  )

# which(datPropEsc$Diff != 0) # should be zero
# which(datPropEsc$pSum != 1) # should be zero

datCRSub <- datCR %>% 
  dplyr::select("CR_MgGrp", "StkGrp", "GroupName", "CatchBelow", 
                "Passage", "CatchAbove")

# Summary table ----
# summary table of proportion of streams with each escapement type and catch/passage
datPropEscCatch <- left_join(datPropEsc, datCRSub, by = "GroupName")

# need to change to add in when streams have both NA and 0's
datPropEscCatch$DataType <- with(datPropEscCatch, 
                                 ifelse(is.na(CatchBelow) & is.na(CatchAbove) & is.na(Passage) & pNA == 1, 
                                        DataTYpe <- "No Data", 
                                        ifelse((is.na(CatchBelow) & is.na(CatchAbove) & is.na(Passage)),
                                               DataType <- "Missing Catch and Passage",
                                               ifelse(pNA == 1 & (CatchBelow > 0 | 
                                                                    CatchAbove > 0 | 
                                                                    Passage > 0),
                                                      DataType <- "No esc (NA) w/ Catch/Passage",
                                                      ifelse(pZero == 1 & (CatchBelow > 0 | 
                                                                             CatchAbove > 0 | 
                                                                             Passage > 0),
                                                             DataType <- "No esc (zero) w/ Catch/Passage",
                                                             ifelse(pVal == 0 & pZero > 0 & pNA > 0 & (CatchBelow > 0 | 
                                                                                                         CatchAbove > 0 | 
                                                                                                         Passage > 0),
                                                                    DataType <- "No esc (zeros and NAs) w/ Catch/Passage",
                                                                    ifelse(pVal > 0 & pNA > 0 & pZero == 0 & (CatchBelow > 0 | 
                                                                                                                CatchAbove > 0 | 
                                                                                                                Passage > 0),
                                                                           DataType <- "Partial esc (NAs) w/ Catch/Passage",
                                                                           ifelse(pVal > 0 & pZero > 0 & pNA == 0 & (CatchBelow > 0 | 
                                                                                                                       CatchAbove > 0 | 
                                                                                                                       Passage > 0),
                                                                                  DataType <- "Partial esc (zeros) w/ Catch/Passage",
                                                                                  ifelse(pVal > 0 & pZero > 0 & pNA > 0 & (CatchBelow > 0 | 
                                                                                                                             CatchAbove > 0 | 
                                                                                                                             Passage > 0),
                                                                                         DataType <- "Partial esc (zeros and NAs) w/ Catch/Passage",
                                                                                         ifelse((pVal == 1 & CatchBelow == 0 & 
                                                                                                   CatchAbove == 0 &
                                                                                                   Passage == 0),
                                                                                                DataType <- "Full esc w/o Catch/Passage",
                                                                                                ifelse(pVal == 1 & (CatchBelow > 0 | 
                                                                                                                      CatchAbove > 0 | 
                                                                                                                      Passage > 0),
                                                                                                       DataType <- "Full esc w/ Catch/Passage",
                                                                                                       DataType <- "Unknown"
                                                                                                )))))))))))

summaryTableAll <- plyr::count(datPropEscCatch$DataType)

summaryTableFraser <- datPropEscCatch %>% 
  filter(CR_MgGrp != "nFra")
summaryTableFraser <- plyr::count(summaryTableFraser$DataType)

summaryTable <- full_join(summaryTableAll, summaryTableFraser, by = "x")
colnames(summaryTable) <- c("Data type", "Frequency all stocks", "Frequency Fraser stocks")
# summaryTable <- as.data.frame(summaryTable)

# Error table ----
d <- datCR[rowSums(datCR[ , c("CatchBelow", "CatchAbove", "Passage")], na.rm = TRUE) == 0,]
`%!in%` <- negate(`%in%`)

names <- c("stocks in RuFEs not in CR100", 
           "stocks in CR100 not in RuFEs", 
           "stocks in CR100 with catch/passage not equal to 0 that are not in RuFEs",
           "mismatched catch/passage values between CR100 and RuFEs",
           "max absolute difference between CR100 and RuFEs values",
           "stocks in Common not in CR100", 
           "stocks in CR100 not in Common", 
           "streams in Common not in escapement data", 
           "streams in escapement data not in Common",
           "duplicated streams in escapement data", 
           "unknown escapement type (not 0, >0, or NA)", 
           "stocks where sum of parts not equal to total stream #",
           "number non-Fraser stocks")
number <- c(length(setdiff(datRF$GroupName, datCR$GroupName)), # should be 0
            length(setdiff(datCR$GroupName, datRF$GroupName)),
            length(which(setdiff(datCR$GroupName, datRF$GroupName) %!in% 
                           d$GroupName)), # should be 0
            length(which(datCheck$pOverallErrors == "error")), # should be 0
            max(c(range(datCheck$DiffA, na.rm = TRUE)[2], 
                  range(datCheck$DiffB, na.rm = TRUE)[2],
                  range(datCheck$DiffP, na.rm = TRUE)[2])),
            length(setdiff(cmtSub$GroupName, datCR$GroupName)), 
            length(setdiff(datCR$GroupName, cmtSub$GroupName)), # should be 0
            length(setdiff(cmtSub$Stream, datEscSub$Stream)),
            length(setdiff(datEscSub$Stream, cmtSub$Stream)), # should be 0
            length(which(duplicated(datCatchEsc$DFOCode))), # should be 0
            length(which(datCatchEsc$EscType == "unknown")), # should be 0
            length(which(datPropEsc$Diff != 0)), # should be 0
            length(which(datPropEscCatch$CR_MgGrp == "nFra")))
errorTable <- as.data.frame(cbind(names, number))
errorTable$number <- as.numeric(errorTable$number)
errorTable$Errors <- with(errorTable, ifelse(number != 0, 
                                             Errors <- "error", 
                                             Errors <- "no error"))
errorTable$Errors[c(2, 5, 6, 8, 13)] <- ""

if(length(which(errorTable$Errors == "error")) > 0){
  dlg_message(paste("There are escapement errors"), type = "ok")
} else {dlg_message(paste("There are no escapement errors"), type = "ok")} 

View(summaryTable)
View(errorTable)

# save summary table
datPropEscCatch <- as.data.frame(datPropEscCatch)
datCheckError <- as.data.frame(datCheckError)

# filename <- paste0(getwd(), "/Summaries/EscapementSummaryTable_", Sys.Date(),".xlsx")

# write.xlsx(summaryTable, filename, row.names = FALSE, sheetName = "Summary")
# write.xlsx(errorTable, filename, row.names = FALSE, sheetName = "Errors", append = TRUE)
# write.xlsx(datPropEscCatch, filename, row.names = FALSE, 
#           sheetName = "EscapementByDNAgroup", append = TRUE)
# write.xlsx(datCheckError, filename, row.names = FALSE, 
#           sheetName = "ErrorChecksCR100vsRuFES", append = TRUE)

