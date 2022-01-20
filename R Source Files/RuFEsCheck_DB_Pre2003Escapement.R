#######################################################
# Code to check RuFEs escapement data for years before 2003

# Created: Serena Wong, April 2021
#
# OBJECT DEFINITIONS: 
#     * datInput (and datI) and objects associated with the input data compiled by 
#       Ian Guthrie and loaded into RuFEs
#     * data with the datRF prefix are all data downloaded from RuFEs 
#       (above = catch above Mission, below = catch below Mission, pass = Mission passage)
#     * data with the cmt prefix is all data from the Common database and used for 
#       stock translation
#     * datEsc is the escapement data downloaded from RuFEs
#     * datCheck has checks for whether the values for catch/passage between the Input 
#       data and RuFEs match in terms of both absolute and percent differences
#     * datCatchEsc, datPropEsc, and datPropEscCatch all give some sense of the data available for each 
#       group (eg. number of streams in a stock group that have escapement data, which DNA 
#       groups have catch above/below and/or passage data)
#######################################################

# Import data ----
# Pre-2003 data input file
datInput <- if(year < 1990){
  readxl::read_excel(
    paste0("//ADAMS/Data Sets/DB_Projects/AccessDBs/RuFEs/DataInputs/pre2003/qryRuFEs_", 
           year, "_Daily_Catch+Passage.xlsx"),
    sheet = "qryRuFEs_Daily_Catch_Passage")
} else {
  readxl::read_excel(
    paste0("//ADAMS/Data Sets/DB_Projects/AccessDBs/RuFEs/DataInputs/pre2003/qryRuFEs", 
           year, "_Daily_Catch+Passage.xlsx"),
    sheet = "qryRuFEs_Daily_Catch_Passage")
}

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

# db_path <- "//ADAMS/Data Sets/DB_Projects/AccessDBs/RuFEs/RuFEsR.accdb"

# Organize Common data ----

funcLoadRuFEs_CommonData(dbPath = db_path)

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

# Get input data at DNA Grp level
datInput %<>% 
  tidyr::drop_na(Catch) %>% 
  dplyr::select(c(Cat, abMission, Type, User, Gear, Area, 
                  Stream, Date, StkGrp, StrId, Catch)) %>%
  dplyr::rename(DFOCode = StrId)

datI <- full_join(datInput, cmtSub, by = c("DFOCode", "Stream"))

datIGrped <- datI %>% 
  group_by(abMission, GroupName) %>% 
  summarize(TotalCatch = sum(Catch, na.rm = TRUE))  %>% 
  tidyr::drop_na(abMission) %>% 
  pivot_wider(id_cols = c(GroupName), # StkGrp
              values_from = TotalCatch, 
              names_from = abMission) %>% 
  rename(CatchAbove = Above, 
         CatchBelow = Below, 
         Passage = Mission) %>% 
  mutate_at(c('CatchAbove', "CatchBelow", "Passage"), as.numeric) %>% 
  # remove nonFraser catch/passage
  filter(GroupName != "NonFraser")

# Organize RuFEs data -----
datRF <- funcLoadRuFEs_ByPop(dbPath = db_path,
                                  Yr = year, 
                                  Table = "rutDailyCatchByStream",
                                  drop_vars = c("rutDailyCatchID", "TACdd", "FName", 
                                                "CatchComment", "DNAComment", 
                                                "PoolComment", "SourceComment", "AdJk", 
                                                "CreateOn", "CreatedBy", "UpdatedOn", 
                                                "UpdatedBy", "MissionDate", "Wt", "nppMiss", 
                                                "FromFile", "RecTime", "DBE", "DBE_Brood")) %>% 
  left_join(., cmtSub, by = "DFOCode") %>% 
  group_by(abMission, Year, GroupName) %>% 
  summarize(SumOfStockCatch = sum(StockCatch)) %>%
  ungroup %>% 
  mutate(abMission = recode(abMission,
                            m = "RF_Passage",
                            a = "RF_CatchAbove", 
                            b = "RF_CatchBelow")) %>%
  tidyr::pivot_wider(names_from = abMission, values_from = SumOfStockCatch) %>% 
  # remove nonFraser
  filter(GroupName != "NonFraser")

# Organize escapement data -----
datEscSub <- funcLoadRuFEs_ByPop(dbPath = db_path,
                                      Yr = year, 
                                      Table = "rutAnnualEscapement") %>% 
  dplyr::rename(c(Stream = `Stock Name(stream)`, 
                  Esc_MgGrp = `Timing Group Codes_Timing Group`,
                  DFOCode = `Stock Group`)) %>% 
  dplyr::select(c(Stream, Year, `Watershed Group Name`, Esc_MgGrp,
                  DFOCode, Total)) %>% 
  mutate(MgGrp = recode(Esc_MgGrp, 
                        "Early Stuart" = "EStu",
                        "Early Summer" = "ESum",
                        "Summer" = "Summ")) %>% 
  filter(!is.na(`Watershed Group Name`))

# Check CR vs. RuFEs -----
datCheck <- full_join(datIGrped, datRF, by = "GroupName")
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
         pErrorsA = ifelse(is.na(pDiffA) & (CatchAbove == 0 | is.na(CatchAbove)),
                           pErrorsA <- "no error",
                           ifelse(pDiffA > threshold,
                                  pErrorsA <- "error", 
                                  pErrorsA <- "no error")),
         pErrorsB  = ifelse(is.na(pDiffB) & (CatchBelow == 0 | is.na(CatchBelow)),
                            pErrorsB <- "no error",
                            ifelse(pDiffB > threshold,
                                   pErrorsB <- "error", 
                                   pErrorsB <- "no error")), 
         pErrorsP = ifelse(is.na(pDiffP) & (Passage == 0 | is.na(Passage)),
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
  dplyr::select(GroupName, CatchAbove, RF_CatchAbove, CatchBelow, 
                RF_CatchBelow, Passage, RF_Passage, DiffA, DiffB, DiffP, 
                pDiffA, pDiffB, pDiffP, pErrorsA, 
                pErrorsB, pErrorsP, pOverallErrors)

# range(datCheck$DiffA, na.rm = TRUE)
# range(datCheck$DiffB, na.rm = TRUE)
# range(datCheck$DiffP, na.rm = TRUE)

# Join data together ----
# even though datInput is at the stream level, you need to do it this way
# so that we include all the streams we do have data for as well
datCatchEsc <- full_join(datIGrped, cmtSub, by = "GroupName")
datCatchEsc <- full_join(datCatchEsc, datEscSub, 
                         by = c("Stream", "DFOCode")) # MgGrp

# which(duplicated(datCatchEsc$DFOCode)) # should be zero

# Escapement data types -----
datCatchEsc$EscType <- with(datCatchEsc, ifelse(
  Total == 0, 
  EscType <- 0, 
  ifelse(Total > 0,
         EscType <- ">0", 
         ifelse(is.na(Total),
                EscType <- "NA",
                EscType <- "unknown"))))  

# which(datCatchEsc$EscType == "unknown") # should be zero

datPropEsc <- datCatchEsc[complete.cases(datCatchEsc$GroupName), ]
datPropEsc %<>%
  dplyr::select(Esc_MgGrp, `Watershed Group Name`, GroupName, 
                Stream, DFOCode, CatchBelow, CatchAbove,
                Passage, Total, 
                EscType) %>% 
  group_by(GroupName, .drop = FALSE) %>% 
  # number of streams in each DNA group etc 
  dplyr::summarize(NumStreams = n_distinct(DFOCode), # same as length(unique())
                   NumNA = sum(is.na(Total)),
                   NumZero = sum(Total == 0, na.rm = TRUE),
                   NumVal = sum(Total > 0, na.rm = TRUE)) %>% 
  group_by(GroupName, .drop = FALSE) %>% 
  dplyr::mutate(Sum = sum(NumNA, NumZero, NumVal),
                Diff = NumStreams - Sum,
                pNA = NumNA/NumStreams, 
                pZero = NumZero/NumStreams,
                pVal = NumVal/NumStreams #, 
                # pSum = (pVal + pZero + pNA)
  )

# which(datPropEsc$Diff != 0) # should be zero
# which(datPropEsc$pSum != 1) # should be zero

# Summary table ----
# summary table of proportion of streams with each escapement type and catch/passage
datPropEscCatch <- left_join(datPropEsc, datIGrped, by = "GroupName")

datPropEscCatch$DataType <- with(datPropEscCatch, 
                                 ifelse(is.na(CatchBelow) & is.na(CatchAbove) & is.na(Passage) & pNA == 1, 
                                        DataTYpe <- "No Data", 
                                        ifelse(pVal == 1 & is.na(CatchBelow) & 
                                                 is.na(CatchAbove) & is.na(CatchAbove), 
                                               DataType <- "Full esc w/ no Catch/Passage",
                                               ifelse(pNA == 1 & (CatchBelow > 0 | 
                                                                    CatchAbove > 0 | 
                                                                    Passage > 0),
                                                      DataType <- "No esc (NA) w/ Catch/Passage",
                                                      ifelse(pZero == 1 & (CatchBelow > 0 | 
                                                                             CatchAbove > 0 | 
                                                                             Passage > 0),
                                                             DataType <- "No esc (zero) w/ Catch/Passage",
                                                             ifelse(pVal > 0 & pNA > 0 & (CatchBelow > 0 | 
                                                                                            CatchAbove > 0 | 
                                                                                            Passage > 0),
                                                                    DataType <- "Partial esc (NAs) w/ Catch/Passage",
                                                                    ifelse(pVal > 0 & pZero > 0 & (CatchBelow > 0 | 
                                                                                                     CatchAbove > 0 | 
                                                                                                     Passage > 0),
                                                                           DataType <- "Partial esc (zeros) w/ Catch/Passage",
                                                                           ifelse((pVal == 1 & CatchBelow == 0 & 
                                                                                     CatchAbove == 0 &
                                                                                     Passage == 0),
                                                                                  DataType <- "Full esc w/o Catch/Passage",
                                                                                  ifelse(pVal == 1 & (CatchBelow > 0 | 
                                                                                                        CatchAbove > 0 | 
                                                                                                        Passage > 0),
                                                                                         DataType <- "Full esc w/ Catch/Passage",
                                                                                         ifelse(pZero > 0 & pNA > 0 & is.na(CatchBelow) & 
                                                                                                  is.na(CatchAbove) & is.na(CatchAbove),
                                                                                                DataType <- "Partial esc w/ no Catch/Passage",
                                                                                                DataType <- "Unknown" 
                                                                                         ))))))))))

# this just contains Fraser River stocks
summaryTable <- plyr::count(datPropEscCatch$DataType)
colnames(summaryTable) <- c("Data type", "Frequency Fraser stocks")
# summaryTable <- as.data.frame(summaryTable)

# Error table ----
d <- datIGrped[rowSums(datIGrped[ , c("CatchBelow", "CatchAbove", "Passage")], na.rm = TRUE) == 0,]
`%!in%` <- negate(`%in%`)

names <- c("stocks in RuFEs not in Input", 
           "stocks in Input not in RuFEs", 
           "stocks with Input catch/passage not equal to 0 that are not in RuFEs",
           "mismatched catch/passage values between Input and RuFEs",
           "max absolute difference between Input and RuFEs values",
           "stocks in Common not in Input", 
           "stocks in Input not in Common", 
           "streams in Common not in escapement data", 
           "streams in escapement data not in Common",
           "duplicated streams in escapement data", 
           "unknown escapement type (not 0, >0, or NA)", 
           "stocks where sum of parts not equal to total stream #")
number <- c(length(setdiff(datRF$GroupName, datIGrped$GroupName)), # should be 0
            length(setdiff(datIGrped$GroupName, datRF$GroupName)),
            length(which(setdiff(datIGrped$GroupName, datRF$GroupName) %!in% 
                           d$GroupName)), # should be 0
            length(which(datCheck$pOverallErrors == "error")), # should be 0
            max(c(range(datCheck$DiffA, na.rm = TRUE)[2], 
                  range(datCheck$DiffB, na.rm = TRUE)[2],
                  range(datCheck$DiffP, na.rm = TRUE)[2])),
            length(setdiff(cmtSub$GroupName, datIGrped$GroupName)), 
            length(setdiff(datIGrped$GroupName, cmtSub$GroupName)), # should be 0
            length(setdiff(cmtSub$Stream, datEscSub$Stream)),
            length(setdiff(datEscSub$Stream, cmtSub$Stream)), # should be 0
            length(which(duplicated(datCatchEsc$DFOCode))), # should be 0
            length(which(datCatchEsc$EscType == "unknown")), # should be 0
            length(which(datPropEsc$Diff != 0))) # should be 0
errorTable <- as.data.frame(cbind(names, number))
errorTable$number <- as.numeric(errorTable$number)
errorTable$Errors <- with(errorTable, ifelse(number != 0, 
                                             Errors <- "error", 
                                             Errors <- "no error"))
errorTable$Errors[c(2, 5, 6, 8)] <- ""

if(length(which(errorTable$Errors == "error")) > 0){
  dlg_message(paste("There are escapement errors"), type = "ok")
} else {dlg_message(paste("There are no escapement errors"), type = "ok")} 


View(summaryTable) # NA is NA escapement with no Catch/Passage
View(errorTable)

# save summary table
datPropEscCatch <- as.data.frame(datPropEscCatch)
datCheckError <- as.data.frame(datCheckError)

# filename <- paste0(getwd(), "/Summaries/EscapementSummaryTable_", Sys.Date(),".xlsx")
# 
# write.xlsx(summaryTable, filename, row.names = FALSE, sheetName = "Summary")
# write.xlsx(errorTable, filename, row.names = FALSE, sheetName = "Errors", append = TRUE)
# write.xlsx(datPropEscCatch, filename, row.names = FALSE, sheetName = "EscapementByDNAgroup", append = TRUE)
# write.xlsx(datCheckError, filename, row.names = FALSE, sheetName = "ErrorChecksInputvsRuFES", append = TRUE)
