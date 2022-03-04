#######################################################
# Code to check RuFEs data import from input files 
# for pre-2003 catch below Mission 
# 
# Created: Serena Wong, April 2021
#
# OBJECT DEFINITIONS: 
#     * datInput (and datI) and objects associated with the input data compiled by 
#       Ian Guthrie and loaded into RuFEs. datIfra is just the Fraser River stocks
#     * data with the datRF prefix is all data downloaded from RuFEs 
#       (A = catch above Mission, B = catch below Mission, P = Mission passage)
#     * data with the cmt prefix is all data from the Common database and used for 
#       stock translation
#     * datMisI gives the rows in the input data that did not have corresponding 
#       matches in the data downloaded from RuFEs
#     * datMisRF gives the rows in the RuFEs data that did not have corresponding
#       matches in the input data
#######################################################
# Functions ----
`%!in%` <- negate(`%in%`)

funcLoadRuFEs_ByPop <- function(dbPath, Yr){
  # set up driver info and database path
  driverInfo <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
  path <- paste0(driverInfo, "DBQ=", dbPath)
  
  # establish connection
  channel <- RODBC::odbcDriverConnect(path)
  
  on.exit(odbcClose(channel))
  
  sqlQuery(channel, paste0("SELECT rutDailyCatchByStream.StockCatch, 
                                   cmtFisheryTypes.FisheryCode, 
                                   cmtGearTypes.GearTypeCode, 
                                   cmtFisheryUsers.FGUserCode, 
                                   rutDailyCatchByStream.Country, 
                                   rutDailyCatchByStream.abMission, 
                                   rutDailyCatchByStream.DataType, 
                                   rutDailyCatchByStream.CatchDate, 
                                   rutDailyCatchByStream.DFOCode, 
                                   rutDailyCatchByStream.Region, 
                                   rutDailyCatchByStream.Year,
                                   rutDailyCatchByStream.Area
                          FROM ((rutDailyCatchByStream 
                                  INNER JOIN cmtGearTypes 
                                  ON rutDailyCatchByStream.cmtGearId = cmtGearTypes.GearTypeID) 
                                  INNER JOIN cmtFisheryTypes 
                                  ON rutDailyCatchByStream.cmtFisheryTypeId = cmtFisheryTypes.FisheryTypeID) 
                                  INNER JOIN cmtFisheryUsers 
                                  ON rutDailyCatchByStream.cmtFisheryUserId = cmtFisheryUsers.FisheryUserID
                          WHERE (rutDailyCatchByStream.Year)=", Yr, ";"))
  
}


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


# db_path <- "//ADAMS/Data Sets/DB_Projects/AccessDBs/RuFEs/RuFEsR.accdb"

# Import data ----
# Pre-2003 data input file
datInput <- if(year %in% c(1980, 1981, 1983, 1985)){
  readxl::read_excel(
    paste0("//ADAMS/Data Sets/DB_Projects/AccessDBs/RuFEs/DataInputs/pre2003/qryRuFEs_", 
           year, "_Daily_Catch+Passage_a23-a27_replaced.xlsx"),
    sheet = "qryRuFEs_Daily_Catch_Passage")
} else if(year %!in% c(1980, 1981, 1983, 1985) & year < 1990){
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

# Organize Common data ----
funcLoadRuFEs_CommonData(dbPath = db_path)

cmtStocks %<>% 
  dplyr::rename("cmtStocksID" = "cmtStocksId")
cmt <- left_join(cmtMG, cmtMGD, by = c("cmtManagementGroupsID", "Source"))
cmt <- full_join(cmt, cmtStocks, by = "cmtStocksID")
cmtSub <- cmt %>% 
  rename(Source = Source.x) %>% 
  dplyr::select(c("Source", "cmtManagementGroupsID", 
                  "cmtStocksID", "GroupName", 
                  "GroupAbbr", "Stream", "DFOCode")) %>% 
  arrange(DFOCode) %>% 
  tidyr::drop_na(DFOCode)

cmtSubW <- cmtSub %>% 
  pivot_wider(id_cols = c(DFOCode, Stream),
              names_from = Source, 
              values_from = c(GroupName, GroupAbbr)) %>% 
  select(c(DFOCode, Stream, `GroupName_DNA Group`, `GroupAbbr_DNA Group`)) %>% 
  rename(DNAgrp = `GroupName_DNA Group`, 
         DNAgrpAbbr = `GroupAbbr_DNA Group`)

# Organize input data ----
datInput %<>% 
  tidyr::drop_na(Catch) %>% 
  # rownames_to_column("RowNum") %>% 
  dplyr::select(c(# RowNum, 
    Cat, abMission, Type, User, Gear, Area, 
    Stream, Date, StkGrp, StrId, Catch)) %>%
  dplyr::rename(c("DFOCode" = "StrId",
                  "I_Type" = "Type",
                  "I_User" = "User",
                  "I_Gear" = "Gear",
                  "I_Area" = "Area",
                  "I_Date" = "Date",
                  "I_StkGrp" = "StkGrp",
                  "I_Catch" = "Catch", 
                  "I_abMission" = "abMission",
                  "I_Cat" = "Cat"))

datI <- full_join(datInput, cmtSubW, by = c("DFOCode", "Stream"))

datI %<>% 
  # mutate(DNAgrpNonFra = ifelse(is.na(DNAgrp) & Stream == "NonFraser", 
  #                             DNAgrpNonFra <- "NonFraser", 
  #                             DNAgrpNonFra <- DNAgrp)) %>% 
  tidyr::drop_na(I_Catch) %>% 
  # group_by(DNAgrpNonFra, I_Cat, I_abMission, I_Type, I_User, I_Gear, 
  group_by(DNAgrp, I_Cat, I_abMission, I_Type, I_User, I_Gear, 
           I_Area, I_Date) %>% 
  summarize(Catch = sum(I_Catch)) %>% 
  rename("I_Stock" = "DNAgrp",
         # "I_Stock" = "DNAgrpNonFra",
         "I_Catch" = "Catch")

# Organize RuFEs data ----

datRF <- funcLoadRuFEs_ByPop(dbPath = db_path,
                                  Yr = year) %>% 
  left_join(., cmtSubW, by = "DFOCode") %>% 
  group_by(Country, FisheryCode, FGUserCode, Region, 
             Area, abMission, GearTypeCode, CatchDate, Year, DataType,
             DNAgrp) %>% 
  summarize(SumOfStockCatch = sum(StockCatch)) %>%
  ungroup %>% 
  mutate(abMission = recode(abMission,
                               m = "Mission",
                            a = "Above", 
                            b = "Below"), 
         RF_Cat = recode(DataType, 
                         Passage = "Mission"), 
         RF_Type = recode(FisheryCode, 
                         PP = "EscPitt", 
                         P = "Esc"), 
         RF_Date = force_tz(CatchDate, tz = "UTC")) %>% 
  dplyr::select(-c(Country, Region, FisheryCode, CatchDate)) %>% 
  dplyr::rename(c("RF_User" = "FGUserCode",
                  "RF_Gear" = "GearTypeCode",
                  # "RF_Area" = "Area",
                  "RF_abMission" = "abMission",
                  "RF_Stock" = "DNAgrp",
                  "RF_Catch" = "SumOfStockCatch")) %>% 
  mutate(RF_Date2 = as.numeric(RF_Date), 
         RF_Area = trimws(Area, which = c("both"))) %>% 
  rownames_to_column("RF_RowNum")

# Convert categories ----
# Rename I Type, User, and Gear to be consistent with RuFEs format
datI %<>% 
  mutate(I_Gear = recode(I_Gear,
                         AN = "An")) %>% 
  mutate(I_Type = recode(I_Type,
                         Rec = "REC",
                         tf = "TF",
                         Exp = "SP",
                         Tf = "TF",
                         Cm = "CM", 
                         Cer = "CER")) %>% 
  mutate(I_User = recode(I_User, 
                         # Ak = "AK", 
                         WDF = "AC-US", 
                         TI = "TI-US", 
                         NI = "AC-US", 
                         FR = "Fraser", 
                         Mrne = "RECm"), 
         I_Area2 = recode(I_Area, 
                          a1 = "Area 1",
                          a2E = "Area 2East",
                          a2w = "Area 2West",
                          a4b = "Area 4",
                          a4b5 = "US Area 4b/5", 
                          a4b56c = "US Area 4b/5/6c", 
                          "a5-10" = "Area 5-10",
                          "a6-7" = "US Area 6/7",
                          a7 = "US Area 7", 
                          a77a = "US Area 7/7A",
                          a7a = "US Area 7A", 
                          a11 = "Area 11",
                          "a11-12" = "Area 11/12",
                          "a11-13" = "Area 11/12/13",
                          "a11-16" = "Area 11/12/13/14/15/16",
                          A12 = "Area 12",
                          a12 = "Area 12", 
                          a12ri = "Area 12",
                          a12nc = "Area 12", 
                          "a12-13" = "Area 12/13",
                          "a12-16" = "Area 12/13/14/15/16",
                          a13 = "Area 13",
                          "a13-16" = "Area 13/14/15/16",
                          "a14,15,16" = "Area 13/14/15/16",
                          "a14-16" = "Area 13/14/15/16",
                          "a14-29" = "Area 14 - 29",
                          a15 = "Area 13/14/15/16", 
                          a16 = "Area 16", 
                          "a16-29" = "Area 14 - 29",
                          "a17-18" = "Area 17/18",
                          a17 = "Area 17/29", 
                          "a17-29" = "Area 17/29", 
                          "a17-29a" = "Area 17/29",
                          a20 = "Area 20",
                          a21 = "Area 19 - 127", # for 1980
                          # a23 = "Area 23", # changed in files
                          # a24 = "Area 24", 
                          # a25 = "Area 25", 
                          # a26 = "Area 26", 
                          # a27 = "Area 27",
                          a29 = "Area 29",
                          A29 = "Area 29",
                          a29a = "Area 29a", 
                          a29b = "Area 29b", 
                          a29d = "Area 29d",
                          A104 = "a104",
                          a104N = "a104",
                          a104S = "a104", 
                          "a104(north)" = "a104", # 1995
                          "a104(south)" = "a104", # 1995
                          a104n = "a104", # 1994
                          a104s = "a104", # 1994
                          "a121-124" = "Area 121/123/124P",
                          a124 = "Area 124", 
                          "a125-127" = "Area 125/126/127",
                          bm = "Area 29bd",
                          "Mis-Har" = "Mission to Harrison",
                          ms = "Mission to Sawmill",
                          as = "Above Sawmill", 
                          a2W = "Area 2West"), 
         I_Type2 = # ifelse(I_Type == "CS" & I_User == "TI",
           #  I_Type2 <- "CER",
           ifelse((I_Type == "EO" | I_Type == "EO/Demo") & 
                    I_User == "FNr", 
                  I_Type2 <- "CM",
                  #  ifelse(I_Type == "EO" & I_User == "FNm", 
                  #        I_Type2 <- "CM",
                  I_Type2 <- I_Type), # ))
         I_User2 =  # ifelse(I_Type == "CS" & I_User == "TI",
           #      I_User2 <- "TI-US",
           ifelse((I_Type == "EO" | I_Type == "EO/Demo" | I_Type == "ESSR") &  
                    I_User == "FNr", 
                  I_User2 <- "Fraser-FSC",
                  ifelse(I_Type == "FSC" & 
                           I_User == "FNr", 
                         I_User2 <- "Fraser-FSC",
                         #  ifelse(I_Type == "CM" & I_User == "TI", 
                         #       I_User2 <- "TI-US",
                         # ifelse(I_User == "NI", 
                         #      I_User2 <- "AC-US",
                         I_User2 <- I_User))) # )))

# summarize datI to combine area 104 north and south if they exist
datI %<>% 
  group_by(I_Stock, I_Cat, I_abMission, I_Type, I_User, I_Gear, 
           I_Date, I_Area2, I_Type2, I_User2) %>% 
  summarize(I_Catch = sum(I_Catch, na.rm = TRUE)) %>% 
  select(c(I_Stock, I_Cat, I_abMission, I_Type, I_User, I_Gear, 
           I_Date, I_Catch, I_Area2, I_Type2, I_User2)) %>% 
  mutate(I_Date2 = as.numeric(I_Date))


# get just Fraser River stocks since RuFEs doesn't have non-Fraser
# datIfra <- datI %>% 
#  filter(I_Stock != "NonFraser") %>% 
#  rownames_to_column("I_RowNum")

# Row matching (checks of catch/passage values) ----

# run loop to search for each row from the input file in the RuFEs data
# make empty vector
rows <- c()
threshold <- 1e-4

for(r in 1:nrow(datI)) {
  RowNum <- which(
    # percent difference
    ((abs(datI$I_Catch[r] - datRF$RF_Catch)/
        ((datI$I_Catch[r] + datRF$RF_Catch)/2)*100) 
     # may need to change difference threshold if getting mismatches for 
     # no reason
     < threshold) &
      # other categories match
      datI$I_Cat[r] == datRF$RF_Cat &
      datI$I_abMission[r] == datRF$RF_abMission &
      datI$I_Date2[r] == datRF$RF_Date2 &
      datI$I_Stock[r] == datRF$RF_Stock &
      datI$I_Type2[r] == datRF$RF_Type & 
      datI$I_User2[r] == datRF$RF_User &
      datI$I_Gear[r] == datRF$RF_Gear &
      datI$I_Area2[r] == datRF$RF_Area)
  
  if(is.integer(RowNum) & length(RowNum)){
    rows[r] <- RowNum
  } else {rows[r] <- "mismatch"
  }
}

# # Look for mismatches ---
# which(rows == "mismatch") # should be 0, if not then it gives the rows in the 
# # Input dataframe that don't have a corresponding value in RuFEs
# length(rows) == length(unique(rows)) # should be TRUE
# which(duplicated(rows, incomparables = "mismatch")) # should be 0 
# # note that duplicated doesn't show the first of the duplicated values

# Get mismatches ----
datMisI <- datI[which(rows == "mismatch"),]
# note that this is just the mismatched input data

# SELECT an unmatched value in the input file (here just using the first value)
Irow <- as.numeric(which(rows == "mismatch")[1])
# search for unmatched/mismatched input values in RuFEs data
# use to update threshold
RFrow <- which(
  datI$I_Cat[Irow] == datRF$RF_Cat &
    datI$I_abMission[Irow] == datRF$RF_abMission &
    datI$I_Date[Irow] == datRF$RF_Date &
    datI$I_Stock[Irow] == datRF$RF_Stock &
    datI$I_Type2[Irow] == datRF$RF_Type & 
    datI$I_User2[Irow] == datRF$RF_User &
    datI$I_Gear[Irow] == datRF$RF_Gear)

# mismatched RuFEs data
datMisRF <- datRF[setdiff(datRF$RF_RowNum, rows), ]

# # Revise threshold ----
# # if there is a number in RFrow, then all other categories match and it may be a
# # threshold issue. Can use mismatch data to revise threshold
# if(length(RFrow) != 0){((abs(datI$I_Catch[Irow] - datRF$RF_Catch[RFrow]))/
#                           ((datI$I_Catch[Irow] + datRF$RF_Catch[RFrow])/2)*100) < threshold 
#   # can revise threshold if FALSE
# }

# Summary ----
# make summary table
names <- c("total catch", "rows", "mismatched/unmatched rows", "# duplicated", 
           "# with negative catch/passage",
           "# stocks", "first date", "last date")
Icol <- c(round(sum(datI$I_Catch), 2), 
          nrow(datI),  
          nrow(datMisI), 
          length(which(duplicated(rows, incomparables = "mismatch"))),
          length(which(datI$I_Catch < 0)), 
          length(unique(datI$I_Stock)), 
          # did not add in length(unique) for I_Area since some are combined in RuFEs
          range(datI$I_Date, na.rm = TRUE)[1], 
          range(datI$I_Date, na.rm = TRUE)[2])
RFcol <- c(round(sum(datRF$RF_Catch), 2), 
           nrow(datRF), 
           nrow(datMisRF), 
           length(which(duplicated(rows, incomparables = "mismatch"))),
           length(which(datRF$RF_Catch < 0)), 
           length(unique(datRF$RF_Stock)), 
           range(datRF$RF_Date, na.rm = TRUE)[1], 
           range(datRF$RF_Date, na.rm = TRUE)[2])
furtherSteps <- c("", "", "see datMisI and datMisRF", 
                  "compare fishery Type/User/Gear categories", 
                  "", "", "", "")
summaryTable <- as.data.frame(cbind(names, as.numeric(Icol), 
                                    as.numeric(RFcol), furtherSteps))
summaryTable$Errors1 <- with(summaryTable, ifelse(V2 != V3,
                                                  Errors1 <- "Error", 
                                                  Errors1 <- "No error"))
summaryTable$Errors2 <- with(summaryTable, ifelse(V2 != 0 | V3 != 0,
                                                  Errors2 <- "Error", 
                                                  Errors2 <- "No error"))
Errors <- c(summaryTable$Errors1[c(1, 2)], summaryTable$Errors2[c(3:5)],
            summaryTable$Errors1[c(6:8)])
summaryTable <- cbind(summaryTable, Errors)
summaryTable %<>% 
  dplyr::select(c("names", "V2", "V3", "Errors", "furtherSteps")) %>% 
  dplyr::rename(c(" " = "names",
                  "Input data (w/ NonFraser)" = "V2",
                  "RuFEs data" = "V3", 
                  "If error:" = "furtherSteps"))
summaryTable[7,2] <- as.character(as.Date(range(datI$I_Date, na.rm = TRUE)[1], 
                                          format = "%d/%m/%yyyy"))
summaryTable[8,2] <- as.character(as.Date(range(datI$I_Date, na.rm = TRUE)[2], 
                                          format = "%d/%m/%yyyy"))
summaryTable[7,3] <- as.character(as.Date(range(datRF$RF_Date, na.rm = TRUE)[1], 
                                          format = "%d/%m/%yyyy"))
summaryTable[8,3] <- as.character(as.Date(range(datRF$RF_Date, na.rm = TRUE)[2], 
                                          format = "%d/%m/%yyyy"))

summaryMsg <- function(rows, datI, datRF, datMisI, datMisRF){ 
  
  if((range(datI$I_Date) == range(datRF$RF_Date))[1] == FALSE |
     (range(datI$I_Date) == range(datRF$RF_Date))[2] == FALSE){
    dlg_message("error - date ranges don't match", type = "ok")
  } else {
    print("no error - date ranges match")
  }
  if(unique(datRF$Year) != 
     unique(as.numeric(format(as.Date(datRF$RF_Date, 
                                      format = "%d/%m/%yyyy"), "%Y")))){
    dlg_message("error - year inconsistant with date in RuFEs", type = "ok")
  } else {
    print("no error - year matches daily date in RuFEs")
  }
  if(round(sum(datI$I_Catch), 2) == 
     round(sum(datRF$RF_Catch), 2)){
    print("no error - rounded total catches are equal")
  } else {
    dlg_message("error - totals differ", type = "ok")
  }
  if(length(unique(datI$I_Stock)) == 
     length(unique(datRF$RF_Stock))){
    print("no error - stock lists are the same length")
  } else {
    dlg_message("error - stock lists differ", type = "ok")
  }
  if(length(setdiff(unique(datI$I_Area2), unique(datRF$RF_Area))) > 0 | 
     length(setdiff(unique(datRF$RF_Area), unique(datI$I_Area2))) > 0){
    dlg_message("error - there are mismatches between Input and RuFEs catch areas", type = "ok")
  } else {
    print("no error - all catch areas match between RuFEs and CR100")
  }
  if(length(which(duplicated(rows, incomparables = "mismatch"))) > 0){
    dlg_message("error - rows in RuFEs data are associated with multiple Input rows", type = "ok")
  } else {
    print("no error - each row in RuFEs is associated with a single Input entry")}
  if(nrow(datRF) != nrow(datI)){
    dlg_message("error - Input and RuFEs data sets are not the same length", type = "ok")
  } else {
    print("no error - Input data and RuFEs data are the same length")
  }
  if((nrow(datMisI) > 0) | (nrow(datMisRF) > 0)){
    dlg_message("error - there are mismatches between enteries in Input and RuFEs", type = "ok")
  } else {
    print("no error - all enteries in RuFEs are also in Input data")
  }
  if(length(which(datI$I_Catch < 0)) > 0){ 
    dlg_message("error - there are negative catch enteries in Input data", type = "ok")
  } 
  if(length(which(datRF$RF_Catch < 0)) > 0){ 
    dlg_message("error - there are negative catch enteries in RuFEs")
  }
}

summaryMsg(rows, datI = datI, datRF, datMisI, datMisRF)
View(summaryTable)
# if there are mismatches but catch values appear the same upon inspecting
# datMisI and datMisRF, look at how the categories are converted from the input data to RuFEs

# write.xlsx(summaryTable, 
#            paste0(getwd(), "/Summaries/SummaryTableCatchAndPassage_", Sys.Date() ,".xlsx"), 
#            row.names = FALSE)
# 
