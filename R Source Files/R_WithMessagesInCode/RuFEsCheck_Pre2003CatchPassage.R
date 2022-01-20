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

# Clear workspace
rm(list = ls())

# Install packages
if(!require(pacman)) install.packages("pacman")
library(pacman)

p_load(here, tidyverse, magrittr, readxl, xlsx)


# SPECIFY YEAR
year <- 2002

# Set working directory
# "C:/Users/wong/OneDrive - Pacific Salmon Commission/Documents/RuFEs/QAQC/"
setwd(paste0(here(), "/", year))
dir.create("Summaries", showWarnings = FALSE)

# Import data ----
# Pre-2003 data input file
datInput <- readxl::read_excel(paste0("qryRuFEs", year, "_Daily_Catch+Passage.xlsx"),
                            sheet = "qryRuFEs_Daily_Catch_Passage")

# Data exported from RuFEs database, everything from here will have prefix RF
datRF_B <- readxl::read_excel(paste0(year, "ruqDataSource_DailyCatchBelow.xlsx"),
                            sheet = "ruqDataSource_DailyCatch")

datRF_A <- readxl::read_excel(paste0(year, "ruqDataSource_DailyCatchAbove.xlsx"),
                              sheet = "ruqDataSource_DailyCatch")

datRF_P <- readxl::read_excel(paste0(year, "ruqDataSource_DailyPassage.xlsx"),
                              sheet = "ruqDataSource_DailyPassage")

# Common data 
cmtStocks <- readxl::read_excel(paste0(here(), "/cmtStocks.xlsx"), sheet = "Sheet1")
# management groups
cmtMG <- readxl::read_excel(paste0(here(),"/cmtManagementGroups.xlsx"), sheet = "Sheet1")
# management group details
cmtMGD <- readxl::read_excel(paste0(here(), "/cmtManagementGroupsDetails.xlsx"), sheet = "Sheet1")

# Organize Common data ----
cmtStocks %<>% 
  dplyr::rename("cmtStocksID" = "cmtStocksId")
cmt <- left_join(cmtMG, cmtMGD, by = c("cmtManagementGroupsID", "Source"))
cmt <- full_join(cmt, cmtStocks, by = "cmtStocksID")
cmtSub <- cmt %>% 
  rename(Source = Source.x) %>% 
  dplyr::select(c("Source", "cmtManagementGroupsID", 
                  "cmtStocksID", "GroupName", 
                  "GroupAbbr", "Stream", "DFOCode")) %>% 
  arrange(DFOCode)

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
  mutate(DNAgrpNonFra = ifelse(is.na(DNAgrp) & Stream == "NonFraser", 
                               DNAgrpNonFra <- "NonFraser", 
                               DNAgrpNonFra <- DNAgrp)) %>% 
  tidyr::drop_na(I_Catch) %>% 
  group_by(DNAgrpNonFra, I_Cat, I_abMission, I_Type, I_User, I_Gear, 
           I_Area, I_Date) %>% 
  summarize(Catch = sum(I_Catch)) %>% 
  rename("I_Stock" = "DNAgrpNonFra",
         "I_Catch" = "Catch")

# Organize RuFEs data ----
datRF_B %<>% 
  tidyr::drop_na(c(Grouping, SumOfStockCatch)) %>%
  dplyr::select(c(Year, Fishery, User, Gear, CatchArea, Date, 
                  Grouping, SumOfStockCatch)) %>%
  dplyr::rename(c("RF_Type" = "Fishery",
                  "RF_User" = "User",
                  "RF_Gear" = "Gear",
                  "RF_Area" = "CatchArea",
                  "RF_Date" = "Date",
                  "RF_Stock" = "Grouping",
                  "RF_Catch" = "SumOfStockCatch")) %>% 
  # is data from above or below (a/b) Mission?
  mutate(RF_abMission = "Below", RF_Cat = "Catch")

datRF_A %<>% 
  tidyr::drop_na(c(Grouping, SumOfStockCatch)) %>%
  dplyr::select(c(Year, Fishery, User, Gear, CatchArea, Date, 
                  Grouping, SumOfStockCatch)) %>%
  dplyr::rename(c("RF_Type" = "Fishery",
                  "RF_User" = "User",
                  "RF_Gear" = "Gear",
                  "RF_Area" = "CatchArea",
                  "RF_Date" = "Date",
                  "RF_Stock" = "Grouping",
                  "RF_Catch" = "SumOfStockCatch")) %>%
  # is data from above or below (a/b) Mission?
  mutate(RF_abMission = "Above", RF_Cat = "Catch")

datRF_P %<>% 
  tidyr::drop_na(c(GroupName, SumOfStockCatch)) %>%
  dplyr::select(c(Year, Date, GroupName, SumOfStockCatch)) %>%
  dplyr::rename(c("RF_Date" = "Date",
                  "RF_Stock" = "GroupName",
                  "RF_Catch" = "SumOfStockCatch")) %>% 
  # is data from above or below (a/b) Mission? In this case it is passage so from Mission
  mutate(RF_abMission = "Mission", RF_Cat = "Mission", 
         RF_User = "PSC", RF_Gear = "HA", 
         RF_Area = "Area 29d", 
         RF_Type = ifelse(RF_Stock == "Pitt_River", 
                          RF_Type <- "EscPitt", 
                          RF_Type <- "Esc"))

# Convert categories ----
# Rename I Type, User, and Gear to be consistent with RuFEs format
datI %<>% 
  mutate(I_Type = recode(I_Type,
                         Rec = "REC",
                         # tf = "TF",
                         Tf = "TF",
                         Cm = "CM", 
                         Cer = "CER")) %>% 
  mutate(I_User = recode(I_User, 
                         # AK = "Ak", 
                         TI = "TI-US", 
                         NI = "AC-US", 
                         FR = "Fraser", 
                         Mrne = "RECm"), 
         I_Area2 = recode(I_Area, 
                          a4b = "Area 4",
                          a4b5 = "US Area 4b/5", 
                          a4b56c = "US Area 4b/5/6c", 
                          "a6-7" = "US Area 6/7",
                          a7 = "US Area 7", 
                          a7a = "US Area 7A", 
                          a11 = "Area 11",
                          "a11-12" = "Area 11/12",
                          "a11-13" = "Area 11/12/13",
                          "a11-16" = "Area 11/12/13/14/15/16",
                          a12 = "Area 12", 
                          a12ri = "Area 12",
                          a12nc = "Area 12", 
                          a13 = "Area 13",
                          "a13-16" = "Area 13/14/15/16",
                          "a14-29" = "Area 14 - 29",
                          # a15 = "Area 13/14/15/16", 
                          a16 = "Area 16", 
                          "a17-29" = "Area 17/29", 
                          "a17-29a" = "Area 17/29",
                          a20 = "Area 20",
                          # a21 = "Area 19 - 127",
                          a29a = "Area 29a", 
                          a29b = "Area 29b", 
                          a29d = "Area 29d",
                          # a104 = 
                          "a121-124" = "Area 121/123/124P",
                          a124 = "Area 124", 
                          "a125-127" = "Area 125/126/127",
                          ms = "Mission to Sawmill",
                          as = "Above Sawmill", 
                          a2W = "Area 2West"), 
         I_Type2 = # ifelse(I_Type == "CS" & I_User == "TI",
           #  I_Type2 <- "CER",
           ifelse(I_Type == "EO" & 
                    I_User == "FNr", 
                  I_Type2 <- "CM",
                  #  ifelse(I_Type == "EO" & I_User == "FNm", 
                  #        I_Type2 <- "CM",
                  I_Type2 <- I_Type), # ))
         I_User2 =  # ifelse(I_Type == "CS" & I_User == "TI",
           #      I_User2 <- "TI-US",
           ifelse(I_Type == "EO" & 
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


# bind RuFEs above/below Mission catch and Mission passage data together
datRF <- rbind(datRF_A, datRF_P, datRF_B)

datI$I_Date2 <- as.numeric(datI$I_Date)
datRF$RF_Date2 <- as.numeric(datRF$RF_Date)

# get just Fraser River stocks since RuFEs doesn't have non-Fraser
datIfra <- datI %>% 
  filter(I_Stock != "NonFraser")
         
# Overview checks ----

CategoryComparisonChecks <- function(datI, datRF){ 
  if((range(datI$I_Date) == range(datRF$RF_Date))[1] == FALSE |
     (range(datI$I_Date) == range(datRF$RF_Date))[2] == FALSE){
    print("DATE RANGES DON'T MATCH")
  } 
  if(unique(datRF$Year) != 
     unique(as.numeric(format(as.Date(datRF$RF_Date, 
                                      format = "%d/%m/%yyyy"), "%Y")))){
    print("DATES (YEARS) DON'T MATCH")
  } 
  if(identical(setdiff(unique(datI$I_Area2), unique(datRF$RF_Area)), 
               character(0)) & 
     identical(setdiff(unique(datRF$RF_Area), unique(datI$I_Area2)), 
               character(0))){print("no error - area categories are the same")
  } else {
    print("error - area categories differ")
  }
  if(identical(setdiff(unique(datI$I_Type2), unique(datRF$RF_Type)), 
               character(0)) & 
     identical(setdiff(unique(datRF$RF_Type), unique(datI$I_Type2)), 
               character(0))){print("no error - fishery type categories are the same")
  } else {
    print("error - fishery type categories differ")
  }
  if(identical(setdiff(unique(datI$I_User2), unique(datRF$RF_User)), 
               character(0)) & 
     identical(setdiff(unique(datRF$RF_User), unique(datI$I_User2)), 
               character(0))){print("no error - fishery user categories are the same")
  } else {
    print("error - fishery user categories differ")
  }
  if(identical(setdiff(unique(datI$I_Gear), unique(datRF$RF_Gear)), 
               character(0)) & 
     identical(setdiff(unique(datRF$RF_Gear), unique(datI$I_Gear)), 
               character(0))){print("no error - fishery gear categories are the same")
  } else {
    print("error - fishery gear categories differ")
  }
}


OverviewAndStockChecks <- function(datIfra, datRF){ 
  if(round(sum(datIfra$I_Catch), 2) == round(sum(datRF$RF_Catch), 2)){
    print("no error - total catch/passage values match")
  } else {
    print("error - total catch/passage values don't match")
  }
  if(nrow(datIfra) == nrow(datRF)){
    print("no error - datasets are the same length")
  } else {
    print("error - datasets have different number of rows")
  }
  if(identical(setdiff(unique(datIfra$I_Stock), unique(datRF$RF_Stock)), 
               character(0)) & 
     identical(setdiff(unique(datRF$RF_Stock), unique(datIfra$I_Stock)), 
               character(0))){print("no error - stock lists are the same")
  } else {
    print("error - stock lists differ")
  }
}

# CHECK THESE OUTPUTS FOR ERRORS
CategoryComparisonChecks(datI, datRF)
OverviewAndStockChecks(datIfra, datRF)

# Row matching (checks of catch/passage values) ----

datIfra %<>% 
  rownames_to_column("I_RowNum")
  
datRF %<>% 
  rownames_to_column("RF_RowNum")

# run loop to search for each row from the input file in the RuFEs data
# make empty vector
rows <- c()

# SET THRESHOLD FOR PERCENT DIFFERENCE
threshold <- 1e-4

for(r in 1:nrow(datIfra)) {
  RowNum <- which(
    # percent difference
    ((abs(datIfra$I_Catch[r] - datRF$RF_Catch)/
        ((datIfra$I_Catch[r] + datRF$RF_Catch)/2)*100) 
     # may need to change difference threshold if getting mismatches for 
     # no reason
     < threshold) &
      # other categories match
      datIfra$I_Cat[r] == datRF$RF_Cat &
      datIfra$I_abMission[r] == datRF$RF_abMission &
      datIfra$I_Date2[r] == datRF$RF_Date2 &
      datIfra$I_Stock[r] == datRF$RF_Stock &
      datIfra$I_Type2[r] == datRF$RF_Type & 
      datIfra$I_User2[r] == datRF$RF_User &
      datIfra$I_Gear[r] == datRF$RF_Gear &
      datIfra$I_Area2[r] == datRF$RF_Area)
  
  if(is.integer(RowNum) & length(RowNum)){
    rows[r] <- RowNum
  } else {rows[r] <- "mismatch"
  }
}

# look for mismatches
which(rows == "mismatch") # should be 0, if not then it gives the rows in the 
# Input dataframe that don't have a corresponding value in RuFEs
length(rows) == length(unique(rows)) # should be TRUE
which(duplicated(rows, incomparables = "mismatch")) # should be 0 
# note that duplicated doesn't show the first of the duplicated values

# Check mismatches ----
datMisI <- datI[which(rows == "mismatch"),]
# note that this is just the mismatched input data

# SELECT an unmatched value in the input file (here just using the first value)
Irow <- as.numeric(which(rows == "mismatch")[1])
# search for unmatched/mismatched input values in RuFEs data
RFrow <- which(
  datIfra$I_Cat[Irow] == datRF$RF_Cat &
    datIfra$I_abMission[Irow] == datRF$RF_abMission &
    datI$I_Date[Irow] == datRF$RF_Date &
    datI$I_Stock[Irow] == datRF$RF_Stock &
    datI$I_Type2[Irow] == datRF$RF_Type & 
    datI$I_User2[Irow] == datRF$RF_User &
    datI$I_Gear[Irow] == datRF$RF_Gear)

# mismatched RuFEs data
datMisRF <- datRF[setdiff(datRF$RF_RowNum, rows), ]

# Revise threshold ----
# if there is a number in RFrow, then all other categories match and it may be a
# threshold issue. Can use mismatch data to revise threshold
if(length(RFrow) != 0){((abs(datI$I_Catch[Irow] - datRF$RF_Catch[RFrow]))/
                          ((datI$I_Catch[Irow] + datRF$RF_Catch[RFrow])/2)*100) < threshold 
  # can revise threshold if FALSE
}

# Check duplicates ----
if(length(which(duplicated(rows, incomparables = "mismatch"))) > 0) { 
  print("multiple rows are matching to the same entry, investigate using the CategoryComparison.R code")}

# Summary ----
# make summary table
names <- c("total catch", "rows", "mismatched/unmatched rows", "# duplicated", 
           "# with negative catch/passage",
           "# stocks", "first date", "last date")
Icol <- c(round(sum(datI$I_Catch), 2), 
          nrow(datI), "", "", 
          length(which(datI$I_Catch < 0)), 
          length(unique(datI$I_Stock)), 
          # did not add in length(unique) for I_Area since some are combined in RuFEs
          range(datI$I_Date, na.rm = TRUE)[1], 
          range(datI$I_Date, na.rm = TRUE)[2])
IfraCol <- c(round(sum(datIfra$I_Catch), 2), 
             nrow(datIfra), 
             nrow(datMisI), 
             length(which(duplicated(rows, incomparables = "mismatch"))),
             length(which(datIfra$I_Catch < 0)), 
             length(unique(datIfra$I_Stock)), 
             # did not add in length(unique) for I_Area since some are combined in RuFEs
             range(datIfra$I_Date, na.rm = TRUE)[1], 
             range(datIfra$I_Date, na.rm = TRUE)[2])
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
summaryTable <- as.data.frame(cbind(names, as.numeric(Icol), as.numeric(IfraCol),
                                    as.numeric(RFcol), furtherSteps))
summaryTable$Errors1 <- with(summaryTable, ifelse(V3 != V4,
                                                  Errors1 <- "Error", 
                                                  Errors1 <- "No error"))
summaryTable$Errors2 <- with(summaryTable, ifelse(V3 != 0 | V4 != 0,
                                                  Errors2 <- "Error", 
                                                  Errors2 <- "No error"))
Errors <- c(summaryTable$Errors1[c(1, 2)], summaryTable$Errors2[c(3:5)],
            summaryTable$Errors1[c(6:8)])
summaryTable <- cbind(summaryTable, Errors)
summaryTable %<>% 
  dplyr::select(c("names", "V2", "V3", "V4", "Errors", "furtherSteps")) %>% 
  dplyr::rename(c(" " = "names",
                  "Input data (w/ NonFraser)" = "V2",
                  "Input data (w/o NonFraser)" = "V3", 
                  "RuFEs data" = "V4", 
                  "If error:" = "furtherSteps"))
summaryTable[7,2] <- as.character(as.Date(range(datI$I_Date, na.rm = TRUE)[1], 
                                          format = "%d/%m/%yyyy"))
summaryTable[8,2] <- as.character(as.Date(range(datI$I_Date, na.rm = TRUE)[2], 
                                          format = "%d/%m/%yyyy"))
summaryTable[7,3] <- as.character(as.Date(range(datIfra$I_Date, na.rm = TRUE)[1], 
                                          format = "%d/%m/%yyyy"))
summaryTable[8,3] <- as.character(as.Date(range(datIfra$I_Date, na.rm = TRUE)[2], 
                                          format = "%d/%m/%yyyy"))
summaryTable[7,4] <- as.character(as.Date(range(datRF$RF_Date, na.rm = TRUE)[1], 
                                          format = "%d/%m/%yyyy"))
summaryTable[8,4] <- as.character(as.Date(range(datRF$RF_Date, na.rm = TRUE)[2], 
                                          format = "%d/%m/%yyyy"))

summaryMsg <- function(rows, datIfra, datRF, datMisI, datMisRF){ 
  if(length(unique(datIfra$I_Stock)) == 
     length(unique(datRF$RF_Stock))){print("no error - stock lists are the same length")
  } else {
    print("error - stock lists differ")
  }
  if(length(which(duplicated(rows, incomparables = "mismatch"))) > 0){
    print("error - rows in RuFEs data are associated with multiple input data rows, investigate using the CategoryComparison.R code")
  } else {print("no error - each row in RuFEs is associated with a single input data entry")}
  if(nrow(datRF) != nrow(datIfra)){print("error - input and RuFEs data sets are not the same length")
  } else {
    print("no error - input data and RuFEs are the same length")
  }
  if((nrow(datMisI) > 0) | (nrow(datMisRF) > 0)){
    print("error - there are mismatches between enteries in input data and RuFEs")
  } else {
    print("no error - all enteries in RuFEs are also in input data")
  }
  if(length(which(datIfra$I_Catch < 0)) > 0){ 
    print("error - there are negative catch enteries in input data")
  } 
  if(length(which(datRF$RF_Catch < 0)) > 0){ 
    print("error - there are negative catch enteries in RuFEs")
  }
}

summaryMsg(rows, datIfra, datRF, datMisI, datMisRF)
View(summaryTable)
# if there are mismatches but catch values appear the same upon inspecting
# datMisI and datMisRF then use CategoryComparison.R code

write.xlsx(summaryTable, 
           paste0(getwd(), "/Summaries/SummaryTableCatchAndPassage_", Sys.Date() ,".xlsx"), 
           row.names = FALSE)

