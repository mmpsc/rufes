#######################################################
# Code to check RuFEs data import from CR100 files 
# for catch below Mission
#
# Created: Serena Wong, February 2021
#
# OBJECT DEFINITIONS: 
#     * data with the datCR prefix are all from the CR100 excel files (data loaded into
#       RuFEs)
#     * data with the datRF prefix are all data downloaded from RuFEs 
#     * datMisCR gives the rows in the CR100 data that did not have corresponding 
#       matches in the data downloaded from RuFEs
#     * datMisRF gives the rows in the RuFEs data that did not have corresponding
#       matches in the input CR100 data
#######################################################

# Import data ----
# DbData sheet from CR100 DataOut files, everything from here will have 
# prefix CR
datCR <- readxl::read_excel(paste0(year, "_CR_2BelowMission_DataOut.xlsm"),
                            sheet = "DbData", skip = 4)

# Data exported from RuFEs database, everything from here will have prefix RF
datRF <- readxl::read_excel(paste0(year, "ruqDataSource_DailyCatchBelow.xlsx"),
                            sheet = "ruqDataSource_DailyCatch")

# Organize data ----
datCR %<>% 
  # remove entries with no catch
  tidyr::drop_na(StkCat) %>% 
  rownames_to_column("RowNum") %>% 
  dplyr::select(c("RowNum", "Type", "User", "Gear", "Area", "Fname", "Date", 
                  "Stock", "StkCat")) %>%
  dplyr::rename(c("CR_RowNum" = "RowNum", 
                  "CR_Type" = "Type",
                  "CR_User" = "User",
                  "CR_Gear" = "Gear",
                  "CR_Area" = "Area",
                  "CR_Locality" = "Fname",
                  "CR_Date" = "Date",
                  "CR_Stock" = "Stock",
                  "CR_Catch" = "StkCat")) %>% 
  # add in a fix for the locality column for June 1st
  mutate(CR_Locality2 = ifelse(CR_Date == 
                                 as.Date(paste(01, 06, year, sep = "-"), 
                                         "%d-%m-%Y") & 
                                 is.na(CR_Locality), 
                               CR_Locality2 <- 0,
                               CR_Locality2 <- CR_Locality), 
         CR_Date2 = as.numeric(CR_Date))

datRF %<>% 
  tidyr::drop_na(c(Grouping, SumOfStockCatch)) %>%
  rownames_to_column("RowNum") %>% 
  dplyr::select(c("RowNum", "Year", "Fishery", "User", 
                  "Gear", "CatchArea", "Locality", "Date", "Grouping", "SumOfStockCatch")) %>%
  dplyr::rename(c("RF_RowNum" = "RowNum", 
                  "RF_Type" = "Fishery",
                  "RF_User" = "User",
                  "RF_Gear" = "Gear",
                  "RF_Area" = "CatchArea",
                  "RF_Locality" = "Locality",
                  "RF_Date" = "Date",
                  "RF_Stock" = "Grouping",
                  "RF_Catch" = "SumOfStockCatch")) %>% 
  # add in a fix for the locality column for June 1st
  mutate(RF_Locality2 = ifelse(RF_Date == 
                                 as.Date(paste(01, 06, year, sep = "-"), 
                                         "%d-%m-%Y") & 
                                 is.na(RF_Locality), 
                               RF_Locality2 <- 0,
                               RF_Locality2 <- RF_Locality), 
         RF_Date2 = as.numeric(RF_Date))

# Convert categories ----
# Rename CR Type, User, and Gear to be consistent with RuFEs format
datCR %<>% 
  mutate(CR_Gear = recode(CR_Gear, 
                          Vmgn = "VMGN",
                          Gn = "GN",
                          gn = "GN",
                          Ps = "PS",
                          Rn = "RN",
                          Tr = "TR"), 
         CR_Type = recode(CR_Type,
                          Rec = "REC",
                          tf = "TF",
                          Tf = "TF",
                          Cm = "CM"), 
         CR_User = recode(CR_User, 
                          AK = "Ak"), 
         CR_Area2 = recode(CR_Area, 
                           a1 = "Area 1",
                           a4b5 = "US Area 4b/5", 
                           a4b56c = "US Area 4b/5/6c", 
                           "a6-7" = "US Area 6/7", 
                           "a6-7a" = "US Area 6/7/7A",
                           a7 = "US Area 7", 
                           a7a = "US Area 7A", 
                           a11 = "Area 11",
                           "a11-12" = "Area 11/12",
                           "a11-16" = "Area 11/12/13/14/15/16",
                           a12 = "Area 12", 
                           a12ri = "Area 12",
                           a12nc = "Area 12", 
                           "a12-13" = "Area 12/13",
                           a13 = "Area 13",
                           "a13-16" = "Area 13/14/15/16", 
                           a15 = "Area 13/14/15/16", 
                           a16 = "Area 16", 
                           "a17-29a" = "Area 17/29",
                           a20 = "Area 20",
                           a21 = "Area 19 - 127",
                           a29a = "Area 29a", 
                           a29b = "Area 29b", 
                           a29d = "Area 29d",
                           "a121-124" = "Area 121/123/124P",
                           a124 = "Area 124", 
                           "a125-127" = "Area 125/126/127"), 
         CR_Type2 = ifelse(CR_Type == "CS" & 
                             CR_User == "TI",
                           CR_Type2 <- "CER",
                           ifelse(CR_Type == "EO" & 
                                    CR_User == "FNr", 
                                  CR_Type2 <- "CM",
                                  ifelse(CR_Type == "EO" & 
                                           CR_User == "FNm", 
                                         CR_Type2 <- "CM",
                                         CR_Type2 <- CR_Type))), 
         CR_User2 =  ifelse(CR_Type == "CS" & 
                              CR_User == "TI",
                            CR_User2 <- "TI-US",
                            ifelse(CR_Type == "EO" & 
                                     CR_User == "FNr", 
                                   CR_User2 <- "Fraser-FSC",
                                   ifelse(CR_Type == "FSC" & 
                                            CR_User == "FNr", 
                                          CR_User2 <- "Fraser-FSC",
                                          ifelse(CR_Type == "CM" & 
                                                   CR_User == "TI", 
                                                 CR_User2 <- "TI-US",
                                                 ifelse(CR_User == "AC", 
                                                        CR_User2 <- "AC-US",
                                                        CR_User2 <- CR_User))))))

# Row checks ----
# run loop to search for each row from the CR100 file in the RuFEs data
# make empty vector
rows <- c()

for(r in 1:nrow(datCR)) {
  RowNum <- which(
    # percent difference
    ((abs(datCR$CR_Catch[r] - datRF$RF_Catch)/
        ((datCR$CR_Catch[r] + datRF$RF_Catch)/2)*100) 
     # may need to change difference threshold if getting mismatches for 
     # no reason
     < threshold) &
      # other categories match
      datCR$CR_Date2[r] == datRF$RF_Date2 &
      datCR$CR_Stock[r] == datRF$RF_Stock &
      datCR$CR_Type2[r] == datRF$RF_Type & 
      datCR$CR_User2[r] == datRF$RF_User &
      datCR$CR_Gear[r] == datRF$RF_Gear &
      datCR$CR_Area2[r] == datRF$RF_Area &
      datCR$CR_Locality2[r] == datRF$RF_Locality2)
  
  if(is.integer(RowNum) & length(RowNum)){
    rows[r] <- RowNum
  } else {rows[r] <- "mismatch"
  }
}

# Get mismatches ----
datMisCR <- datCR[which(rows == "mismatch"),]
# note that this is just the mismatched CR100 data

# SELECT an unmatched value in the CR100 file (here just using the first value)
CRrow <- as.numeric(which(rows == "mismatch")[1])
# search for unmatched/mismatched CR100 values in RuFEs data
# use this to revise threshold:
RFrow <- which(
  datCR$CR_Date[CRrow] == datRF$RF_Date &
    datCR$CR_Stock[CRrow] == datRF$RF_Stock &
    datCR$CR_Type2[CRrow] == datRF$RF_Type & 
    datCR$CR_User2[CRrow] == datRF$RF_User &
    datCR$CR_Gear[CRrow] == datRF$RF_Gear &
    datCR$CR_Locality[CRrow] == datRF$RF_Locality)

# Revise threshold ----
# if there is a number in RFrow, then all other categories match and it may be a
# threshold issue. Can use mismatch data to revise threshold
# if(length(RFrow) != 0){((abs(datCR$CR_Catch[CRrow] - datRF$RF_Catch[RFrow]))/
#    ((datCR$CR_Catch[CRrow] + datRF$RF_Catch[RFrow])/2)*100) < threshold 
# can revise threshold if FALSE
#  }

# mismatched RuFEs data
datMisRF <- datRF[setdiff(datRF$RF_RowNum, rows), ]

# Summary ----
# make summary table
names <- c("total catch", "rows", "mismatched/unmatched rows", "# duplicated", 
           "# with negative catch",
           "# stocks", "# localities", "first date", "last date")
CRcol <- c(round(sum(datCR$CR_Catch), 2), nrow(datCR), nrow(datMisCR), 
           length(which(duplicated(rows, incomparables = "mismatch"))),
           length(which(datCR$CR_Catch < 0)), 
           length(unique(datCR$CR_Stock)), 
           # did not add in length(unique) for CR_Area since some are combined in RuFEs
           length(unique(datCR$CR_Locality)),
           range(datCR$CR_Date, na.rm = TRUE)[1], 
           range(datCR$CR_Date, na.rm = TRUE)[2])
RFcol <- c(round(sum(datRF$RF_Catch), 2), nrow(datRF), nrow(datMisRF), 
           length(which(duplicated(rows, incomparables = "mismatch"))),
           length(which(datRF$RF_Catch < 0)), 
           length(unique(datRF$RF_Stock)), length(unique(datRF$RF_Locality)),
           range(datRF$RF_Date, na.rm = TRUE)[1], 
           range(datRF$RF_Date, na.rm = TRUE)[2])
furtherSteps <- c("", "", "see datMisCR and datMisRF", "look at category conversions", 
                  # "see CategoryComparison.R",   
                "", "", "", "", "")
summaryTable <- as.data.frame(cbind(names, as.numeric(CRcol), 
                                    as.numeric(RFcol), furtherSteps))
summaryTable$Errors1 <- with(summaryTable, ifelse(V2 != V3,
                                                   Errors1 <- "Error", 
                                                   Errors1 <- "No error"))
summaryTable$Errors2 <- with(summaryTable, ifelse(V2 != 0 | V3 != 0,
                                                  Errors2 <- "Error", 
                                                  Errors2 <- "No error"))
Errors <- c(summaryTable$Errors1[c(1, 2)], summaryTable$Errors2[c(3:5)],
            summaryTable$Errors1[c(6:9)])
summaryTable <- cbind(summaryTable, Errors)
summaryTable %<>% 
  dplyr::select(c("names", "V2", "V3", "Errors", "furtherSteps")) %>% 
  dplyr::rename(c(" " = "names",
           "CR100 data" = "V2", 
           "RuFEs data" = "V3", 
           "If error:" = "furtherSteps"))
summaryTable[8,2] <- as.character(as.Date(range(datCR$CR_Date, na.rm = TRUE)[1], 
                                          format = "%d/%m/%yyyy"))
summaryTable[9,2] <- as.character(as.Date(range(datCR$CR_Date, na.rm = TRUE)[2], 
                                          format = "%d/%m/%yyyy"))
summaryTable[8,3] <- as.character(as.Date(range(datRF$RF_Date, na.rm = TRUE)[1], 
                                          format = "%d/%m/%yyyy"))
summaryTable[9,3] <- as.character(as.Date(range(datRF$RF_Date, na.rm = TRUE)[2], 
                                          format = "%d/%m/%yyyy"))

summaryMsg <- function(rows, datCR, datRF, datMisCR, datMisRF){ 
  
  if((range(datCR$CR_Date) == range(datRF$RF_Date))[1] == FALSE |
     (range(datCR$CR_Date) == range(datRF$RF_Date))[2] == FALSE){
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
  if(round(sum(datCR$CR_Catch), 2) == 
     round(sum(datRF$RF_Catch), 2)){
    print("no error - rounded total catches are equal")
  } else {
    dlg_message("error - totals differ", type = "ok")
  }
  if(length(unique(datCR$CR_Stock)) == 
     length(unique(datRF$RF_Stock))){
    print("no error - stock lists are the same length")
  } else {
    dlg_message("error - stock lists differ", type = "ok")
  }
  if(length(setdiff(unique(datCR$CR_Area2), unique(datRF$RF_Area))) > 0 | 
     length(setdiff(unique(datRF$RF_Area), unique(datCR$CR_Area2))) > 0){
    dlg_message("error - there are mismatches between CR100 and RuFEs catch areas", type = "ok")
  } else {
    print("no error - all catch areas match between RuFEs and CR100")
  }
  if(length(which(duplicated(rows, incomparables = "mismatch"))) > 0){
    dlg_message("error - rows in RuFEs data are associated with multiple CR100 rows", type = "ok")
  } else {
    print("no error - each row in RuFEs is associated with a single CR100 entry")}
  if(nrow(datRF) != nrow(datCR)){
    dlg_message("error - CR100 and RuFEs data sets are not the same length", type = "ok")
  } else {
    print("no error - CR100 and RuFEs are the same length")
  }
  if((nrow(datMisCR) > 0) | (nrow(datMisRF) > 0)){
    dlg_message("error - there are mismatches between enteries in CR100 and RuFEs", type = "ok")
  } else {
    print("no error - all enteries in RuFEs are also in CR100")
  }
  if(length(which(datCR$CR_Catch < 0)) > 0){ 
    dlg_message("error - there are negative catch enteries in CR100", type = "ok")
  } 
  if(length(which(datRF$RF_Catch < 0)) > 0){ 
    dlg_message("error - there are negative catch enteries in RuFEs")
  }
}

summaryMsg(rows, datCR, datRF, datMisCR, datMisRF)
View(summaryTable)
# if there are mismatches but catch values appear the same upon inspecting
# datMisCR and datMisRF then use CategoryComparison.R code

# write.xlsx(summaryTable, 
#            file = paste0(getwd(), "/Summaries/SummaryTableBelow_", Sys.Date() ,".xlsx"), 
#            row.names = FALSE)

