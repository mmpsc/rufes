#######################################################
# Code to check RuFEs data import from CR100 files 
# for catch above Mission
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
datCR <- readxl::read_excel(paste0(year, "_CR_5AboveMission_DataOut.xlsm"),
                            sheet = "DbData", skip = 4)

# Data exported from RuFEs database, everything from here will have prefix RF
datRF <- readxl::read_excel(paste0(year, "ruqDataSource_DailyCatchAbove.xlsx"),
                            sheet = "ruqDataSource_DailyCatch")

# Organize data ----
# from CR100 files
datCR %<>% 
  # remove entries with no catch
  tidyr::drop_na(StkCat) %>% 
  rownames_to_column("RowNum") %>% 
  # select and rename columns
  dplyr::select(c("RowNum", "Type", "User", "Gear", "Date", 
                  "Area", "Stock", "StkCat")) %>%
  dplyr::rename(c("CR_RowNum" = "RowNum", 
                  "CR_Type" = "Type",
                  "CR_User" = "User",
                  "CR_Gear" = "Gear",
                  "CR_Date" = "Date",
                  "CR_Area" = "Area",
                  "CR_Stock" = "Stock",
                  "CR_Catch" = "StkCat")) %>% 
  mutate(CR_Date2 = as.numeric(CR_Date))

# data downloaded from RuFEs
datRF %<>% 
  tidyr::drop_na(SumOfStockCatch) %>% 
  rownames_to_column("RowNum") %>% 
  dplyr::select(c("RowNum", "Year", "Fishery", "User", "Gear", 
                  "Date", "CatchArea", "Grouping", "SumOfStockCatch")) %>%
  dplyr::rename(c("RF_RowNum" = "RowNum", 
                  "RF_Type" = "Fishery",
                  "RF_User" = "User",
                  "RF_Gear" = "Gear",
                  "RF_Date" = "Date",
                  "RF_Area" = "CatchArea",
                  "RF_Stock" = "Grouping",
                  "RF_Catch" = "SumOfStockCatch")) %>% 
  mutate(RF_Date2 = as.numeric(RF_Date))

# Convert categories ----
# Rename CR Type, User, Gear, and Area to be consistent with RuFEs format
datCR %<>% 
  mutate(CR_Type = recode(CR_Type,
                          Rec = "REC",
                          tf = "TF",
                          Tf = "TF",
                          Demo = "DEMO",
                          Cm = "CM"), 
         CR_User = recode(CR_User, 
                          FNr = "Fraser-FSC", 
                          RecFR = "RECr"), 
         # to be consistent with catch below Mission checks (for category comparison function)
         CR_User2 = CR_User, 
         CR_Type2 = ifelse(CR_Type == "EO" & 
                             CR_User == "Fraser-FSC", 
                           CR_Type2 <- "CM",
                           CR_Type2 <- CR_Type), 
         CR_Area2 = ifelse(CR_Area == "Mis-Har", 
                           CR_Area2 <- "Mission to Harrison",
                           ifelse(CR_Area == "Har-Hop", 
                                  CR_Area2 <- "Harrison to Hope", 
                                  ifelse(CR_Area == "Hop-Qua", 
                                         CR_Area2 <- "Hope to Qualark",
                                         ifelse(CR_Area == "Qua-Saw", 
                                                CR_Area2 <- "Qualark to Sawmill",
                                                ifelse(CR_Area == "Saw-Lyt", 
                                                       CR_Area2 <- "Sawmill to Lytton",
                                                       ifelse(CR_Area == "Tex-Kel", 
                                                              CR_Area2 <- "Texas to Kelly",
                                                              ifelse(CR_Area == "Kel-Dea", 
                                                                     CR_Area2 <- "Kelly to Deadman",
                                                                     ifelse(CR_Area == "Lyt-Tex", 
                                                                            CR_Area2 <- "Lytton to Texas",
                                                                            ifelse(CR_Area == "Dea-Chi", 
                                                                                   CR_Area2 <- "Deadman to Chilcotin",
                                                                                   ifelse(CR_Area == "Chi-Nav", 
                                                                                          CR_Area2 <- "Chilcotin to Naver",
                                                                                          ifelse(CR_Area == "ChwkCult",
                                                                                                 CR_Area2 <- "Chilliwack and Cultus",
                                                                                                 ifelse(CR_Area == "Nav-IPi", 
                                                                                                        CR_Area2 <- "Naver to Isle Pierre", 
                                                                                                        CR_Area2 <- CR_Area)))))))))))))

# Row checks ----
# run loop to search for each row from the CR100 file in the RuFEs data
# make empty vector
rows <- c()

for(r in 1:nrow(datCR)) {
  RowNum <- which(
    # percent difference
    ((abs(datCR$CR_Catch[r] - datRF$RF_Catch)/
        ((datCR$CR_Catch[r] + datRF$RF_Catch)/2)*100) 
     # may need to change difference threshold if getting mismatches for no reason
     < threshold) &
      # other categories match
      datCR$CR_Date2[r] == datRF$RF_Date2 &
      datCR$CR_Stock[r] == datRF$RF_Stock &
      datCR$CR_Area2[r] == datRF$RF_Area &
      datCR$CR_Type2[r] == datRF$RF_Type & 
      datCR$CR_User[r] == datRF$RF_User &
      datCR$CR_Gear[r] == datRF$RF_Gear)
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
RFrow <- which(
  datCR$CR_Date[CRrow] == datRF$RF_Date &
    datCR$CR_Stock[CRrow] == datRF$RF_Stock &
    datCR$CR_Type2[CRrow] == datRF$RF_Type & 
    datCR$CR_User[CRrow] == datRF$RF_User &
    datCR$CR_Gear[CRrow] == datRF$RF_Gear)
# use this to update thresholds

# mismatched RuFEs data (in RuFEs but not in CR100)
datMisRF <- datRF[setdiff(datRF$RF_RowNum, rows), ]

# Summary ----
# make summary table
names <- c("total catch", "rows", "mismatched/unmatched rows", "# duplicated", 
           "# with negative catch",
           "# stocks", "# catch areas", "first date", "last date")
CRcol <- c(round(sum(datCR$CR_Catch), 2), nrow(datCR), nrow(datMisCR), 
           length(which(duplicated(rows, incomparables = "mismatch"))),
           length(which(datCR$CR_Catch < 0)), 
           length(unique(datCR$CR_Stock)),
           length(unique(datCR$CR_Area2)),
           range(datCR$CR_Date, na.rm = TRUE)[1], 
           range(datCR$CR_Date, na.rm = TRUE)[2])
RFcol <- c(round(sum(datRF$RF_Catch), 2), nrow(datRF), nrow(datMisRF), 
           length(which(duplicated(rows, incomparables = "mismatch"))),
           length(which(datRF$RF_Catch < 0)), 
           length(unique(datRF$RF_Stock)),
           length(unique(datRF$RF_Area)),
           range(datRF$RF_Date, na.rm = TRUE)[1], 
           range(datRF$RF_Date, na.rm = TRUE)[2])
furtherSteps <- c("", "", "see datMisCR and datMisRF", "look at category conversions", 
                  # "see CategoryComparison.R", 
                  "", "check stock lists", "check area lists", "", "")
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
summaryTable <- summaryTable %>% 
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

# write.xlsx(summaryTable, 
#            file = paste0(getwd(), "/Summaries/SummaryTableAbove_", Sys.Date() ,".xlsx"), 
#            row.names = FALSE)

