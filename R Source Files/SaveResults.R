
# Load or create summary workbook -----
# won't run with xlsx library loaded
wb <- loadWorkbook(paste0(here(), "/", year, "/RuFEsQAQC_Summary_", 
                          year, ".xlsx"), create = TRUE)
# set so that it preserves the formatting in the workbook
setStyleAction(wb, XLC$"STYLE_ACTION.NONE")

# Create sheets within the workbook (if they don't already exist)
if(year < 2003) {
  for(sheet in c("CheckingDates", "CatchPassage_Summary", 
                  "Esc_Summary", "Esc_ErrorSummary", "EscPropByDNAGrp", "Esc_Errors",
                  "DBE_MA-RSA","DBE_ByStream", "DBE_Errors", "DBE_vsSEF", 
                  "CatchRollup_Summary", "CatchRollup_Totals", "CatchRollup_Mismatches", "CatchRollup_Errors",
                  "PsgRollup_Summary", "PsgRollup_Totals", "PsgRollup_Mismatches", "PsgRollup_Errors")){
    createSheet(wb, name = sheet)
  }
} else {
  for(sheet in c("CheckingDates",
                 "CatchA_Summary", "CatchB_Summary", "Psg_Summary",
                 "Esc_Summary", "Esc_ErrorSummary", "EscPropByDNAGrp", "Esc_Errors",
                 "DBE_MA-RSA","DBE_ByStream", "DBE_Errors", "DBE_vsSEF", 
                 "CatchRollup_Summary", "CatchRollup_Totals", "CatchRollup_Mismatches", "CatchRollup_Errors",
                 "PsgRollup_Summary", "PsgRollup_Totals", "PsgRollup_Mismatches", "PsgRollup_Errors")){
    createSheet(wb, name = sheet)
  }
}

# Update the dates that checks were performed -----
datDateLog <- readWorksheet(wb, "CheckingDates")
tmpNames <- data.frame(
  Col1 = c("Catch below Mission", "Mission passage", "Catch above Mission", 
           "Escapement", "DBEs", "Catch rollups", "Passage rollups"),
  b = c('CatchBelow', 'Passage', 'CatchAbove', 'Escapement', 
        'DBEs', 'CatchRollups', 'PassageRollups'))

if(length(datDateLog) > 0 & year >= 2003){
  datDateLog[ , 1] <- tmpNames$Col1
  datDateLog$`Date.Last.Checked`[which(tmpNames$b == check)] <- 
    as.character(as.Date(Sys.Date(), "%Y-%m-%d"))
  
} else if(length(datDateLog) > 0 & year < 2003 & 
          (check == "CatchBelow" | check == "CatchAbove" | check == "Passage")){
  
  datDateLog[ , 1] <- tmpNames$Col1
  datDateLog$`Date.Last.Checked`[which(tmpNames$b %in% 
                                         c("CatchBelow", "CatchAbove", "Passage"))] <- 
    as.character(as.Date(Sys.Date(), "%Y-%m-%d"))
  
} else if(length(datDateLog) == 0 & year < 2003 & 
          (check == "CatchBelow" | check == "CatchAbove" | check == "Passage")){
  
  datDateLog <- data.frame(
    Col1 = tmpNames$Col1,
    Date.Last.Checked = c(rep(NA, 7))
  )
  datDateLog$`Date.Last.Checked`[which(tmpNames$b %in% 
                                         c("CatchBelow", "CatchAbove", "Passage"))] <- 
    as.character(as.Date(Sys.Date(), "%Y-%m-%d"))
  
} else if(length(datDateLog) > 0 & year < 2003 & 
          (check != "CatchBelow" & check != "CatchAbove" & check != "Passage")){
  
  datDateLog[ , 1] <- tmpNames$Col1
  datDateLog$`Date.Last.Checked`[which(tmpNames$b == check)] <- 
    as.character(as.Date(Sys.Date(), "%Y-%m-%d"))
  
} else {
  
  datDateLog <- data.frame(
    Col1 = tmpNames$Col1,
    Date.Last.Checked = c(rep(NA, 7))
  )
  datDateLog$`Date.Last.Checked`[which(tmpNames$b == check)] <- 
    as.character(as.Date(Sys.Date(), "%Y-%m-%d"))
  
} 

colnames(datDateLog) <- c("", "Date Last Checked")
writeWorksheet(wb, datDateLog, sheet = "CheckingDates", 
               startRow = 1, startCol = 1, header = TRUE)


if(year < 2003 & 
    (check == "CatchBelow" | check == "CatchAbove" | check == "Passage")){
  writeWorksheet(wb, summaryTable, sheet = "CatchPassage_Summary", 
                 startRow = 1, startCol = 1, header = TRUE)
  
} else if(check == "CatchAbove"){
  writeWorksheet(wb, summaryTable, sheet = "CatchA_Summary", 
                 startRow = 1, startCol = 1, header = TRUE)
  
} else if (check == "CatchBelow"){
  writeWorksheet(wb, summaryTable, sheet = "CatchB_Summary", 
                 startRow = 1, startCol = 1, header = TRUE)
  
} else if (check == "Passage"){
  writeWorksheet(wb, summaryTable, sheet = "Psg_Summary", 
                 startRow = 1, startCol = 1, header = TRUE)
  
} else if (check == "Escapement"){
  writeWorksheet(wb, summaryTable, sheet = "Esc_Summary", 
                 startRow = 1, startCol = 1, header = TRUE)
  writeWorksheet(wb, errorTable, sheet = "Esc_ErrorSummary", 
                 startRow = 1, startCol = 1, header = TRUE)
  writeWorksheet(wb, datPropEscCatch, sheet = "EscPropByDNAGrp", 
                 startRow = 1, startCol = 1, header = TRUE)
  # if(nrow(datCheckError) >= 1){
  writeWorksheet(wb, datCheckError, sheet = "Esc_Errors", 
                 startRow = 1, startCol = 1, header = TRUE)
  
} else if (check == "DBEs"){
  writeWorksheet(wb, datGrpd, sheet = "DBE_MA-RSA", 
                 startRow = 1, startCol = 1, header = TRUE)
  writeWorksheet(wb, datLong, sheet = "DBE_ByStream", 
                 startRow = 1, startCol = 1, header = TRUE)
  writeWorksheet(wb, datError, sheet = "DBE_Errors", 
                 startRow = 1, startCol = 1, header = TRUE)
  writeWorksheet(wb, datSEF, sheet = "DBE_vsSEF", 
                 startRow = 1, startCol = 1, header = TRUE)
  
} else if (check == "CatchRollups"){
  writeWorksheet(wb, summaryTable, sheet = "CatchRollup_Summary", 
                 startRow = 1, startCol = 1, header = FALSE)
  writeWorksheet(wb, datTotal, sheet = "CatchRollup_Totals", 
                 startRow = 1, startCol = 1, header = TRUE)
  writeWorksheet(wb, mismatchTable, sheet = "CatchRollup_Mismatches", 
                 startRow = 1, startCol = 1, header = TRUE)
  writeWorksheet(wb, datError, sheet = "CatchRollup_Errors", 
                 startRow = 1, startCol = 1, header = TRUE)
  
} else if (check == "PassageRollups"){
  writeWorksheet(wb, summaryTable, sheet = "PsgRollup_Summary", 
                 startRow = 1, startCol = 1, header = FALSE)
  writeWorksheet(wb, datTotal, sheet = "PsgRollup_Totals", 
                 startRow = 1, startCol = 1, header = TRUE)
  writeWorksheet(wb, mismatchTable, sheet = "PsgRollup_Mismatches", 
                 startRow = 1, startCol = 1, header = TRUE)
  writeWorksheet(wb, datError, sheet = "PsgRollup_Errors", 
                 startRow = 1, startCol = 1, header = TRUE)
  
}

saveWorkbook(wb)