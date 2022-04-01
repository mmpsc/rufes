#######################################################
# Code to check RuFEs data roll-ups to various management/stock 
# groups from the stream level

# Created: Serena Wong, March 2021
#
# OBJECT DEFINITIONS: 
#     * dat followed by a grouping (e.g. datCU or datStream) gives the level that that 
#       data is aggregated at (e.g. conservation unit and stream-level, respectively)
#     * data with the cmt prefix is all data from the Common database and used for 
#       stock translation
#######################################################

# Import data ----
datStream <- readxl::read_excel(paste0(year, "ruqDataSource_DailyCatch_DNAGrpByPop.xlsx"),
                                sheet = "ruqDataSource_DailyCatch")

# Data exported from RuFEs database for each respective "Mgmt Group Source"
# and with "By population" checked yes
datCU <- readxl::read_excel(paste0(year, "ruqDataSource_DailyCatch_ConservationUnit.xlsx"),
                             sheet = "ruqDataSource_DailyCatch")
datDNA <- readxl::read_excel(paste0(year, "ruqDataSource_DailyCatch_DNAGrp.xlsx"),
                             sheet = "ruqDataSource_DailyCatch")
datFraMgmt <- readxl::read_excel(paste0(year, "ruqDataSource_DailyCatch_FraserPanelMgmtGrp.xlsx"),
                                 sheet = "ruqDataSource_DailyCatch")
datFraStk <- readxl::read_excel(paste0(year, "ruqDataSource_DailyCatch_FraserPanelStockGrp.xlsx"),
                                sheet = "ruqDataSource_DailyCatch")
datMA <- readxl::read_excel(paste0(year, "ruqDataSource_DailyCatch_MA-RSAStocks.xlsx"),
                            sheet = "ruqDataSource_DailyCatch")
datProdMgmt <- readxl::read_excel(paste0(year, "ruqDataSource_DailyCatch_ProductionMgmtGrp.xlsx"),
                                  sheet = "ruqDataSource_DailyCatch")
datProdStk <- readxl::read_excel(paste0(year, "ruqDataSource_DailyCatch_ProductionStockGrp.xlsx"),
                                 sheet = "ruqDataSource_DailyCatch")
datProdStks <- readxl::read_excel(paste0(year, "ruqDataSource_DailyCatch_ProductionStocks.xlsx"),
                                  sheet = "ruqDataSource_DailyCatch")
datReconGrp <- readxl::read_excel(paste0(year, "ruqDataSource_DailyCatch_ReconstructionGrp.xlsx"),
                                  sheet = "ruqDataSource_DailyCatch")
datSpDis <- readxl::read_excel(paste0(year, "ruqDataSource_DailyCatch_SpawningDistrictGrp.xlsx"),
                               sheet = "ruqDataSource_DailyCatch")
datSpDisStk <- readxl::read_excel(paste0(year, "ruqDataSource_DailyCatch_SpawningDistrictStockGrp.xlsx"),
                                  sheet = "ruqDataSource_DailyCatch")

# Common data 
cmtStocks <- readxl::read_excel(paste0(here(), "/Data/cmtStocks.xlsx"), sheet = "Sheet1")
# management groups
cmtMG <- readxl::read_excel(paste0(here(), "/Data/cmtManagementGroups.xlsx"), sheet = "Sheet1")
# management group details
cmtMGD <- readxl::read_excel(paste0(here(), "/Data/cmtManagementGroupsDetails.xlsx"), sheet = "Sheet1")

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

cmtSubW <- cmtSub %>% pivot_wider(id_cols = c(DFOCode, Stream),
                                  names_from = Source, 
                                  values_from = c(GroupName, GroupAbbr))

# should both be 0
# length(unique(cmtSubW$DFOCode)) - nrow(cmtSubW)
# which(duplicated(cmtSubW$DFOCode))

# change values in cells with "NA" to denote that they are NA because they 
# are not contained in that specific rollup

for(c in which(colnames(cmtSubW) == "GroupName_Production Stocks"):
    which(colnames(cmtSubW) == "GroupName_Spawning District Stock Group")){

  cmtSubW[[c]][is.na(cmtSubW[[c]])] <- "NotInRollup"
  
}

# Organize datStream and join with Common ----
datStream %<>% 
  tidyr::drop_na(Year) %>% 
  dplyr::select(-c("CatchArea", "Fishery", "User", 
                   "Gear", "Locality", "Date")) %>% 
  dplyr::select(-Grouping) %>% 
  # rename(Stream = Stock) %>% 
  rename("GroupName_DNA Group" = GroupName)

# join with common to get DFO codes to be associated with the catch values
# note that DNA group is our basis so if streams are found in other groupings
# and are not found in DNA group then this will have to change
dat <- full_join(datStream, cmtSubW, by = c("GroupName_DNA Group", "Stream"))

dat %<>% 
  tidyr::drop_na(Year, SumOfStockCatch) %>% 
  dplyr::select(c(Year, Stream, SumOfStockCatch, DFOCode, 
                  `GroupName_Conservation Unit`,        
                  `GroupName_DNA Group`, 
                  `GroupName_Fraser Panel Mgmt Group`,
                  `GroupName_Fraser Panel Stock Group`, 
                  `GroupName_MA-RSA Stocks`,
                  `GroupName_Production Mgmt Group`,
                  `GroupName_Production Stock Group`,
                  `GroupName_Production Stocks`,
                  `GroupName_Reconstruction Group`,
                  `GroupName_Spawning District Group`,
                  `GroupName_Spawning District Stock Group`,
                  `GroupAbbr_Conservation Unit`, 
                  `GroupAbbr_DNA Group`, 
                  `GroupAbbr_Fraser Panel Mgmt Group`,
                  `GroupAbbr_Fraser Panel Stock Group`, 
                  `GroupAbbr_MA-RSA Stocks`,
                  `GroupAbbr_Production Mgmt Group`,
                  `GroupAbbr_Production Stock Group`,
                  `GroupAbbr_Production Stocks`,
                  `GroupAbbr_Reconstruction Group`,
                  `GroupAbbr_Spawning District Group`,
                  `GroupAbbr_Spawning District Stock Group`))

# Organize grouped data -----

datCU$Source <- "CU"
datDNA$Source <- "DNA"
datFraMgmt$Source <- "FraMgmt"
datFraStk$Source <- "FraStk"
datMA$Source <- "MA"
datProdMgmt$Source <-  "ProdMgmt"
datProdStk$Source <- "ProdStk"
datProdStks$Source <- "ProdStks"
datReconGrp$Source <- "ReconGrp"
datSpDis$Source <- "SpDis"
datSpDisStk$Source <- "SpDisStk"

datL <- list("Conservation Unit" = datCU, 
             "DNA Group" = datDNA, 
             "Fraser Panel Mgmt Group" = datFraMgmt, 
             "Fraser Panel Stock Group" = datFraStk, 
             "MA-RSA Stocks" = datMA, 
             "Production Mgmt Group" = datProdMgmt, 
             "Production Stock Group" = datProdStk,
             "Production Stocks" = datProdStks, 
             "Reconstruction Group" = datReconGrp,
             "Spawning District Group" = datSpDis, 
             "Spawning District Stock Group" = datSpDisStk)

for(i in 1:length(datL)){
  
  # clean up data to remove empty columns/rows and rename grouping column
  # to reflect specific groups
  datL[[i]] <- datL[[i]] %>% 
    tidyr::drop_na(Year) %>% 
    dplyr::select(-c("CatchArea", "Fishery", "User", 
                     "Gear", "Locality", "Date")) %>% 
    dplyr::select(-c(Grouping, Stream)) # %>% 
    # rename(Stream = Stock) %>%
    # dplyr::rename_at(.vars = vars(GroupName), 
    #                 .funs = funs(paste0("GroupName_", names(datL)[i])))
}

# Summarize catch by each grouping -----
datGrpd <- list()

for(c in which(colnames(dat) == "GroupName_Conservation Unit"):
    which(colnames(dat) ==  "GroupName_Spawning District Stock Group")){

  d <- dat %>% 
    group_by(.[[c]]) %>% 
    dplyr::summarize(Catch = sum(SumOfStockCatch)) %>% 
    mutate(GrpSource = colnames(dat)[c])
  
  # colnames(d) <- c(names(dat)[c], "Catch")
  colnames(d) <- c("GroupName", "GrpdCatch", "GrpSource")
  diff <- which(colnames(dat) == "DFOCode")
  
  datGrpd[[c - diff]] <- as.data.frame(d)
}

# length(datGrpd) == length(datL) # should be TRUE

# Match RuFEs data with data summarized here -----
# Compare RuFEs grouped data (datL) with data grouped and summarized 
# in this R code using Common (datGrpd)

datCompare <- list()

for(i in 1:length(datL)){
dL <- datL[[i]]
dG <- datGrpd[[i]]
datCompare[[i]] <- full_join(dL, dG, by = "GroupName") 
datCompare[[i]] <- datCompare[[i]] %>% 
  # difference bt RuFEs value and value calculated in R
  mutate(Diff = SumOfStockCatch - GrpdCatch, 
         # percent difference
         pDiff = abs(SumOfStockCatch - GrpdCatch)/
               ((SumOfStockCatch + GrpdCatch)/2)*100)
datCompare[[i]]$Error <- with(datCompare[[i]], 
                              # can change this threshold for percent difference
                              ifelse(GroupName != "NotInRollup" & 
                                       (pDiff > 0.01 | is.na(pDiff)) &
                                       (SumOfStockCatch != 0 | GrpdCatch != 0), 
                                     Error <- "error", 
                                     Error <- "no error"))

}

# check that DNA group is the most comprehensive... not sure if relevant
# max(sapply(datCompare, nrow)) == nrow(datCompare[[2]])

# Make long dataframe from list ----
# to make more easily accessible for tracing errors and summary table

# make empty dataframe
datLong <- as.data.frame(matrix(nrow = 0, ncol = 0))

# get all list elements into a dataframe
for(i in 1:length(datCompare)){
  datLtmp <- datCompare[[i]]
  datLong <-  rbind(datLong, datLtmp)
}

# check that no rows are lost
# sum(sapply(datCompare, nrow)) == nrow(datLong)

# Make summary tables and flag errors ----

# dataframe of observations with errors
datError <- datLong %>% 
  filter(Error == "error")

dlg_message(paste("Total number of roll-up errors:", nrow(datError)), type = "ok")

# summary table 
dups <- c("Duplicate DFO codes", length(which(duplicated(cmtSubW$DFOCode))))
numErrors <- c("Number of observations with catch roll-up errors", nrow(datError))
summaryTable <- as.data.frame(rbind(dups, numErrors))

# totals
datNotInRollup <- datLong %>% 
  filter(GroupName == "NotInRollup") %>% 
  mutate(GrpSource = gsub(".*_","", GrpSource))

datTotal <- datLong %>% 
  tidyr::drop_na(Source) %>% 
  group_by(Source) %>% 
  summarize(SumRFCatch = sum(SumOfStockCatch),
            SumGrpdCatch = sum(GrpdCatch)) %>% 
  mutate(Diff = SumRFCatch - SumGrpdCatch,
         pDiff = abs(SumRFCatch - SumGrpdCatch)/
           ((SumRFCatch + SumGrpdCatch)/2)*100) %>% 
  mutate(GrpSource = recode(Source, 
                            CU = "Conservation Unit",
                            DNA = "DNA Group",
                            FraMgmt = "Fraser Panel Mgmt Group",
                            FraStk = "Fraser Panel Stock Group", 
                            MA = "MA-RSA Stocks", 
                            ProdMgmt = "Production Mgmt Group", 
                            ProdStk = "Production Stock Group", 
                            ProdStks = "Production Stocks", 
                            ReconGrp = "Reconstruction Group",
                            SpDis = "Spawning District Group", 
                            SpDisStk = "Spawning District Stock Group")) %>% 
  dplyr::select(-Source) %>% 
  relocate(GrpSource, .before = SumRFCatch)

datNotInRollupSub <- datNotInRollup %>% 
  dplyr::select(GrpSource, GrpdCatch)
datTotal <- full_join(datTotal, datNotInRollupSub, by = "GrpSource")

datTotal %<>% 
  rowwise() %>% 
  rename(CatchRemovedInRollup = GrpdCatch) %>% 
  mutate(TotalRFCatch = sum(SumRFCatch, CatchRemovedInRollup, na.rm = TRUE), 
         
         # check for difference between sum of catch for each grouping calculated from 
         # RuFEs rollups and the sum of catch for each grouping calculated in R
         # can change this threshold for percent difference
         MismatchError =  ifelse(pDiff > 0.01 | is.na(pDiff), 
                                 MismatchError <- "error", 
                                 MismatchError <- "no error"), 
         # check for difference between the total RF catch calculated for each 
         # grouping and the sum of catch for each stream
         # can change this threshold for rounding
         ValueError = ifelse(round(TotalRFCatch, 3) != 
                               round(sum(datStream$SumOfStockCatch), 3), 
                             ValueError <- "error", 
                             ValueError <- "no error"))

# mismatch table
# check that no groups for each Source (method of grouping streams) are lost
datLLengths <- sapply(datL, nrow)
datGrpdLengths <- sapply(datGrpd, nrow)
datCompareLengths <- sapply(datCompare, nrow)

mismatchTable <- as.data.frame(cbind(datLLengths, 
                                    datGrpdLengths, datCompareLengths))
colnames(mismatchTable) <- c("RuFEs Groups per Source", "R Groups per Source", 
                            "Joined Groups per Source")

mismatchTable %<>% 
  rownames_to_column("GrpSource") %>% 
  mutate(`Not Included In Rollup` = ifelse(GrpSource %in% datNotInRollup$GrpSource,
                                          `Not Included In Rollup` <- 1,
                                          `Not Included In Rollup` <- 0)) %>% 
  mutate(`Length Mismatch` = ifelse((`RuFEs Groups per Source` + 
                                       `Not Included In Rollup`) != 
                                      `Joined Groups per Source` |
                                      `R Groups per Source` != 
                                      `Joined Groups per Source`, 
                                    `Length Mismatch` <- "mismatch",
                                    `Length Mismatch` <- "no mismatch")) %>% 
  relocate(`Not Included In Rollup`, .after = `RuFEs Groups per Source`)

View(datError)
View(mismatchTable)
# View(summaryTable)
# View(datTotal)

# Save summaries and error tables -----
datTotal <- as.data.frame(datTotal)
datError <- as.data.frame(datError)

# filename <- paste0(here(), "/", year, "/Summaries/CatchRollupSummary_", Sys.Date(),".xlsx")
# 
# write.xlsx(summaryTable, filename, sheetName = "Summary Table", 
#            row.names = FALSE, col.names = FALSE)
# write.xlsx(datTotal, filename, sheetName = "Totaled Catch", 
#            row.names = FALSE, append = TRUE)
# write.xlsx(mismatchTable, filename, sheetName = "Group Mismatch Summary",
#            row.names = FALSE, append = TRUE)
# write.xlsx(datError, filename, sheetName = "Catch Rollup Errors",
#            row.names = FALSE, append = TRUE)

