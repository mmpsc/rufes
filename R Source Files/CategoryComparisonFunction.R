# remove <- as.vector("NonFraser")
`%!in%` <- negate(`%in%`)

CategoryComparisonChecks <- function(dat, datRF, Stock, Area2, Type2, User2, Gear){ 
  # if(identical(setdiff(unique(dat[[Stock]])[unique(dat[[Stock]]) %!in% remove], 
  #                      unique(datRF$RF_Stock)), 
  #              character(0)) & 
  #    identical(setdiff(unique(datRF$RF_Stock), 
  #                      unique(dat[[Stock]])[unique(dat[[Stock]]) %!in% remove]), 
  #              character(0))){
  #   print("no error - stock lists are the same")
  # } else {
  #   print("error - stock lists differ")
  # }
  if(identical(setdiff(unique(dat[[Stock]]), unique(datRF$RF_Stock)), 
               character(0)) & 
     identical(setdiff(unique(datRF$RF_Stock), unique(dat[[Stock]])), 
               character(0))){
    print("no error - stock lists are the same")
  } else {
    print("error - stock lists differ")
    if(length(setdiff(dat[[Stock]], datRF$RF_Stock)) > 0){
      print("     Stock in CR100/Input:")
      print(paste("     ", 
                  as.data.frame(setdiff(dat[[Stock]], datRF$RF_Stock))[1:length(setdiff(dat[[Stock]], datRF$RF_Stock)), 1]))}
    if(length(setdiff(datRF$RF_Stock, dat[[Stock]])) > 0){
      print("     Stock in RuFEs:")
      print(paste("     ", 
                  as.data.frame(setdiff(datRF$RF_Stock, dat[[Stock]]))[1:length(setdiff(datRF$RF_Stock, dat[[Stock]])), 1]))}
  }
  if(identical(setdiff(unique(dat[[Area2]]), unique(datRF$RF_Area)), 
               character(0)) & 
     identical(setdiff(unique(datRF$RF_Area), unique(dat[[Area2]])), 
               character(0))){print("no error - area categories are the same")
  } else {
    print("error - area categories differ")
    if(length(setdiff(dat[[Area2]], datRF$RF_Area)) > 0){
      print("     Area in CR100/Input:")
      print(paste("     ", 
                  as.data.frame(setdiff(dat[[Area2]], datRF$RF_Area))[1:length(setdiff(dat[[Area2]], datRF$RF_Area)), 1]))}
    if(length(setdiff(datRF$RF_Area, dat[[Area2]])) > 0){
      print("     Area in RuFEs:")
      print(paste("     ", 
                  as.data.frame(setdiff(datRF$RF_Area, dat[[Area2]]))[1:length(setdiff(datRF$RF_Area, dat[[Area2]])), 1]))}
  }
  if(identical(setdiff(unique(dat[[Type2]]), unique(datRF$RF_Type)), 
               character(0)) & 
     identical(setdiff(unique(datRF$RF_Type), unique(dat[[Type2]])), 
               character(0))){print("no error - fishery type categories are the same")
  } else {
    print("error - fishery type categories differ")
    if(length(setdiff(dat[[Type2]], datRF$RF_Type)) > 0){
      print("      Type in CR100/Input:")
      print(paste("     ", 
                  as.data.frame(setdiff(dat[[Type2]], datRF$RF_Type))[1:length(setdiff(dat[[Type2]], datRF$RF_Type)), 1]))}
    if(length(setdiff(datRF$RF_Type, dat[[Type2]])) > 0){
      print("     Type in RuFEs:")
      print(paste("     ", 
                  as.data.frame(setdiff(datRF$RF_Type, dat[[Type2]]))[1:length(setdiff(datRF$RF_Type, dat[[Type2]])), 1]))}
  }
  if(identical(setdiff(unique(dat[[User2]]), unique(datRF$RF_User)), 
               character(0)) & 
     identical(setdiff(unique(datRF$RF_User), unique(dat[[User2]])), 
               character(0))){print("no error - fishery user categories are the same")
  } else {
    print("error - fishery user categories differ")
    if(length(setdiff(dat[[User2]], datRF$RF_User)) > 0){
      print("     User in CR100/Input:")
      print(paste("     ", 
                  as.data.frame(setdiff(dat[[User2]], datRF$RF_User))[1:length(setdiff(dat[[User2]], datRF$RF_User)), 1]))}
    if(length(setdiff(datRF$RF_User, dat[[User2]])) > 0){
      print("     User in RuFEs:")
      print(paste("     ", 
                  as.data.frame(setdiff(datRF$RF_User, dat[[User2]]))[1:length(setdiff(datRF$RF_User, dat[[User2]])), 1]))}
  }
  if(identical(setdiff(unique(dat[[Gear]]), unique(datRF$RF_Gear)), 
               character(0)) & 
     identical(setdiff(unique(datRF$RF_Gear), unique(dat[[Gear]])), 
               character(0))){print("no error - fishery gear categories are the same")
  } else {
    print("error - fishery gear categories differ")
    if(length(setdiff(dat[[Gear]], datRF$RF_Gear)) > 0){
      print("     Gear in CR100/Input:")
      print(paste("     ",
                  as.data.frame(setdiff(dat[[Gear]], datRF$RF_Gear))[1:length(setdiff(dat[[Gear]], datRF$RF_Gear)), 1]))}
    if(length(setdiff(datRF$RF_Gear, dat[[Gear]])) > 0){
      print("     Gear in RuFEs:")
      print(paste("     ",
                  as.data.frame(setdiff(datRF$RF_Gear, dat[[Gear]]))[1:length(setdiff(datRF$RF_Gear, dat[[Gear]])), 1]))}
  }
}
