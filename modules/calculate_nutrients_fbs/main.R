##'
##' **Author: Aydan Selek**
##'
##' **Description:**
##'
##' * FBS nutrients
##'
##'
##' **Inputs:**
##'
##' * Nutrients SUA 2022 dataset
##'
##' **Flag assignment:**
##'
##' flagObservationStatus: E
##' flagMethod: e

message("FBS: Nutrients calculation for FBS balanced data is starting...")

## Load the libraries

library(faosws)
library(faoswsUtil)
library(dplyr)
library(data.table)
library(tidyr)
library(openxlsx)


start_time <- Sys.time()

R_SWS_SHARE_PATH <- Sys.getenv("R_SWS_SHARE_PATH")

if (CheckDebug()) {
  library(faoswsModules)
  SETTINGS = ReadSettings("modules/calculate_nutrients_fbs/sws.yml")
  ## Define where your certificates are stored
  faosws::SetClientFiles(SETTINGS[["certdir"]])
  ## Get session information from SWS. Token must be obtained from web interface
  GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                     token = SETTINGS[["token"]])
}


`%!in%` = Negate(`%in%`)

min_year = as.numeric(swsContext.computationParams$min_year)
#as.numeric(swsContext.computationParams$min_year)
max_year = as.numeric(swsContext.computationParams$max_year)
#as.numeric(swsContext.computationParams$max_year)
sofi = swsContext.computationParams$only_sofi

years <- as.integer(min_year:max_year)


#Compute only for TOP66 countries based on user input
# 
# sofy_countries <- c("356","586","180","231","566","50","360","404","834","450","887","4","408","76","484","24","862","368","764","608",
#                     "894","729","704","148","332","818","562","646","170","710","392","854","364","454","320","218","604","140","694","504","466",
#                     "430","598","120","32","178","400","104","68","524","768","340","288","682","384","558","686","804","116","204","762",
#                     "214","706","760","800","716")

sofi_countries <- ReadDatatable('sofi_countries')
sofi_countries <- sofi_countries$m49

# TAKE NUTRIENTS SUA DATA

tot_geoDim =
  GetCodeList(domain = "suafbs", dataset = "nutrients_sua_2022", dimension = "geographicAreaM49")[type == "country", code] %>%
  Dimension(name = "geographicAreaM49", keys = .)

tot_eleDim =
  GetCodeList(domain = "suafbs", dataset = "nutrients_sua_2022", dimension = "measuredElementSuaFbs")[, code] %>%
  Dimension(name = "measuredElementSuaFbs", keys = .)

tot_itemDim =
  GetCodeList(domain = "suafbs", dataset = "nutrients_sua_2022", "measuredItemFbsSua")[,code] %>%
  Dimension(name = "measuredItemFbsSua", keys = .)

tot_timeDim =
  Dimension(name = "timePointYears", keys = as.character(years))

totalsua_key = DatasetKey(domain = "suafbs", dataset = "nutrients_sua_2022", dimensions = list(
  geographicAreaM49 = tot_geoDim,
  measuredElementSuaFbs = tot_eleDim,
  measuredItemFbsSua = tot_itemDim,
  timePointYears = tot_timeDim
))

totalsua <- GetData(totalsua_key)

if(sofi == "yes"){
  totalsua <- totalsua[geographicAreaM49 %in% sofi_countries,]
}


# Flags will be totally discarded.
totalsua[, c('flagObservationStatus', 'flagMethod'):= NULL]


## Remove Element 4001: as Food Edible quantity doesn't make sense for the aggregation.
totalsua <- totalsua[measuredElementSuaFbs != '4001', ]


message("Download FBS hierarchy")

FBS_hierarchy<- setDT(ReadDatatable("fbs_tree"))

setnames(FBS_hierarchy, names(FBS_hierarchy), c( "fbsID1", "fbsID2", "fbsID3","fbsID4", "measuredItemFbsSua"))

FBS_hierarchy[ ,  `:=`
         (fbsID4 = paste0("S", get("fbsID4")),
           fbsID3 = paste0("S", get("fbsID3")),
           fbsID2 = paste0("S", get("fbsID2")),
           fbsID1 = paste0("S", get("fbsID1")))]




totalsua_with_fbs <- merge(totalsua, FBS_hierarchy, by = "measuredItemFbsSua", all.x = TRUE)

### ISSUE 1: There are some items are not anywhere in the FBS hierarchy. This has been already noted by Amanda 
### and the excel file on FAOSTAT has been corrected.
### But since the calculation of SUA-FBS doesn't consider them (yet), I delete them from my file. Later this need to be changed.

totalsua_with_fbs <- totalsua_with_fbs[!is.na(fbsID1) & !is.na(fbsID2) & !is.na(fbsID3) & !is.na(fbsID4), ]

## Calculate the Grand Total: Level 1 
level_1 <- totalsua_with_fbs[, .(level1=sum(Value, na.rm = TRUE)), by = c("geographicAreaM49", "measuredElementSuaFbs", "timePointYears", "fbsID1")]
setnames(level_1, c("fbsID1", "level1"), c("measuredItemFbsSua", "Value"))

## Calculate the Level 2 
level_2 <- totalsua_with_fbs[, .(level2=sum(Value, na.rm = TRUE)), by = c("geographicAreaM49", "measuredElementSuaFbs", "timePointYears", "fbsID2")]
setnames(level_2, c("fbsID2", "level2"), c("measuredItemFbsSua", "Value"))

## Calculate the Level 3 
level_3 <- totalsua_with_fbs[, .(level3=sum(Value, na.rm = TRUE)), by = c("geographicAreaM49", "measuredElementSuaFbs", "timePointYears", "fbsID3")]
setnames(level_3, c("fbsID3", "level3"), c("measuredItemFbsSua", "Value"))

## Calculate the Level 4 
level_4 <- totalsua_with_fbs[, .(level4=sum(Value, na.rm = TRUE)), by = c("geographicAreaM49", "measuredElementSuaFbs", "timePointYears", "fbsID4")]
setnames(level_4, c("fbsID4", "level4"), c("measuredItemFbsSua", "Value"))


totalfbs <- rbind(level_1, level_2, level_3, level_4)


totalfbs[,`:=`(flagObservationStatus = "E", flagMethod = "e")]

totalfbs <- totalfbs[!is.na(Value)]

SaveData("suafbs","nutrients_sua_2022", totalfbs, waitTimeout = Inf)
