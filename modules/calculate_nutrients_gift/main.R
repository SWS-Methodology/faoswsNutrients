##'
##' **Author: Livia Lombardi**
##' **Author: Aydan Selek**
##'
##' **Description:**
##'
##' * GIFT nutrients
##'
##'
##' **Inputs:**
##'
##' * total SUA data
##'
##' **Flag assignment:**
##'
##' flagObservationStatus: E
##' flagMethod: e

message("GIFT: Nutrients calculation for SUA balanced data is starting...")

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
  SETTINGS = ReadSettings("modules/calculate_nutrients_gift/sws.yml")
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


# #Compute only for TOP66 countries based on user input
# 
# sofy_countries <- c("356","586","180","231","566","50","360","404","834","450","887","4","408","76","484","24","862","368","764","608",
# "894","729","704","148","332","818","562","646","170","710","392","854","364","454","320","218","604","140","694","504","466",
# "430","598","120","32","178","400","104","68","524","768","340","288","682","384","558","686","804","116","204","762",
# "214","706","760","800","716")


sofi_countries <- ReadDatatable('sofi_countries')
sofi_countries <- sofi_countries$m49


# TAKE SUA DATA

tot_geoDim =
  GetCodeList(domain = "suafbs", dataset = "sua_balanced", dimension = "geographicAreaM49")[type == "country", code] %>%
  Dimension(name = "geographicAreaM49", keys = .)

tot_eleDim =
  c("5141") %>%
  Dimension(name = "measuredElementSuaFbs", keys = .)

tot_itemDim =
  GetCodeList(domain = "suafbs", dataset = "sua_balanced", "measuredItemFbsSua")[,code] %>%
  Dimension(name = "measuredItemFbsSua", keys = .)

tot_timeDim =
  Dimension(name = "timePointYears", keys = as.character(years))

totalsua_key = DatasetKey(domain = "suafbs", dataset = "sua_balanced", dimensions = list(
  geographicAreaM49 = tot_geoDim,
  measuredElementSuaFbs = tot_eleDim,
  measuredItemFbsSua = tot_itemDim,
  timePointYears = tot_timeDim
))

totalsua <- GetData(totalsua_key)

## GET GLOBAL NCT DATASET

glo_geoDim =
  GetCodeList(domain = "suafbs", dataset = "global_nct", dimension = "geographicAreaM49")[, code] %>%
  Dimension(name = "geographicAreaM49", keys = .)

glo_eleDim =
  GetCodeList(domain = "suafbs", dataset = "global_nct", "measuredElement")[,code] %>%
  Dimension(name = "measuredElement", keys = .)

glo_itemDim =
  GetCodeList(domain = "suafbs", dataset = "global_nct", "measuredItemCPC")[,code] %>%
  Dimension(name = "measuredItemCPC", keys = .)

glo_timeDim =
  Dimension(name = "timePointYearsSP", keys = as.character(0))

global_nct_key = DatasetKey(domain = "suafbs", dataset = "global_nct", dimensions = list(
  geographicAreaM49 = glo_geoDim,
  measuredElementTrade = glo_eleDim,
  measuredItemCPC = glo_itemDim,
  timePointYears = glo_timeDim
))

global_nct <- GetData(global_nct_key)


## GET POPULATION DATA
population_key <- DatasetKey(domain = "population", dataset = "population_unpd", dimensions = list(
  geographicAreaM49 =
    GetCodeList(domain = "suafbs", dataset = "sua_balanced", dimension = "geographicAreaM49")[type == "country", code] %>%
    Dimension(name = "geographicAreaM49", keys = .),
  measuredElementSuaFbs = Dimension(name = "measuredElement", keys = "511"), # 511 = Total population
  timePointYears = Dimension(name = "timePointYears", keys = as.character(years))
))

population <- GetData(population_key)
population <- population[geographicAreaM49 != "156",]


# Fix for missing regional official data in the country total
# Source: DEMOGRAPHIC SURVEY, Kurdistan Region of Iraq, July 2018, IOM UN Migration
# ("the KRI population at 5,122,747 individuals and the overall Iraqi
# population at 36,004,552 individuals", pag.14; it implies 14.22805%)
# https://iraq.unfpa.org/sites/default/files/pub-pdf/KRSO%20IOM%20UNFPA%20Demographic%20Survey%20Kurdistan%20Region%20of%20Iraq_0.pdf
population[geographicAreaM49 == "368" & timePointYears %in% as.character(years), Value := Value * 0.8577195]

# Fix for Moldova Population. We need to exclude Transnistria population, 
# since production and trade data exclude this area.We use an approximate value of 500,000, 
# following indication of 2014 census data provided by UN Population Division

population[geographicAreaM49 == "498" & timePointYears %in% as.character(years), Value := Value - 500]



#3 dataset
#totalsua
#global_nct
#population

if(sofi == "yes"){
  totalsua <- totalsua[geographicAreaM49 %in% sofy_countries,]
  #global_nct <- global_nct[geographicAreaM49 %in% sofy_countries,]
  population <- population[geographicAreaM49 %in% sofy_countries,]
}

# Flags will be totally discarded.
population[, c('flagObservationStatus', 'flagMethod'):= NULL]
totalsua[, c('flagObservationStatus', 'flagMethod'):= NULL]
global_nct [, c('flagObservationStatus', 'flagMethod'):= NULL]

# TODO: For now, keep the coutries (areas) for which food information is available in sua balanced dataset

population <- population[geographicAreaM49 %in% unique(totalsua$geographicAreaM49), ]


## preparing population dataset

population[,measuredElement:=NULL]

setnames(population, 'Value', 'Population')


#preparing sua dataset

totalsua_dcast <- dcast.data.table(totalsua, geographicAreaM49 + measuredItemFbsSua + timePointYears
                                     ~ measuredElementSuaFbs, value.var = c('Value'))



setnames(totalsua_dcast, '5141' , 'Food')

totalsua_pop <- merge(totalsua_dcast, population, by = c('geographicAreaM49', 'timePointYears'), all.x = TRUE)

#removing Sudan datapoints related to years before 2012

totalsua_pop <- totalsua_pop[!(geographicAreaM49 == '729' & timePointYears %in% c('2010','2011')),]

#preparing global nct dataset

global_nct[, c('timePointYearsSP'):= NULL]

mapping_table <- ReadDatatable("nutrients2022")

mapping_table <- as.data.table(mapping_table)


mapping_table$nct <- as.character(mapping_table$nct)
mapping_table$nutrients <- as.character(mapping_table$nutrients)

global_nct <- merge(global_nct, mapping_table[, c("nct","nutrients"), with = FALSE], by.x = 'measuredElement',by.y = 'nct', all.x = TRUE)

# dividing the global nct dataset because of some exceptions of factors by countries

global_nct_wildcard <- global_nct[geographicAreaM49 == '0',]
global_nct_wildcard[, c('geographicAreaM49'):= NULL]

global_nct_country <- global_nct[geographicAreaM49 != '0',]
setnames(global_nct_country, "Value", 'country_Value')
global_nct_country[, c('nutrients'):= NULL]

#creating the dataset to store nutrients per capita per day

calculate_stat <- merge(totalsua_pop, global_nct_wildcard, by.x = 'measuredItemFbsSua',by.y = 'measuredItemCPC', allow.cartesian = TRUE)

calculate_stat <- merge(calculate_stat, global_nct_country, by.x = c('measuredItemFbsSua',"geographicAreaM49", 'measuredElement'),
                        by.y = c('measuredItemCPC',"geographicAreaM49", 'measuredElement'), 
                        all.x = TRUE)

calculate_stat[!is.na(country_Value), Value := country_Value]
calculate_stat[, c('country_Value'):= NULL]

calculate_stat[, food_EP := Value[measuredElement %in% '1061'], by = c("measuredItemFbsSua")]


#removing the element edible portion as it has been inserted as new column and is no need in the groups 

calculate_stat <- calculate_stat[measuredElement %!in% '1061', ]


calculate_stat[,new_value := ((((Food* food_EP)*Value)/Population)/365)*10]

calculate_stat[, c('measuredElement', "Food", "Population", "Value", "food_EP"):= NULL]

setnames(calculate_stat, c("nutrients","new_value"), c("measuredElementSuaFbs", "Value"))


#Grouping by GIFT classes


#GIFT grouping table manipulation

gift_table <- ReadDatatable("gift_sua_grouping")

gift_table <- as.data.table(gift_table)

gift_table[, c("gift_description","cpc_description" ):= NULL]

gift_table$gift <- as.character(gift_table$gift)

# Assign GIFT groups to nutrients table

calculate_stat_gift <- merge(calculate_stat, gift_table, by.x = "measuredItemFbsSua", by.y = "cpc",
                            all.x = TRUE)

calculate_stat_gift[, c("measuredItemFbsSua" ):= NULL]

#summing by nutrietns, GIFT group, country and year

calculate_stat_gift <- calculate_stat_gift[,lapply(.SD, sum),
                                        by = .(geographicAreaM49, timePointYears, measuredElementSuaFbs, gift)]

setnames(calculate_stat_gift, c("gift"), c("measuredItemGIFT"))

total_gift <- calculate_stat_gift[, c("geographicAreaM49","timePointYears","measuredElementSuaFbs","Value"), with = FALSE]

total_gift <- total_gift[,lapply(.SD, sum),
                          by = .(geographicAreaM49, timePointYears, measuredElementSuaFbs)]


total_gift[, "measuredItemGIFT" := "99"]

gift_data <- rbind(calculate_stat_gift,total_gift)
#calculate_stat_gift$measuredItemGIFT <- as.character(calculate_stat_gift$measuredItemGIFT)

gift_data[,`:=`(flagObservationStatus = "E", flagMethod = "e")]

gift_data <- gift_data[!is.na(Value)]

SaveData("suafbs","nutrients_gift_2022", 
         gift_data ,waitTimeout = 2000000)
