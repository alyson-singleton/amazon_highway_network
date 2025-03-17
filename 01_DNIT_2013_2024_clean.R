library(readxl)
library(sf)
library(tidyverse)

#######################################
# load and standardize format 2013-2024
#######################################

#Brazilian Amazon boundaries
brazil_amazon <- st_read("~/Desktop/doctorate/ch3 amazon network/data/Limites_Amazonia_Legal_2022_shp/Limites_Amazonia_Legal_2022.shp")
brazil_amazon$geometry <- st_transform(brazil_amazon$geometry, 4326)

set_colnames <- c("BR", "UF", "CODIGO", "LOCAL_INI", "LOCAL_FIM", "KM_INI", "KM_FIM", "EXTENSAO", "SUPERFICIE", 
                  "TR_COINCID", "EST_COINCID","SUP_ESTADUAL", "VERSAO", "vl_codigo", "name", "number", "geometry")

#2013
DNIT_2013 <- st_read("~/Desktop/doctorate/ch3 amazon network/data/DNIT/201301A/ST_DNIT_RODOVIAS_SNV2013_COMPLETO.shp")
DNIT_2013$VERSAO <- 2013
DNIT_2013$vl_codigo <- DNIT_2013$Código
DNIT_2013$name <- substr(DNIT_2013$vl_codigo,1,6)
DNIT_2013$number <- substr(DNIT_2013$vl_codigo,7,10)
DNIT_2013 <- DNIT_2013[,c(2:3,7:14,17:18,24:27,23)]
colnames(DNIT_2013) <- set_colnames
  
#2015
DNIT_2015 <- st_read("~/Desktop/doctorate/ch3 amazon network/data/DNIT/201503A/snv_201503a.shp")
DNIT_2015$VERSAO <- 2015
DNIT_2015$vl_codigo <- DNIT_2015$codigo
DNIT_2015$name <- substr(DNIT_2015$vl_codigo,1,6)
DNIT_2015$number <- substr(DNIT_2015$vl_codigo,7,10)
DNIT_2015 <- DNIT_2015[,c(3:12,15:16,21:24,20)]
colnames(DNIT_2015) <- set_colnames

#2016
DNIT_2016 <- st_read("~/Desktop/doctorate/ch3 amazon network/data/DNIT/201606A/Rodovias_federais_snv.shp") #other 2016 ops didnt have "ds_sup_fed"
DNIT_2016$VERSAO <- 2016
colnames(DNIT_2016)[8] <- "codigo"
DNIT_2016$vl_codigo <- DNIT_2016$codigo
DNIT_2016$name <- substr(DNIT_2016$vl_codigo,1,6)
DNIT_2016$number <- substr(DNIT_2016$vl_codigo,7,10)
DNIT_2016 <- DNIT_2016[,c(2:3,8:15,18:19,24:27,23)]
colnames(DNIT_2016) <- set_colnames

#2017
DNIT_2017 <- st_read("~/Desktop/doctorate/ch3 amazon network/data/DNIT/201710B/SNV_201710B.shp")
DNIT_2017$VERSAO <- 2017
colnames(DNIT_2017)[7] <- "codigo"
DNIT_2017$vl_codigo <- DNIT_2017$codigo
DNIT_2017$name <- substr(DNIT_2017$vl_codigo,1,6)
DNIT_2017$number <- substr(DNIT_2017$vl_codigo,7,10)
DNIT_2017 <- DNIT_2017[,c(2:3,7:12,23,15,18:19,29:32,28)]
colnames(DNIT_2017) <- set_colnames

#2018
DNIT_2018 <- st_read("~/Desktop/doctorate/ch3 amazon network/data/DNIT/201811A/snv_201811A.shp")
DNIT_2018$VERSAO <- 2018
colnames(DNIT_2018)[7] <- "codigo"
DNIT_2018$vl_codigo <- DNIT_2018$codigo
DNIT_2018$name <- substr(DNIT_2018$vl_codigo,1,6)
DNIT_2018$number <- substr(DNIT_2018$vl_codigo,7,10)
DNIT_2018 <- DNIT_2018[,c(2:3,7:12,24,16,19:20,30:33,29)]
colnames(DNIT_2018) <- set_colnames

#2019
DNIT_2019 <- st_read("~/Desktop/doctorate/ch3 amazon network/data/DNIT/201910A/SNV_201910A.shp")
DNIT_2019$VERSAO <- 2019
colnames(DNIT_2019)[7] <- "codigo"
DNIT_2019$vl_codigo <- DNIT_2019$codigo
DNIT_2019$name <- substr(DNIT_2019$vl_codigo,1,6)
DNIT_2019$number <- substr(DNIT_2019$vl_codigo,7,10)
DNIT_2019 <- DNIT_2019[,c(2:3,7:12,24,16,19:20,30:33,29)]
colnames(DNIT_2019) <- set_colnames

#2020
DNIT_2020 <- st_read("~/Desktop/doctorate/ch3 amazon network/data/DNIT/202010A/SNV_202010A.shp")
DNIT_2020$VERSAO <- 2020
colnames(DNIT_2020)[7] <- "codigo"
DNIT_2020$vl_codigo <- DNIT_2020$codigo
DNIT_2020$name <- substr(DNIT_2020$vl_codigo,1,6)
DNIT_2020$number <- substr(DNIT_2020$vl_codigo,7,10)
DNIT_2020 <- DNIT_2020[,c(2:3,7:12,24,16,19:20,30:33,29)]
colnames(DNIT_2020) <- set_colnames

#2021
DNIT_2021 <- st_read("~/Desktop/doctorate/ch3 amazon network/data/DNIT/202110A/SNV_202110A.shp")
DNIT_2021$VERSAO <- 2021
colnames(DNIT_2021)[7] <- "codigo"
DNIT_2021$vl_codigo <- DNIT_2021$codigo
DNIT_2021$name <- substr(DNIT_2021$vl_codigo,1,6)
DNIT_2021$number <- substr(DNIT_2021$vl_codigo,7,10)
DNIT_2021 <- DNIT_2021[,c(2:3,7:12,24,16,19:20,30:33,29)]
colnames(DNIT_2021) <- set_colnames

#2022
DNIT_2022 <- st_read("~/Desktop/doctorate/ch3 amazon network/data/DNIT/202210C/SNV_202210C.shp")
DNIT_2022$VERSAO <- 2022
colnames(DNIT_2022)[7] <- "codigo"
DNIT_2022$vl_codigo <- DNIT_2022$codigo
DNIT_2022$name <- substr(DNIT_2022$vl_codigo,1,6)
DNIT_2022$number <- substr(DNIT_2022$vl_codigo,7,10)
DNIT_2022 <- DNIT_2022[,c(2:3,7:12,24,16,19:20,30:33,29)]
colnames(DNIT_2022) <- set_colnames

#2023
DNIT_2023 <- st_read("~/Desktop/doctorate/ch3 amazon network/data/DNIT/202310A/SNV_202310A.shp")
DNIT_2023$VERSAO <- 2023
colnames(DNIT_2023)[7] <- "codigo"
DNIT_2023$vl_codigo <- DNIT_2023$codigo
DNIT_2023$name <- substr(DNIT_2023$vl_codigo,1,6)
DNIT_2023$number <- substr(DNIT_2023$vl_codigo,7,10)
DNIT_2023 <- DNIT_2023[,c(2:3,7:12,24,16,19:20,30:33,29)]
colnames(DNIT_2023) <- set_colnames

#2024
DNIT_2024 <- st_read("~/Desktop/doctorate/ch3 amazon network/data/DNIT/202410A/SNV_202410A.shp")
DNIT_2024$VERSAO <- 2024
colnames(DNIT_2024)[7] <- "codigo"
DNIT_2024$vl_codigo <- DNIT_2024$codigo
DNIT_2024$name <- substr(DNIT_2024$vl_codigo,1,6)
DNIT_2024$number <- substr(DNIT_2024$vl_codigo,7,10)
DNIT_2024 <- DNIT_2024[,c(2:3,7:12,24,16,19:20,30:33,29)]
colnames(DNIT_2024) <- set_colnames

#######################################
# reduce to paved and in the amazon
#######################################

years <- c(2013,2015:2024)
years<-2024
for (year in years){
  print(year)
  
  old_df_name <- paste0("DNIT_", year, sep="")
  df <- get(old_df_name)
  
  df$geometry <- st_transform(st_zm(df$geometry), 4326)
  df_bool <- st_covers(brazil_amazon,df$geometry, sparse = FALSE)
  df_amazon <- df[df_bool[1,],]
  #NOTE: write now ive written it like we don't care about duplicated v non duplicated, just PAVED
  df_amazon_paved <- df_amazon[which(df_amazon$SUPERFICIE  %in% c('PAV', 'DUP', 'EOD') | df_amazon$SUP_ESTADUAL %in% c('PAV', 'DUP', 'EOD')),]
  
  new_df_name <- paste0("DNIT_", year, "_amazon_paved", sep="")
  assign(new_df_name, df_amazon_paved)
}

#check
mapview(DNIT_2001_amazon_paved_filled)
mapview(DNIT_2012_amazon_paved_filled)#**double check jumps from 2012-2013
mapview(DNIT_2013_amazon_paved)
mapview(DNIT_2015_amazon_paved)
mapview(DNIT_2016_amazon_paved)
mapview(DNIT_2017_amazon_paved)
mapview(DNIT_2018_amazon_paved)
mapview(DNIT_2019_amazon_paved)
mapview(DNIT_2020_amazon_paved)
mapview(DNIT_2021_amazon_paved)
mapview(DNIT_2022_amazon_paved)
mapview(DNIT_2023_amazon_paved)
mapview(DNIT_2024_amazon_paved)

#store
years <- c(2013,2015:2024)
for (year in years){
  df_name <- paste0("DNIT_", year, "_amazon_paved", sep="")
  df <- get(df_name)
  st_write(df, paste0("~/Desktop/doctorate/ch3 amazon network/data/DNIT_processed/DNIT_yearly_base_maps/", "DNIT_", year, "_base_map.shp", sep=""))
}

#######################################
# scratch
#######################################


