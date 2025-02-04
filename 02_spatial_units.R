library(geobr)
library(sf)
library(ggplot2)
library(geojsonsf)
library(mapview)
library(dplyr)

no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank(),
                 panel.grid.major = element_blank())

###################################
#### load region shapefiles #######
###################################

# brazil amazon from IBGE
brazil_amazon <- st_read("~/Desktop/doctorate/ch3 amazon network/data/Limites_Amazonia_Legal_2022_shp/Limites_Amazonia_Legal_2022.shp")
brazil_amazon$geometry <- st_transform(brazil_amazon$geometry, 4326)
brazil_amazon_municipalities <- st_read("~/Desktop/doctorate/ch3 amazon network/data/Mun_Amazonia_Legal_2022_shp/Mun_Amazonia_Legal_2022.shp")
brazil_amazon_municipalities$geometry <- st_transform(brazil_amazon_municipalities$geometry, 4326)

###################################
#### load road shapefiles #########
###################################

#DNIT 2013
DNIT_2013 <- st_read("~/Desktop/doctorate/ch3 amazon network/data/DNIT/201301A/ST_DNIT_RODOVIAS_SNV2013_COMPLETO.shp")
DNIT_2013$geometry <- st_transform(st_zm(DNIT_2013$geometry), 4326)
DNIT_2013_bool <- st_covers(brazil_amazon,DNIT_2013$geometry, sparse = FALSE)
DNIT_2013_amazon <- DNIT_2013[DNIT_2013_bool[1,],]
DNIT_2013_amazon$Superficie <- factor(DNIT_2013_amazon$Superficie, levels = c('N_PAV','PAV','PLA', 'TRV'))
DNIT_2013_amazon <- DNIT_2013_amazon[which(DNIT_2013_amazon$Superficie %in% c('PAV')),]
DNIT_2013_amazon_reduced <- data.frame(rep('2013',dim(DNIT_2013_amazon)[1]),DNIT_2013_amazon$geometry); colnames(DNIT_2013_amazon_reduced) <- c('year', 'geometry')

#DNIT 2024
DNIT_2024 <- st_read("~/Desktop/doctorate/ch3 amazon network/data/DNIT/202410A/SNV_202410A.shp")
DNIT_2024$geometry <- st_transform(st_zm(DNIT_2024$geometry), 4326)
DNIT_2024_bool <- st_covers(brazil_amazon,DNIT_2024$geometry, sparse = FALSE)
DNIT_2024_amazon <- DNIT_2024[DNIT_2024_bool[1,],]
DNIT_2024_amazon <- DNIT_2024_amazon[which(DNIT_2024_amazon$sg_legenda %in% c('PAV')),]
DNIT_2024_amazon_reduced <- data.frame(rep('2024',dim(DNIT_2024_amazon)[1]),DNIT_2024_amazon$geometry); colnames(DNIT_2024_amazon_reduced) <- c('year', 'geometry')

#find roads that were paved between 2013 and 2024
difference_2013_2024 <- st_difference(DNIT_2013_amazon,DNIT_2024_amazon)
st_write(difference_2013_2024, "~/Desktop/doctorate/ch3 amazon network/data/DNIT/difference_2013_2024.shp")

#find municipalities that intersect these roads
