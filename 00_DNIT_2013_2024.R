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

# navigable waterways
waterways <- st_read("~/Desktop/doctorate/ch3 amazon network/data/BaseHidroHidrovias/fc_hidro_hidrovia_antaq.shp")
waterways$geometry <- st_transform(st_zm(waterways$geometry), 4326)
waterways_bool <- st_covers(brazil_amazon,waterways$geometry, sparse = FALSE)
waterways_amazon <- waterways[waterways_bool[1,],]
waterways_amazon_navegavel <- waterways_amazon[which(waterways_amazon$cla_icacao=="Navegável"),]
mapview(waterways_amazon_navegavel)

#DNIT 2013
DNIT_2013 <- st_read("~/Desktop/doctorate/ch3 amazon network/data/DNIT/201301A/ST_DNIT_RODOVIAS_SNV2013_COMPLETO.shp")
DNIT_2013$geometry <- st_transform(st_zm(DNIT_2013$geometry), 4326)
DNIT_2013_bool <- st_covers(brazil_amazon,DNIT_2013$geometry, sparse = FALSE)
DNIT_2013_amazon <- DNIT_2013[DNIT_2013_bool[1,],]
DNIT_2013_amazon$Superficie <- factor(DNIT_2013_amazon$Superficie, levels = c('N_PAV','PAV','PLA', 'TRV'))
DNIT_2013_amazon_paved <- DNIT_2013_amazon[which(DNIT_2013_amazon$Superficie %in% c('PAV') | DNIT_2013_amazon$Superfíc_1 %in% c('PAV')),]
DNIT_2013_amazon_paved_reduced <- data.frame(rep('2013',dim(DNIT_2013_amazon_paved)[1]),DNIT_2013_amazon_paved$geometry); colnames(DNIT_2013_amazon_paved_reduced) <- c('year', 'geometry')
st_write(DNIT_2013_amazon_paved,"~/Desktop/doctorate/ch3 amazon network/data/DNIT_processed/DNIT_2013_amazon_paved.shp", append=FALSE)

DNIT_2013_map <- ggplot() +
  geom_sf(data = brazil_amazon_municipalities, fill='white', color='lightgrey', size=.15, show.legend = FALSE) +
  geom_sf(data = brazil_amazon, fill=NA, color='black', size=.15, show.legend = FALSE) +
  geom_sf(data = DNIT_2013_amazon_paved, aes(geometry = geometry), color='#CD1076FF', linewidth=0.8, show.legend = "line") +
  #scale_color_manual(name="Surface Type", values=c('#6A359CFF', '#CD1076FF', '#FFB04FFF', '#6497B1FF'),
  #                   labels=c('N_PAV','PAV','PLA', 'TRV')) +
  theme_minimal() +
  no_axis +
  theme(legend.text=element_text(size=12),
        legend.title=element_text(size=14),
        legend.position='right')
DNIT_2013_map

#DNIT 2024
DNIT_2024 <- st_read("~/Desktop/doctorate/ch3 amazon network/data/DNIT/202410A/SNV_202410A.shp")
DNIT_2024$geometry <- st_transform(st_zm(DNIT_2024$geometry), 4326)
DNIT_2024_bool <- st_covers(brazil_amazon,DNIT_2024$geometry, sparse = FALSE)
DNIT_2024_amazon <- DNIT_2024[DNIT_2024_bool[1,],]
#DNIT_2024_amazon$sg_legenda <- factor(DNIT_2024_amazon$sg_legenda, levels = c('N_PAV','PAV','PLA', 'TRV'))
DNIT_2024_amazon_paved <- DNIT_2024_amazon[which(DNIT_2024_amazon$ds_superfi %in% c('PAV') | DNIT_2024_amazon$sup_est_co %in% c('PAV', 'DUP')),]
DNIT_2024_amazon_paved_reduced <- data.frame(rep('2024',dim(DNIT_2024_amazon_paved)[1]),DNIT_2024_amazon_paved$geometry); colnames(DNIT_2024_amazon_paved_reduced) <- c('year', 'geometry')
st_write(DNIT_2024_amazon_paved,"~/Desktop/doctorate/ch3 amazon network/data/DNIT_processed/DNIT_2024_amazon_paved.shp", append=FALSE)

DNIT_2024_map <- ggplot() +
  geom_sf(data = brazil_amazon_municipalities, fill='white', color='lightgrey', size=.15, show.legend = FALSE) +
  geom_sf(data = brazil_amazon_paved, fill=NA, color='black', size=.15, show.legend = FALSE) +
  geom_sf(data = DNIT_2024_amazon, aes(geometry = geometry), color='#CD1076FF', linewidth=0.8, show.legend = "line") +
  #scale_color_manual(name="Surface Type", values=c('#6A359CFF', '#CD1076FF', '#FFB04FFF', '#6497B1FF'),
  #                   labels=c('N_PAV','PAV','PLA', 'TRV')) +
  theme_minimal() +
  no_axis +
  theme(legend.text=element_text(size=12),
        legend.title=element_text(size=14),
        legend.position='right')
DNIT_2024_map

#DNIT 2013 -> 2024
DNIT_2013_2024_map <- ggplot() +
  geom_sf(data = brazil_amazon_municipalities, fill='white', color='lightgrey', size=.15, show.legend = FALSE) +
  geom_sf(data = brazil_amazon, fill=NA, color='black', size=.15, show.legend = FALSE) +
  geom_sf(data = waterways_amazon, fill=NA, color='slategrey', linewidth=0.7, show.legend = FALSE, alpha=0.7) +
  geom_sf(data = DNIT_2024_amazon_paved, aes(geometry = geometry, color='#CD1076FF'), linewidth=0.8, show.legend = "line") +
  geom_sf(data = DNIT_2013_amazon_paved, aes(geometry = geometry, color='#FFB04FFF'), linewidth=0.8, show.legend = "line") +
  scale_color_manual(name="Year Paved", values=c('#CD1076FF','#FFB04FFF'),
                     labels=c('2024','2013')) +
  theme_minimal() +
  #no_axis +
  theme(legend.text=element_text(size=12),
        legend.title=element_text(size=14),
        legend.position='right')
DNIT_2013_2024_map

mapview(DNIT_2024_amazon_paved)
mapview(DNIT_2013_amazon_paved)

#DNIT time series
#DNIT 2016
DNIT_2016 <- st_read("~/Desktop/doctorate/ch3 amazon network/data/DNIT/201609C/snv_201609c.shp")
DNIT_2016$geometry <- st_transform(st_zm(DNIT_2016$geometry), 4326)
DNIT_2016_bool <- st_covers(brazil_amazon,DNIT_2016$geometry, sparse = FALSE)
DNIT_2016_amazon <- DNIT_2016[DNIT_2016_bool[1,],]
DNIT_2016_amazon <- DNIT_2016_amazon[which(DNIT_2016_amazon$ds_superfi %in% c('Pavimentado')),]
DNIT_2016_amazon_reduced <- data.frame(rep(2016,dim(DNIT_2016_amazon)[1]),DNIT_2016_amazon$geometry); colnames(DNIT_2016_amazon_reduced) <- c('year', 'geometry')
mapview(DNIT_2016_amazon)

#DNIT 2019
DNIT_2019 <- st_read("~/Desktop/doctorate/ch3 amazon network/data/DNIT/201907A/SNV_201907A.shp")
DNIT_2019$geometry <- st_transform(st_zm(DNIT_2019$geometry), 4326)
DNIT_2019_bool <- st_covers(brazil_amazon,DNIT_2019$geometry, sparse = FALSE)
DNIT_2019_amazon <- DNIT_2019[DNIT_2019_bool[1,],]
DNIT_2019_amazon <- DNIT_2019_amazon[which(DNIT_2019_amazon$sg_legenda %in% c('PAV')),]
DNIT_2019_amazon_reduced <- data.frame(rep(2019,dim(DNIT_2019_amazon)[1]),DNIT_2019_amazon$geometry); colnames(DNIT_2019_amazon_reduced) <- c('year', 'geometry')
mapview(DNIT_2019_amazon)

#DNIT 2022
DNIT_2022 <- st_read("~/Desktop/doctorate/ch3 amazon network/data/DNIT/202206A/SNV_202206A.shp")
DNIT_2022$geometry <- st_transform(st_zm(DNIT_2022$geometry), 4326)
DNIT_2022_bool <- st_covers(brazil_amazon,DNIT_2022$geometry, sparse = FALSE)
DNIT_2022_amazon <- DNIT_2022[DNIT_2022_bool[1,],]
DNIT_2022_amazon <- DNIT_2022_amazon[which(DNIT_2022_amazon$sg_legenda %in% c('PAV')),]
DNIT_2022_amazon_reduced <- data.frame(rep(2022,dim(DNIT_2022_amazon)[1]),DNIT_2022_amazon$geometry); colnames(DNIT_2022_amazon_reduced) <- c('year', 'geometry')
mapview(DNIT_2022_amazon)

#DNIT 2013, 2016, 2019, 2022, 2024
#gen df
DNIT_all_years <- rbind(DNIT_2013_amazon_reduced,DNIT_2016_amazon_reduced,DNIT_2019_amazon_reduced,DNIT_2022_amazon_reduced,DNIT_2024_amazon_reduced)
DNIT_all_years <- DNIT_all_years %>% arrange(desc(year))
DNIT_all_years <- st_as_sf(DNIT_all_years)
DNIT_processed <- DNIT_all_years
st_write(DNIT_processed, "~/Desktop/doctorate/ch3 amazon network/data/DNIT/DNIT_processed.shp")
mapview(DNIT_all_years, color=DNIT_all_years$year)
DNIT_time_map <- ggplot() +
  geom_sf(data = brazil_amazon_municipalities, fill='white', color='lightgrey', size=.15, show.legend = FALSE) +
  geom_sf(data = brazil_amazon, fill=NA, color='black', size=.15, show.legend = FALSE) +
  geom_sf(data = DNIT_all_years, aes(geometry = geometry, color=year), linewidth=0.8, alpha=1, show.legend = 'line') +
  scale_color_manual(name="Year Paved", values=c('#785EF0', '#409bfc', '#ffc500', '#FF7900','#DC267F'),
                     labels=c('2013', '2016', '2019', '2022', '2024')) +
  theme_minimal() +
  no_axis +
  theme(legend.text=element_text(size=12),
        legend.title=element_text(size=14),
        legend.position='right')
DNIT_time_map

