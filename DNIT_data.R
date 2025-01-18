library(geobr)
library(sf)
library(ggplot2)
library(geojsonsf)

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

#DNIT 2013
DNIT_2013 <- st_read("~/Downloads/201301A/ST_DNIT_RODOVIAS_SNV2013_COMPLETO.shp")
DNIT_2013$geometry <- st_transform(st_zm(DNIT_2013$geometry), 4326)
DNIT_2013_bool <- st_covers(brazil_amazon,DNIT_2013$geometry, sparse = FALSE)
DNIT_2013_amazon <- DNIT_2013[DNIT_2013_bool[1,],]

DNIT_2013_amazon$Superficie <- factor(DNIT_2013_amazon$Superficie, levels = c('N_PAV','PAV','PLA', 'TRV'))
DNIT_2013_amazon <- DNIT_2013_amazon[which(DNIT_2013_amazon$Superficie %in% c('PAV')),]
DNIT_2013_map <- ggplot() +
  geom_sf(data = brazil_amazon_municipalities, fill='white', color='lightgrey', size=.15, show.legend = FALSE) +
  geom_sf(data = brazil_amazon, fill=NA, color='black', size=.15, show.legend = FALSE) +
  geom_sf(data = DNIT_2013_amazon, aes(geometry = geometry), color='#CD1076FF', linewidth=0.8, show.legend = "line") +
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
DNIT_2024_amazon <- DNIT_2024_amazon[which(DNIT_2024_amazon$sg_legenda %in% c('PAV')),]
DNIT_2024_map <- ggplot() +
  geom_sf(data = brazil_amazon_municipalities, fill='white', color='lightgrey', size=.15, show.legend = FALSE) +
  geom_sf(data = brazil_amazon, fill=NA, color='black', size=.15, show.legend = FALSE) +
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
  geom_sf(data = DNIT_2024_amazon, aes(geometry = geometry, color='#CD1076FF'), linewidth=0.8, show.legend = "line") +
  geom_sf(data = DNIT_2013_amazon, aes(geometry = geometry, color='#FFB04FFF'), linewidth=0.8, show.legend = "line") +
  scale_color_manual(name="Year Paved", values=c('#CD1076FF','#FFB04FFF'),
                     labels=c('2024','2013')) +
  theme_minimal() +
  no_axis +
  theme(legend.text=element_text(size=12),
        legend.title=element_text(size=14),
        legend.position='right')
DNIT_2013_2024_map

mapview(DNIT_2024_amazon)
mapview(DNIT_2013_amazon)

#DNIT 2024
SNV_2024 <- st_read("~/Downloads/Rodovias_MT/SNV_202410A.shp")
SNV_2024$geometry <- st_transform(st_zm(SNV_2024$geometry), 4326)
SNV_2024_bool <- st_covers(brazil_amazon,SNV_2024$geometry, sparse = FALSE)
SNV_2024_amazon <- SNV_2024[SNV_2024_bool[1,],]
mapview(SNV_2024_amazon)
