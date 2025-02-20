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

# brazil states from geobr package
brazil_states = read_state(code_state="all", year=2020)
brazil_municipalities = read_municipality(code_muni="all", year=2022)
st_write(brazil_municipalities, "~/Desktop/brazil_municipalities_2020.shp") 
brazil_states$geom <- st_transform(brazil_states$geom, 4326)
brazil_states_bool <- st_covers(brazil_amazon,brazil_states$geom, sparse = FALSE)
brazil_states_amazon <- brazil_states[brazil_states_bool[1,],]

#############################
#### plot osm maps ##########
#############################

# load osm road data
osm_norte_roads <- read_sf("~/Desktop/doctorate/ch3 amazon network/data/osm_december/norte-latest-free.shp/gis_osm_roads_free_1.shp")
osm_norte_roads <- osm_norte_roads[which(osm_norte_roads$fclass %in% c('motorway','trunk','primary','secondary', 'tertiary')),]
osm_norte_roads$fclass <- ifelse(osm_norte_roads$fclass=='motorway', 'trunk', osm_norte_roads$fclass)
osm_norte_roads_bool <- st_covers(brazil_amazon,osm_norte_roads$geometry, sparse = FALSE)
osm_norte_roads_amazon <- osm_norte_roads[osm_norte_roads_bool[1,],]

osm_nordeste_roads <- read_sf("~/Desktop/doctorate/ch3 amazon network/data/osm_december/nordeste-latest-free.shp/gis_osm_roads_free_1.shp")
osm_nordeste_roads <- osm_nordeste_roads[which(osm_nordeste_roads$fclass %in% c('motorway','trunk','primary','secondary', 'tertiary')),]
osm_nordeste_roads$fclass <- ifelse(osm_nordeste_roads$fclass=='motorway', 'trunk', osm_nordeste_roads$fclass)
osm_nordeste_roads_bool <- st_covers(brazil_amazon,osm_nordeste_roads$geometry, sparse = FALSE)
osm_nordeste_roads_amazon <- osm_nordeste_roads[osm_nordeste_roads_bool[1,],]

osm_centro_oeste_roads <- read_sf("~/Desktop/doctorate/ch3 amazon network/data/osm_december/centro-oeste-latest-free.shp/gis_osm_roads_free_1.shp")
osm_centro_oeste_roads <- osm_centro_oeste_roads[which(osm_centro_oeste_roads$fclass %in% c('motorway','trunk','primary','secondary', 'tertiary')),]
osm_centro_oeste_roads$fclass <- ifelse(osm_centro_oeste_roads$fclass=='motorway', 'trunk', osm_centro_oeste_roads$fclass)
osm_centro_oeste_roads_bool <- st_covers(brazil_amazon,osm_centro_oeste_roads$geometry, sparse = FALSE)
osm_centro_oeste_roads_amazon <- osm_centro_oeste_roads[osm_centro_oeste_roads_bool[1,],]

osm_amazon_roads <- rbind(osm_norte_roads_amazon, osm_nordeste_roads_amazon, osm_centro_oeste_roads_amazon)
osm_amazon_roads$fclass <- factor(osm_amazon_roads$fclass, levels = c('trunk','primary','secondary', 'tertiary'))
st_write(osm_amazon_roads, "~/Desktop/doctorate/ch3 amazon network/data/osm_december/osm_amazon_roads.shp")

osm_map <- ggplot() +
  geom_sf(data = brazil_amazon_municipalities, fill='white', color='lightgrey', size=.15, show.legend = FALSE) +
  geom_sf(data = brazil_amazon, fill=NA, color='black', size=.15, show.legend = FALSE) +
  geom_sf(data = osm_amazon_roads, aes(geometry = geometry, color=fclass), linewidth=0.8, show.legend = "line") +
  scale_color_manual(name="OSM (fclass)", values=c('#6A359CFF','#FFB04FFF','#CD1076FF', '#6497B1FF'),
                     labels=c('Trunk','Primary','Secondary', 'Tertiary')) +
  theme_minimal() +
  no_axis +
  theme(legend.text=element_text(size=12),
        legend.title=element_text(size=14),
        legend.position='right')
osm_map

#############################
#### plot grip maps #########
#############################

# load grip roads data
grip4_roads <- read_sf("~/Desktop/doctorate/ch3 amazon network/data/GRIP4_Region2_vector_shp/GRIP4_region2.shp")
grip4_roads_brazil <- grip4_roads[which(grip4_roads$GP_RRG == 5),]
grip4_roads_brazil$GP_RTP <- as.factor(grip4_roads_brazil$GP_RTP)
st_write(grip4_roads_brazil, "~/Desktop/doctorate/ch3 amazon network/data/GRIP4_Region2_vector_shp/grip4_roads_brazil.shp")
grip4_roads_brazil <- grip4_roads_brazil[which(grip4_roads_brazil$GP_RTP %in% c('2','3','4')),]
grip4_roads_brazil_bool <- st_covers(brazil_amazon,grip4_roads_brazil$geometry, sparse = FALSE)
grip4_roads_brazil_amazon <- grip4_roads_brazil[grip4_roads_brazil_bool[1,],]
st_write(grip4_roads_brazil_amazon, "~/Desktop/doctorate/ch3 amazon network/data/GRIP4_Region2_vector_shp/grip4_roads_brazil_amazon.shp")

grip_map_all <- ggplot() +
  geom_sf(data = brazil_amazon_municipalities, fill='white', color='lightgrey', size=.15, show.legend = FALSE) +
  geom_sf(data = brazil_amazon, fill=NA, color='black', size=.15, show.legend = FALSE) +
  geom_sf(data = grip4_roads_brazil_amazon, aes(geometry = geometry, color=GP_RTP), linewidth=0.8, show.legend = "line") +
  scale_color_manual(name="GRIP4 (fclass)", values=c('#6A359CFF','#FFB04FFF','#CD1076FF'),
                     labels=c('Primary','Secondary','Tertiary')) +
  theme_minimal() +
  no_axis +
  theme(legend.text=element_text(size=12),
        legend.title=element_text(size=14),
        legend.position='right')
grip_map_all

grip4_roads_brazil_paved <- grip4_roads_brazil[which(grip4_roads_brazil$GP_RSE == 1),]
grip4_roads_brazil_paved$GP_RTP <- as.factor(grip4_roads_brazil_paved$GP_RTP)
grip4_roads_brazil_paved_bool <- st_covers(brazil_amazon,grip4_roads_brazil_paved$geometry, sparse = FALSE)
grip4_roads_brazil_paved_amazon <- grip4_roads_brazil_paved[grip4_roads_brazil_paved_bool[1,],]

# #6497B1FF, #6A359CFF, #FFB04FFF, #679C35FF, #CD1076FF
grip_map_paved <- ggplot() +
  geom_sf(data = brazil_amazon_municipalities, fill='white', color='lightgrey', size=.15, show.legend = FALSE) +
  geom_sf(data = brazil_amazon, fill=NA, color='black', size=.15, show.legend = FALSE) +
  geom_sf(data = grip4_roads_brazil_paved_amazon, aes(geometry = geometry, color=GP_RTP), linewidth=0.8, show.legend = "line") +
  scale_color_manual(name="GRIP4 Paved (fclass)", values=c('#6A359CFF','#FFB04FFF','#CD1076FF'),
                     labels=c('Primary','Secondary','Tertiary')) +
  theme_minimal() +
  no_axis +
  theme(legend.text=element_text(size=12),
        legend.title=element_text(size=14),
        legend.position='right')
grip_map_paved

#######################################
#### plot source comparison map #######
#######################################

source_map <- ggplot() +
  geom_sf(data = brazil_amazon_municipalities, fill='white', color='lightgrey', size=.15, show.legend = FALSE) +
  geom_sf(data = brazil_amazon, fill=NA, color='black', size=.15, show.legend = FALSE) +
  geom_sf(data = osm_amazon_roads, aes(geometry = geometry, color='#FFB04FFF'), linewidth=0.8, show.legend = "line", alpha=0.6) +
  geom_sf(data = grip4_roads_brazil_paved_amazon, aes(geometry = geometry, color='#CD1076FF'), linewidth=0.8, show.legend = "line", alpha=0.6) +
  scale_color_manual(name="Source", values=c('#FFB04FFF','#CD1076FF'),
                     labels=c('OSM (Trunk)','GRIP4 (Primary)')) +
  theme_minimal() +
  no_axis +
  theme(legend.text=element_text(size=12),
        legend.title=element_text(size=14),
        legend.position='right')
source_map
