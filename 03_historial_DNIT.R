PNV_2001 <- read.csv("~/Desktop/doctorate/ch3 amazon network/data/DNIT_historical/PNV2001_UTF-8.csv", header=T, stringsAsFactors=FALSE)
PNV_2001 <- PNV_2001[!is.na(PNV_2001$BR),]
PNV_2001$vl_codigo <- PNV_2001$CODIGO

DNIT_2024 <- st_read("~/Desktop/doctorate/ch3 amazon network/data/DNIT/202410A/SNV_202410A.shp")
DNIT_2024$geometry <- st_transform(st_zm(DNIT_2024$geometry), 4326)

#look into join
full_join <- full_join(PNV_2001,DNIT_2024, by=c("vl_codigo"))
inner_join <- inner_join(PNV_2001,DNIT_2024, by=c("vl_codigo"))
left_join <- left_join(PNV_2001,DNIT_2024, by=c("vl_codigo"))

joined_sf <- st_as_sf(inner_join)
joined_sf_bool <- st_covers(brazil_amazon,joined_sf$geometry, sparse = FALSE)
joined_sf_amazon <- joined_sf[joined_sf_bool[1,],]
joined_sf_amazon_paved <- joined_sf_amazon[which(joined_sf_amazon$SUPERFICIE == "PAV"),]

mapview(joined_sf_amazon)
mapview(joined_sf_amazon_paved)

DNIT_2013_2024_map <- ggplot() +
  geom_sf(data = brazil_amazon_municipalities, fill='white', color='lightgrey', size=.15, show.legend = FALSE) +
  geom_sf(data = brazil_amazon, fill=NA, color='black', size=.15, show.legend = FALSE) +
  geom_sf(data = waterways_amazon, fill=NA, color='lightblue3', linewidth=0.7, show.legend = FALSE) +
  geom_sf(data = DNIT_2024_amazon_paved, aes(geometry = geometry, color='#CD1076FF'), linewidth=0.8, show.legend = "line") +
  geom_sf(data = joined_sf_amazon_paved, aes(geometry = geometry, color='#FFB04FFF'), linewidth=0.8, show.legend = "line") +
  scale_color_manual(name="Year Paved", values=c('#CD1076FF','#FFB04FFF'),
                     labels=c('2024','2001')) +
  theme_minimal() +
  #no_axis +
  theme(legend.text=element_text(size=12),
        legend.title=element_text(size=14),
        legend.position='right')
DNIT_2013_2024_map

#build mapview version for 2001-2024 comparison
DNIT_2024_amazon_paved_reduced <- data.frame(rep(2024,dim(DNIT_2024_amazon_paved)[1]), 
                                             DNIT_2024_amazon_paved$vl_codigo,
                                             #what other columns to keep?
                                             DNIT_2024_amazon_paved$geometry)
colnames(DNIT_2001_amazon_paved_reduced) <- c('year', 'geometry')
DNIT_2001_amazon_paved_reduced <- data.frame(rep(2001,dim(joined_sf_amazon_paved)[1]),
                                             joined_sf_amazon_paved$geometry)
colnames(DNIT_2001_amazon_paved_reduced) <- c('year', 'geometry')

DNIT_range <- rbind(DNIT_2001_amazon_paved_reduced,DNIT_2024_amazon_paved_reduced)
DNIT_all_years <- DNIT_all_years %>% arrange(desc(year))
DNIT_all_years <- st_as_sf(DNIT_all_years)
DNIT_processed <- DNIT_all_years
mapview(DNIT_all_years, color=DNIT_all_years$year)

# mapview for names for union
DNIT_2024_amazon_paved$name <- substr(DNIT_2024_amazon_paved$vl_codigo,1,6)
mapview(DNIT_2024_amazon_paved, zcol="name")

#union same start name (can split up later if need be, for example 364BMT)

#one iteration test
DNIT_2024_010BMA <- DNIT_2024_amazon_paved[which(DNIT_2024_amazon_paved$name == "010BMA"),]
mapview(DNIT_2024_010BMA)

#if only one linestring
DNIT_2024_010BMA <- st_as_sf(DNIT_2024_010BMA) %>%
  st_combine() %>%
  st_line_merge() %>%
  st_as_sf()

DNIT_2024_010BMA_combine <- st_combine(DNIT_2024_010BMA$geometry)
#DNIT_2024_010BMA_union <- st_union(DNIT_2024_010BMA,by_feature=TRUE)
DNIT_2024_010BMA_merge <- st_line_merge(DNIT_2024_010BMA_combine)
DNIT_2024_010BMA_geom <- st_as_sf(DNIT_2024_010BMA_merge[1])
#DNIT_2024_010BMA <- DNIT_2024_010BMA[which(DNIT_2024_010BMA$vl_extensa == max(DNIT_2024_010BMA$vl_extensa)),]
#DNIT_2024_010BMA$geometry <- DNIT_2024_010BMA_geom
mapview(DNIT_2024_010BMA)

#find if more than one linestring
library(igraph)
DNIT_2024_242BMT <- DNIT_2024_amazon_paved[which(DNIT_2024_amazon_paved$name == "242BMT"),]
mapview(DNIT_2024_242BMT)


########################################
########################################

#build loop to make unique names when linestrings don't touch
for (i in 1:length(unique(DNIT_2024_amazon_paved$name))) {
  print(i)
  name <- unique(DNIT_2024_amazon_paved$name)[i]
  DNIT_2024_int <- DNIT_2024_amazon_paved[which(DNIT_2024_amazon_paved$name == name),]
  my_idx_touches <- st_touches(DNIT_2024_int)
  g <- graph.adjlist(my_idx_touches)
  c <- components(g)
  DNIT_2024_int$groups <- c$membership
  DNIT_2024_int$name_group <- paste(DNIT_2024_int$name, DNIT_2024_int$groups, sep="")
  DNIT_2024_amazon_paved[which(DNIT_2024_amazon_paved$name == name),'name_group'] <- DNIT_2024_int$name_group
}

#loop to create new dataset with unioned sections by name
DNIT_2024_unions_list = vector("list", length = length(unique(DNIT_2024_amazon_paved$name_group)))

for (i in 1:length(unique(DNIT_2024_amazon_paved$name_group))) {
  print(i)
  name_group_i <- unique(DNIT_2024_amazon_paved$name_group)[i]
  DNIT_2024_int <- DNIT_2024_amazon_paved[which(DNIT_2024_amazon_paved$name_group == name_group_i),]
  DNIT_2024_int_geom <- st_as_sf(DNIT_2024_int) %>%
    st_combine() %>%
    st_line_merge() %>%
    st_as_sf()
  #add way to tell length
  DNIT_2024_int_one <- DNIT_2024_int[which(DNIT_2024_int$vl_extensa == max(DNIT_2024_int$vl_extensa)),]
  DNIT_2024_int_one[,"geometry"] <- DNIT_2024_int_geom
  DNIT_2024_unions_list[[i]] <- DNIT_2024_int_one
}

DNIT_2024_unions <- do.call(rbind, DNIT_2024_unions_list)
mapview(DNIT_2024_unions, zcol="name_group")
