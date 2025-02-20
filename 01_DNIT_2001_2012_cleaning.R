library(readxl)

################################
# load data from 2001 and 2024 #
################################

PNV_2001 <- read.csv("~/Desktop/doctorate/ch3 amazon network/data/DNIT_historical/PNV2001_UTF-8.csv", header=T, stringsAsFactors=FALSE)
PNV_2001 <- PNV_2001[!is.na(PNV_2001$BR),]
PNV_2001$vl_codigo <- PNV_2001$CODIGO

PNV_2010 <- read_excel("~/Desktop/doctorate/ch3 amazon network/data/DNIT_historical/PNV2010.xlsx")
colnames(PNV_2010) <- PNV_2010[2,]
PNV_2010 <- PNV_2010[-c(1,2),]
PNV_2010 <- PNV_2010[!is.na(PNV_2010$BR),]
PNV_2010$vl_codigo <- PNV_2010$Código
colnames(PNV_2010)[11:13] <- c("FedCoin1","FedCoin2","FedCoin3")

DNIT_2024 <- st_read("~/Desktop/doctorate/ch3 amazon network/data/DNIT/202410A/SNV_202410A.shp")
DNIT_2024$geometry <- st_transform(st_zm(DNIT_2024$geometry), 4326)

#join to add shapefiles to 2001
join_full_2001 <- full_join(PNV_2001,DNIT_2024, by=c("vl_codigo"))
join_inner_2001 <- inner_join(PNV_2001,DNIT_2024, by=c("vl_codigo"))
join_left_2001 <- left_join(PNV_2001,DNIT_2024, by=c("vl_codigo"))

joined_sf <- st_as_sf(join_inner)
joined_sf_bool <- st_covers(brazil_amazon,joined_sf$geometry, sparse = FALSE)
joined_sf_amazon <- joined_sf[joined_sf_bool[1,],]
joined_sf_amazon_paved <- joined_sf_amazon[which(joined_sf_amazon$SUPERFICIE == "PAV"),]
DNIT_2001_amazon_paved <- joined_sf_amazon_paved

mapview(joined_sf_amazon)
mapview(joined_sf_amazon_paved)

# main concern 1: some 2001 codes dont match 2024 (~700)
codes_w_match <- unique(join_left_2001$vl_codigo)[unique(join_left_2001$vl_codigo) %in% unique(join_inner_2001$vl_codigo)]
codes_wo_match <- unique(join_left_2001$vl_codigo)[!unique(join_left_2001$vl_codigo) %in% unique(join_inner_2001$vl_codigo)]
rows_wo_match <- join_left_2001[which(join_left_2001$vl_codigo %in% codes_wo_match),]
join_left_2001_match_col <- join_left_2001 %>%
  mutate(match = join_left_2001$vl_codigo %in% codes_w_match)
join_left_2001_match_col <- join_left_2001_match_col[,c(1:3,42,4:41)]  
# main concern 2: lots of spottiness that doesn't seem to reflect actual paving status
# (chance these are connected? missing bits are the spots?)