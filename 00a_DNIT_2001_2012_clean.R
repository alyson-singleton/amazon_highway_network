library(readxl)
library(sf)
library(tidyverse)

#######################################
# load and standardize format 2001-2012
#######################################

#2001
PNV_2001 <- read_excel("~/Desktop/doctorate/ch3 amazon network/data/DNIT_historical/PNV2001.xlsx")
PNV_2001 <- PNV_2001[!is.na(PNV_2001$BR),]
PNV_2001$vl_codigo <- PNV_2001$CODIGO
PNV_2001$SUPERFICIE_ESTADUAL <- NA
PNV_2001 <- PNV_2001[,c(1:11,14,12:13)]

#2002
PNV_2002 <- read_excel("~/Desktop/doctorate/ch3 amazon network/data/DNIT_historical/PNV2002.xlsx")
PNV_2002 <- PNV_2002[!is.na(PNV_2002$BR),]
PNV_2002$vl_codigo <- PNV_2002$CODIGO
PNV_2002$SUPERFICIE_ESTADUAL <- NA
PNV_2002 <- PNV_2002[,c(1:11,14,12:13)]

#2003
PNV_2003 <- read_excel("~/Desktop/doctorate/ch3 amazon network/data/DNIT_historical/PNV2003.xlsx")
PNV_2003 <- PNV_2003[!is.na(PNV_2003$BR),]
PNV_2003$vl_codigo <- PNV_2003$CODIGO
PNV_2003$SUPERFICIE_ESTADUAL <- NA
PNV_2003 <- PNV_2003[,c(1:11,14,12:13)]

#2004
PNV_2004 <- read_excel("~/Desktop/doctorate/ch3 amazon network/data/DNIT_historical/PNV2004.xlsx")
PNV_2004 <- PNV_2004[!is.na(PNV_2004$BR),]
PNV_2004$vl_codigo <- PNV_2004$CODIGO
PNV_2004$SUPERFICIE_ESTADUAL <- NA
PNV_2004 <- PNV_2004[,c(1:11,14,12:13)]

#2005
PNV_2005 <- read_excel("~/Desktop/doctorate/ch3 amazon network/data/DNIT_historical/PNV2005.xlsx")
PNV_2005 <- PNV_2005[!is.na(PNV_2005$BR),]
PNV_2005$vl_codigo <- PNV_2005$CODIGO
PNV_2005$SUPERFICIE_ESTADUAL <- NA
PNV_2005 <- PNV_2005[,c(1:11,14,12:13)]

#2006
PNV_2006 <- read_excel("~/Desktop/doctorate/ch3 amazon network/data/DNIT_historical/PNV2006.xlsx")
PNV_2006 <- PNV_2006[!is.na(PNV_2006$BR),]
PNV_2006$vl_codigo <- PNV_2006$CODIGO
PNV_2006$SUPERFICIE_ESTADUAL <- NA
PNV_2006 <- PNV_2006[,c(1:11,14,12:13)]

#2007
PNV_2007 <- read_excel("~/Desktop/doctorate/ch3 amazon network/data/DNIT_historical/PNV2007.xlsx")
PNV_2007 <- PNV_2007[!is.na(PNV_2007$BR),]
PNV_2007$vl_codigo <- PNV_2007$CODIGO
colnames(PNV_2007)[12] <- "SUPERFICIE_ESTADUAL"

#2008
PNV_2008 <- read_excel("~/Desktop/doctorate/ch3 amazon network/data/DNIT_historical/PNV2008.xlsx")
PNV_2008 <- PNV_2008[!is.na(PNV_2008$BR),]
PNV_2008$vl_codigo <- PNV_2008$CODIGO
colnames(PNV_2008)[12] <- "SUPERFICIE_ESTADUAL"

#2009
PNV_2009 <- read_excel("~/Desktop/doctorate/ch3 amazon network/data/DNIT_historical/PNV2009.xlsx")
PNV_2009 <- PNV_2009[!is.na(PNV_2009$BR),]
PNV_2009$vl_codigo <- PNV_2009$CODIGO
colnames(PNV_2009)[12] <- "SUPERFICIE_ESTADUAL"

#2010
PNV_2010 <- read_excel("~/Desktop/doctorate/ch3 amazon network/data/DNIT_historical/PNV2010.xlsx")
colnames(PNV_2010) <- PNV_2010[2,]
PNV_2010 <- PNV_2010[-c(1,2),]
PNV_2010 <- PNV_2010[!is.na(PNV_2010$BR),]
PNV_2010$VERSAO <- 2010
PNV_2010$vl_codigo <- PNV_2010$Código
PNV_2010 <- PNV_2010[,c(1:10,13:14,17:18)]
colnames(PNV_2010) <- colnames(PNV_2009)

#2011
PNV_2011 <- read_excel("~/Desktop/doctorate/ch3 amazon network/data/DNIT_historical/SNV_2011.xlsx")
colnames(PNV_2011) <- PNV_2011[2,]
PNV_2011 <- PNV_2011[-c(1,2),]
PNV_2011 <- PNV_2011[!is.na(PNV_2011$BR),]
PNV_2011$VERSAO <- 2011
PNV_2011$vl_codigo <- PNV_2011$Código
PNV_2011 <- PNV_2011[,c(1,2,6:13,16,17,20,21)]
colnames(PNV_2011) <- colnames(PNV_2009)

#2012
PNV_2012 <- read_excel("~/Desktop/doctorate/ch3 amazon network/data/DNIT_historical/SNV_2012.xlsx")
colnames(PNV_2012) <- PNV_2012[2,]
PNV_2012 <- PNV_2012[-c(1,2),]
PNV_2012 <- PNV_2012[!is.na(PNV_2012$BR),]
PNV_2012$VERSAO <- 2012
PNV_2012$vl_codigo <- PNV_2012$Código
PNV_2012 <- PNV_2012[,c(1,2,6:13,16,17,21,22)]
colnames(PNV_2012) <- colnames(PNV_2009)

#######################################
# add spatial data by linking to 2024
#######################################

#2024
DNIT_2024 <- st_read("~/Desktop/doctorate/ch3 amazon network/data/DNIT/202410A/SNV_202410A.shp")
DNIT_2024$geometry <- st_transform(st_zm(DNIT_2024$geometry), 4326)
DNIT_2024_bool <- st_covers(brazil_amazon,DNIT_2024$geometry, sparse = FALSE)
DNIT_2024_amazon <- DNIT_2024[DNIT_2024_bool[1,],]
DNIT_2024_amazon_paved <- DNIT_2024_amazon[which(DNIT_2024_amazon$ds_superfi %in% c('PAV') | DNIT_2024_amazon$sup_est_co %in% c('PAV', 'DUP')),]
DNIT_2024_reduced <- DNIT_2024_amazon_paved[,c(7,29)]

#loop to add 2024 shapefiles to each year (2001-2012)
df_names <- paste("PNV_",2001:2012,sep="")
for (i in df_names){
  print(i)
  #restart here
  join_left_2001 <- left_join(PNV_2001,DNIT_2024_reduced, by=c("vl_codigo"))
  joined_sf <- st_as_sf(join_left_2001)
  joined_sf_bool <- st_covers(brazil_amazon,joined_sf$geometry, sparse = FALSE)
  joined_sf_amazon <- joined_sf[joined_sf_bool[1,],]
  joined_sf_amazon_paved <- joined_sf_amazon[which(joined_sf_amazon$SUPERFICIE  %in% c("PAV","DUP")),]
  DNIT_2001_amazon_paved <- joined_sf_amazon_paved
}

join_left_2001 <- left_join(PNV_2001,DNIT_2024_reduced, by=c("vl_codigo"))
joined_sf <- st_as_sf(join_left_2001)
joined_sf_bool <- st_covers(brazil_amazon,joined_sf$geometry, sparse = FALSE)
joined_sf_amazon <- joined_sf[joined_sf_bool[1,],]
joined_sf_amazon_paved <- joined_sf_amazon[which(joined_sf_amazon$SUPERFICIE  %in% c("PAV","DUP")),]
DNIT_2001_amazon_paved <- joined_sf_amazon_paved



############### SCRATCH ############### 
join_full_2001 <- full_join(PNV_2001,DNIT_2024_reduced, by=c("vl_codigo"))
join_inner_2001 <- inner_join(PNV_2001,DNIT_2024_reduced, by=c("vl_codigo"))
join_left_2001 <- left_join(PNV_2001,DNIT_2024_reduced, by=c("vl_codigo"))
join_right_2001 <- right_join(PNV_2001,DNIT_2024_reduced, by=c("vl_codigo"))

joined_sf <- st_as_sf(join_left_2001)
joined_sf_bool <- st_covers(brazil_amazon,joined_sf$geometry, sparse = FALSE)
joined_sf_amazon <- joined_sf[joined_sf_bool[1,],]
joined_sf_amazon_paved <- joined_sf_amazon[which(joined_sf_amazon$SUPERFICIE  %in% c("PAV","DUP")),]
DNIT_2001_amazon_paved <- joined_sf_amazon_paved

mapview(DNIT_2001_amazon_paved)
mapview(DNIT_2024_amazon_paved)

# main concern 1: some 2001 codes dont match 2024 (~700)
# main concern 2: lots of spottiness that doesn't seem to reflect actual paving status
# (check if these issues are the same? missing bits are the spots?)

#2001 no match
codes_w_match <- unique(join_left_2001$vl_codigo)[unique(join_left_2001$vl_codigo) %in% unique(join_inner_2001$vl_codigo)]
codes_wo_match <- unique(join_left_2001$vl_codigo)[!unique(join_left_2001$vl_codigo) %in% unique(join_inner_2001$vl_codigo)]
rows_wo_match <- join_left_2001[which(join_left_2001$vl_codigo %in% codes_wo_match),]
join_left_2001_match_col <- join_left_2001 %>%
  mutate(match = join_left_2001$vl_codigo %in% codes_w_match)
join_left_2001_match_col <- join_left_2001_match_col[,c(1:3,15,4:14)]  

#2024 no match
codes_w_match <- unique(join_right_2001$vl_codigo)[unique(join_right_2001$vl_codigo) %in% unique(join_inner_2001$vl_codigo)]
codes_wo_match <- unique(join_right_2001$vl_codigo)[!unique(join_right_2001$vl_codigo) %in% unique(join_inner_2001$vl_codigo)]
rows_wo_match <- join_right_2001[which(join_right_2001$vl_codigo %in% codes_wo_match),]
join_right_2001_match_col <- join_right_2001 %>%
  mutate(match = join_right_2001$vl_codigo %in% codes_w_match)
join_right_2001_match_col <- join_right_2001_match_col[,c(1:3,15,4:14)]

# let's try to remedy one road we know was fully paved in 2001: 364BRO
#which have no match in 2001
join_left_2001_match_col$name <- substr(join_left_2001_match_col$vl_codigo,1,6)
join_left_2001_match_col$number <- substr(join_left_2001_match_col$vl_codigo,7,10)
join_left_2001_match_col <- join_left_2001_match_col[which(join_left_2001_match_col$SUPERFICIE %in% c("PAV","DUP")),] %>% st_as_sf()

all_2001_364BRO <- join_left_2001_match_col[which(join_left_2001_match_col$name == "364BRO"),]
mapview(all_2001_364BRO)
false_2001_364BRO <- all_2001_364BRO[which(all_2001_364BRO$match == FALSE),]

#which have no match in 2024
join_right_2001_match_col$name <- substr(join_right_2001_match_col$vl_codigo,1,6)
join_right_2001_match_col$number <- substr(join_right_2001_match_col$vl_codigo,7,10)
#join_right_2001_match_col <- join_right_2001_match_col[which(join_right_2001_match_col$SUPERFICIE == "PAV"),] %>% st_as_sf()
all_2024_364BRO <- join_right_2001_match_col[which(join_right_2001_match_col$name == "010BTO"),] %>% st_as_sf() #look here to see what you are adding
mapview(all_2024_364BRO)
false_2024_364BRO <- all_2024_364BRO[which(all_2024_364BRO$match == FALSE),]

#########################################################
#let's create a filled in, baseline 2001 (our best guess)
#########################################################

#single example
all_2001_364BRO_new <- rbind(all_2001_364BRO,false_2024_364BRO)
mapview(all_2001_364BRO_new)

#list to consider i.e. paved roads in the amazon that have holes in 2001
DNIT_2001_amazon_paved$name <- substr(DNIT_2001_amazon_paved$vl_codigo,1,6)
DNIT_2001_amazon_paved$number <- substr(DNIT_2001_amazon_paved$vl_codigo,7,10)
unique(DNIT_2001_amazon_paved$name)

our_list <- c("364BRO","174BRR","364BMT","163BMT","070BMT","163BMT","153BTO", "010BMA", "230BTO") #unique(DNIT_2001_amazon_paved$name
#our_list <- unique(DNIT_2001_amazon_paved$name)
DNIT_2001_amazon_paved_baseline <- DNIT_2001_amazon_paved
DNIT_2001_amazon_paved_baseline$match <- TRUE
DNIT_2001_amazon_paved_baseline <- DNIT_2001_amazon_paved_baseline[,c(1:3,17,4:13,15,16,14)]
for (i in 1:length(our_list)) {
  print(i)
  #print(our_list[i])
  name_i <- our_list[i]
  all_unmatched_2024_name_i <- join_right_2001_match_col[which(join_right_2001_match_col$name == name_i & join_right_2001_match_col$match == FALSE),] %>% st_as_sf()
  DNIT_2001_amazon_paved_baseline <- rbind(DNIT_2001_amazon_paved_baseline,all_unmatched_2024_name_i)
}
mapview(DNIT_2001_amazon_paved_baseline)
mapview(DNIT_2001_amazon_paved_baseline_stor)
mapview(DNIT_2001_amazon_paved)
mapview(DNIT_2013_amazon_paved)
mapview(DNIT_2024_amazon_paved)

#########################################################
#now let's look into changes between 2001 and 2006 (halfway through)
#########################################################

PNV_2006 <- read_excel("~/Desktop/doctorate/ch3 amazon network/data/DNIT_historical/PNV2006.xlsx")
PNV_2006 <- PNV_2006[!is.na(PNV_2006$BR),]
PNV_2006$vl_codigo <- PNV_2006$CODIGO
join_left_2006 <- left_join(PNV_2006,DNIT_2024_reduced, by=c("vl_codigo"))
joined_sf <- st_as_sf(join_left_2006)
joined_sf_bool <- st_covers(brazil_amazon,joined_sf$geometry, sparse = FALSE)
joined_sf_amazon <- joined_sf[joined_sf_bool[1,],]
joined_sf_amazon_paved <- joined_sf_amazon[which(joined_sf_amazon$SUPERFICIE  %in% c("PAV","DUP")),]
DNIT_2006_amazon_paved <- joined_sf_amazon_paved
mapview(DNIT_2006_amazon_paved)

#########################################################
#2012
#########################################################

SNV_2012 <- read_excel("~/Desktop/doctorate/ch3 amazon network/data/DNIT_historical/SNV_2012.xlsx")
colnames(SNV_2012) <- SNV_2012[2,]
SNV_2012 <- SNV_2012[-c(1,2),]
SNV_2012 <- SNV_2012[!is.na(SNV_2012$BR),]
SNV_2012$vl_codigo <- SNV_2012$Código
colnames(SNV_2012)[13:15] <- c("FedCoin1","FedCoin2","FedCoin3")
join_left_2012 <- left_join(SNV_2012,DNIT_2024_reduced, by=c("vl_codigo"))
joined_sf <- st_as_sf(join_left_2012)
joined_sf_bool <- st_covers(brazil_amazon,joined_sf$geometry, sparse = FALSE)
joined_sf_amazon <- joined_sf[joined_sf_bool[1,],]
joined_sf_amazon_paved <- joined_sf_amazon[which(joined_sf_amazon$Superficie  %in% c("PAV","DUP")),]
DNIT_2012_amazon_paved <- joined_sf_amazon_paved
mapview(DNIT_2012_amazon_paved)
