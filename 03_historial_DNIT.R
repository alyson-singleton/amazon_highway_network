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
DNIT_2001_amazon_paved <- joined_sf_amazon_paved

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
DNIT_year_amazon_paved <- DNIT_2001_amazon_paved
DNIT_year_amazon_paved$name <- substr(DNIT_year_amazon_paved$vl_codigo,1,6)
for (i in 1:length(unique(DNIT_year_amazon_paved$name))) {
  print(i)
  name <- unique(DNIT_year_amazon_paved$name)[i]
  DNIT_year_int <- DNIT_year_amazon_paved[which(DNIT_year_amazon_paved$name == name),]
  my_idx_touches <- st_touches(DNIT_year_int)
  g <- graph.adjlist(my_idx_touches)
  c <- components(g)
  DNIT_year_int$sections <- c$membership
  DNIT_year_int$name_section <- paste(DNIT_year_int$name, DNIT_year_int$sections, sep="")
  DNIT_year_amazon_paved[which(DNIT_year_amazon_paved$name == name),'name_section'] <- DNIT_year_int$name_section
}

#loop to create new dataset with unioned sections by name_section
DNIT_year_unions_list = vector("list", length = length(unique(DNIT_year_amazon_paved$name_section)))

for (i in 1:length(unique(DNIT_year_amazon_paved$name_section))) {
  print(i)
  name_section_i <- unique(DNIT_year_amazon_paved$name_section)[i]
  DNIT_year_int <- DNIT_year_amazon_paved[which(DNIT_year_amazon_paved$name_section == name_section_i),]
  DNIT_year_int_geom <- st_as_sf(DNIT_year_int) %>%
    st_combine() %>%
    st_line_merge() %>%
    st_as_sf()
  DNIT_year_int_one <- DNIT_year_int[which(DNIT_year_int$vl_extensa == max(DNIT_year_int$vl_extensa)),]
  DNIT_year_int_one[,"geometry"] <- DNIT_year_int_geom
  DNIT_year_unions_list[[i]] <- DNIT_year_int_one
}

DNIT_year_unions <- do.call(rbind, DNIT_year_unions_list)
mapview(DNIT_year_unions, zcol="name")

DNIT_2001_amazon_paved <- DNIT_year_amazon_paved
DNIT_2001_unions <- DNIT_year_unions

st_write(DNIT_2001_unions, "~/Desktop/DNIT_2001_unions.shp")
st_write(DNIT_2024_unions, "~/Desktop/DNIT_2024_unions.shp")
         
#link unions to lengths **this currently isn't working**
#DNIT_2024_amazon_paved_lengths <- DNIT_2024_amazon_paved %>%
#  group_by(name_section) %>%
#  summarize(length = sum(vl_extensa))
#DNIT_2024_amazon_paved_lengths <- DNIT_2024_amazon_paved_lengths[,c(1:2)]
#DNIT_2024_unions_lengths <- st_join(DNIT_2024_unions, DNIT_2024_amazon_paved_lengths, )

# difference tests
mapview(DNIT_2001_unions)
mapview(DNIT_2024_unions)
test <- st_difference(DNIT_2024_unions[which(DNIT_2024_unions$name=="319BRO"),],
                      DNIT_2001_unions[which(DNIT_2001_unions$name=="319BRO"),][1,])
mapview(test)
mapview(DNIT_2024_unions[which(DNIT_2024_unions$name=="364BAC"),])
mapview(DNIT_2001_unions[which(DNIT_2001_unions$name=="364BAC"),])

# difference loop
# *NOTE* find a way to fix missing sections in 2001 later, for now just run as is
DNIT_year1_amazon_paved <- DNIT_2024_unions
DNIT_year2_amazon_paved <- DNIT_2001_unions

difference_geoms <- st_sf(id = integer(), geometry = st_sfc(), crs = 4326)
difference_geoms <- st_transform(difference_geoms,32633)

for (i in 1:length(unique(DNIT_year1_amazon_paved$name_section))) {
  print(i)
  name_section_i <- unique(DNIT_year1_amazon_paved$name_section)[i]
  DNIT_year_int_i <- DNIT_year1_amazon_paved[which(DNIT_year1_amazon_paved$name_section == name_section_i),]
  name_i <- DNIT_year1_amazon_paved[which(DNIT_year1_amazon_paved$name_section == name_section_i),'name'] %>% 
    st_drop_geometry() %>% 
    first()
  name_i <- name_i[1,1]
  DNIT_year2_amazon_paved_matching_name <- DNIT_year2_amazon_paved[which(DNIT_year2_amazon_paved$name == name_i),]
  if(dim(DNIT_year2_amazon_paved_matching_name)[1]>0){
    for (j in 1:length(unique(DNIT_year2_amazon_paved_matching_name$name_section))) {
      name_section_j <- unique(DNIT_year2_amazon_paved_matching_name$name_section)[j]
      DNIT_year_int_j <- DNIT_year2_amazon_paved_matching_name[which(DNIT_year2_amazon_paved_matching_name$name_section == name_section_j),]
      DNIT_year_int_i <- st_transform(DNIT_year_int_i, 32633)
      DNIT_year_int_j <- st_transform(DNIT_year_int_j, 32633)
      if(!is.na(st_is_valid(DNIT_year_int_j))){
        if(dim(st_intersects(DNIT_year_int_i,DNIT_year_int_j))[1]>0){
          DNIT_year_int_i <- st_difference(DNIT_year_int_i, DNIT_year_int_j)
        }
      }
    }
  }
  if(dim(DNIT_year_int_i)[1]>0){
    new_feature <- st_sf(id = i, geometry = DNIT_year_int_i$geometry)
    new_feature_transform <- st_transform(new_feature, 32633)
    difference_geoms <- rbind(difference_geoms, new_feature_transform)
  }
}
mapview(difference_geoms)
st_write(difference_geoms, "~/Desktop/doctorate/ch3 amazon network/data/DNIT_processed/DNIT_2001_2024_differences.shp")

#difference_geoms$lengths <- st_length(difference_geoms)
difference_geoms$geometry <- st_cast(difference_geoms$geometry, "MULTILINESTRING")
difference_geoms$num_lines <- sapply(st_geometry(difference_geoms), function(x) length(x))
difference_geoms_large_lines <- difference_geoms[which(difference_geoms$num_lines<10),]
mapview(difference_geoms_large_lines)

#try to restrict to interconnect, longer, expansions *NOTE to be revised
#*IDEA: could write so keep the names, by hand remove some of the larger obvious scraps
difference_geoms$lengths <- st_length(difference_geoms)
difference_geoms$lengths<-as.numeric(difference_geoms$lengths)
mapview(difference_geoms,zcol="lengths")
difference_geoms_large_lines <- difference_geoms[which(difference_geoms$lengths>400000),]
difference_geoms_large_lines <- st_transform(difference_geoms_large_lines, crs=4326)
mapview(difference_geoms_large_lines)

#intersect with municipalities!
intersecting_polygons <- st_intersects(difference_geoms_large_lines, brazil_amazon_municipalities)
intersecting_polygons <- brazil_amazon_municipalities[unlist(intersecting_polygons), ]
dim(intersecting_polygons)
mapview(intersecting_polygons)
