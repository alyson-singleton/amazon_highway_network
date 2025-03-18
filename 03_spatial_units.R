#########################################################
# load data from 2001 and 2024 (bounds of study period) #
#########################################################

DNIT_2001_amazon_paved_filled <- st_read(paste0("~/Desktop/doctorate/ch3 amazon network/data/DNIT_processed/DNIT_yearly_base_maps/", "DNIT_", "2001", "_base_map.shp", sep=""))
DNIT_2024_amazon_paved_filled <- st_read(paste0("~/Desktop/doctorate/ch3 amazon network/data/DNIT_processed/DNIT_yearly_base_maps/", "DNIT_", "2024", "_base_map.shp", sep=""))


##################################################
# build sections of road for difference workflow #
##################################################

#build loop to make unique names when linestrings don't touch

DNIT_year_amazon_paved <- DNIT_2001_amazon_paved #reset year as needed
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

DNIT_2001_unions <- st_read(DNIT_2001_unions, "~/Desktop/doctorate/ch3 amazon network/data/DNIT_processed/DNIT_2001_unions.shp")
DNIT_2024_unions <- st_read(DNIT_2024_unions, "~/Desktop/doctorate/ch3 amazon network/data/DNIT_processed/DNIT_2024_unions.shp")

#########################################
# find difference between 2001 and 2024 #
#########################################

# difference preliminary tests
mapview(DNIT_2001_unions)
mapview(DNIT_2024_unions)
test <- st_difference(DNIT_2024_unions[which(DNIT_2024_unions$name=="319BRO"),],
                      DNIT_2001_unions[which(DNIT_2001_unions$name=="319BRO"),][1,])
mapview(test)
mapview(DNIT_2024_unions[which(DNIT_2024_unions$name=="364BAC"),])
mapview(DNIT_2001_unions[which(DNIT_2001_unions$name=="364BAC"),])

# difference loop between two preprocessed years
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

###############################################
# explore difference and get spatial unit est #
###############################################

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

#intersect with municipalities to find basic unit calc
intersecting_polygons <- st_intersects(difference_geoms_large_lines, brazil_amazon_municipalities)
intersecting_polygons <- brazil_amazon_municipalities[unlist(intersecting_polygons), ]
dim(intersecting_polygons)
mapview(intersecting_polygons)
