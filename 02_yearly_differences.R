library(readxl)
library(sf)
library(tidyverse)

##################################################
# build sections of road for difference workflow #
##################################################

years <- c(2001:2024)
years <- c(2015)
for (year in years){
  print(year)
  DNIT_year_amazon_paved <- st_read(paste0("~/Desktop/doctorate/ch3 amazon network/data/DNIT_processed/DNIT_yearly_base_maps/", "DNIT_", year, "_base_map.shp", sep=""))
  
  #build loop to make unique names when linestrings don't touch
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
    DNIT_year_int_one <- DNIT_year_int[which(DNIT_year_int$EXTENSA == max(DNIT_year_int$EXTENSA)),]
    DNIT_year_int_one[,"geometry"] <- DNIT_year_int_geom
    DNIT_year_unions_list[[i]] <- DNIT_year_int_one
  }
  
  DNIT_year_unions <- do.call(rbind, DNIT_year_unions_list)
  
  #remove specific 210BRR1 (old242BTO1) duplicate row in 2015
  if (year==2015){
    DNIT_year_unions <- DNIT_year_unions[-c(first(which(DNIT_year_unions$name_section=="135BMA2")),
                                            first(which(DNIT_year_unions$name_section=="210BRR1"))),]
  }
  
  #mapview(DNIT_year_unions, zcol="name")
  st_write(DNIT_year_unions, paste0("~/Desktop/doctorate/ch3 amazon network/data/DNIT_processed/DNIT_unions/", "DNIT_", year, "_unions.shp", sep=""),append=FALSE)
}  

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

# difference loop between two unioned years

years <- c(2001:2023)
years <- c(2015:2023)
for (year in years){
  year_index <- match(year,years)
  year_plus_one <- years[year_index+1]
  year_plus_one <- ifelse(year==2023, 2024, year_plus_one)
  DNIT_year1_amazon_paved <- st_read(paste0("~/Desktop/doctorate/ch3 amazon network/data/DNIT_processed/DNIT_unions/", "DNIT_", year_plus_one, "_unions.shp", sep=""))
  DNIT_year2_amazon_paved <- st_read(paste0("~/Desktop/doctorate/ch3 amazon network/data/DNIT_processed/DNIT_unions/", "DNIT_", year, "_unions.shp", sep=""))
  
  difference_geoms <- st_sf(id = integer(), geometry = st_sfc(), crs = 4326)
  difference_geoms <- st_transform(difference_geoms,32633)
  
  for (i in 1:length(unique(DNIT_year1_amazon_paved$nm_sctn))) {
    print(i)
    name_section_i <- unique(DNIT_year1_amazon_paved$nm_sctn)[i]
    DNIT_year_int_i <- DNIT_year1_amazon_paved[which(DNIT_year1_amazon_paved$nm_sctn == name_section_i),]
    name_i <- DNIT_year1_amazon_paved[which(DNIT_year1_amazon_paved$nm_sctn == name_section_i),'name'] %>% 
      st_drop_geometry() %>% 
      first()
    name_i <- name_i[1,1]
    DNIT_year2_amazon_paved_matching_name <- DNIT_year2_amazon_paved[which(DNIT_year2_amazon_paved$name == name_i),]
    if(dim(DNIT_year2_amazon_paved_matching_name)[1]>0){
      for (j in 1:length(unique(DNIT_year2_amazon_paved_matching_name$nm_sctn))) {
        name_section_j <- unique(DNIT_year2_amazon_paved_matching_name$nm_sctn)[j]
        DNIT_year_int_j <- DNIT_year2_amazon_paved_matching_name[which(DNIT_year2_amazon_paved_matching_name$nm_sctn == name_section_j),]
        DNIT_year_int_i <- st_transform(DNIT_year_int_i, 32633)
        DNIT_year_int_j <- st_transform(DNIT_year_int_j, 32633)
        if(!is.na(st_is_valid(DNIT_year_int_j))){
          if(dim(st_intersects(DNIT_year_int_i,DNIT_year_int_j))[1]>0){
            DNIT_year_int_i <- st_difference(DNIT_year_int_i,DNIT_year_int_j)
            #mapview(DNIT_year_int_i_diff)
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
  difference_geoms$geometry <- st_cast(difference_geoms$geometry, "MULTILINESTRING")
  st_write(difference_geoms, paste0("~/Desktop/doctorate/ch3 amazon network/data/DNIT_processed/DNIT_differences/DNIT_", year, "_", year_plus_one, "_differences.shp"),append=FALSE)
}

mapview(difference_geoms)
