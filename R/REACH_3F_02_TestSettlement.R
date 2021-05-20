source("R/REACH_3F_Calcul_Aggregation.R")
library(fuzzyjoin)
library(rgdal) # used to load the shapefiles
library(dplyr) # because I wouldn't leave home without it
library(tibble)
library(sf)

LocaliteGPK <- "Z:\\1_REACH_NIGER\\02_Project_Management\\2021\\2021_3Frontieres\\10_GIS\\GIS_Data\\Localites\\Localites_3F.gpkg"
LocaliteLayer <- "Localites_3F"

settlements <- st_read(LocaliteGPK, layer = LocaliteLayer)
data<-readxl::read_excel("data/avril2021/Agg_REG_1903B_3F-20210507-172629.xlsx")

settlements$dep_com_loc <-clean_pcode(settlements$dep_com_loc)

Jointure <- data[data$dep_com_loc %in%settlements$dep_com_loc ,]
NonJointure <- data[!data$dep_com_loc %in% settlements$dep_com_loc ,]

writexl::write_xlsx(Jointure,"output/avril2021/Yes_set_Agg_REG_1903B_3F.xlsx")
writexl::write_xlsx(NonJointure,"output/avril2021/No_set_Agg_REG_1903B_3F.xlsx")
