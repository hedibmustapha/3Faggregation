library(dplyr)
library(tibble)
library(readxl)
library(sf)
library(rgdal)
library(readr)
library(writexl)
source("R/REACH_3F_Calcul_Aggregation.R")

db<-read_excel("data/avril2021/Agg_REG_1903B_3F-20210507-172629.xlsx") %>% type_convert()

LocaliteGPK <- "Z:\\1_REACH_NIGER\\02_Project_Management\\2021\\2021_3Frontieres\\10_GIS\\GIS_Data\\Localites\\Localites_3F.gpkg"
LocaliteLayer <- "Localites_3F"
hexagoneSHP <- "Z:\\1_REACH_NIGER\\02_Project_Management\\2021\\2021_3Frontieres\\10_GIS\\GIS_Data\\HEX\\hexagon2.shp"

hexgrid <- readOGR(hexagoneSHP)
settlements<-st_read(LocaliteGPK, layer = LocaliteLayer) %>% as.data.frame()
settlements_hex <- readOGR(LocaliteGPK, layer = LocaliteLayer)

hexgrid@data <- mutate(hexgrid@data, id_grid = as.numeric(rownames(hexgrid@data)))
settlements_hex@data <- mutate(settlements_hex@data, id_sett = as.numeric(rownames(settlements_hex@data)))

setgrids <- over(settlements_hex, hexgrid)
setgrids <- mutate(setgrids, id_sett = as.numeric(rownames(setgrids)))
setgrids <- left_join(settlements_hex@data, setgrids, by = c("id_sett" = "id_sett"))
setgrids$com_loc<-clean_pcode(setgrids$com_loc)
setgrids$dep_com<-clean_pcode(setgrids$dep_com)

db$pay_reg <- clean_pcode(paste(db$info_pays,db$admin1, sep = "_"))
db$reg_dep <- clean_pcode(paste(db$admin1,db$admin2, sep = "_"))
db$dep_com <- clean_pcode(paste(db$admin2,db$admin3, sep = "_"))
db$com_loc <- clean_pcode(paste(db$admin3,db$info_localite_final, sep = "_"))

settlement_dep_num<-settlements %>% count(clean_pcode(reg_dep)) %>% rename("reg_dep"="clean_pcode(reg_dep)")
settlement_com_num <- settlements %>% count(clean_pcode(dep_com)) %>% rename("dep_com"="clean_pcode(dep_com)")

pay_reg <- c("pay_reg",
                 "pdi_temps_arrivee", "refugies_temps_arrivee", 
                 "nourriture_maintenant",  "moyens_existence_obstacle", "marche_maintenant", "prot_maintenant", "sante_maintenant",
                 "edu_maintenant",  "info_difficulte", "eau_debit", "pdi_abris_adequat","abris_dommages", "nutri",
                 "procuration_savon", "reseau_mobile")
reg_dep <- c("reg_dep",
             "pdi_temps_arrivee", "refugies_temps_arrivee", 
             "nourriture_maintenant",  "moyens_existence_obstacle", "marche_maintenant", "prot_maintenant", "sante_maintenant",
             "edu_maintenant",  "info_difficulte", "eau_debit", "pdi_abris_adequat","abris_dommages", "nutri",
             "procuration_savon", "reseau_mobile")
dep_com <- c("dep_com",
             "pdi_temps_arrivee", "refugies_temps_arrivee", 
             "nourriture_maintenant",  "moyens_existence_obstacle", "marche_maintenant", "prot_maintenant", "sante_maintenant",
             "edu_maintenant",  "info_difficulte", "eau_debit", "pdi_abris_adequat","abris_dommages", "nutri",
             "procuration_savon", "reseau_mobile")

hex <- c("State_Id","id_grid",
         "pdi_temps_arrivee", "refugies_temps_arrivee", 
         "nourriture_maintenant",  "moyens_existence_obstacle", "marche_maintenant", "prot_maintenant", "sante_maintenant",
         "edu_maintenant",  "info_difficulte", "eau_debit", "pdi_abris_adequat","abris_dommages", "nutri",
         "procuration_savon", "reseau_mobile")

final_pay_reg<-aggregation_fct(db,pay_reg,"pay_reg")
final_reg_dep<-aggregation_fct(db,reg_dep,"reg_dep")
final_dep_com<-aggregation_fct(db,dep_com,"dep_com")
final_hex<-aggregation_fct_hex(db,setgrids,hex,c("State_Id","id_grid"))

final_pay_reg<-aggregation_fct_xml(db,pay_reg,"pay_reg")
final_reg_dep<-aggregation_fct_xml(db,reg_dep,"reg_dep")
final_dep_com<-aggregation_fct_xml(db,dep_com,"dep_com")
final_hex<-aggregation_fct_xml_hex(db,setgrids,hex,c("State_Id","id_grid"))

final_reg_dep<-left_join(final_reg_dep,settlement_dep_num,"reg_dep")
final_reg_dep$dep_percentage <-final_reg_dep$settlement_assessed_num/final_reg_dep$n
final_dep_com<-left_join(final_dep_com,settlement_com_num,"dep_com")
final_dep_com$dep_percentage <-final_dep_com$settlement_assessed_num/final_dep_com$n



  write_xlsx(
    final_pay_reg,
    paste0("output/avril2021/Reg_Agg_REG_1903B_3F_data_compilation-",humanTime(),".xlsx"))
  write_xlsx(
    final_reg_dep,
    paste0("output/avril2021/Dep_Agg_REG_1903B_3F_data_compilation-",humanTime(),".xlsx"))
  write_xlsx(
    final_dep_com,
    paste0("output/avril2021/Com_Agg_REG_1903B_3F_data_compilation-",humanTime(),".xlsx"))
  write_xlsx(
    final_hex,
    paste0("output/avril2021/Hex_Agg_REG_1903B_3F_data_compilation-",humanTime(),".xlsx"))
