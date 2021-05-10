hexagoneSHP <- "Z:\\1_REACH_NIGER\\02_Project_Management\\2021\\2021_3Frontieres\\10_GIS\\GIS_Data\\HEX\\hexagon2.shp"
hexgrid <- readOGR(hexagoneSHP)

LocaliteGPK <- "Z:\\1_REACH_NIGER\\02_Project_Management\\2021\\2021_3Frontieres\\10_GIS\\GIS_Data\\Localites\\Localites_3F.gpkg"
LocaliteLayer <- "Localites_3F"

settlements <- readOGR(LocaliteGPK, layer = LocaliteLayer)

hexgrid@data <- mutate(hexgrid@data, id_grid = as.numeric(rownames(hexgrid@data)))
settlements@data <- mutate(settlements@data, id_sett = as.numeric(rownames(settlements@data)))

setgrids <- over(settlements, hexgrid)
setgrids <- mutate(setgrids, id_sett = as.numeric(rownames(setgrids)))
setgrids <- left_join(settlements@data, setgrids, by = c("id_sett" = "id_sett"))
setgrids$com_loc<-clean_pcode(setgrids$com_loc)

db$com_loc <- clean_pcode(paste(db$admin3,db$info_localite_final, sep = "_"))
db <-left_join(db,setgrids, by = "com_loc")
db <- distinct(db,com_loc, .keep_all= TRUE)
db <- db %>% filter(!is.na(com_loc))


grid_summary <- db %>% group_by(State_Id) %>%
  summarise(settlement_assessed_num = length(dep_com), ki_num= sum(B_ki_coverage))


# debut de la selection
hex <- c("State_Id","id_grid",
                    "pdi_temps_arrivee", "refugies_temps_arrivee", 
                    "nourriture_maintenant",  "moyens_existence_obstacle", "marche_maintenant", "prot_maintenant", "sante_maintenant",
                    "edu_maintenant",  "info_difficulte", "eau_debit", "pdi_abris_adequat","abris_dommages", "nutri",
                    "procuration_savon", "reseau_mobile")

final_hex<-aggregation_fct_hex(db,hex,c("State_Id","id_grid"))
final <-left_join(final_hex, grid_summary, by = "State_Id")

pdi_noSL <- final$settlement_assessed_num*final$pdi_vivant_cond_adeq_noSL
pdi_no <- final$settlement_assessed_num*final$pdi_vivant_cond_adeq_no
final$pdi_vivant_cond_adeq_no <- pdi_no/pdi_noSL
final$pdi_vivant_cond_adeq_noSL <- NULL
final <- subset(final,!is.na(final$State_Id))

readr::write_excel_csv2(
  final,
  paste0("output/Hex_Agg_REG_1903B_3F_data_compilation-",humanTime(),".csv"))

  