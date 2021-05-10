aggregation_fct <-function(db,selection,admin){
  
  summary <- db %>% group_by_at(admin) %>%
    summarise(settlement_assessed_num = length(!!sym(admin)), ki_num= sum(B_ki_coverage))
  
  Assessed_fields <- db %>% 
    select(all_of(selection))
  
  #Proportion of assessed settlements with IDPs and refugies who have arrived within the last month
  Assessed_fields$idp_refugie_last_month<-ifelse(Assessed_fields$pdi_temps_arrivee == "Au cours du dernier mois" |
                                                   Assessed_fields$refugies_temps_arrivee == "Au cours du dernier mois",1,0)
  
  #Proportion of assessed settlements with IDPs and refugies who have arrived within the last 3 months
  Assessed_fields$idp_refugie_3_months<-ifelse(Assessed_fields$pdi_temps_arrivee == "Au cours du dernier mois" |
                                                 Assessed_fields$pdi_temps_arrivee == "Au cours des derniers 2-3 mois" |
                                                 Assessed_fields$refugies_temps_arrivee == "Au cours du dernier mois" |
                                                 Assessed_fields$refugies_temps_arrivee == "Au cours des derniers 2-3 mois",1,0)
  
  #Proportion of assessed settlements reporting lack of access to food
  Assessed_fields$food_access_no<-ifelse(Assessed_fields$nourriture_maintenant == "Non",1,0)
  
  #Proportion of assessed settlements reporting lack of access to their usual mean of existence
  Assessed_fields$mean_existence_access_no<-ifelse(Assessed_fields$moyens_existence_obstacle == "Non",1,0)
  
  #Proportion of assessed settlements reporting to have access to a market
  Assessed_fields$market_access<-ifelse(Assessed_fields$marche_maintenant == "Oui",1,0)
  
  #Proportion of assessed settlements reporting to have felt unsafe
  Assessed_fields$security_feeling_no<-ifelse(Assessed_fields$prot_maintenant == "Non",1,0)
  
  #Proportion of assessed settlements reporting to have no access to health facilities
  Assessed_fields$health_access_no<-ifelse(Assessed_fields$sante_maintenant == "Non",1,0)
  
  #Proportion of assessed settlements reporting to have no access to education
  Assessed_fields$education_access_no<-ifelse(Assessed_fields$edu_maintenant == "Non",1,0)
  
  #Proportion of assessed settlements reporting to have lack of access to information
  Assessed_fields$diff_info_access<-ifelse(Assessed_fields$info_difficulte == "Oui",1,0)
  
  #Proportion of assessed settlements reporting to have no lack of access to information
  Assessed_fields$diff_info_access_no<-ifelse(Assessed_fields$info_difficulte == "Non",1,0)
  
  #Proportion of assessed settlements reporting to have no access to enough water
  Assessed_fields$enough_water_access_no<-ifelse(Assessed_fields$eau_debit == "Non",1,0)
  
  #Proportion of assessed settlements pdi_vivant_cond_adeq_no
  Assessed_fields$pdi_vivant_cond_adeq_no<-ifelse(Assessed_fields$pdi_abris_adequat == "Non",1,0)
  
  #Proportion of assessed settlements reporting to have des PDI
  Assessed_fields$pdi_vivant_cond_adeq_noSL<-ifelse(Assessed_fields$pdi_abris_adequat == "Oui" |
                                                      Assessed_fields$pdi_abris_adequat == "Non" |
                                                      Assessed_fields$pdi_abris_adequat == "NC" |
                                                      Assessed_fields$pdi_abris_adequat == "Je ne sais pas",1,0)
  
  #Proportion of assessed settlements reporting to have destruction d'abris
  Assessed_fields$abris_dommages<-ifelse(Assessed_fields$abris_dommages == "Oui",1,0)
  
  #Proportion of assessed settlements reporting to have programmes nutritionnels 
  Assessed_fields$nutri_programme<-ifelse(Assessed_fields$nutri =="Oui",1,0)
  
  #Proportion of assessed settlements reporting to have access to savon
  Assessed_fields$procuration_savon<-ifelse(Assessed_fields$procuration_savon == "Oui",1,0)
  
  #Proportion of assessed settlements reporting to have access to tel mobile
  Assessed_fields$tel_mobile<-ifelse(Assessed_fields$reseau_mobile == "Oui",1,0)
  
  ## Aggregation 
  SelAgrege <- c(admin,"idp_refugie_last_month","idp_refugie_3_months", "food_access_no", "mean_existence_access_no", "market_access",
                 "security_feeling_no","health_access_no", "education_access_no", "diff_info_access", "diff_info_access_no", "nutri_programme",
                 "enough_water_access_no", "pdi_vivant_cond_adeq_no", "pdi_vivant_cond_adeq_noSL", "abris_dommages",
                 "procuration_savon", "tel_mobile")
  
  Result_aggregated <- Assessed_fields %>% 
    select(all_of(SelAgrege))%>% 
    group_by_at(admin)%>% 
    summarise_all(list(~mean(.,na.rm = T)))
  
  final <-left_join(Result_aggregated, summary, by = admin)
  # recalcul le champ pdi_vivant_cond_adeq_no en fonction du nombre de localit? qui ne sont pas SL...
  pdi_noSL <- final$settlement_assessed_num*final$pdi_vivant_cond_adeq_noSL
  pdi_no <- final$settlement_assessed_num*final$pdi_vivant_cond_adeq_no
  final$pdi_vivant_cond_adeq_no <- pdi_no/pdi_noSL
  final$pdi_vivant_cond_adeq_noSL <- NULL
  
  return(final)
}

aggregation_fct_xml <-function(db,selection,admin){
  
  summary <- db %>% group_by_at(admin) %>%
    summarise(settlement_assessed_num = length(!!sym(admin)), ki_num= sum(B_ki_coverage))
  
  Assessed_fields <- db %>% 
    select(all_of(selection))
  
  #Proportion of assessed settlements with IDPs and refugies who have arrived within the last month
  Assessed_fields$idp_refugie_last_month<-ifelse(Assessed_fields$pdi_temps_arrivee == "1_mois" |
                                                   Assessed_fields$refugies_temps_arrivee == "1_mois",1,0)
  
  #Proportion of assessed settlements with IDPs and refugies who have arrived within the last 3 months
  Assessed_fields$idp_refugie_3_months<-ifelse(Assessed_fields$pdi_temps_arrivee == "1_mois" |
                                                 Assessed_fields$pdi_temps_arrivee == "3_mois" |
                                                 Assessed_fields$refugies_temps_arrivee == "1_mois" |
                                                 Assessed_fields$refugies_temps_arrivee == "3_mois",1,0)
  
  #Proportion of assessed settlements reporting lack of access to food
  Assessed_fields$food_access_no<-ifelse(Assessed_fields$nourriture_maintenant == "non",1,0)
  
  #Proportion of assessed settlements reporting lack of access to their usual mean of existence
  Assessed_fields$mean_existence_access_no<-ifelse(Assessed_fields$moyens_existence_obstacle == "non",1,0)
  
  #Proportion of assessed settlements reporting to have access to a market
  Assessed_fields$market_access<-ifelse(Assessed_fields$marche_maintenant == "oui",1,0)
  
  #Proportion of assessed settlements reporting to have felt unsafe
  Assessed_fields$security_feeling_no<-ifelse(Assessed_fields$prot_maintenant == "non",1,0)
  
  #Proportion of assessed settlements reporting to have no access to health facilities
  Assessed_fields$health_access_no<-ifelse(Assessed_fields$sante_maintenant == "non",1,0)
  
  #Proportion of assessed settlements reporting to have no access to education
  Assessed_fields$education_access_no<-ifelse(Assessed_fields$edu_maintenant == "non",1,0)
  
  #Proportion of assessed settlements reporting to have lack of access to information
  Assessed_fields$diff_info_access<-ifelse(Assessed_fields$info_difficulte == "oui",1,0)
  
  #Proportion of assessed settlements reporting to have no lack of access to information
  Assessed_fields$diff_info_access_no<-ifelse(Assessed_fields$info_difficulte == "non",1,0)
  
  #Proportion of assessed settlements reporting to have no access to enough water
  Assessed_fields$enough_water_access_no<-ifelse(Assessed_fields$eau_debit == "non",1,0)
  
  #Proportion of assessed settlements pdi_vivant_cond_adeq_no
  Assessed_fields$pdi_vivant_cond_adeq_no<-ifelse(Assessed_fields$pdi_abris_adequat == "non",1,0)
  
  #Proportion of assessed settlements reporting to have des PDI
  Assessed_fields$pdi_vivant_cond_adeq_noSL<-ifelse(Assessed_fields$pdi_abris_adequat == "oui" |
                                                      Assessed_fields$pdi_abris_adequat == "non" |
                                                      Assessed_fields$pdi_abris_adequat == "NC" |
                                                      Assessed_fields$pdi_abris_adequat == "nsp",1,0)
  
  #Proportion of assessed settlements reporting to have destruction d'abris
  Assessed_fields$abris_dommages<-ifelse(Assessed_fields$abris_dommages == "oui",1,0)
  
  #Proportion of assessed settlements reporting to have programmes nutritionnels 
  Assessed_fields$nutri_programme<-ifelse(Assessed_fields$nutri =="oui",1,0)
  
  #Proportion of assessed settlements reporting to have access to savon
  Assessed_fields$procuration_savon<-ifelse(Assessed_fields$procuration_savon == "oui",1,0)
  
  #Proportion of assessed settlements reporting to have access to tel mobile
  Assessed_fields$tel_mobile<-ifelse(Assessed_fields$reseau_mobile == "oui",1,0)
  
  ## Aggregation 
  SelAgrege <- c(admin,"idp_refugie_last_month","idp_refugie_3_months", "food_access_no", "mean_existence_access_no", "market_access",
                 "security_feeling_no","health_access_no", "education_access_no", "diff_info_access", "diff_info_access_no", "nutri_programme",
                 "enough_water_access_no", "pdi_vivant_cond_adeq_no", "pdi_vivant_cond_adeq_noSL", "abris_dommages",
                 "procuration_savon", "tel_mobile")
  
  Result_aggregated <- Assessed_fields %>% 
    select(all_of(SelAgrege))%>% 
    group_by_at(admin)%>% 
    summarise_all(list(~mean(.,na.rm = T)))
  
  final <-left_join(Result_aggregated, summary, by = admin)
  # recalcul le champ pdi_vivant_cond_adeq_no en fonction du nombre de localit? qui ne sont pas SL...
  pdi_noSL <- final$settlement_assessed_num*final$pdi_vivant_cond_adeq_noSL
  pdi_no <- final$settlement_assessed_num*final$pdi_vivant_cond_adeq_no
  final$pdi_vivant_cond_adeq_no <- pdi_no/pdi_noSL
  final$pdi_vivant_cond_adeq_noSL <- NULL
  
  return(final)
}

clean_pcode<-function(x){
  stringi::stri_trans_general(x, "Latin-ASCII") %>% tolower(.) %>% gsub("[^a-z0-9_]", "\\_", .) %>% gsub("^X_|^_|_$","",.) %>% gsub('([_])\\1+', '\\1',.)
}

humanTime <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}

aggregation_fct_hex <-function(db,setgrids,selection,admin){
  
  # db <-left_join(db,setgrids, by = "com_loc", keep=FALSE)
  db$State_Id<- ifelse(is.na(db$dep_com%in%setgrids$dep_com),NA,setgrids$State_Id[which(db$dep_com%in%setgrids$dep_com)])
  db$id_grid<- ifelse(is.na(db$dep_com%in%setgrids$dep_com),NA,setgrids$id_grid[which(db$dep_com%in%setgrids$dep_com)])
  db <- distinct(db,com_loc, .keep_all= TRUE)
  db <- db %>% filter(!is.na(com_loc))
  
  grid_summary <- db %>% group_by(State_Id) %>%
    summarise(settlement_assessed_num = length(dep_com), ki_num= sum(B_ki_coverage))
  
  Assessed_fields <- db %>% 
    select(all_of(selection))
  
  #Proportion of assessed settlements with IDPs and refugies who have arrived within the last month
  Assessed_fields$idp_refugie_last_month<-ifelse(Assessed_fields$pdi_temps_arrivee == "Au cours du dernier mois" |
                                                   Assessed_fields$refugies_temps_arrivee == "Au cours du dernier mois",1,0)
  
  #Proportion of assessed settlements with IDPs and refugies who have arrived within the last 3 months
  Assessed_fields$idp_refugie_3_months<-ifelse(Assessed_fields$pdi_temps_arrivee == "Au cours du dernier mois" |
                                                 Assessed_fields$pdi_temps_arrivee == "Au cours des derniers 2-3 mois" |
                                                 Assessed_fields$refugies_temps_arrivee == "Au cours du dernier mois" |
                                                 Assessed_fields$refugies_temps_arrivee == "Au cours des derniers 2-3 mois",1,0)
  
  #Proportion of assessed settlements reporting lack of access to food
  Assessed_fields$food_access_no<-ifelse(Assessed_fields$nourriture_maintenant == "Non",1,0)
  
  #Proportion of assessed settlements reporting lack of access to their usual mean of existence
  Assessed_fields$mean_existence_access_no<-ifelse(Assessed_fields$moyens_existence_obstacle == "Non",1,0)
  
  #Proportion of assessed settlements reporting to have access to a market
  Assessed_fields$market_access<-ifelse(Assessed_fields$marche_maintenant == "Oui",1,0)
  
  #Proportion of assessed settlements reporting to have felt unsafe
  Assessed_fields$security_feeling_no<-ifelse(Assessed_fields$prot_maintenant == "Non",1,0)
  
  #Proportion of assessed settlements reporting to have no access to health facilities
  Assessed_fields$health_access_no<-ifelse(Assessed_fields$sante_maintenant == "Non",1,0)
  
  #Proportion of assessed settlements reporting to have no access to education
  Assessed_fields$education_access_no<-ifelse(Assessed_fields$edu_maintenant == "Non",1,0)
  
  #Proportion of assessed settlements reporting to have lack of access to information
  Assessed_fields$diff_info_access<-ifelse(Assessed_fields$info_difficulte == "Oui",1,0)
  
  #Proportion of assessed settlements reporting to have no lack of access to information
  Assessed_fields$diff_info_access_no<-ifelse(Assessed_fields$info_difficulte == "Non",1,0)
  
  #Proportion of assessed settlements reporting to have no access to enough water
  Assessed_fields$enough_water_access_no<-ifelse(Assessed_fields$eau_debit == "Non",1,0)
  
  #Proportion of assessed settlements pdi_vivant_cond_adeq_no
  Assessed_fields$pdi_vivant_cond_adeq_no<-ifelse(Assessed_fields$pdi_abris_adequat == "Non",1,0)
  
  #Proportion of assessed settlements reporting to have des PDI
  Assessed_fields$pdi_vivant_cond_adeq_noSL<-ifelse(Assessed_fields$pdi_abris_adequat == "Oui" |
                                                      Assessed_fields$pdi_abris_adequat == "Non" |
                                                      Assessed_fields$pdi_abris_adequat == "NC" |
                                                      Assessed_fields$pdi_abris_adequat == "Je ne sais pas",1,0)
  
  #Proportion of assessed settlements reporting to have destruction d'abris
  Assessed_fields$abris_dommages<-ifelse(Assessed_fields$abris_dommages == "Oui",1,0)
  
  #Proportion of assessed settlements reporting to have programmes nutritionnels 
  Assessed_fields$nutri_programme<-ifelse(Assessed_fields$nutri =="Oui",1,0)
  
  #Proportion of assessed settlements reporting to have access to savon
  Assessed_fields$procuration_savon<-ifelse(Assessed_fields$procuration_savon == "Oui",1,0)
  
  #Proportion of assessed settlements reporting to have access to tel mobile
  Assessed_fields$tel_mobile<-ifelse(Assessed_fields$reseau_mobile == "Oui",1,0)
  
  ## Aggregation 
  SelAgrege <- c(admin,"idp_refugie_last_month","idp_refugie_3_months", "food_access_no", "mean_existence_access_no", "market_access",
                 "security_feeling_no","health_access_no", "education_access_no", "diff_info_access", "diff_info_access_no", "nutri_programme",
                 "enough_water_access_no", "pdi_vivant_cond_adeq_no", "pdi_vivant_cond_adeq_noSL", "abris_dommages",
                 "procuration_savon", "tel_mobile")
  
  Result_aggregated <- Assessed_fields %>% 
    select(all_of(SelAgrege))%>% 
    group_by_at(admin)%>% 
    summarise_all(list(~mean(.,na.rm = T)))
  
  final_hex <-left_join(Result_aggregated, grid_summary, by = "State_Id")
  pdi_noSL <- final_hex$settlement_assessed_num*final_hex$pdi_vivant_cond_adeq_noSL
  pdi_no <- final_hex$settlement_assessed_num*final_hex$pdi_vivant_cond_adeq_no
  final_hex$pdi_vivant_cond_adeq_no <- pdi_no/pdi_noSL
  final_hex$pdi_vivant_cond_adeq_noSL <- NULL
  final_hex <- subset(final_hex,!is.na(final_hex$State_Id))
  
  return(final_hex)
}

aggregation_fct_xml_hex <-function(db,setgrids,selection,admin){
  
  db <-left_join(db,setgrids, by = "com_loc")
  # db$State_Id<- ifelse(is.na(db$dep_com%in%setgrids$dep_com),NA,setgrids$State_Id[which(db$dep_com%in%setgrids$dep_com)])
  # db$id_grid<- ifelse(is.na(db$dep_com%in%setgrids$dep_com),NA,setgrids$id_grid[which(db$dep_com%in%setgrids$dep_com)])
  db <- distinct(db,com_loc, .keep_all= TRUE)
  db <- db %>% filter(!is.na(com_loc))
  
  grid_summary <- db %>% group_by(State_Id) %>%
    summarise(settlement_assessed_num = length(dep_com.x), ki_num= sum(B_ki_coverage))
  
  Assessed_fields <- db %>% 
    select(all_of(selection))
  
  #Proportion of assessed settlements with IDPs and refugies who have arrived within the last month
  Assessed_fields$idp_refugie_last_month<-ifelse(Assessed_fields$pdi_temps_arrivee == "1_mois" |
                                                   Assessed_fields$refugies_temps_arrivee == "1_mois",1,0)
  
  #Proportion of assessed settlements with IDPs and refugies who have arrived within the last 3 months
  Assessed_fields$idp_refugie_3_months<-ifelse(Assessed_fields$pdi_temps_arrivee == "1_mois" |
                                                 Assessed_fields$pdi_temps_arrivee == "3_mois" |
                                                 Assessed_fields$refugies_temps_arrivee == "1_mois" |
                                                 Assessed_fields$refugies_temps_arrivee == "3_mois",1,0)
  
  #Proportion of assessed settlements reporting lack of access to food
  Assessed_fields$food_access_no<-ifelse(Assessed_fields$nourriture_maintenant == "non",1,0)
  
  #Proportion of assessed settlements reporting lack of access to their usual mean of existence
  Assessed_fields$mean_existence_access_no<-ifelse(Assessed_fields$moyens_existence_obstacle == "non",1,0)
  
  #Proportion of assessed settlements reporting to have access to a market
  Assessed_fields$market_access<-ifelse(Assessed_fields$marche_maintenant == "oui",1,0)
  
  #Proportion of assessed settlements reporting to have felt unsafe
  Assessed_fields$security_feeling_no<-ifelse(Assessed_fields$prot_maintenant == "non",1,0)
  
  #Proportion of assessed settlements reporting to have no access to health facilities
  Assessed_fields$health_access_no<-ifelse(Assessed_fields$sante_maintenant == "non",1,0)
  
  #Proportion of assessed settlements reporting to have no access to education
  Assessed_fields$education_access_no<-ifelse(Assessed_fields$edu_maintenant == "non",1,0)
  
  #Proportion of assessed settlements reporting to have lack of access to information
  Assessed_fields$diff_info_access<-ifelse(Assessed_fields$info_difficulte == "oui",1,0)
  
  #Proportion of assessed settlements reporting to have no lack of access to information
  Assessed_fields$diff_info_access_no<-ifelse(Assessed_fields$info_difficulte == "non",1,0)
  
  #Proportion of assessed settlements reporting to have no access to enough water
  Assessed_fields$enough_water_access_no<-ifelse(Assessed_fields$eau_debit == "non",1,0)
  
  #Proportion of assessed settlements pdi_vivant_cond_adeq_no
  Assessed_fields$pdi_vivant_cond_adeq_no<-ifelse(Assessed_fields$pdi_abris_adequat == "non",1,0)
  
  #Proportion of assessed settlements reporting to have des PDI
  Assessed_fields$pdi_vivant_cond_adeq_noSL<-ifelse(Assessed_fields$pdi_abris_adequat == "oui" |
                                                      Assessed_fields$pdi_abris_adequat == "non" |
                                                      Assessed_fields$pdi_abris_adequat == "NC" |
                                                      Assessed_fields$pdi_abris_adequat == "nsp",1,0)
  
  #Proportion of assessed settlements reporting to have destruction d'abris
  Assessed_fields$abris_dommages<-ifelse(Assessed_fields$abris_dommages == "oui",1,0)
  
  #Proportion of assessed settlements reporting to have programmes nutritionnels 
  Assessed_fields$nutri_programme<-ifelse(Assessed_fields$nutri =="oui",1,0)
  
  #Proportion of assessed settlements reporting to have access to savon
  Assessed_fields$procuration_savon<-ifelse(Assessed_fields$procuration_savon == "oui",1,0)
  
  #Proportion of assessed settlements reporting to have access to tel mobile
  Assessed_fields$tel_mobile<-ifelse(Assessed_fields$reseau_mobile == "oui",1,0)
  
  ## Aggregation 
  SelAgrege <- c(admin,"idp_refugie_last_month","idp_refugie_3_months", "food_access_no", "mean_existence_access_no", "market_access",
                 "security_feeling_no","health_access_no", "education_access_no", "diff_info_access", "diff_info_access_no", "nutri_programme",
                 "enough_water_access_no", "pdi_vivant_cond_adeq_no", "pdi_vivant_cond_adeq_noSL", "abris_dommages",
                 "procuration_savon", "tel_mobile")
  
  Result_aggregated <- Assessed_fields %>% 
    select(all_of(SelAgrege))%>% 
    group_by_at(admin)%>% 
    summarise_all(list(~mean(.,na.rm = T)))
  
  final_hex <-left_join(Result_aggregated, grid_summary, by = "State_Id")
  pdi_noSL <- final_hex$settlement_assessed_num*final_hex$pdi_vivant_cond_adeq_noSL
  pdi_no <- final_hex$settlement_assessed_num*final_hex$pdi_vivant_cond_adeq_no
  final_hex$pdi_vivant_cond_adeq_no <- pdi_no/pdi_noSL
  final_hex$pdi_vivant_cond_adeq_noSL <- NULL
  final_hex <- subset(final_hex,!is.na(final_hex$State_Id))
  
  return(final_hex)
}
