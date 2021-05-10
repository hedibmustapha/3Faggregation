bd_3f <- db %>% mutate(
  idp_refugie_last_month=ifelse(pdi_temps_arrivee=="1_mois"|refugies_temps_arrivee=="1_mois",1,0),
  idp_refugie_3_months=ifelse(pdi_temps_arrivee%in%c("1_mois","3_mois")|refugies_temps_arrivee%in%c("1_mois","3_mois"),1,0),
  food_access_no=ifelse(nourriture_maintenant=="non",1,0),
  mean_existence_access_no=ifelse(moyens_existence_obstacle=="non",1,0),
  market_access=ifelse(marche_maintenant=="oui",1,0),
  security_feeling_no=ifelse(prot_maintenant=="non",1,0),
  health_access_no=ifelse(sante_maintenant=="non",1,0),
  education_access_no=ifelse(edu_maintenant=="non",1,0),
  diff_info_access=ifelse(info_difficulte=="oui",1,0),
  diff_info_access_no=ifelse(info_difficulte=="non",1,0),
  enough_water_access_no=ifelse(eau_debit=="non",1,0),
  pdi_vivant_cond_adeq_no=ifelse(pdi_abris_adequat=="non",1,0),
  abris_dommages=ifelse(abris_dommages=="oui",1,0),
  nutri_programme=ifelse(nutri=="oui",1,0),
  procuration_savon=ifelse(procuration_savon=="oui",1,0),
  tel_mobile=ifelse(reseau_mobile=="oui",1,0)
)

groupSel<-c("idp_refugie_last_month","idp_refugie_3_months", "food_access_no", "mean_existence_access_no", "market_access",
            "security_feeling_no","health_access_no", "education_access_no", "diff_info_access", "diff_info_access_no",
            "enough_water_access_no", "pdi_vivant_cond_adeq_no", "abris_dommages","nutri_programme",
            "procuration_savon", "tel_mobile")

reg_results<-mean_agg(bd_3f,"info_pays","admin1",groupSel)
dep_results<-mean_agg(bd_3f,"admin1","admin2",groupSel)
com_results<-mean_agg(bd_3f,"admin2","admin3",groupSel)
