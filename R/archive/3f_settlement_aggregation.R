#import all needed functions
source("R/agg_functions.R")
source("R/utilities.R")
source("R/small_functions.R")
#devtools::install_github("mabafaba/composr")

#laod packages
package.check(c("dplyr","readxl","progress","stringr","composr","readr"))

db<-read_excel("data/REG_1903B_3F_data_compilation_2021-01_VF.xlsx","Clean Data")%>%prepdata()
choices <- readxl::read_excel("asset/REACH REG1903b 3F - Outil multisectoriel_2021-01.xlsx", sheet = "choices")
survey <- readxl::read_excel("asset/REACH REG1903b 3F - Outil multisectoriel_2021-01.xlsx", sheet = "survey")

# if not xml format
db<-from_label_toxml(db,choices,survey,multiple_choices)
db<-sm_label_toxml(db, multiple_choices, separator = ".")

db$info_localite_final <- clean(db$info_localite_final)
champs_synthese<-read.csv("asset/Liste_Champ_synthese_2021.csv")
recode_sl<-read.csv("asset/recode_sl.csv")


# Template dataset to merge at the end
template_data <- db[0,]
template_data<-data.frame(lapply(template_data, as.character), stringsAsFactors=FALSE)

#oth<-which((str_detect(names(db),"_autre")|str_detect(names(db),"_autre$"))&( sapply(db,class)=="factor"|sapply(db,class)=="character"))

champs_aok_all <- na.omit(ifelse(champs_synthese[["fonction"]]=="aok_all",champs_synthese[["name"]],NA))
champs_aok_longest <- na.omit(ifelse(champs_synthese[["fonction"]]=="aok_longest",champs_synthese[["name"]],NA))
champs_aok_mode <- na.omit(ifelse(champs_synthese[["fonction"]]=="aok_mode",champs_synthese[["name"]],NA))
champs_aok_no <- na.omit(ifelse(champs_synthese[["fonction"]]=="aok_no",champs_synthese[["name"]],NA))
champs_aok_no_modal <- na.omit(ifelse(champs_synthese[["fonction"]]=="aok_no_modal",champs_synthese[["name"]],NA))
champs_aok_recent <- na.omit(ifelse(champs_synthese[["fonction"]]=="aok_recent",champs_synthese[["name"]],NA))
champs_aok_tension <- na.omit(ifelse(champs_synthese[["fonction"]]=="aok_tension",champs_synthese[["name"]],NA))
champs_aok_true <- na.omit(ifelse(champs_synthese[["fonction"]]=="aok_true",champs_synthese[["name"]],NA))
champs_aok_yes <- na.omit(ifelse(champs_synthese[["fonction"]]=="aok_yes",champs_synthese[["name"]],NA))
champs_aok_yes_modal <- na.omit(ifelse(champs_synthese[["fonction"]]=="aok_yes_modal",champs_synthese[["name"]],NA))
champs_aok_frequency  <- na.omit(ifelse(champs_synthese[["fonction"]]=="aok_frequency",champs_synthese[["name"]],NA))
champs_aok_longest_modal <- na.omit(ifelse(champs_synthese[["fonction"]]=="aok_longest_modal",champs_synthese[["name"]],NA))
champs_aok_small <- na.omit(ifelse(champs_synthese[["fonction"]]=="aok_small",champs_synthese[["name"]],NA))

groupGeo <- c("info_pays", "admin1", "admin2", "admin3", "info_localite_final")

settlement_all <- settlement_agg(db,groupGeo,champs_aok_all,aok_all)
settlement_longest <- settlement_agg(db,groupGeo,champs_aok_longest,aok_longest)
settlement_mode <- settlement_agg(db,groupGeo,champs_aok_mode,aok_mode)
settlement_no <- settlement_agg(db,groupGeo,champs_aok_no,aok_no)
settlement_no_modal <- settlement_agg(db,groupGeo,champs_aok_no_modal,aok_no_modal)
settlement_recent <- settlement_agg(db,groupGeo,champs_aok_recent,aok_recent)
settlement_tension <- settlement_agg(db,groupGeo,champs_aok_tension,aok_tension)
settlement_true <- settlement_agg(db,groupGeo,champs_aok_true,aok_true)
settlement_yes <- settlement_agg(db,groupGeo,champs_aok_yes,aok_yes)
settlement_yes_modal <- settlement_agg(db,groupGeo,champs_aok_yes_modal,aok_yes_modal)
settlement_frequency <- settlement_agg(db,groupGeo,champs_aok_frequency,aok_frequency)
settlement_longest_modal <- settlement_agg(db,groupGeo,champs_aok_longest_modal,aok_longest_modal)
settlement_small <- settlement_agg(db,groupGeo,champs_aok_small,aok_small)

settlement<- settlement_all %>%
  left_join(settlement_longest,by = groupGeo)%>%
  left_join(settlement_mode,by = groupGeo) %>%
  left_join(settlement_no,by = groupGeo) %>%
  left_join(settlement_no_modal,by = groupGeo) %>%
  left_join(settlement_recent,by = groupGeo) %>%
  left_join(settlement_tension,by = groupGeo) %>%
  left_join(settlement_true,by = groupGeo) %>%
  left_join(settlement_yes,by = groupGeo) %>%
  left_join(settlement_yes_modal,by = groupGeo)%>%
  left_join(settlement_frequency,by=groupGeo) %>%
  left_join(settlement_longest_modal,by=groupGeo)%>%
  left_join(settlement_small,by=groupGeo) %>% 
  left_join(db %>% count(info_pays,admin1,admin2,admin3,info_localite_final,name = "B_ki_coverage"),by = groupGeo)

settlement<-bind_rows(template_data,settlement) %>%prepdata()


parent_created <- re_create_sm(settlement, multiple_choices, separator = ".")
parent_created_sl<-sl_correction(parent_created,recode_sl,multiple_choices)
parent_created_sl<-parent_created_sl%>%mutate(
  month="jan2021",
  dep_loc=clean(paste(admin2,info_localite_final, sep = "_")),
  com_loc=clean(paste(admin3,info_localite_final, sep = "_")),
  loc=clean(info_localite_final)
) %>% select(month,B_ki_coverage,dep_loc,com_loc,loc,everything())

labeled_settelement<-from_xml_tolabel(parent_created_sl,choices,survey,multiple_choices)



write.csv(labeled_settelement,paste0("output/Agg_REG_1903B_3F_data_compilation_2021-01_VF-",humanTime(),".csv"),row.names = F)
