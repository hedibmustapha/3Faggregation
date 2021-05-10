library(rgdal) # used to load the shapefiles
library(dplyr) # because I wouldn't leave home without it
library(tibble)

# Fonction nettoyante qui met en minuscule, enlève les accents
# remplace les espaces les ' et les - par _
clean <- function(x) {
  x <- tolower(x)
  x <- gsub("  "," ",x)
  x <- gsub("  "," ",x)
  #supprime l'espace en debut
  Nettoyage <- x[substr(x, 0, 1)==" "]
  x <- replace(x,substr(x, 0, 1)==" ",substr(Nettoyage, 2, nchar(Nettoyage)))
  #supprime l'espace en fin
  Nettoyage <- x[substr(x, nchar(x), nchar(x)+1)==" "]
  x <- replace(x,substr(x, nchar(x), nchar(x)+1)==" ", substr(Nettoyage, 1, nchar(Nettoyage)-1))
  x <- gsub("à|â|ä","a",x)
  x <- gsub("é|è|ê|ë","e",x)
  x <- gsub("î|ï","i",x)
  x <- gsub("ô|ö","o",x)
  x <- gsub("û|ü|ù","u",x)
  x <- gsub(" |-|'","_",x)
  return (x)
}

# Locations of sources data (csv, .shp)
if (!exists("FileStart")) {
  FileStart <- choose.files(, caption = "Choisir le fichier de départ",multi = FALSE)
}

# files we save
directory = paste(dirname(FileStart),"/../04_DEPARTEMENT/", sep="")
# create folder if not existing
if  (!dir.exists(directory)) {
  dir.create(file.path(directory))
}

fname = paste("Dep_",basename(FileStart), sep="")
OUTPUT <- paste(directory,fname, sep="")

# Choose settlement "1_REACH_NIGER/02_Project_Management/2019/2019_3Frontieres/GIS DATA/Settlements/3F"
if (!exists("settlementsSHP")) {
  settlementsSHP <-choose.files(, caption = "Choisir le fichier 3F_Settlements4",multi = FALSE)
}
settlements <- readOGR(settlementsSHP)
Assessed <- read.csv2(FileStart)

# Create a unique identifier for each department
Assessed$reg_dep <- clean(paste(Assessed$admin1,Assessed$admin2, sep = "_"))


# Extract only the unique department identifier of the whole settlement database
settlements <- settlements@data["reg_dep"]
settlements$reg_dep <-  clean(settlements$reg_dep)

#let us check how many settlements and KIs we have per commune
summary_assessed <- Assessed %>% group_by(reg_dep) %>% 
  summarise(settlement_assessed_num = length(reg_dep), ki_num= sum(B_ki_coverage))


#aggregate the commune by unique identifier and count the settlements
final_commune <- aggregate(x = settlements, by=list(names = settlements$reg_dep), FUN = length)

names(final_commune)[names(final_commune) == "reg_dep"] <- "settlement_dep_num"
names(final_commune)[names(final_commune) == "names"] <- "reg_dep"

# debut de la selection
DebSel <- c("reg_dep")

#============ Lancement du script avec les calculs
# Avant, ça marchait mais ça marche plus et je trouve pas pourquoi :
#PATH <- dirname(sys.frame(1)$ofile)
# ça marche avec ça :
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
PATH <- getwd()
ScriptCalcul = paste(PATH, "REACH_3F_Calcul_Aggregation.R", sep="/")

if (file.exists(ScriptCalcul)) {
  source(ScriptCalcul)
} else {
  ScriptCalcul<-choose.files(, caption = "Choisir le script de calcul",multi = FALSE)
  source(ScriptCalcul)
}


#join the aggregated fields and the number of KI/settlements into each commune

final_assessed <-left_join(Result_aggregated, summary_assessed, by = "reg_dep")

#join the final assessed table and the calculation of number of total settlement into each commune
final <-left_join(final_assessed, final_commune, by = "reg_dep")

#add a field to calculate percentage of communal 
final$dep_percentage <-final$settlement_assessed_num/final$settlement_dep_num

# recalcul le champ pdi_vivant_cond_adeq_no en fonction du nombre de localité qui ne sont pas SL...
pdi_noSL <- final$settlement_assessed_num*final$pdi_vivant_cond_adeq_noSL
pdi_no <- final$settlement_assessed_num*final$pdi_vivant_cond_adeq_no
final$pdi_vivant_cond_adeq_no <- pdi_no/pdi_noSL
final$pdi_vivant_cond_adeq_noSL <- NULL

  write.csv2(
    final,
    file = OUTPUT,
    na = "NA",
    row.names = FALSE)
