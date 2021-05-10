library(rgdal) # used to load the shapefiles
library(dplyr) # because I wouldn't leave home without it
library(tibble)

# Fonction nettoyante qui met en minuscule, enlève les accents
# remplace les espaces, les ' et les - par _
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
  x <- gsub("'","_",x)
  return (x)
}

# Locations of sources data (csv, .shp)



if (!exists("FileStart")) {
  FileStart <- choose.files(, caption = "Choisir le fichier de départ",multi = FALSE)
}
Assessed <- read.csv2(FileStart)

# files we save
directory = paste(dirname(FileStart),"/../03_COMMUNE/", sep="")
# create folder if not existing
if  (!dir.exists(directory)) {
  dir.create(file.path(directory))
}

fname = paste("Com_",basename(FileStart), sep="")
OUTPUT <- paste(directory,fname, sep="")

#-------- fichier des localites geographiques en geopackage
LocaliteGPK <- "Z:\\1_REACH_NIGER\\02_Project_Management\\2021\\2021_3Frontieres\\10_GIS\\GIS_Data\\Localites\\Localites_3F.gpkg"
LocaliteLayer <- "Localites_3F"
print("----- BD Localite")
print(LocaliteGPK)

# ouverture du fichier des localites
if (file.exists(LocaliteGPK)){
  settlements <- readOGR(LocaliteGPK, layer = LocaliteLayer)
}else{
  stop("====== Ou est la couche des localites ?")
}


# Create a unique identifier for each commune
Assessed$dep_com <- clean(paste(Assessed$admin2,Assessed$admin3, sep = "_"))

# Extract only the unique communal identifier of the whole settlement database
settlements <- settlements@data["dep_com"]

#let us check how many settlements and KIs we have per commune
summary_assessed <- Assessed %>% group_by(dep_com) %>% 
  summarise(settlement_assessed_num = length(dep_com), ki_num= sum(B_ki_coverage))


#aggregate the commune by unique identifier and count the settlements
final_commune <- aggregate(x = settlements, by=list(names = settlements$dep_com), FUN = length)

names(final_commune)[names(final_commune) == "dep_com"] <- "settlement_com_num"
names(final_commune)[names(final_commune) == "names"] <- "dep_com"

# debut de la selection
DebSel <- c("dep_com")

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

final_assessed <-left_join(Result_aggregated, summary_assessed, by = "dep_com")

#join the final assessed table and the calculation of number of total settlement into each commune
final <-left_join(final_assessed, final_commune, by = "dep_com")

#add a field to calculate percentage of communal 
final$com_percentage <-final$settlement_assessed_num/final$settlement_com_num

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
