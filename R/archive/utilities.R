'%!in%' = Negate('%in%')

humanTime <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}

package.check <-function(packages) lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

# Fonction nettoyante qui met en minuscule, enl?ve les accents
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
  # x <- gsub("?|?|?","a",x)
  # x <- gsub("?|?|?|?","e",x)
  # x <- gsub("?|?","i",x)
  # x <- gsub("?|?","o",x)
  # x <- gsub("?|?|?","u",x)
  x <- gsub(" |-|'","_",x)
  return (x)
}


settlement_agg<-function(db,by,champs,fct){
  if(length(champs)>0){
  db %>%
    group_by_at(by) %>%
    summarize_at(all_of(champs), ~fct(.))
  } else{db[!duplicated(db[,by]),][,by]}
}

re_create_sm <- function(df, multiple_choices, separator){
  pb<-progress_bar$new(total = length(multiple_choices))
  #Loop throughout received questions
  for (m in 1:length(multiple_choices)) {
    pb$tick()
    choice_char_count <- nchar(multiple_choices[m]) + 2
    choices <- df %>% select(starts_with(paste0(multiple_choices[m], separator)))
    #Loop throughout choices of each question
    for (c in 1:length(choices)) {
      choice_name <- substring(names(choices[c]),choice_char_count)
      #Loop throughout rows of each choice
      for (r in 1:nrow(choices)) {
        if (!is.na(choices[r,c]) & as.numeric(choices[[c]][r]) == 1 & choices[[c]][r] != "NC") {
          temp <- str_detect(df[[multiple_choices[m]]][r], choice_name)
          if(!is.na(temp) & !temp){
            #Insert the choice name to parent question
            df[r, multiple_choices[m]] <- paste(df[[multiple_choices[m]]][r], choice_name, sep = " ")
            df[r, multiple_choices[m]] <- gsub("NC ","",df[r, multiple_choices[m]])
          }
        }
      }
    }
    # cat("\014")
    # print (paste("Creating parent ", m, "of", length(multiple_choices)))
  }
  return(df)
}

sm_label_toxml <- function(df, multiple_choices, separator){
  pb<-progress_bar$new(total = length(multiple_choices))
  for (m in 1:length(multiple_choices)) {
    pb$tick()
    choice_char_count <- nchar(multiple_choices[m]) + 2
    choices <- df %>% select(starts_with(paste0(multiple_choices[m], separator)))
    df[[multiple_choices[m]]]<-""
    for (c in 1:length(choices)) {
      choice_name <- substring(names(choices[c]),choice_char_count)
      for (r in 1:nrow(choices)) {
        if (!is.na(choices[r,c]) & as.numeric(choices[[c]][r]) == 1 & choices[[c]][r] != "NC") {
          if(df[[multiple_choices[m]]][r]==""){
            df[r, multiple_choices[m]] <- choice_name
            df[r, multiple_choices[m]] <- gsub("NC ","",df[r, multiple_choices[m]])
          } else{
            df[r, multiple_choices[m]] <- paste(df[[multiple_choices[m]]][r], choice_name, sep = " ")
            df[r, multiple_choices[m]] <- gsub("NC ","",df[r, multiple_choices[m]])
          }
        }
      }
    }
  }
  return(df)
}

sl_correction<-function(db,sl_definition,multiple_choices,sl_name="name",sl_condition="condition"){
  pb<-progress_bar$new(total = nrow(sl_definition))
          for (i in 1:nrow(sl_definition)) {
            pb$tick()
            qname<-sl_definition[[sl_name]][i]
            if(qname%in%names(db)){
              db[[qname]]<-ifelse(eval(parse(text=sl_definition[[sl_condition]][[i]]),envir = db),db[[qname]],"SL")
              if(qname%in%multiple_choices){
                sm<-names(db)[str_detect(names(db),paste0(qname,"."))]
                for(j in 1:length(sm)){
                  db[[sm[j]]]<-ifelse(db[[qname]]=="SL","SL",db[[sm[j]]])
                }
              }
            }
          }
            return(db)
}

from_xml_tolabel<-function(db,choices,survey,multiple_choices){
  chr_names<-db %>% select_if(~ !(all(is.na(.x)) | all(. == ""))) %>% select_if(~ is.character(.)) %>% names
  chr_names<-chr_names[!str_detect(chr_names,paste(paste0(multiple_choices,"."),collapse = "|"))]
  names(choices)<-gsub(":.*","",names(choices))
  names(survey)<-gsub(":.*","",names(survey))
  choice_labels <- choices[["label"]]
  survey_labels <- survey[["label"]]
  
  pb<-progress_bar$new(total =length(chr_names))
  pb$tick(0)
  for (i in 1: length(chr_names)){
    pb$tick()
    if(chr_names[i]%in%multiple_choices){
      split_sm<-str_split(db[[chr_names[i]]]," ")
      db[[chr_names[i]]]<-lapply(split_sm, function(x)match(x, choices[["name"]])) %>% 
        lapply(.,function(x){ifelse(is.na(x),x,choice_labels[x])}) %>% lapply(., function(x)paste(x,collapse = " ")) %>% unlist
    } else{
      var_label <- match(db[[chr_names[i]]], choices[["name"]])
      db[[chr_names[i]]]<-ifelse(is.na(var_label),db[[chr_names[i]]],choice_labels[var_label])
    }
  }
  names(db)<-gsub(".*[.]","",names(db))
  label_indices<-match(names(db),survey[["name"]])
  names(db)<-ifelse(is.na(label_indices)|is.na(survey_labels[label_indices]),names(db),survey_labels[label_indices])
  choices_indices<-match(names(db),choices[["name"]])
  names(db)<-ifelse(is.na(choices_indices),names(db),choice_labels[choices_indices])
  return(db)
}

mean_agg<-function(db,first_lev,second_lev,groupSel){
  db[["agg_col"]]<-paste0(db[[first_lev]],"_",db[[second_lev]])
  res<- db %>% group_by(agg_col) %>% select(all_of(groupSel)) %>% summarise_all(~list(mean(.,na.rm=T)))
  return(res)
}

from_label_toxml<-function(db,choices,survey,multiple_choices){
  chr_names<-db %>% select_if(~ !(all(is.na(.x)))) %>% select_if(~ is.character(.)) %>% names
  chr_names<-chr_names[!str_detect(chr_names,paste(paste0(multiple_choices,"."),collapse = "|"))]
  chr_names<-chr_names[which(chr_names%!in%multiple_choices)]
  names(choices)<-gsub(":.*","",names(choices))
  names(survey)<-gsub(":.*","",names(survey))
  choice_xml <- choices[["name"]]
  survey_xml <- survey[["name"]]
  pb<-progress_bar$new(total =length(chr_names))
  pb$tick(0)
  for (i in 1: length(chr_names)){
    pb$tick()
    var_xml <- match(db[[chr_names[i]]], choices[["label"]])
    db[[chr_names[i]]]<-ifelse(is.na(var_xml),db[[chr_names[i]]],choice_xml[var_xml])
  }
  return(db)
}

multiple_choices <- c(
  "groupes_presents",
  "pas_nourriture_raison",
  "strat_survie",
  "revenu_source",
  "activites_actuelles_non",
  "activite_agricole_perturbe",
  "services_sante_niger",
  "services_sante_burkina",
  "services_sante_mali",
  "nutri_programme",
  "population_inquietudes",
  "besoin_bna",
  "bna_pas_dispo",
  "aap"
)