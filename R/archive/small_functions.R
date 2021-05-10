latin_to_utf8<-function(x, from="latin1", to="UTF-8"){Encoding(x) <- from;iconv(x, from, to,sub='')}

remove_blank_headings<-function(data){data[,names(data)!=""]}
remove_vars<-function(data,vars){data[,names(data) %!in%vars]}

package.check <-function(packages) lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

`%!in%` = Negate(`%in%`)

humanTime <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}

rec_missing<-function(x,missings=c(NULL,'NULL','N/A','n/a',999,998,888,' ','(vide)','d/m','','NA','na',""," ")) {
  x[x %in% missings] <- NA
  return(x)
}

rec_missing_all<-function(data){lapply(data,rec_missing) %>% bind_cols}

cleanheaders<-function(data){names(data)<-gsub("^X_","",names(data));names(data)<-gsub("^_","",names(data));names(data)<-gsub("\\/",".",names(data));return(data)}

prepdata<-function(data){data %>% cleanheaders %>% rec_missing_all %>% remove_blank_headings %>%type_convert}

ch<-as.character
chr<-as.character

label_clog<- function(clog,pathtosurvey,survey_label_col="label",choices_label_col="label"){
  
  questionnaire<-read_excel(pathtosurvey,"survey")
  choices<-read_excel(pathtosurvey,"choices")
  question.name_label <- match(clog[["question.name"]], questionnaire[["name"]])
  old.value_label <- match(clog[["old.value"]], choices[["name"]])
  new.value_label <- match(clog[["new.value"]], choices[["name"]])
  if.other.text.entry_label <- match(clog[["if.other.text.entry"]], questionnaire[["name"]])
  other.text.var_label <- match(clog[["other.text.var"]], choices[["name"]])
  choice_labels <- choices[[choices_label_col]]
  question_labels <- questionnaire[[survey_label_col]]
  
  labeled_clog <- clog %>%
    mutate(question.name_label = ifelse(is.na(question.name_label),question.name,question_labels[question.name_label]),
           old.value_label = ifelse(is.na(old.value_label),old.value,choice_labels[old.value_label]),
           new.value_label = ifelse(is.na(new.value_label),new.value,choice_labels[new.value_label]),
           if.other.text.entry_label = ifelse(is.na(if.other.text.entry_label),if.other.text.entry,question_labels[if.other.text.entry_label]),
           other.text.var_label = ifelse(is.na(other.text.var_label),other.text.var,choice_labels[other.text.var_label]))
  
  vars<-c("today","base","enumerator","uuid","question.name","question.name_label","old.value","old.value_label","new.value","new.value_label","if.other.text.entry","if.other.text.entry_label","other.text.var","other.text.var_label")
  labeled_clog<-labeled_clog %>% select(all_of(vars),everything())
  
  return(labeled_clog)
}

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
  # x <- gsub(" |-|'","_",x)
  return (x)
}
