latin_to_french <- function(word, rules){
  word <- tolower(word)
  ans <-  paste0("Starting from ", word)
  for(i in seq_len(dim(rules)[1])){
    if(grepl(rules[i,"Pattern"], word,perl=TRUE)){
      word <- gsub(rules[i,"Pattern"],rules[i,"Replacement"], word,perl=TRUE)
      ans <-
        paste0(
          ans,
          "\n rule n°",i ," :",ifelse(rules[i,"Date"]!=-Inf,paste0(" at ",rules[i,"Date"]),"") ," ", rules[i, "Explanation"] , " => " , word)
    }
    
  }
  ans
}

#' @export
latin_to_french2 <- function(word, rules_, exceptions = character(0), ipa2kirsh_){
  word <- tolower(word)
  dt_ans <- data.table()
  #ans <-  paste0("Starting from ", word)
  rules_ <- rules_[!rule_id %in% exceptions]
  for(i in seq_len(dim(rules_)[1])){
    if(grepl(rules_[i,"Pattern"], word, perl = TRUE)){
      word_to_print <- gsub(rules_[i,"Pattern"],paste0("<strong>",rules_[i,"Replacement"],"</strong>"), word,perl=TRUE)
      word <- gsub("</strong>","",gsub("<strong>","",word_to_print))
      
      base <-   ipa2kirsh_[
        as.data.table(
          strsplit(
            word,
            split="")
        ),
        on=.(ipa=V1)][
          is.na(kirshenbaum),
          `:=`(kirshenbaum=ipa, for_filename=ipa)]
      
      kirshenbaum = paste(base$kirshenbaum, collapse="")
      for_filename = paste(base$for_filename, collapse="")      
      
      dt_ans <- rbind(
        dt_ans,
        data.table(rule_id = rules_[i]$rule_id, date = rules_[i]$Date,
                   explanation = rules_[i]$Explanation, word = word,
                   word_to_print = word_to_print, kirshenbaum = kirshenbaum,
                   for_filename = for_filename)
      )
    }}
  dt_ans
  
}

#' @export
pretty_print <- function(dt, row_highlight=0, sound = TRUE){
  dt[, century:=fcase(
    is.infinite(date), "Preliminaries",
    date < 0, "1st century BC",
    date < 100, "1st century AD",
    date < 200, "2nd century AD",
    date < 300, "3rd century AD",
    date>=300, paste0(floor(date/100)+1,"th century AD")
  )]
  
  dt <- dt[data.table(century = c("Preliminaries","1st century BC","1st century AD",
                                  "2nd century AD","3rd century AD",
                                  paste0(4:18, "th century AD"))),on =.(century)]
  
  dt[,period:=fcase(
    century == "Preliminaries", "Preliminaries",
    century %chin% c("1st century BC","1st century AD","2nd century AD","3rd century AD"),
    "Common Romance Transformation",
    default = "French transformations")]
  
  dt2 <- dt[
    ,.(period, century, explanation, word_to_print, for_filename,
       rule_id=as.character(rule_id))][,n:=seq_len(.N),century][
         n!=1, century:=""][,n:=NULL]
  dt2[is.na(explanation), explanation:=""][
    is.na(word_to_print), word_to_print:=""][is.na(rule_id),rule_id:=""]
  dt2[century == "Preliminaries", century:=""]
  dt2[,note:=
        fifelse(century %chin% c("9th century AD","10th century AD"),
                "orthograph fixation", "")]
  dt2 <- dt2[!(period=="Preliminaries" & explanation=="")]
  
  dt2 = rbind(
    dt2[period == "Preliminaries"],
    data.table(century="Starting from : ",
               period = "Preliminaries",
               explanation = "",
               word_to_print = tail(dt2[period == "Preliminaries"]$word_to_print,1),
               rule_id = "", note = "",
               for_filename= tail(dt2[period == "Preliminaries"]$for_filename,1)),
    dt2[period != "Preliminaries"])
  
  index_prelim <- which(dt2$period == "Preliminaries")
  index_common <- which(dt2$period == "Common Romance Transformation")
  index_french <- which(dt2$period == "French transformations")
  if(sound){
    dt2[(period!="Preliminaries" | century == "Starting from : ") & word_to_print!="", sound:=
          paste0(
            "
            <audio 
            controls
            src='", for_filename,".wav'>
            </audio>")]
    
    dt2[is.na(sound), sound:=""]
  }
  
  res <- kbl(
    dt2[,!c("period","for_filename")],
    format.args=list(na.encode=TRUE), escape = FALSE) 
  if(length(index_prelim)>0){
    res <- res %>% pack_rows(
      group_label = "Preliminaries",
      start_row = min(index_prelim),
      end_row = max(index_prelim),
      background = "plum") %>%
      row_spec(row = index_prelim, background = "plum")}
  if(length(index_common)>0){
    res <- res %>% 
      pack_rows(
        group_label = "Common Romance Transformation",
        start_row = min(index_common),
        end_row = max(index_common),
        background = "lightsalmon") %>%
      row_spec(row = index_common, background = "lightsalmon")}
  if(length(index_french)>0){
    res <- res %>% pack_rows(
      group_label = "French transformations",
      start_row = min(index_french),
      end_row = max(index_french),
      background = "skyblue") %>%
      row_spec(row = index_french, background = "skyblue")}
  
  if(row_highlight!=0){
    res <- res %>% row_spec(
      row=c(
        which(dt2$century=="Starting from"),
        which(dt2$period!="Preliminaries" & dt2$word!=""))[row_highlight],
      background = "lightgreen",
      bold = TRUE)
  }
  #res %>% column_spec(which(names(dt2)=="note")-1, bold = TRUE) %>%
  # cell_spec(c(3,2), format= "html")%>%
  res %>% kable_styling()
}

#' @export
create_sounds <- function(dt){
  if(!dir.exists("www")){dir.create("www")}
  liste_pho <- rbind(
    tail(dt[is.infinite(date)],1),
    dt[!is.infinite(date)])[,.(kirshenbaum, for_filename)]
  for(k in seq_len(nrow(liste_pho))){
    system(paste0("espeak -w www/",liste_pho[k]$for_filename,".wav -v mb-fr1 \"[[",liste_pho[k]$kirshenbaum,"]]\""))
  }
}