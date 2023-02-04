library("data.table")
install.packages("kableExtra")
library("kableExtra")
dt <- latin_to_french2("lugdunum", rules=rules)
rules[91]
gsub("u\\b", "o", "lugdunu")
pretty_print(dt)
dt[, century:=fcase(
  is.infinite(date), "Preliminaries",
  date < 100, "1st century AD",
  date < 200, "2nd century AD",
  date < 300, "3rd century AD",
  date>=300, paste0(floor(date/100)+1,"th century AD")
)]

dt <- dt[data.table(century = c("Preliminaries","1st century AD",
                       "2nd century AD","3rd century AD",
           paste0(4:18, "th century AD"))),on =.(century)]

dt[,period:=fcase(
  is.infinite(date), "Preliminaries",
  century %chin% c("1st century AD","2nd century AD","3rd century AD"),
  "Common Romance Transformation",
  default = "French transformations")]

dt2 <- dt[,.(century, explanation, word, rule_id=as.character(rule_id))][,n:=seq_len(.N),century][n!=1, century:=""][,n:=NULL]
dt2[is.na(explanation), explanation:=""][is.na(word), word:=""][is.na(rule_id),rule_id:=""]

dt2[,note:=
      fifelse(century %chin% c("9th century AD","10th century AD"), "orthograph fixation", "")]
kable(dt2, format.args=list(na.encode=TRUE)) %>%
  pack_rows(
    group_label = "Preliminaries",
    start_row = min(which(dt$period == "Preliminaries")),
    end_row = max(which(dt$period == "Preliminaries")),
    background = "violet") %>%
  row_spec(row = which(dt$period == "Preliminaries"), background = "violet") %>%
  pack_rows(
    group_label = "Common Romance Transformation",
    start_row = min(which(dt$period == "Common Romance Transformation")),
    end_row = max(which(dt$period == "Common Romance Transformation")),
    background = "orange") %>%
  row_spec(row = which(dt$period == "Common Romance Transformation"), background = "orange") %>%
  pack_rows(
    group_label = "French transformations",
    start_row = min(which(dt$period == "French transformations")),
    end_row = max(which(dt$period == "French transformations")),
    background = "skyblue") %>%
  column_spec(which(names(dt2)=="note"), bold = TRUE) %>%
  row_spec(row = which(dt$period == "French transformations"), background = "skyblue") %>%
  kableExtra::kable_styling()

