devtools::install_github("gadenbuie/shinyThings")
remotes::install_github("daqana/dqshiny")
install.packages("rhandsontable")
library(shiny)
library(dqshiny)
library(data.table)

dico <-readRDS("dico.Rds")
entries_for_search<-lapply(dico, function(x)x$entry_for_search)
entries<-lapply(dico, function(x)x$entry)
rules<-data.table::fread("rules2.csv")
latin_to_french <- function(word, rules = rules2){
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
latin_to_french2 <- function(word, rules = rules2){
  word <- tolower(word)
  dt_ans <- data.table()
  ans <-  paste0("Starting from ", word)
  for(i in seq_len(dim(rules)[1])){
    if(grepl(rules[i,"Pattern"], word,perl=TRUE)){
      word <- gsub(rules[i,"Pattern"],rules[i,"Replacement"], word,perl=TRUE)
      dt_ans <- rbind(
        dt_ans,
        data.table(rule_id = i, date = rules[i]$Date, explanation = rules[i]$Explanation, word = word)
      )
  }}
  dt_ans
}
pretty_print <- function(dt){
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
    kable_styling()

  
  }


shinyApp(
  ui =
    navbarPage(
      title = "From Latin to French",
      tabPanel(
        "Transformation",
        fluidPage(
          fluidRow(
            shinyThings::radioSwitchButtons(
              "choice",
              choices = c(
                "Examples",
                "Search in dictionary")
              )
            ),
          fluidRow(
            htmlOutput("selection")
          ),
          fluidRow(
            htmlOutput("meaning")
          ),
          fluidRow(
            column(10,
                   tags$label("Transformation:"),
                   tags$head(
                     tags$style(
                       "#meaning{color:red; font-size:12px; font-style:italic; overflow-y:scroll; max-height: 250px; background: ghowtwith;}")),
                   tableOutput("transfo"),
                   tags$style( "#transfo {white-space: pre-wrap;}")
            )
          )
        )
      ),
      tabPanel(
        "Transformation rules",
        dq_handsontable_output("all_rules", 9L)
      )
    ),
  server = function(input, output) {
    output$selection <-
      renderUI({
        if(input$choice == "Examples"){
          column(
            3,
            selectInput(
              inputId = "examples",
              label = "Examples",
              selected = "c\u0101\u0301m\u0115ra",
              choices = c(
                          "b\u0115\u0301ll\u014ds",#ok
                          "b\u0115\u0301n\u0115",
                          "b\u014f\u0301v\u0115",
                          "c\u0101\u0301m\u0115ra",
                          "cant\u0101\u0301t\u016D",
                          "c\u0101\u0301p\u016D",
                          "c\u0101\u0301r\u016D",
                          "c\u014f\u0301m\u012dte",
                          "c\u014f\u0301mp\u016Dtat",
                          "fa\u0301ct\u016D",
                          "f\u012d\u0301de",
                          "f\u012b\u0301l\u012d\u016Ds",
                          "f\u014f\u0301l\u012d\u0103",
                          "h\u014f\u0301sp\u012dte",
                          "l\u0115\u0301ct\u016D",
                          "ma\u0301los",
                          "ma\u0301los",
                          "mat\u016B\u0301r\u016D",
                          "merc\u0113\u0301de",
                          "nau\u0301s\u0115a",
                          "n\u0115p\u014d\u0301te",
                          "n\u016B\u0301d\u016D",
                          "p\u0115\u0301de",
                          "pla\u0301n\u016D",
                          "p\u014f\u0301tet",
                          "pr\u0103\u0301t\u016D",
                          "pr\u0103t\u016D",
                          "p\u016D\u0301gn\u016D",#ok
                          "t\u0113\u0301ct\u016D",#ok
                          "t\u0115\u0301n\u0115r\u016D",#ok
                          "v\u012b\u0301ta",
                          "v\u012d\u0301nc\u0115re")

            )
          )
        } else {
            column(
              3,
              autocomplete_input(
                "auto2",
                "Search in dictionary:",
                max_options = 1000,
                value="capra",
                structure(entries, names = entries_for_search))
            )

        }
      })
    output$meaning <-
      renderUI({
        req(input$choice)
        if(input$choice != "Examples"){
          req(input$auto2)
        fluidRow(
          column(10,
                 tags$label("Meaning:"),
                 tags$head(
                   tags$style(
                     "#meaning{color:black; font-size:12px; font-style:italic; overflow-y:scroll; max-height: 250px; background: ghowtwith;}")),
                 paste0(unlist(dico[entries==input$auto2][[1]]$senses), collapse="    ")
          )
        )} else {NULL}
      })
      # renderPrint({
      #   req(input$choice)
      #   if(input$choice != "Examples"){
      #     dico[entries==input$auto2][[1]]$senses}
      #     })
    output$transfo <- function() {
      req(input$choice)
      if(input$choice == "Examples"){
        pretty_print(
          latin_to_french2(
            input$examples,
            rules = rules)
        )
      } else {
        pretty_print(
          latin_to_french2(
            input$auto2,
            rules = rules)
        )
      }
    }
    
    dq_render_handsontable(
      "all_rules",
      rules[
        ,.(
          Date=
            fifelse(
              is.infinite(
                Date),
              "Initialization",
              as.character(as.integer(Date))), Explanation)],
      cols_param=list(colWidths = c(2.2,12)))

  }
)
