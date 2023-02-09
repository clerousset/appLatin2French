# devtools::install_github("gadenbuie/shinyThings")
# remotes::install_github("daqana/dqshiny")
# install.packages("rhandsontable")
# install.packages("kableExtra")
library(kableExtra)
library(shiny)
library(dqshiny)
library(data.table)
ipa2kirsh <-
  data.table(
    ipa = c("\u025b","\u0254","\u02C8","\u0259","\u0303","\u03b2","\u03b3",
            "\u03b4","\u03b8","\u0292","\u0283","\u00f8","\u0153","\u0265",
            "\u026b","\u02b7","\u0280","\u00e7","\u014b"),
    kirshenbaum = c("E","O","'","@","~","B","Q","D","T","Z","S","Y","W","j<rnd>",
                    "L","<w>","R","C","N")
  )

list_example <- 
  data.table(latin = c(
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
  "mat\u016B\u0301r\u016D",
  "merc\u0113\u0301de",
  "nau\u0301s\u0115a",
  "n\u0115p\u014d\u0301te",
  "n\u016B\u0301d\u016D",
  "p\u0115\u0301de",
  "pla\u0301n\u016D",
  "p\u014f\u0301tet",
  "pr\u0103\u0301t\u016D",
  "p\u016D\u0301gn\u016D",#ok
  "t\u0113\u0301ct\u016D",#ok
  "t\u0115\u0301n\u0115r\u016D",#ok
  "v\u012b\u0301ta",
  "v\u012d\u0301nc\u0115re"),
  french = c(
    "beau [bo] (en : beautiful)",
    "bien [bjɛ\u0303] (en : good)",
    "boeuf [b\u00f8] (en : ox)",
    "chambre [\u0283a\u0303b\u0280] (en : room)",
    "chanté [\u0283a\u0303te] (en : sung)",
    "chef [\u0283\u025bf] (en : chief)",
    "cher [\u0283\u025b\u0280] (en : expensive, dear)",
    "compte [] (en :  ",
    "conte",
    "fait",
    "foi [fwa] (en : faith)","","",
    "hôte [ot] (en : host)",
    "lit [li] (en : bed)",
    "",
    "mûr [my\u0280] (en : mature)",
    "merci [m\u025b\u0280si] (en : thanks)",
    "noise [nwaz] (en : quarrel)",
    "neveu [n\u0259v\u00f8] (en : nephew)",
    "nu [ny] (en : nude)",
    "pied [pje] (en : foot)",
    "plein [pl\u025b\u0303] (en : full)",
    "peut [p\u00f8] (en : (he) can)",
    "pré [p\u0280e] (en : meadow)",
    "poing [pw\u025b\u0303] (en : fist)",
    "toit [twa] (en : roof)",
    "tendre [ta\u0303d\u0280] (en : tender)",
    "vie [vi] (en : life)",
    "vaincre [v\u025b\u0303k\u0280] (en : overcome)"
    ))

dico <-readRDS("dico.Rds")
entries_for_search<-lapply(dico, function(x)x$entry_for_search)
entries<-lapply(dico, function(x)x$entry)
rules<-data.table::fread("rules2.csv")
rules[,rule_id := 1:.N]
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
latin_to_french2 <- function(word, rules_ = rules, exceptions = character(0), ipa2kirsh_=ipa2kirsh){
  word <- tolower(word)
  dt_ans <- data.table()
  #ans <-  paste0("Starting from ", word)
  rules_ <- rules_[!rule_id %in% exceptions]
  for(i in seq_len(dim(rules_)[1])){
    if(grepl(rules_[i,"Pattern"], word, perl = TRUE)){
      word_to_print <- gsub(rules_[i,"Pattern"],paste0("<strong>",rules_[i,"Replacement"],"</strong>"), word,perl=TRUE)
      word <- gsub("</strong>","",gsub("<strong>","",word_to_print))
      kirshenbaum = paste(
        ipa2kirsh[
          as.data.table(
            strsplit(
              word,
              split="")
          ),
          on=.(ipa=V1)][
            is.na(kirshenbaum),
            kirshenbaum:=ipa]$kirshenbaum,
        collapse = "")
      dt_ans <- rbind(
        dt_ans,
        data.table(rule_id = rules_[i]$rule_id, date = rules_[i]$Date,
                   explanation = rules_[i]$Explanation, word = word,
                   word_to_print = word_to_print, kirshenbaum = kirshenbaum)
      )
  }}
  dt_ans
  
}
pretty_print <- function(dt, row_highlight=0, sound = TRUE){
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
    century == "Preliminaries", "Preliminaries",
    century %chin% c("1st century AD","2nd century AD","3rd century AD"),
    "Common Romance Transformation",
    default = "French transformations")]
  
  dt2 <- dt[,.(period, century, explanation, word_to_print, rule_id=as.character(rule_id))][,n:=seq_len(.N),century][n!=1, century:=""][,n:=NULL]
  dt2[is.na(explanation), explanation:=""][is.na(word_to_print), word_to_print:=""][is.na(rule_id),rule_id:=""]
  dt2[century == "Preliminaries", century:=""]
  dt2[,note:=
        fifelse(century %chin% c("9th century AD","10th century AD"), "orthograph fixation", "")]
  dt2 <- dt2[!(period=="Preliminaries" & explanation=="")]
  
  dt2 = rbind(
    dt2[period == "Preliminaries"],
    data.table(century="Starting from : ",
               period = "Preliminaries",
               explanation = "",
               word_to_print = tail(dt2[period == "Preliminaries"]$word_to_print,1),
               rule_id = "", note = ""),
    dt2[period != "Preliminaries"])
  
  index_prelim <- which(dt2$period == "Preliminaries")
  index_common <- which(dt2$period == "Common Romance Transformation")
  index_french <- which(dt2$period == "French transformations")
  if(sound){
    dt2[c(
      ifelse(length(index_prelim)==0,integer(0),max(index_prelim)),
      index_common,
      index_french) & word_to_print!="", sound:=
          paste0(
            "<a onclick='this.firstChild.play()'>
            <audio src='Effect.mp3'>
            </audio>▸</a>")]
    
    dt2[is.na(sound), sound:=""]
  }
  
  res <- kbl(
      dt2[,!"period"],
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
    res %>% column_spec(which(names(dt2)=="note")-1, bold = TRUE) %>%
      # cell_spec(c(3,2), format= "html")%>%
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
            # a("<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-github" viewBox="0 0 16 16">
            #     <path d="M8 0C3.58 0 0 3.58 0 8c0 3.54 2.29 6.53 5.47 7.59.4.07.55-.17.55-.38 0-.19-.01-.82-.01-1.49-2.01.37-2.53-.49-2.69-.94-.09-.23-.48-.94-.82-1.13-.28-.15-.68-.52-.01-.53.63-.01 1.08.58 1.23.82.72 1.21 1.87.87 2.33.66.07-.52.28-.87.51-1.07-1.78-.2-3.64-.89-3.64-3.95 0-.87.31-1.59.82-2.15-.08-.2-.36-1.02.08-2.12 0 0 .67-.21 2.2.82.64-.18 1.32-.27 2-.27.68 0 1.36.09 2 .27 1.53-1.04 2.2-.82 2.2-.82.44 1.1.16 1.92.08 2.12.51.56.82 1.27.82 2.15 0 3.07-1.87 3.75-3.65 3.95.29.25.54.73.54 1.48 0 1.07-.01 1.93-.01 2.2 0 .21.15.46.55.38A8.012 8.012 0 0 0 16 8c0-4.42-3.58-8-8-8z"/>
            #     </svg>", href="https://github.com/clerousset/applatin2french"),
           p("Pronounciation of Latin changed during time especially in
              today's French territory. General transformation rules have been dated  
              by linguists. The one explained in the book have been coded using
              regular expression github link. The examples of the book are presented
             below. A Latin dictionary is also connected allowing to see the
             natural transformation of any Latin word. Some rules may be skipped with
             the exception input")
          
          ),
          fluidRow(
            shinyThings::radioSwitchButtons(
              "choice",
              choices = c(
                "Examples",
                "Search in dictionary")
              )
            ),
          fluidRow(
            htmlOutput("selection"), htmlOutput("example_explained")
          ),
          fluidRow(
            htmlOutput("meaning")
          ), 
          fluidRow(
            column(
              6,
              selectInput(
                "exceptions",
                "Exceptions : enter the rule ids you want to skip",
                choices = 1:nrow(rules),
                multiple = TRUE)
            ),
            column(6, checkboxInput("sound", "Add sound", value=TRUE))
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
              choices = list_example$latin)
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
    output$example_explained <- renderUI({
      req(input$choice)
      if(input$choice == "Examples"){
        req(input$examples)
    column(6, h3("Latin word ", strong(input$examples), "becomes French word", 
    strong(list_example[latin == input$examples]$french)))}
      else {
        req(input$auto2)
        column(6,
               h3(
                 "Natural transformation of Latin word ",
                 strong(input$auto2),
                 " following the phonetic rules"))
                         }})
    output$meaning <-
      renderUI({
        req(input$choice)
        if(input$choice != "Examples"){
          req(input$auto2)
        fluidRow(
          column(9,
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
    table <- reactive({
      req(input$choice)
      if(input$choice == "Examples"){
        req(input$examples)
          latin_to_french2(
            input$examples,
            rules = rules,
            exceptions = input$exceptions
          )
        
      } else {
        req(input$auto2)
          latin_to_french2(
            input$auto2,
            rules = rules,
            exceptions = input$exceptions
          )
      }
    })
    # row_to_play = reactiveVal(0)
    # observeEvent(input$play,{
    #   row_to_play(row_to_play()+1) # increment x by 1
    # })

    output$transfo <- function() {
      req(table())
      #req(input$sound)
     # req(row_to_play())
      # if(input$sound){
      #   #create_sounds()    save mp3 to wwww/
      # }
      pretty_print(table(), 
                   #row_highlight = row_to_play(),
                   sound = input$sound)}

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
