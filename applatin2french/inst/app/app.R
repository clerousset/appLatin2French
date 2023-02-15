# devtools::install_github("gadenbuie/shinyThings", upgrade = "never")
# remotes::install_github("daqana/dqshiny", upgrade = "never")
# install.packages("rhandsontable", upgrade = "never")
# install.packages("kableExtra", upgrade = "never")
library(kableExtra)
library(shiny)
library(dqshiny)
library(data.table)
library(applatin2french)
ipa2kirsh <-
  data.table(
    ipa = c("\u025b","\u0254","\u02C8","\u0259","\u0303","\u03b2","\u03b3",
            "\u03b4","\u03b8","\u0292","\u0283","\u00f8","\u0153","\u0265",
            "\u026b","\u02b7","\u0280","\u00e7","\u014b","\u032c", "\u02D0",
            "\u0251"),
    kirshenbaum = c("E","O","'","@","~","B","Q","D","T","Z","S","Y","W","j<rnd>",
                    "L","<w>","R","C","N",";",":","A")
  )
ipa2kirsh[kirshenbaum!=make.names(kirshenbaum), for_filename := as.character(1:.N)]
ipa2kirsh[is.na(for_filename), for_filename := kirshenbaum]

list_example <-
  data.table(latin = c(
    "b\u0115ll\u014ds",#ok
    "b\u0115n\u0115",
    "b\u014fv\u0115",
    "c\u0101m\u0115ra",
    "cant\u0101t\u016D",
    "c\u0101p\u016D",
    "c\u0101r\u016D",
    "c\u014fm\u012dte",
    "c\u014fmp\u016Dtat",
    "fact\u016D",
    "f\u012dde",
    "f\u012bl\u012d\u016Ds",
    "f\u014fl\u012d\u0103",
    "h\u014fsp\u012dte",
    "l\u0115ct\u016D",
    "malos",
    "mat\u016Br\u016D",
    "merc\u0113de",
    "naus\u0115a",
    "n\u0115p\u014dte",
    "n\u016Bd\u016D",
    "p\u0115de",
    "plan\u016D",
    "p\u014ftet",
    "pr\u0103t\u016D",
    "p\u016Dgn\u016D",#ok
    "t\u0113ct\u016D",#ok
    "t\u0115n\u0115r\u016D",#ok
    "v\u012bta",
    "v\u012dnc\u0115re"),
    expected_kirshenbaum = c(
      "b'o",
      "bj'E~",
      "b'Y",
      "S'A~bR",
      "SA~t'e",
      "S'Ef",
      "S'ER",
      "k'O~t",
      "k'O~t",
      "f'E",
      "fw'a",
      "f'is",
      "f'Wj",
      "'ot",
      "l'i",
      "m'o",
      "m'yR",
      "mERS'i",
      "nw'az",
      "n@v'Y",
      "n'y",
      "pj'e",
      "pl'E~",
      "p'Y",
      "pR'e",
      "pw'E~",
      "tw'a",
      "t'A~dR",
      "v'i",
      "v'E~kR"
    ),
    french = c(
      "beau [bo] (en : beautiful)",
      "bien [bjɛ\u0303] (en : good)",
      "boeuf [b\u00f8] (en : ox)",
      "chambre [\u0283a\u0303b\u0280] (en : room)",
      "chanté [\u0283a\u0303te] (en : sung)",
      "chef [\u0283\u025bf] (en : chief)",
      "cher [\u0283\u025b\u0280] (en : expensive, dear)",
      "comte [k\u0254\u0303] (en : county)",
      "compte [k\u0254\u0303] (en : account)",
      "fait [f\u025bt] (en : fact)",
      "foi [fwa] (en : faith)",
      "fils [fis] (en : son)",
      "feuille [f\u0153y] (en : leaf)",
      "hôte [ot] (en : host)",
      "lit [li] (en : bed)",
      "maux [mo] (en : evil)",
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
rules<-data.table::fread("rulesStress.csv")
rules[,rule_id := 1:.N]
rules[,rule_id:=0]
list_example$result_kirsh <- sapply(list_example$latin,
       function(word){
         tail(latin_to_french2(word, rules_ = rules, ipa2kirsh_ = ipa2kirsh)$kirshenbaum,1)})

# ## keep only working examples for the moment
list_example <- list_example[result_kirsh==expected_kirshenbaum]


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
           p("The pronunciation of Latin has evolved over time in French territory.
           The rules of transformation have been stated and dated by linguists.
           Those explained in the book",strong("Précis de phonétique historique, Noëlle Laborderie, Armand Colin"), " have been coded with the regular expression technique.
           Examples of the book are presented by selecting", strong("Examples."),"A Latin dictionary is also connected to see the natural transformations of any Latin word, select ",strong("Search in dictionary.")," Some rules may be skipped with
             the exception input. The source code is", a("here", href = "https://github.com/clerousset/applatin2french"), "for any remarks or desire to participate make an issue in the github or directly by ", a(href = "mailto:clerousset@protonmail.com", "mail."),
             "The sounds are generated by", strong("espeak"), "with", strong("mbrola"), "additional voices")

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
            )#,
            #column(6, checkboxInput("sound", "Add sound", value=TRUE))
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
              selected = "c\u0101m\u0115ra",
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
      create_sounds(table())
      # }
      pretty_print(table(), 
                   #row_highlight = row_to_play(),
                   sound = TRUE)}

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
