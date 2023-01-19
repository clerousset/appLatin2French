#devtools::install_github("gadenbuie/shinyThings")
#remotes::install_github("daqana/dqshiny")
library(shiny)
library(dqshiny)
library(data.table)
# create 100k random words
dico <- readRDS('dico.Rds')
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
                   verbatimTextOutput("transfo", placeholder = TRUE),
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
    output$transfo <-
      renderPrint({
        req(input$choice)
        if(input$choice == "Examples"){
          cat(
            latin_to_french(
              input$examples,
              rules = rules)
            )
        } else {
        cat(
          latin_to_french(
            input$auto2,
            rules = rules)
        )
          }
        })

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
