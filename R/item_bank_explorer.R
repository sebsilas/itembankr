# library(tidyverse)
# library(shiny)
# library(DT)
# library(ggplot2)
# library(stringr)
# library(rjson)
# library(xlsx)
# library(hrep)
# library(readr)
# library(igraph)
# library(visNetwork)
# library(itembankr)
# library(musicassessr)

#dir = "/Users/sebsilas/Desktop/data-programming/coursework/project/FINAL/one_script_version/R"
#setwd(dir)

sim_matrix_to_graph <- function(melody) {

  ngrams <- dplyr::bind_rows(lapply(3:length(melody), function(x) get_all_ngrams(melody, N = x)))

  sim.matrix <- combn(ngrams$value, 2, FUN = function(x) itembankr::ngrukkon(itembankr::str_mel_to_vector(x[1], sep = ","),
                                                                             itembankr::str_mel_to_vector(x[2], sep = ",")))

  sim.matrix <- matrix(sim.matrix, ncol = length(ngrams$value), nrow = length(ngrams$value))

  #html <- paste0("<p style = 'color: red;'>",ngrams$value,'</p>')

  #nodes <- data.frame(id = ngrams$value, label = ngrams$value)
  #nodes <- data.frame(id = ngrams$value, label = "\uf286 <b>This</b> is an\n<i>html</i> <b><i>multi-</i>font</b> <code>label</code>'")
  nodes <- data.frame(id = ngrams$value, label = "\uf286 <div id = 'sheet_music'></div> <b>This</b> is an\n<i>html</i> <b><i>multi-</i>font</b> <code>label</code>'")

  #print(nodes)
  #print(present_stimuli_midi_notes_visual(stimuli = 60:65))
  #nodes <- data.frame(id = ngrams$value, label = paste0('\uf286 ', present_stimuli_midi_notes_visual(stimuli = 60:65)))
  # nodes <- data.frame(id = ngrams$value, label = lapply(ngrams$value, function(x) paste0('\uf286 <b> This </b> is')))
  # nodes <- data.frame(id = ngrams$value, label = paste0('\uf286 <div><b><i>This</i></b></div> is'))
  # nodes <- data.frame(id = ngrams$value, label = rep(HTML(paste0('\uf286 <div><b><i>This</i></b></div> is')), length(ngrams$value)))

  # # put row names to col names
  row.names(sim.matrix) <- ngrams$value
  colnames(sim.matrix) <- ngrams$value
  sim.matrix
  # hm, where are the diagonal ones?

  # create adjacency matrix
  threshold <- 0.2
  g <- graph.adjacency(sim.matrix > threshold)
  print(g)

  # create edges
  edges <- as.data.frame(get.edgelist(g))
  colnames(edges) <- c("from","to")

  network <- visNetwork(nodes, edges) %>%
    visPhysics(stabilization = FALSE) %>%
    visEdges(smooth = FALSE) %>%
    visLayout(randomSeed = 12, improvedLayout = FALSE) %>%
    visNodes(font = list(multi = TRUE, face = 'FontAwesome', ital = list(mod = ''),
                         bold = list(mod = ''))) %>%
    addFontAwesome()
  network

}

item_bank_explorer <- function(item_bank) {

  ui <- shiny::basicPage(

    musicassessr::musicassessr_js_scripts(api_url = "https://255uxe6ajl.execute-api.us-east-1.amazonaws.com/api",
                                          bucket_name = "shinny-app-source-41630",
                                          bucket_region = "us-east-1",
                                          identity_pool_id = "us-east-1:feecdf7e-cdf6-416f-94d0-a6de428c8c6b",
                                          destination_bucket = "shinny-app-destination-41630"),

    shiny::tags$h2("Corpus Explorer"),

    musicassessr::present_stimuli_midi_notes_visual(stimuli = 60:65, present_div = FALSE),

    visNetwork::visNetworkOutput("network"),

    shiny::htmlOutput('melodyNotation'),

    DT::dataTableOutput("melodies")

  )


  server <- function(input, output) {


    output$melodies <- DT::renderDataTable(dat, selection = 'single', escape = FALSE,
                                           options = list(searching = TRUE, pageLength = 20))


    output$melodyNotation <- renderUI({

      if (is.null(input$melodies_rows_selected)) {
        print("nothing selected")
      }
      else {
        #print(dat[[input$melodies_rows_selected, "melody"]])
        #print(itembankr::str_mel_to_vector(dat[[input$melodies_rows_selected, "melody"]], sep = ","))
        melody <- itembankr::str_mel_to_vector(dat[[input$melodies_rows_selected, "melody"]], sep = ",")
        # print(melody)
        # ngrams <- bind_rows(lapply(3:length(melody), function(x) get_all_ngrams(melody, N = x)))
        # print(ngrams)
        #
        # sim.matrix <- combn(ngrams$value, 2, FUN = function(x) ngrukkon(itembankr::str_mel_to_vector(x[1], sep = ","),
        #                                             itembankr::str_mel_to_vector(x[2], sep = ",")))
        # print(sim.matrix)
        abs_melody <- cumsum(melody) + 60
        musicassessr::present_stimuli_midi_notes_both(stimuli = abs_melody, note_length = 0.5)
      }
    })

    output$network <- renderVisNetwork({
      melody <- itembankr::str_mel_to_vector(dat[[input$melodies_rows_selected, "melody"]], sep = ",")
      print(melody)
      sim_matrix_to_graph(melody)
    })


  }

  shiny::shinyApp(ui, server)
}


#item_bank_explorer(itembankr::subset_item_bank(itembankr::Berkowitz("main"), item_length = c(5, NULL)))
