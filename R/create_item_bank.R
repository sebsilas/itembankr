

create_item_bank_from_files <- function(corpus_name, midi_file_dir = NULL, musicxml_file_dir = NULL, prefix = NULL) {


  if(!is.null(midi_file_dir) & !is.null(musicxml_file_dir)) {

    # this first file path is used later on e.g with musicassessr:
    midi_file_dir <- paste0('item_banks/', corpus_name, '/', midi_file_dir)
    musicxml_file_dir <- paste0('item_banks/', corpus_name, '/', musicxml_file_dir)

    # this one is used internally at the itembankr level:
    midi_file_dir_prefix <- add_prefix(midi_file_dir, prefix)
    musicxml_file_dir_prefix <- add_prefix(musicxml_file_dir, prefix)
    files <- list.files(path = midi_file_dir_prefix, pattern = "\\.mid$",  full.names = TRUE)
    file_key <- list.files(path = musicxml_file_dir_prefix, pattern = "\\.musicxml$",  full.names = FALSE)
    file_key <- remove_prefix(file_key, prefix)
    file_key <- tools::file_path_sans_ext(file_key)
    res <- dplyr::bind_rows(lapply(files, midi_file_to_notes_and_durations, prefix = prefix))
    res$file_key <- file_key
    musicxml_files <- data.frame(music_xml = list.files(path = musicxml_file_dir_prefix, pattern = "\\.musicxml$",  full.names = TRUE),
                                 file_key = tools::file_path_sans_ext(list.files(path = musicxml_file_dir_prefix, pattern = "\\.musicxml$",  full.names = FALSE)))
    res$musicxml_file <- remove_prefix(musicxml_files[match(res$file_key, musicxml_files$file_key), "music_xml"], prefix)
    res
  }
  else if(!is.null(midi_file_dir)) {
    # this first file path is used later on e.g with musicassessr:
    midi_file_dir <- paste0('item_banks/', corpus_name, '/', midi_file_dir)

    # this one is used internally at the itembankr level:
    midi_file_dir_prefix <- add_prefix(midi_file_dir, prefix)

    files <- list.files(path = midi_file_dir_prefix, pattern = "\\.mid$",  full.names = TRUE)
    files <- dplyr::bind_rows(lapply(files, midi_file_to_notes_and_durations, prefix = prefix))
    files
  }
  else if(!is.null(musicxml_file_dir)) {
    # this first file path is used later on e.g with musicassessr:
    musicxml_file_dir <- paste0('item_banks/', corpus_name, '/', musicxml_file_dir)
    # this one is used internally at the itembankr level:
    musicxml_file_dir_prefix <- add_prefix(musicxml_file_dir, prefix)

    files <- list.files(path = musicxml_file_dir_prefix, pattern = "\\.musicxml$",  full.names = TRUE)
    files <- dplyr::bind_rows(lapply(files, musicxml_file_to_notes_and_durations, prefix = prefix))
    files
  }
  else {
    stop("File extension not recognised. Currently only .mid or .musicxml supported.")
  }


  res$musicxml_file <- unlist(lapply(res$musicxml_file, function(x) if(substr(x, 1, 1) == "/") {
    substr(x, 2, nchar(x))
  }))

  res$midi_file <- unlist(lapply(res$midi_file, function(x) if(substr(x, 1, 1) == "/") {
    substr(x, 2, nchar(x))
  }))

  res <- res %>% dplyr::rowwise() %>% dplyr::mutate(melody = paste0(diff(str_mel_to_vector(notes)), collapse = ",")) %>% dplyr::ungroup()
  res
}


# internal functions


convert_pitches_and_durs <- function(pitches, durs, relativeMelodies, relativeDurations, tempo) {
  if(relativeMelodies) {
    pitches <- diff(pitches)
  }
  if(!relativeDurations) {
    durations <- rel_bpm_to_seconds(as.numeric(unlist(durs)), bpm = tempo)
  }
  list("pitches" = pitches,
       "durations" = durations)
}


#' Get notes and durations from a MIDI file
#'
#' @param midi_file
#' @param prefix
#'
#' @return
#' @export
#'
#' @examples
midi_file_to_notes_and_durations <- function(midi_file, prefix = NULL) {
  midi_file_dat <- tuneR::readMidi(midi_file)

  tempo <- midi_file_dat %>% dplyr::filter(event == "Set Tempo") %>% dplyr::pull(parameterMetaSystem)
  tempo <- as.numeric(tempo)

  if(length(tempo) != 1) {
    tempo_bpm <- microseconds_per_beat_to_bpm(tempo)
    tempo <- tempo[1]
  }
  notes <- tuneR::getMidiNotes(midi_file_dat) %>%
    dplyr::mutate(onset = ticks_to_ms(time, ppq = get_division_from_midi_file(midi_file), tempo = tempo),
                  durations = ticks_to_ms(length, ppq = get_division_from_midi_file(midi_file), tempo = tempo),
                  interval = c(NA, diff(note)))

  notes_and_durs <- notes %>% dplyr::summarise(notes = paste0(note, collapse = ","),
                                               durations = paste0(round(durations, 2), collapse = ",")) %>%
    dplyr::mutate(midi_file = remove_prefix(midi_file, prefix))
}


musicxml_file_to_notes_and_durations <- function(musicxml_file, relativeMelodies = TRUE, relativeDurations = FALSE) {
  data <- XML::xmlParse(musicxml_file)
  xml_data <- XML::xmlToList(data)
  part <- xml_data$part
  tempo <- as.numeric(part$measure$direction$sound["tempo"])
  part <- part[names(part) == "measure"]
  notes <- lapply(part, function(measure) measure[names(measure) == "note"])
  notes <- unlist(notes, recursive = FALSE)
  pitches <- unlist(lapply(notes, function(note) paste0(note$pitch$step, note$pitch$octave)))
  pitches <- sci_notation_to_midi(pitches)
  durations <- lapply(notes, function(note) note$duration )
  converted_pitches_and_durs <- convert_pitches_and_durs(pitches, durations, relativeMelodies, relativeDurations, tempo)
  pitches <- converted_pitches_and_durs$pitches
  durations <- converted_pitches_and_durs$durations
  data.frame(melody = paste0(pitches, collapse = ","),
             durations = paste0(durations, collapse = ","),
             musicxml_file = musicxml_file
  )
}



#' Segment a note track by adding phraseend and phrasebeg columns with boolean markers.
#'
#' @param note_track a data frame with an "onset" column
#'
#' @return
#' @export
#'
#' @examples
add_phrase_info2 <- function(note_track){
  # this is the original function from KF
  note_track <- note_track %>% dplyr::mutate(ioi = c(diff(onset), NA), note_pos = 1:n())
  outliers <- note_track %>% dplyr::pull(ioi) %>% boxplot(plot = FALSE) %>% `[[`("out")
  outliers <- outliers[outliers > .65]
  note_track %>%
    dplyr::mutate(phrasend = as.numeric(ioi >.96 | ioi %in% outliers),
           phrasbeg = as.numeric(dplyr::lag(phrasend) | note_pos ==1))
}

add_phrase_info <- function(note_track, end_track){

  note_track <- note_track %>% dplyr::mutate(onset = time/1000)
  final_ioi <- diff(c(note_track$onset[length(note_track$onset)], end_track/1000))
  note_track <- note_track %>% dplyr::mutate(ioi = round(c(diff(onset), final_ioi), 2),
                                             note_pos = 1:dplyr::n())

  outliers <- note_track %>% dplyr::pull(ioi) %>% boxplot(plot = F) %>% `[[`("out")
  #outliers <- outliers[outliers > .65]
  note_track %>%
    dplyr::mutate(phrasend = as.numeric(ioi %in% outliers | note_pos == length(note_pos)),
                  phrasbeg = as.numeric(dplyr::lag(phrasend) | note_pos == 1))
}

create_phrases_db <- function(corpus_name, midi_file_dir, compute_melody_features = TRUE, prefix) {

  no_files <- length(list.files(midi_file_dir, full.names = TRUE, pattern = "\\.mid$"))

  # to keep IDs in correct order
  files <- paste0(midi_file_dir, "/", corpus_name, 1:no_files, ".mid")

  phrase <- dplyr::bind_rows(lapply(files, function(f) {

    t <- tuneR::readMidi(f)

    end_time <- t %>% dplyr::filter(event == "End of Track") %>%
      dplyr::select(time)
    end_time <- as.vector(as.numeric(end_time))
    t2 <- t %>% dplyr::filter(event == "Note On") %>%
      dplyr::select(-c(type, channel, parameterMetaSystem, track, event, parameter2)) %>%
      dplyr::rename(note = parameter1)
    t3 <- add_phrase_info(t2, end_time)
    mid_file <- remove_prefix(f, paste0(prefix, "/") )
    t3 <- t3 %>% dplyr::mutate(midi_file = mid_file)
  }), .id = "id")

  phrase_beg <- phrase %>% dplyr::filter(phrasbeg == 1) %>%
    dplyr::summarise(id = id,
                     start = note_pos,
                     midi_file = midi_file
                     )

  phrase_end <- phrase %>% dplyr::filter(phrasend == 1) %>%
    dplyr::summarise(end = note_pos)

  phrases <- cbind(phrase_beg, phrase_end)

  phrases$notes <- unlist(lapply(1:nrow(phrases), function(i) {
    row_ids <- phrases[i, "start"]:phrases[i, "end"]
    paste0(phrase[row_ids, "note"], collapse = ",")
  }))

  phrases$melody <- unlist(lapply(1:nrow(phrases), function(i) {
    row_ids <- phrases[i, "start"]:phrases[i, "end"]
    paste0(diff(phrase[row_ids, "note"]), collapse = ",")
  }))

  phrases$durations <- unlist(lapply(1:nrow(phrases), function(i) {
    row_ids <- phrases[i, "start"]:phrases[i, "end"]
    paste0(phrase[row_ids, "ioi"], collapse = ",")
  }))

  phrases$N <- unlist(lapply(1:nrow(phrases), function(i) {
    row_ids <- phrases[i, "start"]:phrases[i, "end"]
    length(phrase[row_ids, "note"])
  }))

  phrases <- phrases %>% dplyr::filter(melody != "")

  if(compute_melody_features) {
    phrases_freq_db <- count_freqs(phrases)
    phrases <- get_melody_features(phrases_freq_db, mel_sep = ",", durationMeasures = TRUE)
  }
  phrases

}

input_check <- function(midi_file_dir, musicxml_file_dir, corpus_df) {

  if(is.null(midi_file_dir) & is.null(musicxml_file_dir) & is.null(corpus_df)) {

    stop('At least one of midi_file_dir, musicxml_file_dir or corpus_df is required.')

  } else if(!is.null(midi_file_dir) & !is.null(corpus_df)) {

    stop('You may only use the corpus_df OR the midi_file_dir arguments.')

  } else if(!is.null(musicxml_file_dir) & !is.null(corpus_df)) {

    stop('You may only use the corpus_df OR the musicxml_file_dir arguments.')

  } else if(!is.null(midi_file_dir) & !is.null(musicxml_file_dir) & !is.null(corpus_df)) {

    stop('You may only use the corpus_df OR the musicxml_file_dir and midi_file_dir arguments.')

  } else {
    return()
  }

}

corpus_to_item_bank <- function(corpus_name,
                                midi_file_dir = NULL,
                                musicxml_file_dir = NULL,
                                prefix = NULL,
                                corpus_df = NULL,
                                phrases_db = NULL,
                                output_type = c("both", "ngram", "phrases"),
                                launch_app = TRUE) {

  input_check(midi_file_dir, musicxml_file_dir, corpus_df)

  if(!is.null(midi_file_dir) | !is.null(musicxml_file_dir)) {

    files_db <- create_item_bank_from_files(corpus_name = corpus_name,
                                            midi_file_dir = midi_file_dir,
                                            musicxml_file_dir = musicxml_file_dir,
                                            prefix = prefix)

    ngram_db <- split_item_bank_into_ngrams(files_db)
    freq_db <- count_freqs(ngram_db)
    main_db <- get_melody_features(freq_db, mel_sep = ",", durationMeasures = TRUE)

    if(is.null(phrases_db)) {
      phrases_db <- create_phrases_db(corpus_name = corpus_name, midi_file_dir = add_prefix(paste0('item_banks/', corpus_name, '/', midi_file_dir), prefix), prefix = prefix)
    }


  } else {
    files_db <- NA
    ngram_db <- split_item_bank_into_ngrams(corpus_df)
    freq_db <- count_freqs(ngram_db)
    main_db <- get_melody_features(freq_db, mel_sep = ",", durationMeasures = TRUE)
    phrases_db <- count_freqs(phrases_db)
    phrases_db <- get_melody_features(phrases_db, mel_sep = ",", durationMeasures = TRUE)
  }


  # return a function which can access the different dfs
  item_bank <- function(key) {
    l <- list("files" = files_db,
         "ngram" = ngram_db,
         "main" = main_db,
         "phrases" = phrases_db)
    l[[key]]
  }

  save(item_bank, file = paste0(corpus_name, ".rda"))

  if(launch_app) {
    item_bank_explorer(item_bank)
  }

  item_bank

}

