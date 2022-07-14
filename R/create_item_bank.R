
create_item_bank_from_files <- function(corpus_name, midi_file_dir = NULL, musicxml_file_dir = NULL, prefix = NULL) {


  if(!is.null(midi_file_dir) & !is.null(musicxml_file_dir)) {

    # this first file path is used later on e.g with musicassessr:
    midi_file_dir <- paste0('item_banks/', corpus_name, '/', midi_file_dir)
    musicxml_file_dir <- paste0('item_banks/', corpus_name, '/', musicxml_file_dir)

    # this one is used internally at the itembankr level:
    midi_file_dir_prefix <- add_prefix(midi_file_dir, prefix)
    musicxml_file_dir_prefix <- add_prefix(musicxml_file_dir, prefix)

    files <- list.files(path = midi_file_dir_prefix, pattern = "\\.mid$",  full.names = TRUE)
    file_key <- list.files(path = musicxml_file_dir_prefix, pattern = "\\.musicxml$",  full.names = FALSE) %>%
      remove_prefix(file_key, prefix) %>% tools::file_path_sans_ext(file_key)

    res <- purrr:map_dfr(files, midi_file_to_notes_and_durations, prefix = prefix)
    res$file_key <- file_key
    musicxml_files <- tibble::tibble(music_xml = list.files(path = musicxml_file_dir_prefix, pattern = "\\.musicxml$",  full.names = TRUE),
                                 file_key = tools::file_path_sans_ext(list.files(path = musicxml_file_dir_prefix, pattern = "\\.musicxml$",  full.names = FALSE)))
    res$musicxml_file <- remove_prefix(musicxml_files[match(res$file_key, musicxml_files$file_key), "music_xml"], prefix)

  } else if(!is.null(midi_file_dir)) {
    # this first file path is used later on e.g with musicassessr:
    midi_file_dir <- paste0('item_banks/', corpus_name, '/', midi_file_dir)

    # this one is used internally at the itembankr level:
    midi_file_dir_prefix <- add_prefix(midi_file_dir, prefix)

    files <- list.files(path = midi_file_dir_prefix, pattern = "\\.mid$",  full.names = TRUE)
    files <- purrr:map_dfr(files, midi_file_to_notes_and_durations, prefix = prefix)

  } else if(!is.null(musicxml_file_dir)) {
    # this first file path is used later on e.g with musicassessr:
    musicxml_file_dir <- paste0('item_banks/', corpus_name, '/', musicxml_file_dir)
    # this one is used internally at the itembankr level:
    musicxml_file_dir_prefix <- add_prefix(musicxml_file_dir, prefix)

    files <- list.files(path = musicxml_file_dir_prefix, pattern = "\\.musicxml$",  full.names = TRUE)
    files <- purrr:map_dfr(files, musicxml_file_to_notes_and_durations, prefix = prefix)
  } else {
    stop("File extension not recognised. Currently only .mid or .musicxml supported.")
  }

  res <- res %>% dplyr::rowwise() %>%
    dplyr::mutate(melody = paste0(diff(str_mel_to_vector(notes)), collapse = ","),
                  musicxml_file = remove_slash(musicxml_file),
                  midi_file = remove_slash(midi_file)) %>% dplyr::ungroup()

  res
}


# internal functions

remove_slash <- function(x) {
  if(substr(x, 1, 1) == "/") {
  substr(x, 2, nchar(x))
  }
}

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
#' @param string_df
#'
#' @return
#' @export
#'
#' @examples
midi_file_to_notes_and_durations <- function(midi_file, prefix = NULL, string_df = TRUE, produce_extra_melodic_features = TRUE) {

  midi_file_dat <- tuneR::readMidi(midi_file)

  tempo <- midi_file_dat %>% dplyr::filter(event == "Set Tempo") %>% dplyr::pull(parameterMetaSystem) %>% as.numeric()

  if(length(tempo) != 1) {
    tempo_bpm <- microseconds_per_beat_to_bpm(tempo)
    tempo <- tempo[1]
  }

  notes <- tuneR::getMidiNotes(midi_file_dat) %>% tibble::as_tibble()

  out <- notes %>% dplyr::mutate(onset = round(ticks_to_ms(time, ppq = get_division_from_midi_file(midi_file), tempo = tempo), 2),
                          durations = round(ticks_to_ms(length, ppq = get_division_from_midi_file(midi_file), tempo = tempo), 2)) %>%
    dplyr::select(onset, durations, note)

  if(!is.null(prefix)) {
    midi_file <- remove_prefix(midi_file, prefix)
  }
  out$midi_file <- midi_file

  if(produce_extra_melodic_features) {
    out <- out %>% produce_extra_melodic_features()
  }
  if(string_df) {
    out <- out %>% musicassessr::to_string_df(exclude_cols = "midi_file")
  }
  out
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
  durations <- lapply(notes, function(note) note$duration)
  converted_pitches_and_durs <- convert_pitches_and_durs(pitches, durations, relativeMelodies, relativeDurations, tempo)
  pitches <- converted_pitches_and_durs$pitches
  durations <- converted_pitches_and_durs$durations
  tibble::tibble(melody = paste0(pitches, collapse = ","),
             durations = paste0(durations, collapse = ","),
             musicxml_file = musicxml_file
  )
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

  phrase <- purrr::map_dfr(files, function(f) {

    res <- tuneR::readMidi(f)

    end_time <- res %>% dplyr::filter(event == "End of Track") %>%
      dplyr::pull(time) %>% as.numeric()

    res <- res %>% dplyr::filter(event == "Note On") %>%
      dplyr::select(-c(type, channel, parameterMetaSystem, track, event, parameter2)) %>%
      dplyr::rename(note = parameter1) %>% add_phrase_info(end_time)

    mid_file <- remove_prefix(f, paste0(prefix, "/") )

    res <- res %>% dplyr::mutate(midi_file = mid_file)
  })

  phrase_beg <- phrase %>% dplyr::filter(phrasbeg == 1) %>%
    dplyr::summarise(id = id, start = note_pos, midi_file = midi_file)

  phrase_end <- phrase %>% dplyr::filter(phrasend == 1) %>% dplyr::summarise(end = note_pos)

  phrases <- cbind(phrase_beg, phrase_end)

  phrases <- purrr::pmap(phrases, function(id, start, midi_file, end) {

    start <- phrases %>% slice(i) %>% pull(start)
    end <- phrases %>% slice(i) %>% pull(end)
    phrase <- phrase %>% slice(start:end)
    note <- phrase %>% pull(note)
    durations <- phrase %>% pull(durations)
    ioi <- phrase %>% pull(ioi)


    tibble::tibble(notes = paste0(phrase, collapse = ","),
                   melody = paste0(diff(phrase), collapse = ","),
                   durations = paste0(durations, ","),
                   ioi = paste0(ioi, ","),
                    N = length(phrase))
  })

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



#' Convert a corpus to a useful item bank
#'
#' @param corpus_name
#' @param midi_file_dir
#' @param musicxml_file_dir
#' @param prefix
#' @param corpus_df
#' @param phrases_db
#' @param output_type
#' @param launch_app
#' @param main_db_combine_ngram_and_phrases
#'
#' @return
#' @export
#'
#' @examples
corpus_to_item_bank <- function(corpus_name,
                                midi_file_dir = NULL,
                                musicxml_file_dir = NULL,
                                prefix = NULL,
                                corpus_df = NULL,
                                phrases_db = NULL,
                                output_type = c("both", "ngram", "phrases"),
                                launch_app = TRUE,
                                main_db_combine_ngram_and_phrases = TRUE) {

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
      phrases_db <- create_phrases_db(corpus_name = corpus_name,
                                      midi_file_dir = add_prefix(paste0('item_banks/', corpus_name, '/', midi_file_dir), prefix),
                                      prefix = prefix)
    }


  } else {
    files_db <- NA
    corpus_df <- corpus_df %>% mutate(midi_file = NA, musicxml_file = NA)
    ngram_db <- split_item_bank_into_ngrams(corpus_df)
    ngram_db <- ngram_db[!duplicated(ngram_db), ]
    freq_db <- count_freqs(ngram_db)
    main_db <- get_melody_features(freq_db, mel_sep = ",", durationMeasures = TRUE)
    main_db <- main_db[!duplicated(main_db), ]
    phrases_db <- count_freqs(phrases_db) %>% get_melody_features(mel_sep = ",", durationMeasures = TRUE)
    phrases_db <- phrases_db[!duplicated(phrases_db), ]
    if(main_db_combine_ngram_and_phrases) {
      joint_names <- intersect(names(main_db), names(phrases_db))
      main_db <- main_db %>% dplyr::select(joint_names)
      phrases_db <- phrases_db %>% dplyr::select(joint_names)
      main_db <- rbind(main_db, phrases_db)
    }
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

# d <- midi_file_to_notes_and_durations("/Users/sebsilas/true.mid")
