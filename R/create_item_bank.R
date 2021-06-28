library(tuneR)
library(dplyr)
require(XML)
#library(psych)
library(ggplot2)
library(tidyr)
#library(Hmisc)
#library(data.table)
library(purrr)


# constants

load("R/sysdata.rda")


# funs

create_item_bank_from_files <- function(midi_file_dir = NULL, musicxml_file_dir = NULL) {

  if(!is.null(midi_file_dir) & !is.null(musicxml_file_dir)) {
    files <- list.files(path = midi_file_dir, pattern = "\\.mid$",  full.names = TRUE)
    file_key <- list.files(path = midi_file_dir, pattern = "\\.mid$",  full.names = FALSE)
    file_key <- tools::file_path_sans_ext(file_key)
    res <- bind_rows(lapply(files, midi_file_to_notes_and_durations))
    res$file_key <- file_key
    musicxml_files <- data.frame(music_xml = list.files(path = musicxml_file_dir, pattern = "\\.musicxml$",  full.names = TRUE),
                                 file_key = tools::file_path_sans_ext(list.files(path = musicxml_file_dir, pattern = "\\.musicxml$",  full.names = FALSE)))

    res$musicxml_file <- musicxml_files[match(res$file_key, musicxml_files$file_key), "music_xml"]
    res
  }
  else if(!is.null(midi_file_dir)) {
    files <- list.files(path = midi_file_dir, pattern = "\\.mid$",  full.names = TRUE)
    files <- bind_rows(lapply(files, midi_file_to_notes_and_durations))
    files
  }
  else if(!is.null(musicxml_file_dir)) {
    files <- list.files(path = musicxml_file_dir, pattern = "\\.musicxml$",  full.names = TRUE)
    files <- bind_rows(lapply(files, musicxml_file_to_notes_and_durations))
    files
  }
  else {
    stop("File extension not recognised. Currently only .mid or .musicxml supported.")
  }
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
       "durations" = durations
       )
}

midi_file_to_notes_and_durations <- function(midi_file, relativeMelodies = TRUE, relativeDurations = FALSE, ticks = 480) {
  midi_file_dat <- tuneR::readMidi(midi_file)
  tempo <- midi_file_dat %>% filter(event == "Set Tempo") %>% dplyr::select(parameterMetaSystem)
  tempo <- as.numeric(unlist(tempo))
  tempo <- microseconds_per_beat_to_bpm(tempo)
  note_on <- midi_file_dat %>% filter(event == "Note On")
  pitches <- as.vector(unlist(note_on %>% dplyr::select(parameter1)))
  durations <- as.vector(unlist(note_on %>% dplyr::select(time) %>% transmute(duration = c(diff(time), diff(c(time[length(time)-1], time[length(note_on$time)])) ) )))
  durations <- durations/ticks
  warning('Not sure if this constant, 480 ticks, applies universally to MIDI files.')
  converted_pitches_and_durs <- convert_pitches_and_durs(pitches, durations, relativeMelodies, relativeDurations, tempo)
  pitches <- converted_pitches_and_durs$pitches
  durations <- converted_pitches_and_durs$durations
  data.frame(melody = paste0(pitches, collapse = ","),
             durations = paste0(durations = durations, collapse = ","),
             midi_file = midi_file
             )
}


musicxml_file_to_notes_and_durations <- function(musicxml_file, relativeMelodies = TRUE, relativeDurations = FALSE) {
  data <- xmlParse(musicxml_file)
  xml_data <- xmlToList(data)
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


# tests

# Berkowitz_files_db <- create_item_bank_from_files(midi_file_dir = "data-raw/Berkowitz/berkowitz_midi_rhythmic_100bpm",
#                                         musicxml_file_dir = "data-raw/Berkowitz/berkowitz_musicxml"
#                                         )
# Save. N.B: The name of the object should be the same as the name of the file
#save(Berkowitz_files_db, file = "data/Berkowitz_files_db.RData")
#load("data/Berkowitz_files_db.RData")
#Berkowitz_ngram_db <- split_item_bank_into_ngrams(Berkowitz_files_db)
#save(Berkowitz_ngram_db, file = "data/Berkowitz_ngram_db.RData")
#Berkowitz_freq_db <- count_freqs(Berkowitz_ngram_db)
#save(Berkowitz_freq_db, file = "data/Berkowitz_freq_db.RData")
#Berkowitz <- get_melody_features(Berkowitz_freq_db, mel_sep = ",", durationMeasures = FALSE)
usethis::use_data(Berkowitz, Berkowitz_freq_db, Berkowitz_ngram_db, Berkowitz_files_db)
