#' Convert a relative melody to an absolute melody
#'
#' @param rel_mel
#' @param start_note
#'
#' @return
#' @export
#'
#' @examples
rel_to_abs_mel <- function(rel_mel, start_note = 60) {
  c(0, cumsum(rel_mel)) + start_note
}


#' Convert a string of a melody, separated by a to a vector
#'
#' @param str_mel
#' @param sep
#' @param numeric
#'
#' @return
#' @export
#'
#' @examples
str_mel_to_vector <- function(str_mel, sep = ",", numeric = TRUE) {
  vector_mel <- unlist(strsplit(str_mel, sep))
  if(numeric) {
    as.numeric(vector_mel)
  } else {
    vector_mel
  }
}

#' Convert MIDI note to pitch class
#'
#' @param midi_note
#'
#' @return
#' @export
#'
#' @examples
midi_to_pitch_class <- function(midi_note) {
  if (length(midi_note) == 1) {
    pitch_class <- midi.to.pitch.classes.list[[as.character(midi_note)]]
  }
  else {
    pitch_class <- unlist(lapply(midi_note, function(x) midi.to.pitch.classes.list[[as.character(x)]]))
  }
  pitch_class
}


#' Convert MIDI note to pitch class, in numeric form
#'
#' @param midi_note
#'
#' @return
#' @export
#'
#' @examples
midi_to_pitch_class_numeric <- function(midi_note) {
  if (length(midi_note) == 1) {
    pitch_class <- midi.to.pitch.classes.numeric.list[[as.character(midi_note)]]
  }
  else {
    pitch_class <- unlist(lapply(midi_note, function(x) midi.to.pitch.classes.numeric.list[[as.character(x)]]))
  }
  pitch_class
}


#' Convert MIDI note to its corresponding representation as musical scientific notation
#'
#' @param midi_note
#'
#' @return
#' @export
#'
#' @examples
midi_to_sci_notation <- function(midi_note) {
  if (length(midi_note) == 1) {
    pitch_class <- midi.to.sci.notation.list[[as.character(midi_note)]]
  } else {
    pitch_class <- unlist(lapply(midi_note, function(x) {
      midi.to.sci.notation.list[[as.character(x)]]
      }))
  }
}

#' Convert musical scientific notation note to its corresponding representation as a MIDI note
#'
#' @param sci_notation
#'
#' @return
#' @export
#'
#' @examples
sci_notation_to_midi <- function(sci_notation) {
  if (length(sci_notation) == 1) {
    midi <- sci_notation_to_midi_low_level(sci_notation)
  }
  else {
    print(sci_notation)
    midi <- unlist(lapply(sci_notation, sci_notation_to_midi_low_level))
  }
  midi
}



sci_notation_to_midi_low_level <- function(sci_notation) {

  note_without_octave <- remove_last_char_of_string(sci_notation)
  last_char <- get_last_char_of_string(note_without_octave)

  if(last_char == "b") {
    midi <- sci.notation.to.midi.list.flat[[as.character(sci_notation)]]
  }
  else if(last_char == "#") {
    print(sci_notation)
    midi <- sci.notation.to.midi.list.sharp[[as.character(sci_notation)]]
  }
  else {
    midi <- sci.notation.to.midi.list[[as.character(sci_notation)]]
  }
}


#' Convert pitch classes to numeric pitch classes
#'
#' @param pitch_class
#'
#' @return
#' @export
#'
#' @examples
pitch_class_to_numeric_pitch_class <- function(pitch_class) {
  if(substr(pitch_class, nchar(pitch_class), nchar(pitch_class)) == "b") {
    which(pitch.classes.flat == pitch_class)
  } else {
    which(pitch.classes.sharp == pitch_class)
  }
}

# itembankr::pitch_class_to_numeric_pitch_class("Gb")

#' Return MIDI note(s) corresponding to a given pitch class
#'
#' @param pitch_class
#'
#' @return
#' @export
#'
#' @examples
pitch_class_to_midi_notes <- function(pitch_class) {
  if(length(pitch_class) == 1) {
    pitch.class.to.midi.list[[pitch_class]]
  } else {
    lapply(pitch_class, function(x) {
      pitch.class.to.midi.list[[x]]
    })
  }
}


#' Grab the last character of a string
#'
#' @param string
#'
#' @return
#' @export
#'
#' @examples
get_last_char_of_string <- function(string) {
  substr(string, nchar(string), nchar(string))
}

#' Remove the last character of a string
#'
#' @param string
#'
#' @return
#' @export
#'
#' @examples
remove_last_char_of_string <- function(string) {
  substr(string, 1, nchar(string)-1)
}


#' Test if scientific music notation
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
is_sci_notation <- function(x) {

  last_char <- get_last_char_of_string(x)

  if (is.na(as.numeric(last_char))) {
    stop('Last character is not a number, so entry is not in sci_notation format')
  }
  # below can be used to detect if entry contains digit:
  #stringr::str_detect("C", "[0-9]")
  #stringr::str_detect("C4", "[0-9]")
}


bpm_to_seconds_per_beat <- function(bpm) {
  seconds_in_a_minute <- 60
  seconds_per_beat <- seconds_in_a_minute/bpm
}

rel_bpm_to_seconds <- function(v, bpm) {
  # convert note lengths as relative numbers (i.e 4 = semibreve, 2 = minim, 1 = crotchet)
  # to abs values in seconds, based on a bpm
  seconds_per_beat <- bpm_to_seconds_per_beat(bpm)
  seconds_per_beat/v
}

microseconds_per_beat_to_bpm <- function(microseconds) {
  # midi uses microseconds per beat to set the tempo of a track, so use this to convert
  60/(microseconds/1000000)
}


#' Produce arrhythmic durations for a given melody
#'
#' @param x
#' @param dur
#'
#' @return
#' @export
#'
#' @examples
produce_arrhythmic_durations <- function(x, dur = .50) {
  # take in a pitch vector and produce a vector of arrhythmic durations
  res <- rep(dur, length(x))
}

add_prefix <- function(file_path, prefix = NULL) {
  if(!is.null(prefix)) {
    paste0(prefix, '/', file_path)
  }
}

remove_prefix <- function(file_path, prefix = NULL) {
  if(!is.null(prefix)) {
    stringr::str_remove(file_path, prefix)
  }
}


get_division_from_midi_file <- function(file){
  # borrowed from tuneR's readMidi
  con <- file(description = file, open = "rb")
  on.exit(close(con))
  MThd <- readChar(con, 4)
  if(MThd != "MThd")
    stop("No Header Chunk in this Midi (?)")
  MThd_length <- readBin(con, integer(0), n = 1, size = 4, endian = "big")
  if(MThd_length != 6)
    stop("Unexpected Header Chunk size")
  MThd_format <- readBin(con, integer(0), n = 1, size = 2, endian = "big", signed = FALSE)
  if(!(MThd_format %in% 0:2))
    stop("Unexpected Mide file format")
  MThd_tracks <- readBin(con, integer(0), n = 1, size = 2, endian = "big", signed = FALSE)

  # FIXME: MThd_division < 0 can appear in Midi files with a very different interpretation!
  MThd_division <- readBin(con, integer(0), n = 1, size = 2, signed = TRUE, endian = "big")
  if(MThd_division < 0){
    stop("Midi representation of timing: Frames per second / ticks per frame not yet implemented, please ask the author")
  }
  MThd_division
}


ticks_to_ms <- function(ticks, ppq, tempo) {
  us_per_quarter <- tempo # Tempo in latest Set Tempo event>
  us_per_tick <- us_per_quarter / ppq
  seconds_per_tick <- us_per_tick / 1000000
  seconds <- ticks * seconds_per_tick
  seconds
}


#' Get all ngrams from a given vector
#'
#' @param x
#' @param N
#'
#' @return
#' @export
#'
#' @examples
get_all_ngrams <- function(x, N = 3){
  l <- length(x) - N + 1
  stopifnot(l > 0)
  purrr::map_dfr(1:l, function(i){
    ngram <- x[i:(i + N - 1)]
    tidyr::tibble(start = i, N = N, value = paste(ngram, collapse = ","))
  })
}


#' Produce extra melodic features from a pyin note track
#'
#' @param pyin_style_res
#'
#' @return
#' @export
#'
#' @examples
produce_extra_melodic_features <- function(pyin_style_res) {

  if(!"note" %in% names(pyin_style_res) & "freq" %in% names(pyin_style_res)) {
    pyin_style_res <- pyin_style_res %>%
      dplyr::mutate(note = round(hrep::freq_to_midi(freq)))
  }

  pyin_style_res <- pyin_style_res %>%
    dplyr::mutate(
      sci_notation = midi_to_sci_notation(note),
      interval = c(NA, diff(note)),
      ioi = c(NA, diff(onset)),
      ioi_class = classify_duration(ioi)) %>%
    segment_phrase()

  if(!"freq" %in% names(pyin_style_res)) {
    pyin_style_res <- pyin_style_res %>% dplyr::mutate(freq = hrep::midi_to_freq(note))
  }

  pyin_style_res %>% dplyr::mutate(
    cents_deviation_from_nearest_midi_pitch = vector_cents_between_two_vectors(round(hrep::midi_to_freq(hrep::freq_to_midi(freq))), freq),
    # the last line looks tautological, but, by converting back and forth, you get the quantised pitch and can measure the cents deviation from this
    pitch_class = itembankr::midi_to_pitch_class(round(hrep::freq_to_midi(freq))),
    pitch_class_numeric = itembankr::midi_to_pitch_class(round(hrep::freq_to_midi(freq)))
  )

}

#' Classify ioi class (see Frieler.. )
#'
#' @param dur_vec best, should be a ioi vector
#' @param ref_duration
#'
#' @return
#' @export
#'
#' @examples
classify_duration <- function(dur_vec, ref_duration = .5){
  rel_dur <- dur_vec/ref_duration
  rhythm_class <- rep(-2, length(rel_dur))
  #rhythm_class[rel_dur <= .45] <- -2
  rhythm_class[rel_dur > 0.45] <- -1
  rhythm_class[rel_dur > 0.9] <- 0
  rhythm_class[rel_dur > 1.8] <- 1
  rhythm_class[rel_dur > 3.3] <- 2
  rhythm_class
}

#' Segment a note track by adding phrasend and phrasbeg columns with boolean markers.
#'
#' @param note_track a data frame with an "onset" column
#'
#' @return
#' @export
#'
#' @examples
segment_phrase <- function(note_track){
  # (originally add_phrase_info from KF)
  note_track <- note_track %>% dplyr::mutate(ioi = c(0, diff(onset)), note_pos = 1:dplyr::n())
  outliers <- note_track %>% dplyr::pull(ioi) %>% boxplot(plot = FALSE) %>% `[[`("out")
  outliers <- outliers[outliers > .65]
  r <- note_track %>%
    dplyr::mutate(phrasend = as.numeric(ioi >.96 | ioi %in% outliers),
                  phrasbeg = as.numeric(dplyr::lag(phrasend) | note_pos == 1))

  r$phrasend[is.na(r$phrasend)] <- 0
  r$phrasend[length(r$phrasend)] <- 1
  r
}


# tests

#rel_bpm_to_seconds(c(2, 2, 4, 2, 2), bpm = 120)

#microseconds_per_beat_to_bpm(500000)

#sci_notation_to_midi(c("C#4", "Eb4", "G#4", "Bb4", "A#4", "Db4", "E#4", "Gb2"))

