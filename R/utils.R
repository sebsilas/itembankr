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
#'
#' @return
#' @export
#'
#' @examples
str_mel_to_vector <- function(str_mel, sep = ",") {
  vector_mel <- as.numeric(unlist(strsplit(str_mel, sep)))
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

#sci_notation_to_midi(c("C#4", "Eb4", "G#4", "Bb4", "A#4", "Db4", "E#4", "Gb2"))


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


pitch_class_to_numeric_pitch.class <- function(pitch_class) {
  which(pitch.classes == pitch_class)
}

pitch_class_to_midi.notes <- function(pitch_class) {
  pitch.class.to.midi.list[[pitch_class]]
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

# tests

#rel_bpm_to_seconds(c(2, 2, 4, 2, 2), bpm = 120)

#microseconds_per_beat_to_bpm(500000)
