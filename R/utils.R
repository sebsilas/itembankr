

#' Make sure melodies have a minimum absolute value of x (and scale the rest of the durations to this minimum value).
#'
#' @param df
#' @param x
#'
#' @return
#' @export
#'
#' @examples
scale_durations_to_have_min_abs_value_of_x_seconds <- function(df, x = 0.25) {

  if(is.na(x)) {
    return(df)
  } else {
  df <- df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(durations_v = list( str_mel_to_vector(durations) ),
           min_duration = min( unlist(durations_v) , na.rm = TRUE),
           min_dur_scale_factor = x/min_duration,
           durations_v = list(
             dplyr::case_when(min_dur_scale_factor > 1 ~ round(durations_v * min_dur_scale_factor, 2), TRUE ~ durations_v)
           ),
           durations = paste0(unlist(durations_v), collapse = ",") ) %>% # corrected
    dplyr::ungroup() %>%
    dplyr::select(-c(durations_v, min_duration, min_dur_scale_factor))
  }
  df
}



is.null.or <- function(x, f) {
  is.null(x) || f(x)
}

is.scalar.character <- function(x) {
  is.character(x) && is.scalar(x)
}

is.scalar.numeric <- function(x) {
  is.numeric(x) && is.scalar(x)
}

is.scalar.logical <- function(x) {
  is.logical(x) && is.scalar(x)
}

is.scalar <- function(x) {
  identical(length(x), 1L)
}

#' Check item bank class
#'
#' @param x
#'
#' @return
#' @export
is_item_bank <- function(x) is(x, "item_bank")


#' Unclass item bank class (to tibble)
#'
#' @param x
#'
#' @return
#' @export
unclass_item_bank <- function(x) if(is(x, "item_bank")) tibble::as_tibble(x) else x

#' Set item bank class
#'
#' @param obj
#' @param extra
#'
#' @return
#' @export
#'
#' @examples
set_item_bank_class <- function(obj, extra = NULL) {

  stopifnot(is.null(extra) | assertthat::is.string(extra))

  suppressWarnings({ # is.na() will warn when it checks a function
    if(is_na_scalar(obj)) {
      return(NA)
    } else {

      attr(obj, "class") <- c(class(obj), "item_bank")

      if(is.null(extra)) {
        return(obj)
      } else {
        attr(obj, "class") <- c(class(obj), extra)
        return(obj)
      }

    }
  })


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
    tibble::tibble(start = i, N = N, value = paste(ngram, collapse = ","))
  })
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
segment_phrase <- function(note_track) {
  # (originally add_phrase_info from KF; see below)
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


add_phrase_info <- function(note_track, end_track){

  note_track <- note_track %>% dplyr::mutate(onset = time/1000)
  final_ioi <- diff(c(note_track$onset[length(note_track$onset)], end_track/1000))
  note_track <- note_track %>% dplyr::mutate(ioi = round(c(diff(onset), final_ioi), 2),
                                             note_pos = 1:dplyr::n())

  outliers <- note_track %>% dplyr::pull(ioi) %>% boxplot(plot = FALSE) %>% `[[`("out")
  #outliers <- outliers[outliers > .65]
  note_track %>%
    dplyr::mutate(phrasend = as.numeric(ioi %in% outliers | note_pos == length(note_pos)),
                  phrasbeg = as.numeric(dplyr::lag(phrasend) | note_pos == 1))
}


#' Get the cents between two vectors
#'
#' @param vectora
#' @param vectorb
#'
#' @return
#' @export
#'
#' @examples
vector_cents_between_two_vectors <- function(vectora, vectorb) {
  # for each note (as a freq) in a vector, get the cents difference of each note in vector A and vector B
  res <- c()
  for (n in 1:length(vectora)) {
    cent_res <- cents(vectora[n], vectorb[n])
    res <- c(res, cent_res)
  }
  res
}


#' Get cents between two notes
#'
#' @param notea The frequency in Hz of note a.
#' @param noteb The frequency in Hz of note b.
#'
#' @return
#' @export
#'
#' @examples
cents <- function(notea, noteb) {
  # get the cents between two notes (as frequencies)
  res <- 1200 * log2(noteb/notea)
  res
}

#' Get cents between vector and reference note
#'
#' @param reference_note In Hz.
#' @param vector_of_values In Hz.
#'
#' @return
#' @export
#'
#' @examples
vector_cents <- function(reference_note, vector_of_values) {
  # given a vector of values and a target note, give the cents of the vector note relative to the target note
  res <- vapply(vector_of_values, cents, "notea" = reference_note, FUN.VALUE = 100.001)
  res
}


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
  stopifnot(
    is.numeric(rel_mel) && length(rel_mel) > 0,
    is.scalar.numeric(start_note)
    )
  rel_mel <- c(0, cumsum(rel_mel))
  rel_mel + start_note
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
    pitch_class <- if(is.na(midi_note)) NA else midi.to.sci.notation.list[[as.character(midi_note)]]
  } else {
    pitch_class <- unlist(lapply(midi_note, function(x) {
      if(is.na(x)) NA else midi.to.sci.notation.list[[as.character(x)]]
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
sci_notation_to_midi <- Vectorize(function(sci_notation) {

  note_without_octave <- remove_last_char_of_string(sci_notation)
  last_char <- get_last_char_of_string(note_without_octave)

  if(last_char == "b") {
    midi <- sci.notation.to.midi.list.flat[[as.character(sci_notation)]]
  }
  else if(last_char == "#") {
    midi <- sci.notation.to.midi.list.sharp[[as.character(sci_notation)]]
  }
  else {
    midi <- sci.notation.to.midi.list[[as.character(sci_notation)]]
  }

  return(midi)

})


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
pitch_class_to_midi_notes <- Vectorize(function(pitch_class) {
    pitch.class.to.midi.list[[pitch_class]]
})


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




vector_cents_first_note <- function(vector_of_values) {
  # given a vector of frequencies, give the cents relative to the first note
  res <- vapply(vector_of_values, cents, "notea" = vector_of_values[1], FUN.VALUE = 100.001)
  res
}


not_all_na <- function(x) any(!is.na(x))


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
       "durations" = round(durations, 2))
}


input_check <- function(midi_file_dir, musicxml_file_dir, input_df) {

  if(is.null(midi_file_dir) & is.null(musicxml_file_dir) & is.null(input_df)) {

      stop('At least one of midi_file_dir, musicxml_file_dir or input_df is required.')

  } else if(!is.null(midi_file_dir) & !is.null(input_df)) {

      stop('You may only use the input_df OR the midi_file_dir arguments.')

  } else if(!is.null(musicxml_file_dir) & !is.null(input_df)) {

      stop('You may only use the input_df OR the musicxml_file_dir arguments.')

  } else if(!is.null(midi_file_dir) & !is.null(musicxml_file_dir) & !is.null(input_df)) {

      stop('You may only use the input_df OR the musicxml_file_dir and midi_file_dir arguments.')

  } else {
      return()
  }

}



get_item_bank_name <- function(item_bank) attr(item_bank, which = "item_bank_name")

is_na_scalar <- function(x) {
  all(is.na(x)) & length(x) == 1
}

is_null_scalar <- function(x) {
  all(is.null(x)) & length(x) == 1
}


sym_diff <- function(a,b) setdiff(union(a,b), intersect(a,b))


possible_output_types <- function() {
  c("all", "file", "item", "phrase", "ngram", "combined")
}


rename_to_parent <- function(df) {
  df %>%
    dplyr::rename(parent_abs_melody = abs_melody,
                  parent_melody = melody,
                  parent_durations = durations,
                  parent_N = N)
}


# Plotting functions


#' Plot histograms of the item bank vars/features
#'
#' @param item_bank
#' @param nrow
#' @param ncol
#'
#' @return
#' @export
#'
#' @examples
hist_item_bank <- function(item_bank, nrow = NULL, ncol = NULL) {

  item_bank %>%
    dplyr::select(where(is.numeric)) %>%
    tidyr::gather() %>%
    ggplot2::ggplot(ggplot2::aes(value)) +
    ggplot2::geom_histogram() +
    ggplot2::facet_wrap(~key, scales = "free_x", nrow = nrow, ncol = ncol)

}

