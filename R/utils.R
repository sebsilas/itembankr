rel_to_abs_mel <- function(rel_mel, start_note = 60) {
  c(0, cumsum(rel_mel)) + start_note
}

str_mel_to_vector <- function(str_mel, sep) {
  vector_mel <- as.numeric(unlist(strsplit(str_mel, sep)))
}

midi_to_pitch_class <- function(midi_note) {
  if (length(midi_note) == 1) {
    pitch_class <- midi.to.pitch.classes.list[[as.character(midi_note)]]
  }
  else {
    pitch_class <- unlist(lapply(midi_note, function(x) midi.to.pitch.classes.list[[as.character(x)]]))
  }
  pitch_class
}


midi_to_pitch_class_numeric <- function(midi_note) {
  if (length(midi_note) == 1) {
    pitch_class <- midi.to.pitch.classes.numeric.list[[as.character(midi_note)]]
  }
  else {
    pitch_class <- unlist(lapply(midi_note, function(x) midi.to.pitch.classes.numeric.list[[as.character(x)]]))
  }
  pitch_class
}


midi_to_sci_notation <- function(midi_note) {
  if (length(midi_note) == 1) {
    pitch_class <- midi.to.sci.notation.list[[as.character(midi_note)]]
  }
  else { pitch_class <- unlist(lapply(midi_note, function(x) midi.to.sci.notation.list[[as.character(x)]]))
  }
  pitch_class
}


sci_notation_to_midi <- function(sci_notation) {
  if (length(sci_notation) == 1) {
    midi <- sci.notation.to.midi.list[[as.character(sci_notation)]]
  }
  else {
    midi <- unlist(lapply(sci_notation, function(x) sci.notation.to.midi.list[[as.character(x)]]))
  }
  midi
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


# tests

#rel_bpm_to_seconds(c(2, 2, 4, 2, 2), bpm = 120)

#microseconds_per_beat_to_bpm(500000)
