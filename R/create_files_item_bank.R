

# t <- create_item_bank_from_files(musicxml_file_dir = '/Users/sebsilas/Berkowitz/data-raw/berkowitz_musicxml')
# t2 <- create_item_bank_from_files(midi_file_dir = '/Users/sebsilas/Berkowitz/data-raw/berkowitz_midi_rhythmic')
# t3 <- create_item_bank_from_files(midi_file_dir = '/Users/sebsilas/Berkowitz/data-raw/berkowitz_midi_rhythmic',
#                                   musicxml_file_dir = '/Users/sebsilas/Berkowitz/data-raw/berkowitz_musicxml')

create_item_bank_from_files <- function(midi_file_dir = NULL,
                                        musicxml_file_dir = NULL) {


  if(!is.null(midi_file_dir)) {

    midi_files <- list.files(path = midi_file_dir, pattern = "\\.mid$",  full.names = TRUE)
    midi_files_df <- purrr::map_dfr(midi_files, function(f) {
      print(f)
      midi_file_to_notes_and_durations(f, produce_extra_melodic_features = FALSE)
    })  %>%
      dplyr::mutate(file_key = tools::file_path_sans_ext(midi_file)) %>%
      dplyr::rename(orig_abs_melody = note) %>%
      dplyr::select(orig_abs_melody, durations, midi_file, file_key)

    res <- midi_files_df

  }

  if(!is.null(musicxml_file_dir)) {

    # NB. For some reason musicxml files currently don't add up often, with orig_abs_melody and durations

    music_xml_files <- list.files(path = musicxml_file_dir, pattern = "\\.musicxml$",  full.names = TRUE)
    music_xml_files_df <- purrr::map_dfr(music_xml_files, function(f) {
      print(f)
      musicxml_file_to_notes_and_durations(f)
    }) %>%
      dplyr::mutate(file_key = tools::file_path_sans_ext(musicxml_file))
    res <- music_xml_files_df

  }

  if(!is.null(midi_file_dir) & !is.null(musicxml_file_dir)) {

    res <- midi_files_df %>% dplyr::left_join(music_xml_files_df, by = "file_key", suffix = c("_midi", "_musicxml"))

  }

  res <- res %>%
    dplyr::rowwise() %>%
      dplyr::mutate(N = length(itembankr::str_mel_to_vector(orig_abs_melody))) %>%
    dplyr::ungroup()
}





#' Get notes and durations from a MIDI file
#'
#' @param midi_file
#' @param string_df
#' @param produce_extra_melodic_features
#'
#' @return
#' @export
#'
#' @examples
midi_file_to_notes_and_durations <- function(midi_file, string_df = TRUE, produce_extra_melodic_features = TRUE) {

  midi_file_dat <- tuneR::readMidi(midi_file)

  tempo <- midi_file_dat %>%
    dplyr::filter(event == "Set Tempo") %>%
    dplyr::pull(parameterMetaSystem) %>% as.numeric()

  if(length(tempo) != 1) {
    tempo_bpm <- microseconds_per_beat_to_bpm(tempo)
    tempo <- tempo[1]
  }

  notes <- tuneR::getMidiNotes(midi_file_dat) %>%
    tibble::as_tibble()

  out <- notes %>% dplyr::mutate(onset = round(ticks_to_ms(time, ppq = get_division_from_midi_file(midi_file), tempo = tempo), 2),
                                 durations = round(ticks_to_ms(length, ppq = get_division_from_midi_file(midi_file), tempo = tempo), 2)) %>%
    dplyr::select(onset, durations, note) %>%
      dplyr::mutate(midi_file = basename(midi_file),
                    N = nrow(notes))

  if(produce_extra_melodic_features) {
    out <- out %>% produce_extra_melodic_features()
  }
  if(string_df) {
    out <- out %>% musicassessr::to_string_df(exclude_cols = "midi_file")
  }
  out
}

musicxml_file_to_notes_and_durations <- function(musicxml_file, relativeMelodies = TRUE, relativeDurations = FALSE) {

  # For some reason musicxml files currently don't add up often, with orig_abs_melody and durations


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
  # pitches <- converted_pitches_and_durs$pitches
  durations <- converted_pitches_and_durs$durations

  tibble::tibble(orig_abs_melody = paste0(pitches, collapse = ","),
                 durations = paste0(durations, collapse = ","),
                 musicxml_file = basename(musicxml_file),
                 N = length(pitches))
}

