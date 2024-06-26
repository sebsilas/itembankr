

create_item_bank_from_files <- function(midi_file_dir = NULL,
                                        musicxml_file_dir = NULL,
                                        slice_head = NULL) {


  if(!is.null(midi_file_dir)) {

    midi_files <- list.files(path = midi_file_dir, pattern = "\\.midi$|\\.mid$",  full.names = TRUE, ignore.case = TRUE)

    if(!is.null(slice_head)) midi_files <- midi_files[1:slice_head]

    midi_files_df <- purrr::map_dfr(midi_files, function(f) {
      print(f)
      tryCatch({
        midi_file_to_notes_and_durations(f, produce_extra_melodic_features = FALSE) },
        error = function(err) {
          f <- basename(f)
          print(paste0("Error with ", f))
          print(err)
          tibble::tibble(onset = NA, durations = NA, note = NA, midi_file = f, N = NA)
        })
    })  %>%
      dplyr::mutate(file_key = tools::file_path_sans_ext(midi_file)) %>%
      dplyr::rename(abs_melody = note) %>%
      dplyr::select(abs_melody, durations, onset, midi_file, file_key)

    res <- midi_files_df %>%
      dplyr::filter(!is.na(abs_melody))

  }

  if(!is.null(musicxml_file_dir)) {

    # NB. For some reason musicxml files currently don't add up often, with abs_melody and durations

    music_xml_files <- list.files(path = musicxml_file_dir, pattern = "\\.musicxml$",  full.names = TRUE, ignore.case = TRUE)
    music_xml_files_df <- purrr::map_dfr(music_xml_files, function(f) {
      print(f)
      musicxml_file_to_notes_and_durations(f)
    }) %>%
      dplyr::mutate(file_key = tools::file_path_sans_ext(musicxml_file))
    res <- music_xml_files_df

  }

  if(!is.null(midi_file_dir) & !is.null(musicxml_file_dir)) {

    res <- midi_files_df %>%
      dplyr::left_join(music_xml_files_df, by = "file_key", suffix = c("_midi", "_musicxml")) %>%
      dplyr::select(-c(abs_melody_musicxml, durations_musicxml)) %>%  # Use MIDI. musicxml seems more reliable
      dplyr::rename(abs_melody = abs_melody_midi,
                    durations = durations_midi)
  }

  res %>%
    dplyr::rowwise() %>%
      dplyr::mutate(N = length(itembankr::str_mel_to_vector(abs_melody))) %>%
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

  # For some reason musicxml files currently don't add up often, with abs_melody and durations


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
  durations <- converted_pitches_and_durs$durations

  tibble::tibble(abs_melody = paste0(pitches, collapse = ","),
                 durations = paste0(durations, collapse = ","),
                 musicxml_file = basename(musicxml_file),
                 N = length(pitches))
}
