

create_item_bank_from_files <- function(midi_file_dir = NULL,
                                        musicxml_file_dir = NULL,
                                        audio_file_dir = NULL,
                                        slice_head = NULL,
                                        audio_only = FALSE) {

  #──────────────────────────────────────────────────────────────
  # MIDI FILES
  #──────────────────────────────────────────────────────────────
  if (!is.null(midi_file_dir)) {

    midi_files <- list.files(
      path = midi_file_dir,
      pattern = "\\.midi$|\\.mid$",
      full.names = TRUE,
      ignore.case = TRUE
    )

    if (length(midi_files) == 0) {
      cli::cli_abort(cli::col_red("midi_file_dir specified but no MIDI files found in directory."))
    }

    if (!is.null(slice_head)) midi_files <- midi_files[1:slice_head]

    midi_files_df <- purrr::map_dfr(midi_files, function(f) {

      cli::cli_inform(cli::col_blue(paste("Processing MIDI:", f)))

      tryCatch({
        midi_file_to_notes_and_durations(f, produce_extra_melodic_features = FALSE)
      }, error = function(err) {
        fbase <- basename(f)
        cli::cli_alert_danger(paste("Error with file:", fbase))
        tibble::tibble(
          onset = NA, durations = NA, note = NA,
          midi_file = fbase, N = NA, bpm = NA
        )
      })
    })

    midi_files_df <- midi_files_df %>%
      dplyr::mutate(
        file_key = tools::file_path_sans_ext(midi_file)
      ) %>%
      dplyr::rename(abs_melody = note) %>%
      dplyr::select(abs_melody, durations, onset, midi_file, file_key, bpm)

    res_midi <- midi_files_df %>% dplyr::filter(!is.na(abs_melody))
  }

  #──────────────────────────────────────────────────────────────
  # MUSICXML FILES
  #──────────────────────────────────────────────────────────────
  if (!is.null(musicxml_file_dir)) {

    music_xml_files <- list.files(
      path = musicxml_file_dir,
      pattern = "\\.musicxml$",
      full.names = TRUE,
      ignore.case = TRUE
    )

    if (length(music_xml_files) == 0) {
      cli::cli_abort(cli::col_red("musicxml_file_dir specified but no MusicXML files found in directory."))
    }

    music_xml_files_df <- purrr::map_dfr(music_xml_files, function(f) {
      cli::cli_inform(cli::col_blue(paste("Processing MusicXML:", f)))
      musicxml_file_to_notes_and_durations(f)
    }) %>%
      dplyr::mutate(file_key = tools::file_path_sans_ext(musicxml_file))

    res_musicxml <- music_xml_files_df
  }

  #──────────────────────────────────────────────────────────────
  # AUDIO FILES
  #──────────────────────────────────────────────────────────────
  if (!is.null(audio_file_dir)) {

    audio_files <- list.files(
      path = audio_file_dir,
      pattern = "\\.wav$|\\.mp3$|\\.flac$",
      full.names = TRUE,
      ignore.case = TRUE
    )

    if (length(audio_files) == 0) {
      cli::cli_abort(cli::col_red("audio_file_dir specified but no audio files found in directory."))
    }

    if (!is.null(slice_head)) audio_files <- audio_files[1:slice_head]

    res_audio <- tibble::tibble(audio_file = basename(audio_files)) %>%
      dplyr::mutate(file_key = tools::file_path_sans_ext(audio_file))
  }

  #──────────────────────────────────────────────────────────────
  # COMBINE SOURCES IF MULTIPLE ARE PROVIDED
  # Priority order: MIDI > MusicXML > Audio (you can change)
  #──────────────────────────────────────────────────────────────

  res_list <- list()

  if (exists("res_midi")) res_list <- append(res_list, list(res_midi))
  if (exists("res_musicxml")) res_list <- append(res_list, list(res_musicxml))
  if (exists("res_audio")) res_list <- append(res_list, list(res_audio))

  if (length(res_list) == 0) {
    cli::cli_abort("No data sources provided.")
  }

  # If only one source, return it
  res <- dplyr::bind_rows(res_list) %>%
    dplyr::group_by(file_key) %>%
    dplyr::slice_head(n = 1) %>%   # first available (MIDI if present)
    dplyr::ungroup()

  if(audio_only) {
    res <- res %>% dplyr::rename(META_file_key = file_key)
  } else {
    res <- res %>%
      dplyr::rowwise() %>%
      dplyr::mutate(N = length(itembankr::str_mel_to_vector(abs_melody))) %>%
      dplyr::ungroup()
  }


  return(res)

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
    tempo <- tempo[1]
  }

  tempo_bpm <- microseconds_per_beat_to_bpm(tempo)


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

  out <- out %>%
    dplyr::mutate(bpm = round(tempo_bpm))

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
