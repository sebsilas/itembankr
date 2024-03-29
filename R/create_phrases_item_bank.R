

create_phrases_db <- function(item_df) {

  new <- item_df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(tmp_df = list(get_phrase_helper(abs_melody, durations))) %>%
    dplyr::ungroup() %>%
    dplyr::pull(tmp_df) %>%
    dplyr::bind_rows()

  if("midi_file" %in% names(item_df)) {
    midi <- item_df %>% dplyr::select(midi_file)
    item_df <- cbind(new, midi)
  }

  if("musicxml_file" %in% names(item_df)) {
    musicxml <- item_df %>% dplyr::select(musicxml_file)
    item_df <- cbind(new, musicxml)
  }

  if(!"midi_file" %in% names(item_df) & !"musicxml_file" %in% names(item_df)) {
    item_df <- new
  }


  item_df

}

get_phrase_helper <- function(abs_melody, durations) {

  # NB. TO FIX: The need for the tryCatch is only when using a files_db created from musicxml file.
  # For some reason musicxml files currently don't add up often, with abs_melody and durations

  tryCatch({
    t <- tibble::tibble(abs_melody = itembankr::str_mel_to_vector(abs_melody),
                        durations = itembankr::str_mel_to_vector(durations))
    t %>%
      musicassessr::expand_string_df_row() %>%
      dplyr::mutate(onset = cumsum(durations)) %>%
      segment_phrase() %>%
      musicassessr::to_string_df()
  }, error = function(err) {
    warning("Item removed because mismatch in abs_melody and durations.")
    tibble::tibble(abs_melody = NA, durations = NA, onset = NA, ioi = NA, note_pos = NA, phrasend = NA, phrasbeg = NA)
  })


}



