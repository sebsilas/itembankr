

create_phrases_db <- function(item_df, phrase_segment_outlier_threshold = .65, ioi_threshold = .96) {

  if("midi_file" %in% names(item_df)) {
    phrase_df <- item_df %>%
      dplyr::rowwise() %>%
      dplyr::mutate(tmp_df = list(get_phrase_helper(abs_melody, durations, midi_file = midi_file, phrase_segment_outlier_threshold = phrase_segment_outlier_threshold, ioi_threshold = ioi_threshold))) %>%
      dplyr::ungroup() %>%
      dplyr::pull(tmp_df) %>%
      dplyr::bind_rows()
  } else if("musicxml_file" %in% names(item_df)) {
    phrase_df <- item_df %>%
      dplyr::rowwise() %>%
      dplyr::mutate(tmp_df = list(get_phrase_helper(abs_melody, durations, musicxml_file = musicxml_file, phrase_segment_outlier_threshold = phrase_segment_outlier_threshold, ioi_threshold = ioi_threshold))) %>%
      dplyr::ungroup() %>%
      dplyr::pull(tmp_df) %>%
      dplyr::bind_rows()
  } else if("midi_file" %in% names(item_df) && "musicxml_file" %in% names(item_df)) {
    phrase_df <- item_df %>%
      dplyr::rowwise() %>%
      dplyr::mutate(tmp_df = list(get_phrase_helper(abs_melody, durations, midi_file = midi_file, musicxml_file = musicxml_file, phrase_segment_outlier_threshold = phrase_segment_outlier_threshold, ioi_threshold = ioi_threshold))) %>%
      dplyr::ungroup() %>%
      dplyr::pull(tmp_df) %>%
      dplyr::bind_rows()
  } else {
    phrase_df <- item_df %>%
      dplyr::rowwise() %>%
      dplyr::mutate(tmp_df = list(get_phrase_helper(abs_melody, durations, phrase_segment_outlier_threshold = phrase_segment_outlier_threshold, ioi_threshold = ioi_threshold))) %>%
      dplyr::ungroup() %>%
      dplyr::pull(tmp_df) %>%
      dplyr::bind_rows()
  }

  phrase_df

}

get_phrase_helper <- function(abs_melody, durations, midi_file = NULL, musicxml_file = NULL, phrase_segment_outlier_threshold = .65, ioi_threshold = .96) {

  stopifnot(
    is.null.or(midi_file, is.scalar.character),
    is.null.or(musicxml_file, is.scalar.character),
    is.scalar.numeric(phrase_segment_outlier_threshold),
    is.scalar.numeric(ioi_threshold)
  )

  # NB. TO FIX: The need for the tryCatch is only when using a files_db created from musicxml file.
  # For some reason musicxml files currently don't add up often, with abs_melody and durations

  tryCatch({

    tibble::tibble(abs_melody = itembankr::str_mel_to_vector(abs_melody),
                   durations = itembankr::str_mel_to_vector(durations)) %>%
      dplyr::mutate(onset = cumsum(durations)) %>%
      segment_phrase(phrase_segment_outlier_threshold = phrase_segment_outlier_threshold,
                     ioi_threshold = ioi_threshold)

  }, error = function(err) {
    logging::logerror(err)
    logging::logerror("Item removed probably because mismatch in abs_melody and durations.")
    tibble::tibble(abs_melody = NA, durations = NA, onset = NA, ioi = NA, note_pos = NA, phrasend = NA, phrasbeg = NA)
  }) %>%
    dplyr::mutate(midi_file = !! midi_file,
                  musicxml_file = !! musicxml_file)


}



#' Segment a note track by adding phrasend and phrasbeg columns with boolean markers.
#'
#' @param note_track a data frame with an "onset" column
#' @param phrase_segment_outlier_threshold A threshold for selecting outliers for durations in phrase segmentation.
#' @param ioi_threshold A threshold for selecting outliers for iois in phrase segmentation.
#'
#' @return
#' @export
#'
#' @examples
segment_phrase <- function(note_track, phrase_segment_outlier_threshold = .65, ioi_threshold = .96) {

  # (originally add_phrase_info from KF; see below)
  note_track <- note_track %>% dplyr::mutate(ioi = c(0, diff(onset)), note_pos = dplyr::row_number())

  bp <- note_track %>% dplyr::pull(ioi) %>% boxplot(plot = FALSE)

  if(length(bp$out) > 0) {
    outliers <- bp$out
    outliers <- outliers[outliers > phrase_segment_outlier_threshold]
    note_track <- note_track %>%
      dplyr::mutate(phrasend = as.numeric(ioi > ioi_threshold | ioi %in% outliers),
                    phrasbeg = as.numeric(dplyr::lag(phrasend) | note_pos == 1))

    note_track$phrasend[is.na(note_track$phrasend)] <- 0
    note_track$phrasend[length(note_track$phrasend)] <- 1
  } else {
    note_track <- note_track %>%
      dplyr::mutate(phrasend = c(rep(0, nrow(.)-1), 1),
                    phrasbeg = c(1, c(rep(0, nrow(.)-1))))
  }


  phrase_info <- note_track %>%
    dplyr::mutate(phrasebeg_pos = dplyr::case_when(phrasbeg == 1 ~ note_pos, TRUE ~ NA),
                  phrasend_pos = dplyr::case_when(phrasend == 1 ~ note_pos, TRUE ~ NA)
    )

  phrase_info <-  tibble::tibble(phrasebeg_pos = phrase_info$phrasebeg_pos[!is.na(phrase_info$phrasebeg_pos)],
                                 phrasend_pos = phrase_info$phrasend_pos[!is.na(phrase_info$phrasend_pos)]
  )

  phrase_db <- purrr::pmap_dfr(phrase_info, function(phrasebeg_pos, phrasend_pos) {
    note_track %>% dplyr::filter(dplyr::between(note_pos, phrasebeg_pos, phrasend_pos)) %>%
      musicassessr::to_string_df() %>%
      dplyr::select(-c(note_pos, phrasend, phrasbeg))
  })

  return(phrase_db)
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

