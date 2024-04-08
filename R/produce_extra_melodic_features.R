


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
    segment_phrase() %>%
    musicassessr::expand_string_df_row() %>%
    dplyr::mutate(sci_notation = midi_to_sci_notation(note),
                  interval = c(NA, diff(note)), ioi = c(NA, diff(onset)),
                  ioi_class = classify_duration(ioi))

  if(!"freq" %in% names(pyin_style_res)) {
    pyin_style_res <- pyin_style_res %>% dplyr::mutate(freq = hrep::midi_to_freq(note))
  }

  pyin_style_res %>% dplyr::mutate(
    cents_deviation_from_nearest_midi_pitch = vector_cents_between_two_vectors(round(hrep::midi_to_freq(hrep::freq_to_midi(freq))), freq),
    # the last line looks tautological, but, by converting back and forth, you get the quantised pitch and can measure the cents deviation from this
    pitch_class = midi_to_pitch_class(round(hrep::freq_to_midi(freq))),
    pitch_class_numeric = midi_to_pitch_class_numeric(round(hrep::freq_to_midi(freq))),
    interval_cents = itembankr::cents(dplyr::lag(freq), freq)
  )

}
