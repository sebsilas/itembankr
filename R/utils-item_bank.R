

# TODO: Optimise the 3 remove_melodies function (there is redundancy between them)

remove_melodies <- function(item_bank, remove_melodies_with_only_repeated_notes, remove_melodies_with_any_repeated_notes) {

  item_bank <- item_bank %>%
    {if(remove_melodies_with_only_repeated_notes) remove_melodies_with_only_repeated_notes(.) else . } %>%
    {if(remove_melodies_with_any_repeated_notes) remove_melodies_with_any_repeated_notes(.) else . }

  item_bank
}


remove_melodies_with_any_repeated_notes <- function(item_bank) {
  item_bank %>%
    dplyr::rowwise() %>%
    dplyr::mutate(consec_repeated_values = try_or_log_error_return_na(dplyr::case_when(any(rle( str_mel_to_vector(abs_melody) )$lengths > 1) ~ TRUE, TRUE ~ FALSE ))) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!consec_repeated_values)
    dplyr::select(-consec_repeated_values)
}


remove_melodies_with_only_repeated_notes <- function(item_bank) {
  item_bank %>%
    dplyr::rowwise() %>%
    dplyr::mutate(consec_repeated_values = try_or_log_error_return_na(dplyr::case_when( stats::var(str_mel_to_vector(abs_melody)) == 0 ~ TRUE, TRUE ~ FALSE ))) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!consec_repeated_values) %>%
    dplyr::select(-consec_repeated_values)
}


# ---- helper: attach AUD_* by META_file_key if possible ----
attach_audio_features <- function(mel_df, aud_df) {

  mel_df$META_audio_file <- NULL
  if (is_na_scalar(mel_df) || is_na_scalar(aud_df)) return(mel_df)
  if (!all(c("META_file_key") %in% c(names(mel_df), names(aud_df)))) return(mel_df)

  aud_keep <- dplyr::select(
    aud_df,
    META_file_key,
    META_audio_file,
    dplyr::starts_with("AUD_")
  )

  dplyr::full_join(mel_df, aud_keep, by = "META_file_key")
}







