

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
    dplyr::mutate(consec_repeated_values = dplyr::case_when(any(rle( str_mel_to_vector(abs_melody) )$lengths > 1) ~ TRUE, TRUE ~ FALSE )) %>%
    dplyr::ungroup() %>%
    dplyr::filter(consec_repeated_values == FALSE)
    dplyr::select(-consec_repeated_values)
}


remove_melodies_with_only_repeated_notes <- function(item_bank) {
  item_bank %>%
    dplyr::rowwise() %>%
    dplyr::mutate(consec_repeated_values = dplyr::case_when( stats::var(str_mel_to_vector(abs_melody)) == 0 ~ TRUE, TRUE ~ FALSE )) %>%
    dplyr::ungroup() %>%
    dplyr::filter(consec_repeated_values == FALSE) %>%
    dplyr::select(-consec_repeated_values)
}






