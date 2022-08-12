

split_item_bank_into_ngrams <- function(item_bank, M = NULL, get_ngrukkon = TRUE) {

  if(!"midi_file" %in% names(item_bank)) {
    item_bank$midi_file <- NA
  }

  if(!"musicxml_file" %in% names(item_bank)) {
    item_bank$musicxml_file <- NA
  }

  item_bank <- item_bank %>%
    dplyr::select(orig_abs_melody, orig_durations, orig_N, orig_melody, midi_file, musicxml_file) %>%
    dplyr::mutate(id = 1:nrow(item_bank))


  ngrams <- purrr::pmap_dfr(item_bank, function(orig_abs_melody,
                                                orig_durations,
                                                orig_N,
                                                orig_melody,
                                                midi_file,
                                                musicxml_file,
                                                id) {

    cat('row id is: ', id,'/',nrow(item_bank), '\n')


    if(is.null(M)) {
      M <- orig_N-1
    }

    ngram_res <- get_ngrams_multiple_sizes(str_mel_to_vector(orig_abs_melody), M) %>%
      dplyr::mutate(orig_durations = orig_durations,
                    orig_abs_melody = orig_abs_melody,
                    orig_melody = orig_melody,
                    orig_N = orig_N,
                    midi_file = midi_file,
                    musicxml_file = musicxml_file)
  })


  ngrams$id <- 1:nrow(ngrams)


  if(!is.null(item_bank$orig_durations)) {
    ngrams <- clip_durations(ngrams)
  }

  ngrams <- ngrams %>%
    dplyr::rowwise() %>%
    dplyr::mutate(melody = list(diff(str_mel_to_vector(abs_melody)))) %>%
    dplyr::ungroup()


  if(get_ngrukkon) {
    print('computing similarity')
    ngrams <- ngrams %>%
    dplyr::filter(N > 3 & length(itembankr::str_mel_to_vector(orig_abs_melody))) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(ngrukkon = musicassessr::ngrukkon(itembankr::str_mel_to_vector(orig_abs_melody),
                                                    itembankr::str_mel_to_vector(abs_melody))) %>%
    dplyr::ungroup()
    print('similarity complete')

  }

  ngrams <- ngrams %>%
    dplyr::select(-id) %>%
    dplyr::relocate(abs_melody, durations, melody, N)

  ngrams

}



#' Get ngrams of multiple sizes
#'
#' @param rel_melody
#' @param M
#'
#' @return
#' @export
#'
#' @examples
get_ngrams_multiple_sizes <- function(abs_melody, M) {

  if (length(abs_melody) == 1) {
    ngrams.multi <- tidyr::tibble(start = NA, N = 1, value = paste(abs_melody, collapse = ","))
  } else if (length(abs_melody) == 2) {
    ngrams.multi <- tidyr::tibble(start = NA, N = 2, value = paste(abs_melody, collapse = ","))
  } else {
    if(length(abs_melody) < M) {
      M <- length(abs_melody)
    }

    # grab all ngrams from 1:M for a given relative melody
    ngrams.multi <- purrr::map_dfr(1:M, get_all_ngrams, x = abs_melody)
    # but this shouldn't be called if the melody length is shorter than the ngram length, obv..
  }
  ngrams.multi
}



# internal functions


clip_durations <- function(df) {

  purrr::pmap_dfr(df, function(start, N, value, orig_durations, orig_abs_melody, orig_melody,
                                orig_N, midi_file, musicxml_file, id) {

    print(paste0('clipping durations: ', id, '/', nrow(df)))

    start <- as.numeric(start)

    if(is.na(start) | is.null(start)) {
      durations <- NA
    } else {
      end <- start + as.numeric(N)-1
      dur_v <- orig_durations %>% str_mel_to_vector()
      durations <- dur_v[start:end]
      durations <- paste0(durations, collapse = ",")
    }

    tibble::tibble(id = id,
                   orig_melody = orig_melody,
                   orig_abs_melody = orig_abs_melody,
                   abs_melody = value,
                   orig_durations = orig_durations,
                   durations = durations,
                   N = length(start:end),
                   orig_N = orig_N,
                   midi_file = midi_file,
                   musicxml_file = musicxml_file)

  })


}




