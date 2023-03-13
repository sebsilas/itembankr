

create_ngram_item_bank <- function(df, name) {

  df %>%
    rename_to_parent() %>%
    split_item_bank_into_ngrams() %>%
    count_freqs() %>%
    compute_ngram_similarity() %>%
    get_melody_features()
}

split_item_bank_into_ngrams <- function(item_bank, M = NULL, get_ngrukkon = TRUE) {

  if(!"midi_file" %in% names(item_bank)) {
    item_bank$midi_file <- NA
  }

  if(!"musicxml_file" %in% names(item_bank)) {
    item_bank$musicxml_file <- NA
  }

  item_bank <- item_bank %>%
    dplyr::select(parent_durations, parent_N, parent_abs_melody, parent_melody, midi_file, musicxml_file) %>%
    dplyr::mutate(id = 1:nrow(item_bank))

  ngrams <- purrr::pmap_dfr(item_bank, function(parent_durations,
                                                parent_N,
                                                parent_abs_melody,
                                                parent_melody,
                                                midi_file,
                                                musicxml_file,
                                                id) {

    cat('row id is: ', id,'/',nrow(item_bank), '\n')


    if(is.null(M)) {
      M <- parent_N-1
    }

    ngram_res <- get_ngrams_multiple_sizes(str_mel_to_vector(parent_abs_melody), M) %>%
      dplyr::mutate(parent_durations = parent_durations,
                    parent_abs_melody = parent_abs_melody,
                    parent_melody = parent_melody,
                    parent_N = parent_N,
                    midi_file = midi_file,
                    musicxml_file = musicxml_file)
  })


  ngrams$id <- 1:nrow(ngrams)


  if(!is.null(item_bank$parent_durations)) {
    ngrams <- clip_durations(ngrams)
  }


  ngrams %>%
    dplyr::rowwise() %>%
    dplyr::mutate(melody = paste0(diff(str_mel_to_vector(abs_melody)), collapse = ",")) %>%
    dplyr::ungroup()

}


compute_ngram_similarity <- function(ngrams, get_ngrukkon = TRUE) {

  print('compute_ngram_similarity')
  print(ngrams)

  if(get_ngrukkon) {
    print('computing similarity')
    ngrams <- ngrams %>%
      dplyr::filter(N > 3 & length(itembankr::str_mel_to_vector(parent_abs_melody)),
                    N > 3 & length(itembankr::str_mel_to_vector(abs_melody))) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(ngrukkon = musicassessr::ngrukkon(itembankr::str_mel_to_vector(parent_abs_melody),
                                                      itembankr::str_mel_to_vector(abs_melody))) %>%
      dplyr::ungroup()
    print('similarity complete')

  }

  ngrams <- ngrams %>%
    dplyr::select(-id) %>%
    dplyr::relocate(durations, abs_melody, N)
}



#' Get ngrams of multiple sizes
#'
#' @param abs_melody
#' @param M
#'
#' @return
#' @export
#'
#' @examples
get_ngrams_multiple_sizes <- function(abs_melody, M) {

  if (length(abs_melody) == 1) {
    ngrams.multi <- tibble::tibble(start = NA, N = 1, value = paste(abs_melody, collapse = ","))
  } else if (length(abs_melody) == 2) {
    ngrams.multi <- tibble::tibble(start = NA, N = 2, value = paste(abs_melody, collapse = ","))
  } else {
    if(length(abs_melody) < M) {
      M <- length(abs_melody)
    }

    # grab all ngrams from 1:M for a given melody
    ngrams.multi <- purrr::map_dfr(3:M, get_all_ngrams, x = abs_melody)
    # but this shouldn't be called if the melody length is shorter than the ngram length, obv..
  }
  ngrams.multi
}



clip_durations <- function(df) {

  print(names(df))

  purrr::pmap_dfr(df, function(start, N, value, parent_durations, parent_abs_melody,
                               parent_melody, parent_N, midi_file, musicxml_file, id) {

    print(paste0('clipping durations: ', id, '/', nrow(df)))

    start <- as.numeric(start)
    end <- start + as.numeric(N)-1

    if(is.null(start) | is.null(end) |
       is.na(start) | is.na(end)) {
       durations <- NA
       len <- as.numeric(NA)
    } else {
        dur_v <- parent_durations %>% str_mel_to_vector()
        durations <- dur_v[start:end]
        durations <- paste0(durations, collapse = ",")
        len <- length(start:end)
    }


    tibble::tibble(id = id,
                   parent_abs_melody = parent_abs_melody,
                   abs_melody = value,
                   parent_durations = parent_durations,
                   durations = durations,
                   N = as.numeric(len),
                   parent_N = parent_N,
                   midi_file = midi_file,
                   musicxml_file = musicxml_file)

  })


}




