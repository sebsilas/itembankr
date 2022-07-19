
split_item_bank_into_ngrams <- function(item_bank, M = NULL) {

  #warning('This could take a long time.')


  ngrams <- purrr::pmap_dfr(item_bank, function(id,
                                                orig_abs_melody,
                                                durations,
                                                N,
                                                melody,
                                                midi_file,
                                                musicxml_file) {

    cat('row id is: ', id, '\n')
    print('durations?')
    print(durations)


    if(is.null(M)) {
      M <- N-1
    }

    ngram_res <- get_ngrams_multiple_sizes(str_mel_to_vector(melody), M)

    cbind(ngram_res,
          tibble::tibble(durations = durations,
                        melody = melody,
                        midi_file = midi_file,
                        musicxml_file = musicxml_file))
  })

  print('here?')

  ngrams$row_id <- 1:nrow(ngrams)

  if(!is.na(item_bank$durations) & is.null(item_bank$durations)) {
    ngrams <- clip_durations(ngrams)
  }


  if("value" %in% names(ngrams)) {
    ngrams <- ngrams %>%
      dplyr::select(-melody) %>%
      dplyr::rename(melody = value)
  }

  shared_names <- intersect(names(item_bank), names(ngrams))

  item_bank <- item_bank %>% dplyr::select(shared_names)
  ngrams <- ngrams %>% dplyr::select(shared_names)

  rbind(item_bank, ngrams)
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
get_ngrams_multiple_sizes <- function(rel_melody, M) {

  if (length(rel_melody) == 1) {
    ngrams.multi <- tidyr::tibble(start = NA, N = 1, value = paste(rel_melody, collapse = ","))
  } else if (length(rel_melody) == 2) {
    # ngrams.multi <- as.list(rel_melody)
    ngrams.multi <- tidyr::tibble(start = NA, N = 2, value = paste(rel_melody, collapse = ","))
  } else {
    if(length(rel_melody) < M) {
      M <- length(rel_melody)
    }

    # grab all ngrams from 1:M for a given relative melody
    ngrams.multi <- purrr::map_dfr(1:M, get_all_ngrams, x = rel_melody)
    # but this shouldn't be called if the melody length is shorter than the ngram length, obv..
  }
  ngrams.multi
}



# internal functions


clip_durations <- function(df) {

  purrr::pmap_dfr(df, function(start, N, value, orig_abs_melody, durations,
                           melody, midi_file, musicxml_file, row_id) {

    cat('no rows ngram db: ', nrow(df), '\n')
    cat('current row: ', row_id, '\n')


    start <- as.numeric(start)

    if(is.na(start) | is.null(start)) {
      durations <- NA
    } else {
      end <- start + as.numeric(N) + 1
      dur_v <- durations %>% str_mel_to_vector()
      res <- dur_v[start:(end-1)]
      durations <- paste0(res, collapse = ",")
    }

    tibble::tibble(melody = value, durations = durations,
                   midi_file = midi_file, musicxml_file = musicxml_file)

  })


}


