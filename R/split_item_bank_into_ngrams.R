

split_item_bank_into_ngrams <- function(item_bank) {

  warning('This could take a long time.')
  ngrams <- apply(item_bank, MARGIN = 1, function(row) {

    ngram_res <- get_ngrams_multiple_sizes(str_mel_to_vector(row['melody'], sep = ","), 8)

    if(!is.null(row['midi_file']) & !is.na(row['midi_file'])) {
      ngram_res <-  cbind(ngram_res, midi_file = row['midi_file'])
    }
    if(!is.null(row['musicxml_file']) & !is.na(row['musicxml_file'])) {
      ngram_res <- cbind(ngram_res, data.frame(musicxml_file = row['musicxml_file']))
    }
    if(!is.null(row['durations']) & !is.na(row['durations'])) {
      ngram_res <- cbind(ngram_res, data.frame(durations = row['durations']))
    }
    ngram_res
  })

  ngrams <- dplyr::bind_rows(ngrams)
  row.names(ngrams) <- 1:nrow(ngrams)
  ngrams <- clip_durations(ngrams)

  if("value" %in% names(ngrams)) {
    ngrams <- ngrams %>% dplyr::rename(melody = value)
  }

  ngrams
}


count_freqs <- function(item_bank) {

  values_counts <- item_bank %>%
    dplyr::add_count(melody, name = "freq") %>%
      dplyr::arrange(dplyr::desc(freq))

  total_freq <- sum(values_counts$freq)

  # add N
  values_counts$N <- lapply(values_counts$melody, function(x) length(str_mel_to_vector(x, ",")))

  values_counts <- values_counts %>% dplyr::mutate(rel_freq = freq/total_freq)
}


# internal functions


clip_durations <- function(df) {
  durs <- apply(df, MARGIN = 1, function(row) {
    if(is.na(row['start'])) {
      row['durations']
    } else {
      start <- as.numeric(row['start'])
      N <- as.numeric(row['N'])
      end <- start + N + 1
      dur_v <- str_mel_to_vector(row['durations'], ",")
      res <- dur_v[start:(end-1)]
      paste0(res, collapse = ",")
    }
  })

  df$durations <- durs
  df
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
    #ngrams.multi <- rel_melody
    ngrams.multi <- tidyr::tibble(start = NA, N = 1, value = paste(rel_melody, collapse = ","))
  }

  else if (length(rel_melody) == 2) {
    # ngrams.multi <- as.list(rel_melody)
    ngrams.multi <- tidyr::tibble(start = NA, N = 2, value = paste(rel_melody, collapse = ","))
  }

  else {
    if(length(rel_melody) < M) {
      M <- length(rel_melody)
    }
    # grab all ngrams from 1:M for a given relative melody
    ngrams.multi <- dplyr::bind_rows(mapply(get_all_ngrams, N = 1:M, MoreArgs = list(
      "x" = rel_melody),
      "SIMPLIFY" = FALSE))
  }
  ngrams.multi
}
