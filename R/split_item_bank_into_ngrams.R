

split_item_bank_into_ngrams <- function(item_bank) {
  warning('This could take a long time.')

  ngrams <- apply(item_bank, MARGIN = 1, function(row) {
    ngram_res <- get_ngrams_multiple_sizes(str_mel_to_vector(row['melody'], sep = ","), 8)
    cbind(ngram_res,
          data.frame(midi_file = row['midi_file'],
                     musicxml_file = row['musicxml_file'],
                     durations = row['durations'])
    )
  })
  ngrams <- dplyr::bind_rows(ngrams)
  ngrams <- clip_durations(ngrams)
  ngrams <- ngrams %>% dplyr::rename(melody = value)
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
    start <- as.numeric(row['start'])
    N <- as.numeric(row['N'])
    end <- start + N
    dur_v <- str_mel_to_vector(row['durations'], ",")
    res <- dur_v[start:(end-1)]
    paste0(res, collapse = ",")
  })

  df$durations <- durs
  df
}

get_ngrams_multiple_sizes <- function(rel_melody, M) {

  if (length(rel_melody) == 1) {
    ngrams.multi <- rel_melody
  }

  else if (length(rel_melody) == 2) {
    ngrams.multi <- as.list(rel_melody)
  }

  else {
    # grab all ngrams from 1:M for a given relative melody
    ngrams.multi <- dplyr::bind_rows(mapply(get_all_ngrams, N = 1:M, MoreArgs = list(
      "x" = rel_melody),
      "SIMPLIFY" = FALSE))
  }
  ngrams.multi
}

