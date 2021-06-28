

split_item_bank_into_ngrams <- function(item_bank) {
  warning('This could take a long time.')

  ngrams <- apply(item_bank, MARGIN = 1, function(row) {
    ngram_res <- get_ngrams_multiple_sizes(str_mel_to_vector(row['melody'], ","), 8)
    cbind(ngram_res,
          data.frame(midi_file = row['midi_file'],
                     musicxml_file = row['musicxml_file'],
                     durations = row['durations'])
    )
  })
  ngrams <- bind_rows(ngrams)
  ngrams <- clip_durations(ngrams)
  ngrams <- ngrams %>% dplyr::rename(melody = value)
}


count_freqs <- function(item_bank) {

  value.counts <- base::table(item_bank$melody)

  # to DF
  value.counts.df <- as.data.frame(value.counts)

  # rename and sort descending
  value.counts.df <- value.counts.df %>% dplyr::rename(melody = Var1, freq = Freq) %>%
    dplyr::arrange(dplyr::desc(freq))

  # convert from factor
  value.counts.df$melody <- as.character(value.counts.df$melody)

  value.counts.df

  total_freq <- sum(value.counts.df$freq)

  # add N
  value.counts.df$N <- lapply(value.counts.df$melody, function(x) length(str_mel_to_vector(x, ",")))

  value.counts.df <- value.counts.df %>% mutate(rel_freq = freq/total_freq)
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
    ngrams.multi <- bind_rows(mapply(get_all_ngrams, N = 1:M, MoreArgs = list(
      "x" = rel_melody),
      "SIMPLIFY" = FALSE))
  }
  ngrams.multi
}

