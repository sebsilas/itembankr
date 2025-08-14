

#' Create ngram item bank
#'
#' @param df
#' @param lower_ngram_bound
#' @param upper_ngram_bound
#' @param get_ngrukkon
#' @param get_features
#'
#' @return
#' @export
#'
#' @examples
create_ngram_item_bank <- function(df, lower_ngram_bound = 3L, upper_ngram_bound = NULL, get_ngrukkon = TRUE, get_features = TRUE) {

  df %>%
    rename_to_parent() %>%
    split_item_bank_into_ngrams(lower_ngram_bound, upper_ngram_bound, get_ngrukkon) %>%
    count_freqs() %>%
    compute_ngram_similarity(get_ngrukkon) %>%
    { if(get_features) get_melody_features(.) else . }
}

split_item_bank_into_ngrams <- function(item_bank,
                                        lower_ngram_bound = 3L,
                                        upper_ngram_bound = NULL,
                                        get_ngrukkon = TRUE) {

  # make sure expected columns exist, regardless of prefixing upstream
  item_bank <- ensure_parent_cols(item_bank)

  # keep a minimal set (safe-select)
  item_bank <- item_bank %>%
    dplyr::select(dplyr::any_of(c(
      "parent_durations", "parent_N", "parent_abs_melody", "parent_melody",
      "midi_file", "musicxml_file"
    ))) %>%
    dplyr::mutate(id = dplyr::row_number())

  ngrams <- purrr::pmap_dfr(item_bank, function(parent_durations,
                                                parent_N,
                                                parent_abs_melody,
                                                parent_melody,
                                                midi_file,
                                                musicxml_file,
                                                id) {

    cat('Row id is: ', id, '/', nrow(item_bank), '\n')

    ub <- upper_ngram_bound
    if (is.null(ub)) ub <- parent_N - 1L

    get_ngrams_multiple_sizes(
      str_mel_to_vector(parent_abs_melody),
      lower_ngram_bound, ub
    ) %>%
      dplyr::mutate(
        parent_durations  = parent_durations,
        parent_abs_melody = parent_abs_melody,
        parent_melody     = parent_melody,
        parent_N          = parent_N,
        midi_file         = midi_file,
        musicxml_file     = musicxml_file
      )
  })

  # Give new ID
  ngrams <- ngrams %>% dplyr::mutate(id = dplyr::row_number())

  # Optional duration clipping if parent_durations exists
  if ("parent_durations" %in% names(ngrams)) {
    ngrams <- clip_durations(ngrams)
  }

  # derive relative melody & onset for each n-gram
  ngrams %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      melody = paste0(diff(str_mel_to_vector(abs_melody)), collapse = ","),
      onset  = paste0(
        cumsum(c(0, str_mel_to_vector(durations)[-length(str_mel_to_vector(durations))])),
        collapse = ","
      )
    ) %>%
    dplyr::ungroup()
}



compute_ngram_similarity <- function(ngrams, get_ngrukkon = TRUE) {

  if(get_ngrukkon) {
    logging::loginfo('Computing Similarity. nrows = %s', nrow(ngrams))
    ngrams <- ngrams %>%
      dplyr::filter(N > 3L & length(itembankr::str_mel_to_vector(parent_abs_melody)),
                    N > 3L & length(itembankr::str_mel_to_vector(abs_melody))) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(ngrukkon_with_parent_melody = musicassessr::ngrukkon(itembankr::str_mel_to_vector(parent_abs_melody),
                                                      itembankr::str_mel_to_vector(abs_melody))) %>%
      dplyr::ungroup()
    logging::loginfo('Similarity computation complete.')

  }

  ngrams <- ngrams %>%
    dplyr::select(-id) %>%
    dplyr::relocate(durations, abs_melody, N)
}



#' Get ngrams of multiple sizes
#'
#' @param abs_melody
#' @param lower_ngram_bound
#' @param upper_ngram_bound
#'
#' @return
#' @export
#'
#' @examples
get_ngrams_multiple_sizes <- function(abs_melody, lower_ngram_bound = 3L, upper_ngram_bound) {

  if (length(abs_melody) == 1) {
    ngrams.multi <- tibble::tibble(start = NA, N = 1, value = paste(abs_melody, collapse = ","))
  } else if (length(abs_melody) == 2) {
    ngrams.multi <- tibble::tibble(start = NA, N = 2, value = paste(abs_melody, collapse = ","))
  } else {
    if(length(abs_melody) < upper_ngram_bound) {
      upper_ngram_bound <- length(abs_melody)
    }

    # Grab all N-grams from 3:M for a given melody
    ngrams.multi <- purrr::map_dfr(lower_ngram_bound:upper_ngram_bound, get_all_ngrams, x = abs_melody)

  }
  ngrams.multi
}



clip_durations <- function(df) {

  purrr::pmap_dfr(df, function(start, N, value, parent_durations, parent_abs_melody,
                               parent_melody, parent_N, midi_file, musicxml_file, id) {

    logging::loginfo('Clipping durations %s/%s', id, nrow(df))


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
                   N = as.integer(len),
                   parent_N = parent_N,
                   midi_file = midi_file,
                   musicxml_file = musicxml_file)

  })


}


# ---- helper: ensure the columns split_* needs actually exist ----
ensure_parent_cols <- function(df) {
  nm <- names(df)

  # pick first available source for each parent_* field
  pick_first <- function(cands) {
    hit <- cands[cands %in% nm]
    if (length(hit)) hit[[1]] else NA_character_
  }

  src_abs  <- pick_first(c("parent_abs_melody", "MEL_parent_abs_melody", "abs_melody", "MEL_abs_melody"))
  src_mel  <- pick_first(c("parent_melody",     "MEL_parent_melody",     "melody",     "MEL_melody"))
  src_dur  <- pick_first(c("parent_durations",  "MEL_parent_durations",  "durations",  "MEL_durations"))
  src_N    <- pick_first(c("parent_N",          "MEL_parent_N",          "N",          "MEL_N"))

  # create the parent_* columns if missing and we found a source
  if (!"parent_abs_melody" %in% nm && !is.na(src_abs)) df$parent_abs_melody <- df[[src_abs]]
  if (!"parent_melody"     %in% nm && !is.na(src_mel)) df$parent_melody     <- df[[src_mel]]
  if (!"parent_durations"  %in% nm && !is.na(src_dur)) df$parent_durations  <- df[[src_dur]]
  if (!"parent_N"          %in% nm && !is.na(src_N))   df$parent_N          <- df[[src_N]]

  # ensure midi_file / musicxml_file exist even if only META_* are present
  if (!"midi_file" %in% nm)      df$midi_file      <- if ("META_midi_file" %in% nm) df$META_midi_file else NA_character_
  if (!"musicxml_file" %in% nm)  df$musicxml_file  <- if ("META_musicxml_file" %in% nm) df$META_musicxml_file else NA_character_

  df
}


