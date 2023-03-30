
#' For a db of melodies, get melodic features
#'
#' @param df with column "melody" as a relative melody e.g, 2, 2, -1, 3 and "freq" a count of the number of occurrences of the dataset from which it came
#' @param mel_sep
#' @param durationMeasures
#'
#' @return
#' @export
#'
#' @examples
get_melody_features <- function(df, mel_sep = ",", durationMeasures = TRUE, abs_melody_name = "abs_melody") {

  if(durationMeasures & ! "durations" %in% names(df)) {
    stop("If durationMeasures is TRUE, there must be a durations column in dataframe.")
  }

  if(abs_melody_name %in% names(df)) {
    df <- df %>% dplyr::filter(!is.na( !!abs_melody_name ))
  }

  abs_melody_name <- as.name(abs_melody_name)

  df <- df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      melody = paste0(diff(str_mel_to_vector(!! abs_melody_name)), collapse = ","),
      N = length(as.numeric(str_mel_to_vector(!! abs_melody_name))),
      # interval entropy
      i.entropy = compute.entropy(str_mel_to_vector(!! abs_melody_name), phr.length.limits[2]-1),
      # calculate melody spans, to make sure melodies can be presented within a user's range
      span = compute_span(str_mel_to_vector(!! abs_melody_name))
    ) %>%
    dplyr::ungroup()

  # tonality
  tonality <- df %>%
    dplyr::select(!! abs_melody_name) %>%
    purrr::map_dfr(get_tonality, mel_sep)

  # step contour
  step_contour_df <- df %>%
    dplyr::select(!! abs_melody_name) %>%
    purrr::map_dfr(get_step_contour, mel_sep = mel_sep, relative = FALSE) %>%
    dplyr::select(step.cont.glob.var, step.cont.glob.dir, step.cont.loc.var)


  # difficulty measures from Klaus
  difficulty_measures <- df %>% dplyr::select(!! abs_melody_name) %>%
    purrr::map_dfr(function(x) int_ngram_difficulty(int_to_pattern(x))) %>%
    dplyr::select(-value)


  df <- df %>% cbind(tonality, step_contour_df, difficulty_measures)

  if(durationMeasures) {
    # duration measures
    duration_df <- purrr::map_dfr(df$durations, get_duration_measures)
    names(duration_df) <- c("d.entropy", "d.eq.trans")

    df <- df %>% cbind(duration_df)

    if(!is_null_scalar(df$durations) & !is_na_scalar(df$durations)) {
      df <- df %>%
        dplyr::rowwise() %>%
        dplyr::mutate(mean_duration = mean(itembankr::str_mel_to_vector(durations))) %>%
        dplyr::ungroup()
    } else {
      df$mean_duration <- NA
    }

  }


  numeric_vars <- c("step.cont.glob.dir", "step.cont.glob.var",
                    "step.cont.loc.var", "tonal.clarity", "tonal.spike", "tonalness", "mean_int_size",
                    "int_range", "dir_change", "mean_dir_change", "int_variety", "pitch_variety",
                    "mean_run_length", "i.entropy")

  if(durationMeasures) {
    numeric_vars <- c(numeric_vars, "d.entropy", "d.eq.trans", "mean_duration")
  }

  # round all numeric columns to two decimal places
  df <- df %>% dplyr::mutate_at(dplyr::vars(numeric_vars), dplyr::funs(round(., 2)))

  print(hist_item_bank(dplyr::select(df, numeric_vars)))

  df

}


compute_span <- function(x) {
  diff(range(x))
}


# internal functions

count_freqs <- function(item_bank) {

  values_counts <- item_bank %>%
    dplyr::add_count(melody, name = "freq") %>%
    dplyr::arrange(dplyr::desc(freq))

  total_freq <- sum(values_counts$freq)

  values_counts <- values_counts %>%
    dplyr::mutate(rel_freq = freq/total_freq,
                  log_freq = log(rel_freq),
                  idf = log( nrow(item_bank)/freq) )
}


get_tonality <- function(abs_melody, sep) {

  # wrap the FANTASTIC functions and add in some default durations if need be
  if (!is_na_scalar(abs_melody)) {
    pitch <- str_mel_to_vector(abs_melody, sep)
    len <- length(pitch)
    dur16 <- rep(.25, length(pitch))
    tonality.vector <- compute.tonality.vector(pitch,dur16,make.tonal.weights(maj.vector,min.vector))
    ton.results <- compute.tonal.features(tonality.vector)
  }

  else {
    ton.results <- as.data.frame(matrix(c(NA, NA, NA, NA), nrow = 1, ncol = 4))
  }

}

# add local stepwise contour
get_step_contour <- function(melody, mel_sep = ",", relative = FALSE) {

  if(!is_na_scalar(melody)) {

    melody <- str_mel_to_vector(melody, sep = mel_sep)

    # wrap the FANTASTIC functions and add in some default durations
    if(relative) {
      melody <- rel_to_abs_mel(melody)
    }

    len <- length(melody)
    dur16 <- rep(.25, length(melody))
    step.contour.vector <- step.contour(melody,dur16)
    step.contour <- compute.step.cont.feat(step.contour.vector)
  } else {
    ton.results <- matrix(c(NA, NA, NA, NA), nrow = 1, ncol = 4) %>% as.data.frame()
  }
}

get_duration_measures <- function(durations) {
  # wrap the FANTASTIC functions and add in some default durations

  if (!is.na(durations) & !is.null(durations)) {

    dur16 <- str_mel_to_vector(durations, ",")
    d.entropy <- compute.entropy(dur16,phr.length.limits[2])
    d.ratios <- round(dur16[1:length(dur16)-1]/ dur16[2:length(dur16)],2)
    d.eq.trans <- sum(sign(d.ratios[d.ratios==1])) / length(d.ratios)

    dur.results <- as.data.frame(matrix(c(d.entropy, d.eq.trans), nrow = 1, ncol = 2))
  }

  else {
    dur.results <- as.data.frame(matrix(c(NA, NA), nrow = 1, ncol = 2))
  }
}

# Frieler difficulty measures

pattern_to_int <- function(x){
  if(length(x) > 1){
    return(lapply(x, pattern_to_int))
  }
  strsplit(gsub("\\]", "", gsub("\\[", "", x)), ",") %>% unlist() %>% as.integer()
}

#input is a vector of string, of the form [i_1, i_2,...], where the i's are integer semitone intervals.
int_ngram_difficulty <- function(pattern){
  if(length(pattern) > 1){
    return(purrr::map_dfr(pattern, int_ngram_difficulty))
  }
  v <- pattern_to_int(pattern)
  mean_abs_int <- mean(abs(v))
  int_range <- max(abs(v))
  l <- length(v)
  r <- rle(sign(v))
  dir_change <- length(r$values) - 1
  mean_dir_change <- (length(r$values) - 1)/(l-1)
  mean_run_length <- 1 - mean(r$lengths)/l
  int_variety <- dplyr::n_distinct(v)/l
  pitch_variety <- dplyr::n_distinct(c(0, cumsum(v)))/(l+1)
  res <- tidyr::tibble(value = pattern,
         mean_int_size = mean_abs_int,
         int_range = int_range,
         dir_change = dir_change,
         mean_dir_change = mean_dir_change,
         int_variety = int_variety,
         pitch_variety = pitch_variety,
         mean_run_length = mean_run_length)
  res$mean_dir_change[res$mean_dir_change == "NaN"] <- NA
  res
}



int_to_pattern <- function (v) {
  paste0('[', paste0(v, collapse = ","), ']')
}


# plotting functions


#' Plot histograms of the item bank vars/features
#'
#' @param item_bank
#' @param nrow
#' @param ncol
#'
#' @return
#' @export
#'
#' @examples
hist_item_bank <- function(item_bank, nrow = NULL, ncol = NULL) {

  item_bank %>%
    dplyr::select(where(is.numeric)) %>%
    tidyr::gather() %>%
    ggplot2::ggplot(ggplot2::aes(value)) +
    ggplot2::geom_histogram() +
    ggplot2::facet_wrap(~key, scales = "free_x", nrow = nrow, ncol = ncol)

}

