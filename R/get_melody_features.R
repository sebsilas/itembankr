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
get_melody_features <- function(df, mel_sep = ",", durationMeasures = TRUE) {


  df <- df[!is.na(df$melody), ]

  df$log_freq <- get_log_freq(df$rel_freq)

  df$N <- get_stimuli_length(df$melody, mel_sep)

  # tonality
  tonality <- dplyr::bind_rows(lapply(df$melody, get_tonality, mel_sep))
  df <- cbind(df, tonality)

  # step contour
  step_contour_df <- dplyr::bind_rows(lapply(df$melody, function(x) get_step_contour(str_mel_to_vector(x, mel_sep), TRUE)))
  step_contour_df <- step_contour_df[, c("step.cont.glob.var", "step.cont.glob.dir", "step.cont.loc.var")]
  df <- dplyr::bind_cols(df, step_contour_df)

  # interval entropy
  df$i.entropy <- get_interval_entropy(df$melody)

  if(durationMeasures) {
    # duration measures
    duration_df <- dplyr::bind_rows(lapply(df$durations, get_duration_measures))
    names(duration_df) <- c("d.entropy", "d.eq.trans")
    df <- dplyr::bind_cols(df, duration_df)

    df <- df %>% dplyr::rowwise() %>%
      dplyr::mutate(mean_duration = mean(itembankr::str_mel_to_vector(durations))) %>% dplyr::ungroup()

  }

  # difficulty measures from Klaus
  difficulty_measures <- dplyr::bind_rows(lapply(df$melody, function(x) int_ngram_difficulty(int_to_pattern(x))))
  df <- cbind(df, difficulty_measures)

  # calculate melody spans, to make sure melodies can be presented within a user's range
  span <- lapply(df$melody, function(x) sum(abs(range(rel_to_abs_mel(str_mel_to_vector(x, ","), start_note = 1)))) )
  df$span <- unlist(span)


  numeric_vars <- c("log_freq", "step.cont.glob.dir", "step.cont.glob.var",
                    "step.cont.loc.var", "tonal.clarity", "tonal.spike", "tonalness", "mean_int_size",
                    "int_range", "dir_change", "mean_dir_change", "int_variety", "pitch_variety",
                    "mean_run_length", "d.entropy", "d.eq.trans", "mean_duration", "i.entropy")

  # round all numeric columns to two decimal places
  df <- df %>% dplyr::mutate_at(dplyr::vars(numeric_vars), dplyr::funs(round(., 2)))

  print(hist_item_bank(dplyr::select(df, numeric_vars)))
  df
}




# internal functions

get_rel_freq <- function(freq_col) {
  # the column should be a column of frequency values for each item
  res <- lapply(freq_col, function(x) x/sum(freq_col))
  cat("Sanity check, this should at to 1: ", sum(unlist(res)))
  as.numeric(res)
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


get_log_freq <- function(rel_freq_col) {
  # log freq
  as.numeric(lapply(rel_freq_col, log))
}

get_stimuli_length <- function(melody_col, sep) {
  # add melody length, should be
  res <- lapply(melody_col, function(x) length(unlist(strsplit(x, sep)))+1)
  res <- as.numeric(res)
  res
}

get_interval_entropy <- function(rel_melody_col, sep = ",") {
  # add interval entropy
  res <- purrr::map_dbl(rel_melody_col, function(x) compute.entropy(str_mel_to_vector(unlist(x), sep), (phr.length.limits[2]-1)))
}

get_tonality <- function(melody, sep) {
  # wrap the FANTASTIC functions and add in some default durations if need be
  if (!is.na(melody)) {
    pitch <- rel_to_abs_mel(str_mel_to_vector(melody, sep))
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
get_step_contour <- function(melody, relative = TRUE) {

  # wrap the FANTASTIC functions and add in some default durations
  if(relative) {
    melody <- rel_to_abs_mel(melody)
  }

  if (!is.na(melody)) {
    len <- length(melody)
    dur16 <- rep(.25, length(melody))
    step.contour.vector <- step.contour(melody,dur16)
    step.contour <- compute.step.cont.feat(step.contour.vector)
  }

  else {
    ton.results <- as.data.frame(matrix(c(NA, NA, NA, NA), nrow = 1, ncol = 4))
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
  #browser()
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
  ggplot2::ggplot(tidyr::gather(item_bank), ggplot2::aes(value)) +
    ggplot2::geom_histogram() +
    ggplot2::facet_wrap(~key, scales = 'free_x', nrow = nrow, ncol = ncol)
}

