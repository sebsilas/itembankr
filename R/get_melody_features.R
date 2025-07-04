
#' For a db of melodies, get melodic features
#'
#' @param df with column "melody" as a relative melody e.g, 2, 2, -1, 3 and "freq" a count of the number of occurrences of the dataset from which it came
#' @param mel_sep What is the melody note separator?
#' @param durationMeasures Should duration measures be computed?
#' @param print_histogram Print histogram of results?
#'
#' @return
#' @export
#'
#' @examples
get_melody_features <- function(df, mel_sep = ",", durationMeasures = TRUE, abs_melody_name = "abs_melody", print_histogram = FALSE) {

  if(durationMeasures & ! "durations" %in% names(df)) {
    stop("If durationMeasures is TRUE, there must be a durations column in dataframe.")
  }

  if(abs_melody_name %in% names(df)) {
    df <- df %>%
      dplyr::filter(!is.na( !!abs_melody_name ))
  }

  abs_melody_name <- as.name(abs_melody_name)

  df <- df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      melody = paste0(diff(str_mel_to_vector(!! abs_melody_name)), collapse = ","),
      N = length(as.numeric(str_mel_to_vector(!! abs_melody_name))),
      # interval entropy
      i.entropy = compute.entropy(diff(str_mel_to_vector(!! abs_melody_name)), phr.length.limits[2]-1),
      # Calculate melody spans, to make sure melodies can be presented within a user's range
      span = compute_span(str_mel_to_vector(!! abs_melody_name)),
      mean_information_content = get_mean_information_content(str_mel_to_vector(!! abs_melody_name)),
      # Calculate tonality measures
      tonality = list( get_tonality(!! abs_melody_name, sep = mel_sep) ),
      # Calculate step contour measures
      step_contour = list( get_step_contour(!! abs_melody_name, mel_sep = mel_sep, relative = FALSE) ),
      # Caulcuate KF features
      kf_features = list( dplyr::select(int_ngram_difficulty(int_to_pattern(str_mel_to_vector(melody))), -value) ) ,
      # Calculate duration measures
      duration_measures = get_duration_measures(durations),
      mean_duration = mean(itembankr::str_mel_to_vector(durations))
    ) %>%
    dplyr::ungroup() %>%
    tidyr::unnest(c(tonality, step_contour, kf_features, duration_measures))


  numeric_vars <- c("step.cont.glob.dir", "step.cont.glob.var",
                    "step.cont.loc.var", "tonal.clarity", "tonal.spike", "tonalness", "mean_int_size",
                    "int_range", "dir_change", "mean_dir_change", "int_variety", "pitch_variety",
                    "mean_run_length", "i.entropy")

  if(durationMeasures) {
    numeric_vars <- c(numeric_vars, "d.entropy", "d.eq.trans", "mean_duration")
  }

  # round all numeric columns to two decimal places
  df <- df %>% dplyr::mutate_at(dplyr::vars(numeric_vars), dplyr::funs(round(., 2)))

  if(print_histogram) {
    print(hist_item_bank(dplyr::select(df, numeric_vars)))
  }

  df

}


compute_span <- function(x) {
  diff(range(x))
}


# Internal functions

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


get_tonality <- function(abs_melody, sep = ",") {

  tryCatch({
    # wrap the FANTASTIC functions and add in some default durations if need be
    if (!is_na_scalar(abs_melody)) {
      pitch <- str_mel_to_vector(abs_melody, sep)
      len <- length(pitch)
      dur16 <- rep(.25, length(pitch))
      tonality.vector <- compute.tonality.vector(pitch,dur16,make.tonal.weights(maj.vector,min.vector))
      ton.results <- compute.tonal.features(tonality.vector)
    }

    else {
      ton.results <- tibble::tibble(tonalness = NA, tonal.clarity = NA, tonal.spike = NA, mode = NA)
    }
    ton.results
  }, error = function(err) {
    logging::logerror(err)
    ton.results <- tibble::tibble(tonalness = NA, tonal.clarity = NA, tonal.spike = NA, mode = NA)
    ton.results
  })

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
  names(dur.results) <- c("d.entropy", "d.eq.trans")
  dur.results
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



get_mean_information_content <- function(seq) {

  if(check_pkg_installed("ppm")) {
    seq <- factor(seq)
    mod <- ppm::new_ppm_simple(alphabet_size = 108)
    res <- ppm::model_seq(mod, seq)
    return(round(mean(res$information_content, na.rm = TRUE), 2))
  } else {
    return(NA)
  }

}


# create_item_bank(name = "Test",
#                 input = "phrase_df",
#                 output = "all",
#                 input_df = tibble::tibble(abs_melody = c("61,62,63,64,65,66",
#                                                          "72, 73, 75, 78"),
#                                           durations = c("1,2,3,4,5,6",
#                                                         "1, 1, 1, 1")))

# load('Test_combined.rda')
# load('Test_ngram.rda')
# load('Test_phrase.rda')



# t <- create_item_bank(name = "Test",
#                       input = "item_df",
#                       output = "item",
#                       input_df = tibble::tibble(abs_melody = c("61,62,63,64,65,66",
#                                                                "72, 73, 75, 78"),
#                                                 durations = c("1,2,3,4,5,6",
#                                                               "1, 1, 1, 1")),
#                       return_item_bank = TRUE)
