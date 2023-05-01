


#' Produce a table describing and operationalising melodic features (for paper-writing)
#'
#' @param features
#'
#' @return
#' @export
#'
#' @examples
get_melodic_feature_table <- function(features = NULL) {

  tbl <- tibble::tibble(

    Feature = c("target_melody_length", "freq", "rel_freq", "log_freq", "IDF",
                "i.entropy", "span", "tonalness", "tonal.clarity",
                "tonal.spike", "mode", "step.cont.glob.var", "step.cont.glob.dir",
                "step.cont.loc.var", "mean_int_size", "int_range", "dir_change",
                "mean_dir_change", "int_variety", "pitch_variety", "mean_run_length",
                "d.entropy", "d.eq.trans", "mean_duration", "mean_information_content"),


    Description = c("The length of the target melody.",
                    "The count of its occurence in the corpus.",
                    "The relative count of a frequency in the corpus",
                    "The log of the relative count of a frequency in the corpus",
                    "The inverse document frequency",
                    'The average level of "information" or "surprise" in intervallic representations. Specifically, a variant of Shannon entropy on interval representations (Shannon, 1948)',
                    "The difference between the highest MIDI pitch and the lowest MIDI pitch in the melody.",
                    "The magnitude of the highest correlation value in a vector of tonality correlation values computed with the Krumhansl-Schmuckler algorithm (Krumhansl, 1990). It expresses how strongly a melody correlates to a single key.",
                    "Inspired by Temperley’s notion of tonal clarity (Temperley, 2007), the ratio between the magnitude of the highest correlation in a tonality.vector (see tonalness) A0 and the second highest correlation A1.",
                    "Similar to tonal.clarity, tonal.spike depends on the magnitude of the highest correlation but in contrast to the previous feature is divided by the sum of all correlation values > 0.",
                    "The mode of the tonality with the highest correlation in the tonality.vector. It can assume the values major and minor.",
                    "step.cont.glob.var",
                    "step.cont.glob.dir",
                    "The mean absolute difference between adjacent values in the vector representing of step contour.",
                    'The average level of "information" or "surprise" in rhythm values. Specifically, a variant of Shannon entropy on rhythmic representations (Shannon, 1948)',
                    "d.eq.trans",
                    "The average interval size",
                    "int_range.",
                    "dir_change",
                    "mean_dir_change",
                    "int_variety",
                    'The average level of "information" or "surprise" in rhythm values. Specifically, a variant of Shannon entropy on rhythmic representations (Shannon, 1948)',
                    "mean_run_length",
                    "mean_duration",
                    "The average information content contained in the pitch values of a melody. Can be thought of as quantifying a melody's self-similarity."),

    Equation = c(" - ",
                 "freq", "rel_freq", "log_freq", "IDF",
                 "$- \\frac{ \\sum_{i} f_{i} \\cdot \\log_{2} f_{i}}{\\log_{2} 139}$",

                 "span",
                 " - ", "tonal.clarity",
                 "tonal.spike", "mode", "step.cont.glob.var", "step.cont.glob.dir",
                 "$\\frac{ \\sum_{i=1}^{N-1} \\lvert x_{i+1} - x_{i} \\rvert }{N-1}$",
                 "mean_int_size", "int_range", "dir_change",
                 "mean_dir_change", "int_variety", "pitch_variety", "mean_run_length",
                 "$- \\frac{ \\sum_{i} f_{i} \\cdot \\log_{2} f_{i}}{\\log_{2} 140}$",
                 "d.eq.trans", "mean_duration", " - "),

    Reference = c(" - ", " - ", " - ", " - ", " - ",
                  "Müllensiefen, 2009", "span", "Müllensiefen, 2009", "Müllensiefen, 2009",
                  "Müllensiefen, 2009", "Müllensiefen, 2009", "Müllensiefen, 2009.", "Müllensiefen, 2009", "Müllensiefen, 2009",
                  "Beaty, R. E., Frieler, K., Norgaard, M., Merseal, H. M., MacDonald, M. C., & Weiss, D. J. (2021)", "Beaty, R. E., Frieler, K., Norgaard, M., Merseal, H. M., MacDonald, M. C., & Weiss, D. J. (2021)", "Beaty, R. E., Frieler, K., Norgaard, M., Merseal, H. M., MacDonald, M. C., & Weiss, D. J. (2021)",
                  "Beaty, R. E., Frieler, K., Norgaard, M., Merseal, H. M., MacDonald, M. C., & Weiss, D. J. (2021)", "Beaty, R. E., Frieler, K., Norgaard, M., Merseal, H. M., MacDonald, M. C., & Weiss, D. J. (2021)",
                  "Beaty, R. E., Frieler, K., Norgaard, M., Merseal, H. M., MacDonald, M. C., & Weiss, D. J. (2021)", "Beaty, R. E., Frieler, K., Norgaard, M., Merseal, H. M., MacDonald, M. C., & Weiss, D. J. (2021)",
                  "Müllensiefen, 2009", "Müllensiefen, 2009", "mean_duration", "Harrison, Bianco, Chait & Pearce (2020)")
  )

  if(is.null(features)) {
    return(tbl %>% knitr::kable(escape=FALSE))
  } else {
    return(dplyr::filter(tbl, Feature %in% features) %>% knitr::kable(escape=FALSE))
  }

}

# get_melodic_feature_table(c("target_melody_length", "i.entropy", "tonalness",
#                             "mean_information_content",
#                             "step.cont.loc.var", "d.entropy"))

