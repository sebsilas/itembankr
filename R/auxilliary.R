


#' Produce a table describing and operationalising melodic features (for paper-writing)
#'
#' @param features Optional character vector of feature names to filter.
#' @param return_kable If TRUE (default), return knitr::kable with escape = FALSE.
#'
#' @return tibble or knitr_kable
#' @export
#'
#' @examples
#' get_melodic_feature_table(c("target_melody_length","i.entropy","tonalness"))
get_melodic_feature_table <- function(features = NULL, return_kable = TRUE) {

  tbl <- tibble::tibble(

    Feature = c(
      "target_melody_length",
      "freq", "rel_freq", "log_freq", "idf",
      "i.entropy",
      "span",
      "tonalness", "tonal.clarity", "tonal.spike", "mode",
      "step.cont.glob.var", "step.cont.glob.dir", "step.cont.loc.var",
      "mean_int_size", "int_range", "dir_change", "mean_dir_change",
      "int_variety", "pitch_variety", "mean_run_length",
      "d.entropy", "d.eq.trans", "mean_duration",
      "mean_information_content"
    ),

    Description = c(
      "Number of notes in the target melody.",
      "Absolute frequency of the melody in the corpus.",
      "Relative frequency of the melody in the corpus.",
      "Log-transformed relative frequency.",
      "Inverse document frequency of the melody in the corpus.",

      "Interval entropy: uncertainty of successive pitch intervals, reflecting intervallic diversity.",
      "Pitch span: difference between highest and lowest MIDI pitch in the melody.",

      "Strength of implied key: maximum correlation between melody and major/minor key profiles.",
      "Tonal clarity: ratio between the strongest and second-strongest key correlations.",
      "Tonal spike: dominance of the strongest key correlation relative to all positive correlations.",
      "Mode (major or minor) of the strongest implied key.",

      "Global variation of step contour: standard deviation of the step contour representation.",
      "Global direction of step contour: correlation between contour height and time.",
      "Local variation of step contour: mean absolute difference between adjacent contour values.",

      "Mean absolute pitch interval size.",
      "Maximum absolute pitch interval.",
      "Number of changes in melodic direction.",
      "Proportion of direction changes relative to possible transitions.",

      "Interval variety: proportion of distinct intervals.",
      "Pitch variety: proportion of distinct pitch classes visited.",
      "Mean run length: inverse measure of directional persistence.",

      "Duration entropy: uncertainty of rhythmic values.",
      "Proportion of equal-duration transitions.",
      "Mean note duration.",

      "Mean information content of pitches under a statistical learning model."
    ),

    Equation = c(
      "$N$",
      "$f$", "$f / \\sum f$", "$\\log(f)$", "$\\log(N / f)$",

      "$- \\frac{ \\sum_i f_i \\log_2 f_i }{ \\log_2 23 }$",
      "$\\max(p) - \\min(p)$",

      "$\\max(\\mathrm{corr}(\\text{key profiles}, melody))$",
      "$A_0 / A_1$",
      "$A_0 / \\sum_{i: A_i > 0} A_i$",
      "argmax key",

      "$\\sqrt{ \\sum (x_i - \\bar{x})^2 / (N-1) }$",
      "$\\mathrm{cor}(x, 1{:}N)$",
      "$\\frac{1}{N-1} \\sum |x_{i+1} - x_i|$",

      "$\\frac{1}{N} \\sum |\\Delta p_i|$",
      "$\\max |\\Delta p|$",
      "$\\sum \\mathbb{1}[\\Delta p_i \\Delta p_{i+1} < 0]$",
      "$\\frac{\\text{dir changes}}{N-1}$",

      "$|\\{\\Delta p\\}| / N$",
      "$|\\{p\\}| / (N+1)$",
      "$1 - \\frac{\\overline{\\text{run length}}}{N}$",

      "$- \\frac{ \\sum_i f_i \\log_2 f_i }{ \\log_2 24 }$",
      "$\\sum \\mathbb{1}[r_i = 1] / |R|$",
      "$\\frac{1}{N} \\sum d_i$",

      "$\\frac{1}{N} \\sum IC(p_i)$"
    ),

    Reference = c(
      "Müllensiefen (2009)",
      "Corpus statistics",
      "Corpus statistics",
      "Corpus statistics",
      "Corpus statistics",

      "Müllensiefen (2009)",
      "Müllensiefen (2009)",

      "Krumhansl (1990); Müllensiefen (2009)",
      "Temperley (2007); Müllensiefen (2009)",
      "Müllensiefen (2009)",
      "Müllensiefen (2009)",

      "Müllensiefen (2009)",
      "Müllensiefen (2009)",
      "Müllensiefen (2009)",

      "Müllensiefen (2009)",
      "Müllensiefen (2009)",
      "Müllensiefen (2009)",
      "Müllensiefen (2009)",

      "Müllensiefen (2009)",
      "Müllensiefen (2009)",
      "Müllensiefen (2009)",

      "Müllensiefen (2009)",
      "Steinbeck (1982); Müllensiefen (2009)",
      "Descriptive statistic",

      "Harrison et al. (2020)"
    )
  )

  if (!is.null(features)) {
    tbl <- dplyr::filter(tbl, Feature %in% features)
  }

  if (return_kable) {
    tbl <- knitr::kable(tbl, escape = FALSE)
  } else {
    warning("Remember to escape kable output so equations render correctly.")
  }

  tbl
}


# get_melodic_feature_table(c("target_melody_length", "i.entropy", "tonalness",
#                             "mean_information_content",
#                             "step.cont.loc.var", "d.entropy"))



#' Produce a table describing and operationalising audio features (for paper-writing)
#'
#' This documents the columns created by `extract_audio_features()` /
#' `create_audio_feature_bank()`. Feature names match the code; a few are
#' historical misnomers and are noted inline.
#'
#' @param features Optional character vector of feature names to filter to.
#' @param return_kable If TRUE (default), returns a LaTeX/HTML-ready kable (escape=FALSE).
#'
#' @return A tibble (or knitr_kable) with columns: Feature, Description, Equation, Reference.
#' @export
#'
#' @examples
#' get_audio_feature_table(c("duration","rms","acoustic_complexity_index","mfcc_mean_1..N"))
get_audio_feature_table <- function(features = NULL, return_kable = TRUE) {

  tbl <- tibble::tibble(
    Feature = c(
      # temporal / signal basics (extract_temporal_features)
      "duration", "sample_rate", "bit_depth", "rms", "zero_crossing_rate",
      "shannon_entropy", "attack_time", "decay_time", "sustain_level",
      "release_time", "temporal_centroid",

      # spectral props (seewave::specprop on meanspec/spec)
      "mean", "sd", "sem", "median", "mode", "Q25", "Q75", "IQR",
      "cent", "skewness", "kurtosis", "sfm", "sh", "prec",

      # peaks / counts
      "frequency_peaks_number",

      # ecoacoustic indices (note: some names are legacy)
      "acoustic_complexity_index",        # actually ACI total (left)
      "acoustic_diversity_index",         # legacy name: actually ACI total (left)
      "acoustic_diversity_index2",        # legacy name: ACI per-minute (left)
      "acoustic_diversity_index3",        # true ADI from soundecology::acoustic_diversity()
      "acoustic_evenness_index",          # AEI (Gini on band activity)
      "acoustic_entropy_index",           # H = sh * th
      "bioacoustic_index",                # BI (area under spectrum in biophony band)
      "amplitude_index",                  # seewave::M (median envelope)
      "normalized_difference_soundscape_index",
      "spectral_entropy_ndsi",            # actually NDSI value (legacy column name)
      "spectral_entropy_anthrophony",     # anthrophony energy (1–2 kHz by default)
      "spectral_entropy_biophony",        # biophony energy (2–8/11 kHz by default)
      "spectral_entropy2",                # spectral entropy sh on mean spectrum
      "temporal_entropy",                 # temporal entropy th

      # MFCC summaries
      "mfcc_mean_1..N", "mfcc_sd_1..N"
    ),

    Description = c(

      # temporal / signal basics
      "Duration of the signal (s).",
      "Sampling rate (Hz).",
      "Bit depth of the waveform.",
      "Root-mean-square amplitude over time.",
      "Average rate of sign changes per frame — proxy for noisiness/brightness.",
      "Total (global) entropy H of the signal (product of spectral and temporal entropy).",
      "Attack: time for the normalised envelope to rise from 0.1 to 0.9.",
      "Decay: time from the attack peak (≈0.9) to the sustain level.",
      "Sustain: mean normalised level after attack (first half of signal in this implementation).",
      "Release: time for the envelope to drop from the late-peak to 0.1.",
      "Temporal centroid (energy ‘center of gravity’ in time).",

      # spectral props
      "Mean frequency of the probability mass function (PMF) of the spectrum.",
      "Standard deviation of frequency (PMF).",
      "Standard error of the mean frequency.",
      "Median (50th percentile) frequency.",
      "Mode (dominant) frequency.",
      "First quartile (25th percentile) frequency.",
      "Third quartile (75th percentile) frequency.",
      "Interquartile range (Q75 − Q25).",
      "Spectral centroid (energy ‘center of gravity’ in frequency).",
      "Skewness of the spectral PMF (asymmetry).",
      "Kurtosis of the spectral PMF (peakedness).",
      "Spectral flatness measure (geometric mean / arithmetic mean).",
      "Shannon spectral entropy of the spectrum.",
      "Frequency precision of the spectrum (bin resolution).",

      # peaks
      "Number of peaks detected in the mean spectrum.",

      # ecoacoustic indices
      "Acoustic Complexity Index (ACI), total over frequencies (left channel).",
      "LEGACY NAME — actually ACI total (left) from soundecology::acoustic_complexity().",
      "LEGACY NAME — ACI aggregated by minute (left).",
      "Acoustic Diversity Index (ADI): Shannon diversity across kHz bands above threshold.",
      "Acoustic Evenness Index (AEI): Gini coefficient of band activity.",
      "Acoustic Entropy Index H = sh × th (dimensionless, 0–1).",
      "Bioacoustic Index (BI): area of the mean spectrum within the biophony band above a noise floor.",
      "Median of the amplitude envelope (seewave::M).",
      "NDSI: normalized difference of biophony and anthrophony bands.",
      "LEGACY NAME — actually NDSI value (left).",
      "Anthrophony band energy (typically 1–2 kHz).",
      "Biophony band energy (typically 2–8/11 kHz).",
      "Spectral entropy computed on the mean spectrum (seewave::sh).",
      "Temporal entropy of the amplitude envelope (seewave::th).",

      # MFCC summaries
      "Mean of each Mel-frequency cepstral coefficient across frames (1..N).",
      "Standard deviation of each MFCC across frames (1..N)."
    ),

    Equation = c(

      # temporal / signal basics
      "$\\text{duration} = \\tfrac{N}{f_s}$",
      "$f_s$",
      " - ",
      "$\\mathrm{RMS} = \\sqrt{\\tfrac{1}{N}\\sum_{n=1}^{N} x_n^2}$",
      "$\\mathrm{ZCR} = \\tfrac{1}{N-1}\\sum_{n=1}^{N-1} \\mathbb{1}[x_n\\,x_{n+1} < 0]$",
      "$H = \\mathrm{sh} \\times \\mathrm{th}$",
      "$t_\\mathrm{attack} = \\tfrac{i_{0.9}-i_{0.1}}{f_s}$",
      "$t_\\mathrm{decay} = \\tfrac{i_{\\text{sustain}}-i_{0.9}}{f_s}$",
      "$S = \\overline{e(t)}$",
      "$t_\\mathrm{release} = \\tfrac{i_{0.1(\\text{end})}-i_{\\text{peak}(2nd\\,half)}}{f_s}$",
      "$\\mathrm{TC} = \\tfrac{\\sum_{i} t_i\\, e_i}{\\sum_i e_i}$",

      # spectral props
      "$\\mu_f = \\sum_i x_i y_i$",
      "$\\sigma_f = \\sqrt{\\sum_i (x_i-\\bar{x})^2/(N-1)}$",
      "$\\mathrm{SEM} = \\sigma_f/\\sqrt{N}$",
      " - ", " - ", " - ", " - ", "$\\mathrm{IQR} = Q_{75}-Q_{25}$",
      "$C = \\sum_{i=1}^{N} x_i y_i$",
      "$S = \\dfrac{\\sum_{i=1}^{N}(x_i-\\bar{x})^{3}}{(N-1)\\,\\sigma^{3}}$",
      "$K = \\dfrac{\\sum_{i=1}^{N}(x_i-\\bar{x})^{4}}{(N-1)\\,\\sigma^{4}}$",
      "$\\mathrm{SFM} = N\\,\\dfrac{\\sqrt[N]{\\prod_{i=1}^{N} y_i}}{\\sum_{i=1}^{N} y_i}$",
      "$\\mathrm{sh} = -\\dfrac{\\sum_{i=1}^{N} y_i\\,\\log_2 y_i}{\\log_2 N}$",
      " - ",

      # peaks
      " - ",

      # ecoacoustic indices
      "$\\displaystyle \\mathrm{ACI} = \\sum_f \\dfrac{\\sum_{t} |A_{f,t+1}-A_{f,t}|}{\\sum_t A_{f,t}}$",
      " - ",
      " - ",
      "$\\mathrm{ADI} = -\\sum_{k=1}^{K} p_k\\,\\log p_k$",
      "$\\mathrm{AEI} = G = \\dfrac{\\sum_{i=1}^{K} (2i-K-1)\\,x_{(i)}}{K\\sum_{i=1}^{K} x_i}$",
      "$H = \\mathrm{sh}\\times\\mathrm{th}$",
      "$\\mathrm{BI} = \\sum_{f \\in \\text{bio}} \\max\\{S(f)-S_0,0\\}\\,\\Delta f$",
      " - ",
      "$\\mathrm{NDSI} = \\dfrac{B - A}{B + A}$",
      " - ", " - ", " - ",
      "$\\mathrm{sh} = -\\dfrac{\\sum y_i\\log_2 y_i}{\\log_2 N}$",
      "$\\mathrm{th} = -\\dfrac{\\sum e_i\\log_2 e_i}{\\log_2 N}$",

      # MFCC summaries
      "$c_n = \\sum_{k=1}^{K} \\log S_k\\,\\cos\\!\\left[\\tfrac{\\pi n}{K}\\,(k-\\tfrac12)\\right]$",
      " as above (SD across frames) "
    ),

    Reference = c(

      # temporal / signal basics
      "seewave manual; standard DSP.",
      "tuneR::readWave/readMP3.",
      "tuneR.",
      "Standard DSP textbook.",
      "seewave::zcr — Zero-crossing rate.",
      "seewave::H — Total entropy (product of sh & th).",
      "ADSR envelope conventions (synth).",
      "ADSR envelope conventions (synth).",
      "ADSR envelope conventions (synth).",
      "ADSR envelope conventions (synth).",
      "Temporal centroid (Peeters, 2004).",

      # spectral props
      "seewave::specprop.",
      "seewave::specprop.",
      "seewave::specprop.",
      "seewave::specprop.",
      "seewave::specprop.",
      "seewave::specprop.",
      "seewave::specprop.",
      "seewave::specprop.",
      "seewave::specprop (centroid).",
      "seewave::specprop (skewness).",
      "seewave::specprop (kurtosis).",
      "seewave::sfm.",
      "seewave::sh.",
      "seewave::specprop.",

      # peaks
      "seewave::fpeaks.",

      # ecoacoustic indices
      "Pieretti et al., 2011; seewave::ACI/soundecology::acoustic_complexity.",
      "LEGACY name: see previous.",
      "LEGACY name: see previous.",
      "Villanueva-Rivera & Pijanowski (soundecology::acoustic_diversity).",
      "Villanueva-Rivera & Pijanowski (soundecology::acoustic_evenness).",
      "Sueur et al., 2008; seewave::H.",
      "Boelman et al., 2007; soundecology::bioacoustic_index.",
      "seewave::M.",
      "Kasten et al., 2012; soundecology::ndsi / seewave::NDSI.",
      "LEGACY name: see NDSI.",
      "soundecology::ndsi (anthrophony).",
      "soundecology::ndsi (biophony).",
      "seewave::sh.",
      "seewave::th.",

      # MFCC summaries
      "Davis & Mermelstein, 1980; tuneR::melfcc.",
      "Davis & Mermelstein, 1980; tuneR::melfcc."
    )
  )

  if (!is.null(features)) {
    tbl <- tbl %>% dplyr::filter(.data$Feature %in% features)
  }

  if (return_kable) {
    tbl <- tbl %>% knitr::kable(escape = FALSE)
  } else {
    warning("Remember to escape kable when you use it so equations render.")
  }

  tbl
}

