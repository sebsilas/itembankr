% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_item_bank.R
\name{create_item_bank}
\alias{create_item_bank}
\title{Convert corpus to item bank}
\usage{
create_item_bank(
  name = "",
  input = c("files", "files_phrases", "item_df", "phrase_df"),
  output = c("all", "file", "item", "phrase", "ngram", "combined"),
  midi_file_dir = NULL,
  musicxml_file_dir = NULL,
  input_df = NULL,
  launch_app = FALSE,
  remove_redundancy = TRUE,
  remove_melodies_with_only_repeated_notes = TRUE,
  remove_melodies_with_any_repeated_notes = FALSE,
  scale_durations_to_have_min_abs_value_of_x_seconds = 0.25,
  slice_head = NULL,
  distinct_based_on_melody_only = TRUE,
  lower_ngram_bound = 3L,
  upper_ngram_bound = NULL,
  get_ngrukkon = TRUE,
  phrase_segment_outlier_threshold = 0.65,
  phrase_segment_ioi_threshold = 0.96,
  return_item_bank = FALSE,
  save_item_bank_to_file = TRUE
)
}
\arguments{
\item{name}{A string of the item bank name.}

\item{input}{A string denoting the input. Must be one of "files", "files_phrases", "item_df" or "phrases_df". files_phrases is when the e.g., MIDI files have already segmented phrases, and you don't want segmentation.}

\item{output}{A character vector denoting the desired output type or types. You cannot create an output backwards in the hierarchy from the input.}

\item{midi_file_dir}{If the input is files, a directory with MIDI files. Files should be in the format item_bank_name0.mid.}

\item{musicxml_file_dir}{If the input is files, a directory with musicxml files. Files should be in the format item_bank_name0.musicxml.}

\item{input_df}{If using input item_df or _phrases_df, an input dataframe.}

\item{launch_app}{Should the app be launched at the end?}

\item{remove_redundancy}{Should redundant relative melodies be removed? i.e., multiple representations of the same melody in relative form.}

\item{remove_melodies_with_only_repeated_notes}{Remove any melodies which consist only of a single note repeated.}

\item{remove_melodies_with_any_repeated_notes}{Remove any melodies which contain any consecutive repeated notes.}

\item{scale_durations_to_have_min_abs_value_of_x_seconds}{Scale melody durations to have a minimum of x seconds.}

\item{slice_head}{NULL by default. Can be an integer to slice up to a certain number of items - useful for testing.}

\item{distinct_based_on_melody_only}{If TRUE, when removing redundancy, check for uniqueness only based on the melody column. Otherwise check based on melody and durations. Default is TRUE.}

\item{lower_ngram_bound}{The lowest ngram size to use.}

\item{upper_ngram_bound}{The highest ngram to use, default is the melody length -1.}

\item{get_ngrukkon}{Whether to compute similarity between parent melodies and sub-melodies.}

\item{phrase_segment_outlier_threshold}{A threshold for phrase segmenetation sensitivity.}

\item{phrase_segment_ioi_threshold}{A threshold for phrase segmenetation sensitivity.}

\item{return_item_bank}{If TRUE, return the item bank from the function}

\item{save_item_bank_to_file}{Should the item_bank be saved?}
}
\description{
Convert corpus to item bank
}
