

#' Convert corpus to item bank
#'
#' @param name A string of the item bank name.
#' @param input A string denoting the input. Must be one of "files", "files_phrases", "item_df" or "phrase_df". files_phrases is when the e.g., MIDI files have already segmented phrases, and you don't want segmentation.
#' @param output A character vector denoting the desired output type or types. You cannot create an output backwards in the hierarchy from the input.
#' @param midi_file_dir If the input is files/files_phrases, a directory with MIDI files. Files should be in the format item_bank_name0.mid.
#' @param musicxml_file_dir If the input is files/files_phrases, a directory with musicxml files. Files should be in the format item_bank_name0.musicxml.
#' @param audio_file_dir Optional directory containing audio files (.wav or .mp3).
#'   When set, an "audio" item bank is created (see \code{output}), and audio
#'   features are prefixed \code{AUD_}. If MIDI/MusicXML are also present,
#'   melodic features are prefixed \code{MEL_}.
#' @param input_df If using input item_df or phrase_df, an input dataframe.
#' @param launch_app Should the app be launched at the end?
#' @param remove_redundancy Should redundant relative melodies be removed? i.e., multiple representations of the same melody in relative form.
#' @param remove_melodies_with_only_repeated_notes Remove any melodies which consist only of a single note repeated.
#' @param remove_melodies_with_any_repeated_notes Remove any melodies which contain any consecutive repeated notes.
#' @param scale_durations_to_have_min_abs_value_of_x_seconds Scale melody durations to have a minimum of x seconds.
#' @param slice_head NULL by default. Can be an integer to slice up to a certain number of items - useful for testing.
#' @param distinct_based_on_melody_only If TRUE, when removing redundancy, check for uniqueness only based on the melody column. Otherwise check based on melody and durations. Default is TRUE.
#' @param lower_ngram_bound The lowest ngram size to use.
#' @param upper_ngram_bound The highest ngram to use, default is the melody length -1.
#' @param get_ngrukkon Whether to compute similarity between parent melodies and sub-melodies.
#' @param phrase_segment_outlier_threshold A threshold for phrase segmenetation sensitivity.
#' @param phrase_segment_ioi_threshold A threshold for phrase segmenetation sensitivity.
#' @param return_item_bank If TRUE, return the item bank from the function
#' @param save_item_bank_to_file Should the item_bank be saved?
#' @param reencode_audio_files_to_lame_mp3 If producing an audio item bank, should audio files be "normalised" to lame mp3 first?
#' @param reencode_audio_files_to_lame_mp3_verbose  Regarding the above argument, should the output be verbose?
#'
#' @return
#' @export
#'
#' @examples
create_item_bank <- function(name = "",
                             input = c("files", "files_phrases", "item_df", "phrase_df"),
                             output = c("all", "file", "item", "phrase", "ngram", "combined", "audio"),
                             midi_file_dir = NULL,
                             musicxml_file_dir = NULL,
                             audio_file_dir = NULL,
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
                             phrase_segment_outlier_threshold = .65,
                             phrase_segment_ioi_threshold = .96,
                             return_item_bank = FALSE,
                             save_item_bank_to_file = TRUE,
                             reencode_audio_files_to_lame_mp3 = FALSE,
                             reencode_audio_files_to_lame_mp3_verbose = FALSE) {

  stopifnot(
    assertthat::is.string(name),
    input %in% c("files", "files_phrases", "item_df", "phrase_df"),
    is.character(output) & length(output) > 0,
    output %in% possible_output_types(),
    assertthat::is.string(midi_file_dir) | is.null(midi_file_dir),
    assertthat::is.string(musicxml_file_dir) | is.null(musicxml_file_dir),
    assertthat::is.string(audio_file_dir) | is.null(audio_file_dir),
    is.data.frame(input_df) | is.null(input_df),
    is.logical(launch_app),
    is.null.or(input_df, function(x) { all(c("abs_melody", "durations") %in% names(x)) }),
    is.logical(remove_redundancy),
    is.scalar.logical(remove_melodies_with_only_repeated_notes),
    is.scalar.logical(remove_melodies_with_any_repeated_notes),
    is.scalar.numeric(scale_durations_to_have_min_abs_value_of_x_seconds) | is.na(scale_durations_to_have_min_abs_value_of_x_seconds),
    is.null.or(slice_head, is.scalar.numeric),
    is.scalar.logical(distinct_based_on_melody_only),
    is.integer(lower_ngram_bound),
    is.null.or(upper_ngram_bound, is.integer),
    is.scalar.logical(get_ngrukkon),
    is.numeric(phrase_segment_outlier_threshold),
    is.numeric(phrase_segment_ioi_threshold),
    is.scalar.logical(return_item_bank),
    is.scalar.logical(save_item_bank_to_file),
    is.scalar.logical(reencode_audio_files_to_lame_mp3),
    is.scalar.logical(reencode_audio_files_to_lame_mp3_verbose)
  )

  # NOTE: original input_check signature (audio-only not supported there)
  input_check(midi_file_dir, musicxml_file_dir, input_df)

  if (remove_melodies_with_any_repeated_notes) {
    # stricter option subsumes the weaker one
    remove_melodies_with_only_repeated_notes <- FALSE
  }

  # -------------------------------------------------------------------
  # init placeholders
  file_item_bank     <- NA
  item_item_bank     <- NA
  phrase_item_bank   <- NA
  ngram_item_bank    <- NA
  combined_item_bank <- NA
  audio_item_bank    <- NA

  # -------------------------------------------------------------------
  # FILE BANK (meta only; no features yet)
  if (input %in% c("files", "files_phrases")) {
    file_item_bank <- create_item_bank_from_files(midi_file_dir, musicxml_file_dir, slice_head) %>%
      remove_melodies(remove_melodies_with_only_repeated_notes, remove_melodies_with_any_repeated_notes) %>%
      dplyr::mutate(item_type = "file",
                    item_id   = paste0(name, "_file_", dplyr::row_number()))
  }

  if (!is_na_scalar(file_item_bank)) {
    file_item_bank <- janitor::remove_empty(file_item_bank, which = "cols")
    file_item_bank <- prefix_melodic_bank(file_item_bank) %>%
      order_item_bank_cols()
  }

  save_item_bank(save_item_bank_to_file, file_item_bank, name, type = "file")

  # -------------------------------------------------------------------
  # AUDIO BANK
  if (!is.null(audio_file_dir) && any(output %in% c("audio","all"))) {

    audio_item_bank <- create_audio_feature_bank(
      audio_file_dir,
      slice_head = slice_head,
      reencode_audio_files_to_lame_mp3 = reencode_audio_files_to_lame_mp3,
      reencode_audio_files_to_lame_mp3_verbose = reencode_audio_files_to_lame_mp3_verbose
    )

    # Prefix first so we have META_file_key / AUD_* ready
    if (!is_na_scalar(audio_item_bank)) {
      audio_item_bank <- prefix_audio_bank(audio_item_bank)
    }

    # Join identifiers from FILE bank (if available) using META_file_key
    if (!is_na_scalar(audio_item_bank) && !is_na_scalar(file_item_bank)) {
      id_join <- dplyr::select(
        file_item_bank,
        META_file_key,
        dplyr::any_of(c("META_midi_file", "META_musicxml_file")),
        dplyr::any_of(c("MEL_abs_melody", "MEL_durations", "MEL_bpm"))
      )
      audio_item_bank <- dplyr::left_join(audio_item_bank, id_join, by = "META_file_key")
    }

    # Add META ids (already prefixed; no second prefixing needed)
    if (!is_na_scalar(audio_item_bank)) {
      audio_item_bank <- audio_item_bank %>%
        dplyr::mutate(
          META_item_type = "audio",
          META_item_id   = paste0(name, "_audio_", dplyr::row_number())
        ) %>%
        dplyr::relocate(META_item_id, META_item_type, .before = 1) %>%
        order_item_bank_cols()
    }

    save_item_bank(save_item_bank_to_file, audio_item_bank, name, type = "audio")
  }



  # -------------------------------------------------------------------
  # ITEM BANK (melodic features)
  if (input %in% c("files", "files_phrases", "item_df") &&
      any(output %in% c("item","all","ngram"))) {

    if (input %in% c("files", "files_phrases")) {
      # start from file bank meta, compute melodic features
      item_item_bank <- deprefix_cols(file_item_bank) %>%               # ensure unprefixed for feature funcs
        get_melody_features() %>%
        remove_melodies(remove_melodies_with_only_repeated_notes, remove_melodies_with_any_repeated_notes) %>%
        scale_durations_to_have_min_abs_value_of_x_seconds(x = scale_durations_to_have_min_abs_value_of_x_seconds) %>%
        dplyr::mutate(item_type = "item",
                      item_id   = paste0(name, "_item_", dplyr::row_number()))
    } else {
      # input_df path: assume unprefixed abs_melody/durations provided
      item_item_bank <- input_df %>%
        get_melody_features() %>%
        remove_melodies(remove_melodies_with_only_repeated_notes, remove_melodies_with_any_repeated_notes) %>%
        scale_durations_to_have_min_abs_value_of_x_seconds(x = scale_durations_to_have_min_abs_value_of_x_seconds) %>%
        dplyr::mutate(item_type = "item",
                      item_id   = paste0(name, "_item_", dplyr::row_number()))
    }
  }

  if (!is_na_scalar(item_item_bank)) {
    item_item_bank <- janitor::remove_empty(item_item_bank, which = "cols")
  }

  # redundancy BEFORE prefixing (expects unprefixed melody/durations)
  item_item_bank <- remove_redundancy(remove_redundancy, item_item_bank, distinct_based_on_melody_only)

  # now prefix melodic/meta columns
  if (!is_na_scalar(item_item_bank)) {
    item_item_bank <- prefix_melodic_bank(item_item_bank) %>%
      order_item_bank_cols()
  }

  save_item_bank(save_item_bank_to_file, item_item_bank, name, type = "item")

  # -------------------------------------------------------------------
  # PHRASE BANK
  if (input %in% c("files", "files_phrases", "item_df", "phrase_df") &&
      any(output %in% c("phrase","ngram","all"))) {

    if (input == "phrase_df") {
      # user-supplied phrases (assume unprefixed)
      phrase_item_bank <- input_df

    } else if (input == "files_phrases") {
      # reuse items as phrases, but deprefix to keep downstream funcs happy
      phrase_item_bank <- deprefix_cols(item_item_bank)

    } else { # input in {"files","item_df"} → segment from file bank
      phrase_item_bank <- deprefix_cols(file_item_bank) %>%
        create_phrases_db(phrase_segment_outlier_threshold = phrase_segment_outlier_threshold,
                          ioi_threshold = phrase_segment_ioi_threshold)
    }

    # compute features if not already computed (files_phrases got features via items)
    if (input != "files_phrases") {
      phrase_item_bank <- phrase_item_bank %>%
        remove_melodies(remove_melodies_with_only_repeated_notes, remove_melodies_with_any_repeated_notes) %>%
        scale_durations_to_have_min_abs_value_of_x_seconds(x = scale_durations_to_have_min_abs_value_of_x_seconds) %>%
        get_melody_features() %>%
        dplyr::mutate(item_type = "phrase",
                      item_id   = paste0(name, "_phrase_", dplyr::row_number()))
    } else {
      # ensure ids/types exist for files_phrases case
      if (!"item_type" %in% names(phrase_item_bank)) phrase_item_bank$item_type <- "phrase"
      if (!"item_id"   %in% names(phrase_item_bank)) phrase_item_bank$item_id   <- paste0(name, "_phrase_", seq_len(nrow(phrase_item_bank)))
    }
  }

  if (!is_na_scalar(phrase_item_bank)) {
    phrase_item_bank <- janitor::remove_empty(phrase_item_bank, which = "cols")
  }

  # redundancy before prefix
  phrase_item_bank <- remove_redundancy(remove_redundancy, phrase_item_bank, distinct_based_on_melody_only)

  # prefix after redundancy
  if (!is_na_scalar(phrase_item_bank)) {
    phrase_item_bank <- prefix_melodic_bank(phrase_item_bank) %>%
      order_item_bank_cols()
  }

  save_item_bank(save_item_bank_to_file, phrase_item_bank, name, type = "phrase")

  # -------------------------------------------------------------------
  # NGRAM BANK
  if (any(output %in% c("ngram","all"))) {

    # ensure we have a phrase source (unprefixed) to split from
    src_for_ngrams <- phrase_item_bank
    if (is_na_scalar(src_for_ngrams)) src_for_ngrams <- item_item_bank
    src_for_ngrams <- deprefix_cols(src_for_ngrams)

    ngram_item_bank <- src_for_ngrams %>%
      create_ngram_item_bank(lower_ngram_bound, upper_ngram_bound, get_ngrukkon) %>%
      remove_melodies(remove_melodies_with_only_repeated_notes, remove_melodies_with_any_repeated_notes) %>%
      dplyr::mutate(item_type = "ngram",
                    item_id   = paste0(name, "_ngram_", dplyr::row_number()))
  }

  if (!is_na_scalar(ngram_item_bank)) {
    ngram_item_bank <- janitor::remove_empty(ngram_item_bank, which = "cols")
  }

  ngram_item_bank <- remove_redundancy(remove_redundancy, ngram_item_bank, distinct_based_on_melody_only)

  if (!is_na_scalar(ngram_item_bank)) {
    ngram_item_bank <- prefix_melodic_bank(ngram_item_bank) %>%
      order_item_bank_cols()
  }

  save_item_bank(save_item_bank_to_file, ngram_item_bank, name, type = "ngram")

  # COMBINED (melodic banks with AUD_* columns joined in; no separate audio rows)
  if ("combined" %in% output) {

    # enrich each melodic bank with AUD_* (if available)
    item_aug   <- attach_audio_features(item_item_bank, audio_item_bank)
    phrase_aug <- attach_audio_features(phrase_item_bank, audio_item_bank)
    ngram_aug  <- attach_audio_features(ngram_item_bank, audio_item_bank)
    # NB: eventually include cut audio n-grams here

    # keep only non-empty data frames
    banks_to_bind <- list(item_aug, phrase_aug, ngram_aug) %>%
      purrr::keep(~ is.data.frame(.x) && nrow(.x) > 0) %>%
      purrr::map(tibble::as_tibble)

    if (length(banks_to_bind) == 0L) {
      combined_item_bank <- NA
    } else {
      combined_item_bank <- banks_to_bind %>%
        dplyr::bind_rows() %>%
        janitor::remove_empty("rows") %>%
        janitor::remove_empty("cols") %>%
        order_item_bank_cols()
    }

    # de-dup on melody (respecting your flag) happens after combining
    combined_item_bank <- remove_redundancy(remove_redundancy, combined_item_bank, distinct_based_on_melody_only)

    save_item_bank(save_item_bank_to_file, combined_item_bank, name, type = "combined")
  }



  # -------------------------------------------------------------------
  if (launch_app && !is_na_scalar(combined_item_bank)) {
    logging::loginfo("Launching app")
    itembankexplorer::item_bank_explorer(combined_item_bank)
  }

  logging::loginfo("Finished processing item bank")

  if (return_item_bank) {
    return(list(
      file     = file_item_bank,
      item     = item_item_bank,
      phrase   = phrase_item_bank,
      ngram    = ngram_item_bank,
      audio    = audio_item_bank,
      combined = combined_item_bank
    ))
  }
}

#' Save an item bank
#'
#' @param save_item_bank_to_file
#' @param item_bank
#' @param name
#' @param type
#'
#' @return
#' @export
#'
#' @examples
save_item_bank <- function(save_item_bank_to_file, item_bank, name, type = c("item", "phrase", "ngram", "combined")) {
  if(save_item_bank_to_file) {
    logging::loginfo("Saving item bank..")
    orig_length <- attributes(item_bank)$item_bank_orig_length
    if(!is_na_scalar(item_bank)) {
      attr(item_bank, "item_bank_name") <- name
      attr(item_bank, "item_bank_type") <- type
      attr(item_bank, "proportion_non_redundant") <- if(is_na_scalar(item_bank)) NA else round(nrow(item_bank)/orig_length, 2)
      item_bank <- set_item_bank_class(item_bank, extra = paste0(type, "_item_bank"))
      save(item_bank, file = paste0(name, '_', type, '.rda'), compress = "xz")
    }
  }
}


#' Remove redundancy from an item bank
#'
#' @param remove_redundancy
#' @param item_bank
#' @param distinct_based_on_melody_only
#'
#' @return
#' @export
#'
#' @examples
remove_redundancy <- function(remove_redundancy, item_bank, distinct_based_on_melody_only = TRUE) {
  if (!remove_redundancy || is_na_scalar(item_bank)) return(item_bank)

  item_bank_orig_length <- nrow(item_bank)

  # Detect column names with/without MEL_ prefix
  mel_col <- dplyr::coalesce(
    c("melody", "MEL_melody")[c("melody","MEL_melody") %in% names(item_bank)][1],
    NA_character_
  )
  dur_col <- dplyr::coalesce(
    c("durations", "MEL_durations")[c("durations","MEL_durations") %in% names(item_bank)][1],
    NA_character_
  )

  # If we can’t find the required columns, just attach the attribute and return
  if (is.na(mel_col) || (!distinct_based_on_melody_only && is.na(dur_col))) {
    attr(item_bank, "item_bank_orig_length") <- item_bank_orig_length
    return(item_bank)
  }

  if (distinct_based_on_melody_only) {
    item_bank <- dplyr::distinct(item_bank, dplyr::across(dplyr::all_of(mel_col)), .keep_all = TRUE)
  } else {
    item_bank <- dplyr::distinct(item_bank, dplyr::across(dplyr::all_of(c(mel_col, dur_col))), .keep_all = TRUE)
  }

  attr(item_bank, "item_bank_orig_length") <- item_bank_orig_length
  item_bank
}


# TODO: Documentation
# TODO: Put remove_redundancy before melodic feature computation and similarity stuff, to optimise things
# TODO: Add more/better logging. In remove_redundancy, markers of what steps the script is in, etc.
