
# TODO: Put remove_redundancy before melodic feature computation and similarity stuff, to optimise things
# TODO: Add more/better logging. In remove_redundancy, markers of what steps the script is in, etc.

#' Convert corpus to item bank
#'
#' @param name A string of the item bank name.
#' @param input A string denoting the input. Must be one of "files", "files_phrases", "item_df" or "phrases_df". files_phrases is when the e.g., MIDI files have already segmented phrases, and you don't want segmentation.
#' @param output A character vector denoting the desired output type or types. You cannot create an output backwards in the hierarchy from the input.
#' @param midi_file_dir If the input is files, a directory with MIDI files. Files should be in the format item_bank_name0.mid.
#' @param musicxml_file_dir If the input is files, a directory with musicxml files. Files should be in the format item_bank_name0.musicxml.
#' @param input_df If using input item_df or _phrases_df, an input dataframe.
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
#'
#' @return
#' @export
#'
#' @examples
create_item_bank <- function(name = "",
                            input = c("files", "files_phrases", "item_df", "phrase_df"),
                            output = c("all", "file", "item", "phrase", "ngram", "combined"),
                            midi_file_dir = NULL,
                            musicxml_file_dir = NULL,
                            input_df = NULL,
                            launch_app = TRUE,
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
                            return_item_bank = FALSE) {

  stopifnot(
    assertthat::is.string(name),
    input %in% c("files", "files_phrases", "item_df", "phrase_df"),
    is.character(output) & length(output) > 0,
    output %in% possible_output_types(),
    assertthat::is.string(midi_file_dir) | is.null(midi_file_dir),
    assertthat::is.string(musicxml_file_dir) | is.null(musicxml_file_dir),
    is.data.frame(input_df) | is.null(input_df),
    is.logical(launch_app),
    psychTestR::is.null.or(input_df, function(x) { all(c("abs_melody", "durations") %in% names(x)) }),
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
    is.scalar.logical(return_item_bank)
  )

  input_check(midi_file_dir, musicxml_file_dir, input_df)

  if(remove_melodies_with_any_repeated_notes) {
    remove_melodies_with_only_repeated_notes <- FALSE # Because remove_melodies_with_any_repeated_notes is stricter. So don't waste processing time.
  }

  # Init vars which might get overwritten later
  file_item_bank <- NA
  phrase_item_bank <- NA
  item_item_bank <- NA
  ngram_item_bank <- NA
  combined_item_bank <- NA


  # Create file item bank
  if(input %in% c("files", "files_phrases") ) {
    file_item_bank <- create_item_bank_from_files(midi_file_dir, musicxml_file_dir, slice_head) %>%
      remove_melodies(remove_melodies_with_only_repeated_notes, remove_melodies_with_any_repeated_notes) %>%
      dplyr::mutate(item_type = "file",
                    item_id = paste0(name, "_", item_type, "_", dplyr::row_number()))

  }

  # Tidy up
  if(!is_na_scalar(file_item_bank)) {
    file_item_bank <- janitor::remove_empty(file_item_bank, which = "cols")
  }

  # Save
  save_item_bank(file_item_bank, name, type = "file")

  # Create item bank with features
  if(input %in% c("files", "files_phrases", "item_df") & any(output %in% c("item", "all", "ngram", "combined")))  {
    if(input %in% c("files", "files_phrases") ) {
      item_item_bank <- file_item_bank %>%
        get_melody_features()  %>%
        remove_melodies(remove_melodies_with_only_repeated_notes, remove_melodies_with_any_repeated_notes) %>%
        scale_durations_to_have_min_abs_value_of_x_seconds(x = scale_durations_to_have_min_abs_value_of_x_seconds) %>%
        dplyr::mutate(item_type = "item",
                      item_id = paste0(name, "_", item_type, "_", dplyr::row_number()))

    } else {
      item_item_bank <- input_df %>%
        get_melody_features() %>%
        remove_melodies(remove_melodies_with_only_repeated_notes, remove_melodies_with_any_repeated_notes) %>%
        scale_durations_to_have_min_abs_value_of_x_seconds(x = scale_durations_to_have_min_abs_value_of_x_seconds) %>%
        dplyr::mutate(item_type = "item",
                      item_id = paste0(name, "_", item_type, "_", dplyr::row_number()))
    }
  }

  # Tidy up
  if(!is_na_scalar(item_item_bank)) {
    item_item_bank <- janitor::remove_empty(item_item_bank, which = "cols")
  }
  # Remove redundancy
  item_item_bank <-  remove_redundancy(remove_redundancy, item_item_bank, distinct_based_on_melody_only)
  # Save
  save_item_bank(item_item_bank, name, type = "item")

  # Create phrase item bank (with features) i.e., chop up items based on segmentation
  if(input %in% c("files", "files_phrases", "item_df", "phrase_df") & any(output %in% c("phrase", "ngram", "all", "combined")))  {

    if(input == "phrase_df") {
      phrase_item_bank <- input_df
    } else if(input == "files_phrases") {
      phrase_item_bank <- item_item_bank
    } else {
      phrase_item_bank <- file_item_bank %>%
        create_phrases_db(phrase_segment_outlier_threshold = phrase_segment_outlier_threshold, ioi_threshold = phrase_segment_ioi_threshold)
    }

    if(input != "files_phrases") {
      phrase_item_bank <-  phrase_item_bank %>%
        remove_melodies(remove_melodies_with_only_repeated_notes, remove_melodies_with_any_repeated_notes) %>%
        scale_durations_to_have_min_abs_value_of_x_seconds(x = scale_durations_to_have_min_abs_value_of_x_seconds) %>%
        get_melody_features() %>%
        dplyr::mutate(item_type = "phrase",
                      item_id = paste0(name, "_", item_type, "_", dplyr::row_number()))
    }

  }

  # Tidy up
  if(!is_na_scalar(phrase_item_bank)) {
    phrase_item_bank <- janitor::remove_empty(phrase_item_bank, which = "cols")
  }
  # Remove redundancy
  phrase_item_bank <-  remove_redundancy(remove_redundancy, phrase_item_bank, distinct_based_on_melody_only)
  # Save
  save_item_bank(phrase_item_bank, name, type = "phrase")

  # Create ngram item bank (with features) i.e., chop up items into N-grams
  if(any(output %in% c("ngram", "all", "combined"))) {

    if(is_na_scalar(phrase_item_bank)) {
      phrase_item_bank <- item_item_bank
    }

    ngram_item_bank <- phrase_item_bank %>%
      create_ngram_item_bank(lower_ngram_bound, upper_ngram_bound, get_ngrukkon) %>%
      remove_melodies(remove_melodies_with_only_repeated_notes, remove_melodies_with_any_repeated_notes) %>%
      dplyr::mutate(item_type = "ngram",
                    item_id = paste0(name, "_", item_type, "_", dplyr::row_number()))
  }

  # Tidy up
  if(!is_na_scalar(ngram_item_bank)) {
    ngram_item_bank <- janitor::remove_empty(ngram_item_bank, which = "cols")
  }
  # Remove redundancy
  ngram_item_bank <-  remove_redundancy(remove_redundancy, ngram_item_bank, distinct_based_on_melody_only)
  # Save
  save_item_bank(ngram_item_bank, name, type = "ngram")

  # Create combined item bank (i.e., put everything together)
  if(any(c("all", "combined") %in% output)) {

    all_names <- list(
                    names(item_item_bank),
                    names(ngram_item_bank),
                    names(phrase_item_bank)
                   ) %>% purrr::compact() # Remove any NULLs

    # The files DB is meaningless here, so leave it out

    joint_names <- purrr::reduce(all_names, function(x, y) base::intersect(x, y))

    # use as_tibble to temporaily remove class, otherwise dplyr::select will raise error
    combined_item_bank <- rbind(
                                if(is_na_scalar(item_item_bank)) NA else item_item_bank %>% tibble::as_tibble() %>% dplyr::select(joint_names),
                                if(is_na_scalar(ngram_item_bank)) NA else ngram_item_bank %>% tibble::as_tibble() %>% dplyr::select(joint_names),
                                if(is_na_scalar(phrase_item_bank)) NA else phrase_item_bank %>% tibble::as_tibble() %>% dplyr::select(joint_names)
                                ) %>%
      janitor::remove_empty(which = "rows")

  }

  # Tidy up
  if(!is_na_scalar(combined_item_bank)) {
    combined_item_bank <- janitor::remove_empty(combined_item_bank, which = "cols")
  }

  # Remove redundancy
  combined_item_bank <-  remove_redundancy(remove_redundancy, combined_item_bank, distinct_based_on_melody_only)
  # Save
  save_item_bank(combined_item_bank, name, type = "combined")

  if(launch_app & ! is_na_scalar(combined_item_bank)) {
    itembankexplorer::item_bank_explorer(combined_item_bank)
  }

  if(return_item_bank) {
    return(list(
      file = file_item_bank,
      item = item_item_bank,
      phrase = phrase_item_bank,
      ngram = ngram_item_bank,
      combined = combined_item_bank
    ))
  }

}



create_item_bank_function <- function() {
  # TODO

  # Return a function which can access the different dfs
  item_bank <- function(key) {
    l <- list("file" =  set_item_bank_class(file_item_bank, extra = "file_item_bank"),
              "item" = set_item_bank_class(item_item_bank, extra = "item_item_bank"),
              "ngram" = set_item_bank_class(ngram_item_bank, extra = "ngram_item_bank"),
              "phrase" = set_item_bank_class(phrase_item_bank, extra = "phrase_item_bank"),
              "combined" = set_item_bank_class(combined_item_bank, extra = "combined_item_bank"))
    l[[key]]
  }
}


#' Save an item bank
#'
#' @param item_bank
#' @param name
#' @param type
#'
#' @return
#' @export
#'
#' @examples
save_item_bank <- function(item_bank, name, type = c("item", "phrase", "ngram", "combined")) {
  orig_length <- attributes(item_bank)$item_bank_orig_length
  if(!is_na_scalar(item_bank)) {
    attr(item_bank, "item_bank_name") <- name
    attr(item_bank, "item_bank_type") <- type
    attr(item_bank, "proportion_non_redundant") <- if(is_na_scalar(item_bank)) NA else round(nrow(item_bank)/orig_length, 2)
    item_bank <- set_item_bank_class(item_bank, extra = paste0(type, "_item_bank"))
    save(item_bank, file = paste0(name, '_', type, '.rda'), compress = "xz")
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
  if(remove_redundancy) {
    # Get orig lengths
    item_bank_orig_length <- if(is_na_scalar(item_bank)) NA else nrow(item_bank)

    item_bank <- if(is_na_scalar(item_bank)) NA else if(distinct_based_on_melody_only) {
        item_bank %>% dplyr::distinct(melody, .keep_all = TRUE)
      } else {
        item_bank %>% dplyr::distinct(melody, durations, .keep_all = TRUE)
      }

    attr(item_bank, "item_bank_orig_length") <- item_bank_orig_length

    return(item_bank)

  } else {
    return(item_bank)

  }
}


# TODO: Documentation

# Example:
# item_bank <- create_item_bank("Test", input = "phrase_df", output = "all", input_df = phrases_df)

