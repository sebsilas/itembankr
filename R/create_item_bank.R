


#' Convert corpus to item bank
#'
#' @param name A string of the item bank name.
#' @param input A string denoting the input. Must be one of "files", "item_df" or "phrases_df".
#' @param output A character vector denoting the desired output type or types. You cannot create an output backwards in the hierarchy from the input.
#' @param midi_file_dir If the input is files, a directory with MIDI files. Files should be in the format item_bank_name0.mid.
#' @param musicxml_file_dir If the input is files, a directory with musicxml files. Files should be in the format item_bank_name0.musicxml.
#' @param input_df If using input item_df or _phrases_df, an input dataframe.
#' @param launch_app Should the app be launched at the end?
#' @param remove_redundancy Should redundant relative melodies be removed? i.e., multiple representations of the same melody in relative form.
#' @param remove_melodies_with_only_repeated_notes Remove any melodies which consist only of a single note repeated.
#' @param remove_melodies_with_any_repeated_notes Remove any melodies which contain any consecutive repeated notes.
#' @param scale_durations_to_have_min_abs_value_of_x_seconds Scale melody durations to have a minimum of x seconds.
#'
#' @return
#' @export
#'
#' @examples
create_item_bank <- function(name = "",
                            input = c("files", "item_df", "phrase_df"),
                            output = c("all", "file", "item", "phrase", "ngram", "combined"),
                            midi_file_dir = NULL,
                            musicxml_file_dir = NULL,
                            input_df = NULL,
                            launch_app = TRUE,
                            remove_redundancy = TRUE,
                            remove_melodies_with_only_repeated_notes = TRUE,
                            remove_melodies_with_any_repeated_notes = FALSE,
                            scale_durations_to_have_min_abs_value_of_x_seconds = 0.25) {

  stopifnot(
    assertthat::is.string(name),
    input %in% c("files", "item_df", "phrase_df"),
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
    is.scalar.numeric(scale_durations_to_have_min_abs_value_of_x_seconds)
  )

  input_check(midi_file_dir, musicxml_file_dir, input_df)

  if(remove_melodies_with_any_repeated_notes) {
    remove_melodies_with_only_repeated_notes <- FALSE # Because remove_melodies_with_any_repeated_notes is stricter. So don't waste processing time.
  }

  # Create file item bank
  if(input == "files") {
    file_item_bank <- create_item_bank_from_files(midi_file_dir, musicxml_file_dir) %>%
      remove_melodies(remove_melodies_with_only_repeated_notes, remove_melodies_with_any_repeated_notes) %>%
      dplyr::mutate(item_type = "file",
                    item_id = paste0(name, "_", item_type, "_", dplyr::row_number()))

  } else {
    file_item_bank <- NA
  }

  # Create item bank with features
  if(input %in% c("files", "item_df") & any(output %in% c("item", "all", "combined")))  {
    if(input == "files") {
      item_item_bank <- get_melody_features(file_item_bank)  %>%
        remove_melodies(remove_melodies_with_only_repeated_notes, remove_melodies_with_any_repeated_notes) %>%
        scale_durations_to_have_min_abs_value_of_x_seconds(x = scale_durations_to_have_min_abs_value_of_x_seconds) %>%
        dplyr::mutate(item_type = "item",
                      item_id = paste0(name, "_", item_type, "_", dplyr::row_number()))

    } else {
      item_item_bank <- get_melody_features(input_df) %>%
        remove_melodies(remove_melodies_with_only_repeated_notes, remove_melodies_with_any_repeated_notes) %>%
        scale_durations_to_have_min_abs_value_of_x_seconds(x = scale_durations_to_have_min_abs_value_of_x_seconds) %>%
        dplyr::mutate(item_type = "item",
                      item_id = paste0(name, "_", item_type, "_", dplyr::row_number()))
    }
  } else {
    item_item_bank <- NA
  }


  # Create phrase item bank (with features) i.e., chop up items based on segmentation
  if(input %in% c("files", "item_df", "phrase_df") & any(output %in% c("phrase", "all", "combined")))  {

    if(input == "phrase_df") {
      phrase_item_bank <- input_df
    } else {
      phrase_item_bank <- file_item_bank %>%
        create_phrases_db()
    }

    phrase_item_bank <-  phrase_item_bank %>%
      remove_melodies(remove_melodies_with_only_repeated_notes, remove_melodies_with_any_repeated_notes) %>%
      scale_durations_to_have_min_abs_value_of_x_seconds(x = scale_durations_to_have_min_abs_value_of_x_seconds) %>%
      get_melody_features() %>%
      dplyr::mutate(item_type = "phrase",
                    item_id = paste0(name, "_", item_type, "_", dplyr::row_number()))

  }

  # Create ngram item bank (with features) i.e., chop up items into N-grams
  if(any(output %in% c("ngram", "all", "combined"))) {
    ngram_item_bank <- phrase_item_bank %>%
      create_ngram_item_bank(name = name) %>%
      remove_melodies(remove_melodies_with_only_repeated_notes, remove_melodies_with_any_repeated_notes) %>%
      dplyr::mutate(item_type = "ngram",
                    item_id = paste0(name, "_", item_type, "_", dplyr::row_number()))
  } else {
    ngram_item_bank <- NA
  }

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

  } else {
    combined_item_bank <- NA
  }


  # Tidy up
  if(!is_na_scalar(file_item_bank)) {
    file_item_bank <- janitor::remove_empty(file_item_bank, which = "cols")
  }
  if(!is_na_scalar(item_item_bank)) {
    item_item_bank <- janitor::remove_empty(item_item_bank, which = "cols")
  }

  if(!is_na_scalar(phrase_item_bank)) {
    phrase_item_bank <- janitor::remove_empty(phrase_item_bank, which = "cols")
  }

  if(!is_na_scalar(ngram_item_bank)) {
    ngram_item_bank <- janitor::remove_empty(ngram_item_bank, which = "cols")
  }

  if(!is_na_scalar(combined_item_bank)) {
    combined_item_bank <- janitor::remove_empty(combined_item_bank, which = "cols")
  }

  # Remove redundancy
  if(remove_redundancy) {
    # Get orig lengths
    item_item_bank_orig_length <- if(is_na_scalar(item_item_bank)) NA else nrow(item_item_bank)
    phrase_item_bank_orig_length <- if(is_na_scalar(phrase_item_bank)) NA else nrow(phrase_item_bank)
    ngram_item_bank_orig_length <- if(is_na_scalar(ngram_item_bank)) NA else nrow(ngram_item_bank)
    combined_item_bank_orig_length <- if(is_na_scalar(combined_item_bank)) NA else nrow(combined_item_bank)


    item_item_bank <- if(is_na_scalar(item_item_bank)) NA else item_item_bank %>% dplyr::distinct(melody, .keep_all = TRUE)
    phrase_item_bank <- if(is_na_scalar(phrase_item_bank)) NA else phrase_item_bank %>% dplyr::distinct(melody, .keep_all = TRUE)
    ngram_item_bank <- if(is_na_scalar(ngram_item_bank)) NA else ngram_item_bank %>% dplyr::distinct(melody, .keep_all = TRUE)
    combined_item_bank <- if(is_na_scalar(combined_item_bank)) NA else combined_item_bank %>% dplyr::distinct(melody, .keep_all = TRUE)

  }


  if(!is_na_scalar(file_item_bank)) {
    attr(file_item_bank, "item_bank_name") <- name
    attr(file_item_bank, "item_bank_type") <- "file"
    file_item_bank <- set_item_bank_class(file_item_bank, extra = "file_item_bank")
    save(file_item_bank, file = paste0(name, "_file.rda"))
  }

  if(!is_na_scalar(item_item_bank)) {
    attr(item_item_bank, "item_bank_name") <- name
    attr(item_item_bank, "item_bank_type") <- "item"
    item_item_bank <- set_item_bank_class(item_item_bank, extra = "item_item_bank")
    attr(item_item_bank, "proportion_non_redundant") <- if(is_na_scalar(item_item_bank)) NA else round(nrow(item_item_bank)/item_item_bank_orig_length, 2)
    save(item_item_bank, file = paste0(name, "_item.rda"))
  }

  if(!is_na_scalar(ngram_item_bank)) {
    attr(ngram_item_bank, "item_bank_name") <- name
    attr(ngram_item_bank, "item_bank_type") <- "ngram"
    ngram_item_bank <- set_item_bank_class(ngram_item_bank, extra = "ngram_item_bank")
    attr(ngram_item_bank, "proportion_non_redundant") <- if(is_na_scalar(ngram_item_bank)) NA else round(nrow(ngram_item_bank)/ngram_item_bank_orig_length, 2)
    save(ngram_item_bank, file = paste0(name, '_ngram.rda'))
  }

  if(!is_na_scalar(phrase_item_bank)) {
    attr(phrase_item_bank, "item_bank_name") <- name
    attr(phrase_item_bank, "item_bank_type") <- "phrase"
    phrase_item_bank <- set_item_bank_class(phrase_item_bank, extra = "phrase_item_bank")
    attr(phrase_item_bank, "proportion_non_redundant") <- if(is_na_scalar(phrase_item_bank)) NA else round(nrow(phrase_item_bank)/phrase_item_bank_orig_length, 2)
    save(phrase_item_bank, file = paste0(name, '_phrase.rda'))
  }

  if(!is_na_scalar(combined_item_bank)) {
    attr(combined_item_bank, "item_bank_name") <- name
    attr(combined_item_bank, "item_bank_type") <- "combined"
    combined_item_bank <- set_item_bank_class(combined_item_bank, extra = "combined_item_bank")
    attr(combined_item_bank, "proportion_non_redundant") <- if(is_na_scalar(combined_item_bank)) NA else round(nrow(combined_item_bank)/combined_item_bank_orig_length, 2)
    save(combined_item_bank, file = paste0(name, '_combined.rda'))
  }


  if(launch_app & ! is_na_scalar(combined_item_bank)) {
    itembankexplorer::item_bank_explorer(combined_item_bank)
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

# TODO: Documentation

# Example:
# item_bank <- create_item_bank("Test", input = "phrase_df", output = "all", input_df = phrases_df)

