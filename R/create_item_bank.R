

# t <- corpus_to_item_bank("Test", input = "phrase_df", output = "all",
# input_df = tibble::tibble(abs_melody = rep("61,62,63,64,65,66", 2), durations = rep("1,2,3,4,5,6", 2)))
# t2 <- t("ngram")


# TODO: Documentation

# Example:
# item_bank <- corpus_to_item_bank("Test", input = "phrase_df", output = "all", input_df = phrases_df)



#' Convert corpus to item bank
#'
#' @param name A string of the item bank name.
#' @param input A string denoting the input. Must be one of "files", "item_df" or "phrases_df".
#' @param output A character vector denoting the desired output type or types. You cannot create an output backwards in the hierarchy from the input.
#' @param midi_file_dir If the input is files, a directory with MIDI files. Files should be in the format item_bank_name0.mid.
#' @param musicxml_file_dir If the input is files, a directory with musicxml files. Files should be in the format item_bank_name0.musicxml.
#' @param input_df If using input item_df or _phrases_df, an input dataframe.
#' @param launch_app Should the app be launched at the end?
#'
#' @return
#' @export
#'
#' @examples
corpus_to_item_bank <- function(name = "",
                                input = c("files", "item_df", "phrase_df"),
                                output = c("all", "file", "item", "phrase", "ngram", "combined"),
                                midi_file_dir = NULL,
                                musicxml_file_dir = NULL,
                                input_df = NULL,
                                launch_app = TRUE){

  stopifnot(
    assertthat::is.string(name),
    assertthat::is.string(input),
    is.character(output) & length(output) > 0,
    assertthat::is.string(midi_file_dir) | is.null(midi_file_dir),
    assertthat::is.string(musicxml_file_dir) | is.null(musicxml_file_dir),
    is.data.frame(input_df) | is.null(input_df),
    is.logical(launch_app)
  )

  input_check(midi_file_dir, musicxml_file_dir, input_df)

  if(input == "files") {
    file_item_bank <- create_item_bank_from_files(midi_file_dir, musicxml_file_dir)
  } else {
    file_item_bank <- NA
  }

  if(input %in% c("files", "item_df") & "item" %in% output | "combined" %in% output)  {
    if(input == "files") {
      item_item_bank <- get_melody_features(file_item_bank)  %>% dplyr::mutate(item_type = "item")
    } else {
      item_item_bank <- get_melody_features(input_df) %>% dplyr::mutate(item_type = "item")
    }
  } else {
    item_item_bank <- NA
  }

  if(input %in% c("files", "item_df") & "phrase" %in% output | "all" %in% output | "combined" %in% output)  {
    if(input == "phrase_df") {
      phrases_item_bank <- get_melody_features(input_df)  %>% dplyr::mutate(item_type = "phrase")
    } else {
      phrases_item_bank <- create_phrases_db(file_item_bank) %>% get_melody_features() %>% dplyr::mutate(item_type = "phrase")
    }
  }

  if("ngram" %in% output | "all" %in% output  | "combined" %in% output) {
    if(input == "phrase_df") {

      phrase_item_bank <- input_df %>% get_melody_features()

      ngram_item_bank <- input_df %>%
        dplyr::rowwise() %>%
        dplyr::mutate(orig_melody = paste0(diff(str_mel_to_vector(abs_melody)), collapse = ","),
                      orig_N = length(str_mel_to_vector(abs_melody))) %>%
        dplyr::ungroup() %>%
        dplyr::rename(orig_durations = durations) %>%
        unique() %>%
        split_item_bank_into_ngrams() %>%
        count_freqs() %>%
        unique() %>%
        compute_ngram_similarity() %>%
        get_melody_features() %>%
        dplyr::mutate(item_type = "ngram")

    } else {
      ngram_item_bank <- split_item_bank_into_ngrams(phrases_item_bank) %>%  get_melody_features() %>% dplyr::mutate(item_type = "ngram")
    }
  }

  if("combined" %in% output) {
    # print('names phrases')
    # print(names(phrases_item_bank))
    # print('names ngram')
    # print(names(ngram_item_bank))
    # print('diff:')
    # print(setdiff(names(ngram_item_bank), names(phrases_item_bank)))
    # combined_item_bank <- rbind(phrases_item_bank, ngram_item_bank)
    # if(!is.na(item_item_bank)) {
    #   combined_item_bank <- rbind(combined_item_bank, item_item_bank)
    # }
  }

  combined_item_bank <- NA # for now


  if(!is.na(file_item_bank)) file_item_bank <- janitor::remove_empty(file_item_bank, which = "cols")
  if(!is.na(item_item_bank)) item_item_bank <- item_item_bank <- janitor::remove_empty(item_item_bank, which = "cols")
  phrases_item_bank <- janitor::remove_empty(phrases_item_bank, which = "cols")
  ngram_item_bank <- janitor::remove_empty(ngram_item_bank, which = "cols")
  #combined_item_bank <- janitor::remove_empty(combined_item_bank, which = "cols")


  # return a function which can access the different dfs
  item_bank <- function(key) {
    l <- list("file" = file_item_bank,
              "item" = item_item_bank,
             "ngram" = ngram_item_bank,
             "phrase" = phrases_item_bank,
             "combined" = combined_item_bank)
    l[[key]]
  }

  #save(item_bank, file = paste0(name, ".rda"))
  #save(item_bank, file = paste0(name, ".rda"))
  save(phrases_item_bank, file = paste0(name, '_phrase.rda'))
  save(ngram_item_bank, file = paste0(name, '_ngram.rda'))

  if(launch_app) {
    itembankexplorer::item_bank_explorer(item_bank("phrase"))
  }

  item_bank

}
