# --- meta prefixer ----------------------------------------------------
# Rename only if the META_* version doesn't already exist.
prefix_meta_cols <- function(df) {
  if (is.null(df) || is_na_scalar(df)) return(df)
  cols <- names(df)

  base_meta <- c("item_id", "item_type", "file_key",
                 "midi_file", "musicxml_file", "audio_file", "mp3")

  src <- intersect(base_meta, cols)
  if (!length(src)) return(df)

  # only rename when META_* doesn't already exist
  need <- src[!paste0("META_", src) %in% cols]
  if (!length(need)) return(df)

  rename_map <- stats::setNames(need, paste0("META_", need))  # NEW = OLD
  dplyr::rename(df, !!!rename_map)
}

# --- generic prefixer you already use (exclude = keep list) ----------
# Prefix all columns NOT in `exclude` and NOT already starting with `prefix`.
prefix_feature_cols <- function(df, prefix,
                                exclude = c("META_", "MEL_", "AUD_")) {
  if (is.null(df) || is_na_scalar(df)) return(df)
  cols <- names(df)

  # build exclusion set:
  # 1) any column whose name starts with any of the known prefixes
  # 2) any literal column names passed in exclude (for convenience)
  starts_with_any <- function(nm, prefixes) {
    Reduce(`|`, lapply(prefixes, function(p) startsWith(nm, p)), init = FALSE)
  }
  keep <- unique(c(cols[starts_with_any(cols, exclude)],
                   intersect(exclude, cols)))

  # to prefix = not in keep and not already with this prefix
  feat <- setdiff(cols, keep)
  feat <- feat[!startsWith(feat, prefix)]
  if (!length(feat)) return(df)

  # dplyr::rename(.data, NEW = OLD)
  rename_map <- stats::setNames(feat, paste0(prefix, feat))
  dplyr::rename(df, !!!rename_map)
}

# --- bank-specific helpers -------------------------------------------
# Prefix ALL non-meta/non-prefixed columns in a melodic bank to MEL_.
prefix_melodic_bank <- function(df) {
  if (is.null(df) || is_na_scalar(df)) return(df)
  df <- prefix_meta_cols(df)
  # keep META_* and anything already prefixed (so we don't double-prefix)
  keep <- c("META_", "MEL_", "AUD_")
  prefix_feature_cols(df, "MEL_", exclude = keep)
}

# Prefix ALL non-meta/non-prefixed columns in an audio bank to AUD_.
prefix_audio_bank <- function(df) {
  if (is.null(df) || is_na_scalar(df)) return(df)
  df <- prefix_meta_cols(df)
  keep <- c("META_", "MEL_", "AUD_")
  prefix_feature_cols(df, "AUD_", exclude = keep)
}


#' Removes leading META_/MEL_/AUD_ once (no recursion).
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
deprefix_cols <- function(df) {
  if (is.null(df) || is_na_scalar(df)) return(df)
  nm <- names(df)
  new <- sub("^(META_|MEL_|AUD_)", "", nm, perl = TRUE)
  names(df) <- new
  df
}
