
#' Subset Item Bank
#'
#' @param item_bank
#' @param item_length
#' @param quantile_cut
#' @param span_min
#' @param span_max
#' @param tonality
#' @param return_as_item_bank_class
#' @param retain_classes Should the item bank keep any extra classes it had?
#'
#' @return
#' @export
#' @examples
subset_item_bank <- function(item_bank,
                             item_length = NULL,
                             quantile_cut = -Inf,
                             span_min = 0L,
                             span_max = Inf,
                             tonality = NULL,
                             min_mean_duration = 0,
                             return_as_item_bank_class = FALSE,
                             retain_classes = FALSE) {

  stopifnot(is(item_bank, "tbl"), # This checks for a tibble, but allows a database backend too (i.e., from tbl(db_con, "tbl_name"))
            is.null.or(item_length, function(x) length(x) %in% 1:2),
            is.scalar.numeric(quantile_cut),
            is.scalar.numeric(span_min),
            is.scalar.numeric(span_max),
            is.null.or(tonality, assertthat::is.string),
            is.scalar.numeric(min_mean_duration),
            is.scalar.logical(return_as_item_bank_class),
            is.scalar.logical(retain_classes)
           )

  attributes <- attributes(item_bank)
  classes <- attributes$class

  item_bank <- unclass_item_bank(item_bank)

  item_length <- parse_item_bank_length(item_length, item_bank)

  if (!is.null(tonality)) {
    item_bank <- item_bank %>% dplyr::filter(mode == tonality)
  }

  item_bank <- item_bank %>% dplyr::filter(dplyr::between(N, item_length[1], item_length[2]),
                                           dplyr::between(span, span_min, span_max),
                                           mean_duration > min_mean_duration)

  if(quantile_cut > -Inf && "log_freq" %in% names(item_bank)) {
    item_bank <- item_bank %>% dplyr::filter(log_freq >= quantile_cut)
  }

  # Check no rows
  if("tbl_sql" %in% class(item_bank)) {
    nrows <- item_bank %>%
      dplyr::summarise(num_rows = dplyr::n()) %>%
      dplyr::pull(num_rows) %>%
      as.integer()
  } else {
    nrows <- nrow(item_bank)
  }

  if(nrows == 0) {
    stop("No items could be found for this set of parameters. Try being less restrictive with your subset values, or use a different item bank.")
  }

  if(return_as_item_bank_class) {
    item_bank <- item_bank %>% set_item_bank_class()
  }

  if(retain_classes) {
    attr(item_bank, "item_bank_name") <- attributes$item_bank_name
    attr(item_bank, "item_bank_type") <- attributes$item_bank_type
    attr(item_bank, "proportion_non_redundant") <- attributes$proportion_non_redundant
    attr(item_bank, "item_bank_orig_length") <- attributes$item_bank_orig_length
  }
  item_bank
}



top_quantile <- function(item_bank, quantile_cut = .95) {
  cut <- quantile(item_bank$log_freq, 1-quantile_cut)
}


parse_item_bank_length <- function(specified_length, item_bank) {
  if (is.null(specified_length)) {
    specified_length <- c(1, max(item_bank$N))
  } else if (length(specified_length) == 1) {
    specified_length <- c(specified_length, specified_length)
  } else if (is.na(specified_length[2])) { # the NULL gets coerced to NA
    specified_length[2] <- max(item_bank$N)
  } else if (is.na(specified_length[1])) { # the NULL gets coerced to NA
    specified_length[1] <- min(item_bank$N)
  } else if(length(specified_length) == 2 & !all(is.na(specified_length))) {
    specified_length
  } else if(length(specified_length) > 2) {
    specified_length <- c(specified_length[1], specified_length[length(specified_length)])
  } else {
    stop('Unknown length format')
  }
  specified_length
}


is.scalar.character <- function(x) {
  is.character(x) && is.scalar(x)
}

is.scalar.numeric <- function(x) {
  is.numeric(x) && is.scalar(x)
}

is.scalar.logical <- function(x) {
  is.logical(x) && is.scalar(x)
}

is.scalar <- function(x) {
  identical(length(x), 1L)
}

is.integerlike <- function(x) {
  all(round(x) == x)
}

is.scalar.integerlike <- function(x) {
  is.scalar(x) && is.integerlike(x)
}

is.null.or <- function(x, f) {
  is.null(x) || f(x)
}

