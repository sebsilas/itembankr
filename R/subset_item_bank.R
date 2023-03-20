
#' Subset Item Bank
#'
#' @param item_bank
#' @param item_length
#' @param quantile_cut
#' @param span_min
#' @param span_max
#' @param tonality
#' @param return_as_item_bank_class
#'
#' @return
#' @export
#' @examples
subset_item_bank <- function (item_bank,
                              item_length = NULL,
                              quantile_cut = if("log_freq" %in% names(item_bank)) min(item_bank$log_freq) else -Inf,
                              span_min = min(item_bank$span),
                              span_max = max(item_bank$span),
                              tonality = NULL,
                              min_mean_duration = 0,
                              return_as_item_bank_class = FALSE) {

  stopifnot(tibble::is_tibble(item_bank),
           is.null.or(item_length, function(x) length(x) %in% 1:2),
           is.scalar.numeric(quantile_cut),
            is.scalar.numeric(span_min),
            is.scalar.numeric(span_max),
           is.null.or(tonality, assertthat::is.string),
           is.scalar.numeric(min_mean_duration),
           is.scalar.logical(return_as_item_bank_class))

  item_bank <- unclass_item_bank(item_bank)

  item_length <- parse_item_bank_length(item_length, item_bank)

  if (!is.null(tonality)) {
    item_bank <- item_bank %>% dplyr::filter(mode == tonality)
  }

  item_bank <- item_bank %>% dplyr::filter(dplyr::between(N, item_length[1], item_length[2]),
                                           dplyr::between(span, span_min, span_max),
                                           mean_duration > min_mean_duration)

  if(!is.na(quantile_cut) & "log_freq" %in% names(item_bank)) {
    item_bank <- item_bank %>% dplyr::filter(log_freq >= quantile_cut)
  }

  if(return_as_item_bank_class) {
    item_bank <- item_bank %>% set_item_bank_class()
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

