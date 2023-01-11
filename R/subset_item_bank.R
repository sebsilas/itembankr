
#' Subset Item Bank
#'
#' @param item_bank
#' @param item_length
#' @param quantile_cut
#' @param span_min
#' @param span_max
#' @param tonality
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
                              min_mean_duration = 0) {

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

  item_bank
}


top_quantile <- function(item_bank, quantile_cut = .95) {
  cut <- quantile(item_bank$log_freq, 1-quantile_cut)
  print(cut)
  # filt <- item_bank %>% dplyr::filter(log_freq > cut)
  # ggplot(filt) +
  #   geom_histogram(aes(log_freq))
  cut
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




# d_1 <- subset_item_bank(item_bank = WJD, item_length = 5:15, span_min = 12, span_max = 30, tonality = "major")
#
# d_2 <- subset_item_bank(item_bank = WJD, item_length = 5:15, span_min = 12, tonality = "minor")
#
# d_3 <- subset_item_bank(item_bank = WJD, item_length = 5:15, span_min = 12, tonality = "minor")
#
# d_4 <- subset_item_bank(item_bank = WJD, item_length = c(3, NA))
#
# d_5 <- subset_item_bank(item_bank = WJD, item_length = 3)

# item_bank_sub <- subset_item_bank(item_bank = WJD, N_range = c(3, NULL), span_min = 12, tonality = "major")


#subset_item_bank(item_bank = WJD)
# subset_item_bank(item_bank = Berkowitz("phrases"))


