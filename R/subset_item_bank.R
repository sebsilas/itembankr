
subset_item_bank <- function(item_bank, item_length = NULL, quantile_cut = NULL, span_min = NULL, span_max = NULL, tonality = NULL) {
  # item_bank should be a df read in e.g by read_rds
  item_length <- parse_item_bank_length(item_length, item_bank)

  if (is.null(quantile_cut)) { quantile_cut <- min(item_bank$log.freq) }
  if (is.null(span_min)) { span_min <- min(item_bank$span) }
  if (is.null(span_max)) { span_max <- max(item_bank$span) }
  if(!is.null(tonality)) { item_bank <- item_bank %>% filter(mode == tonality) }
  item_bank %>% filter(log.freq > quantile_cut & N >= item_length[1] & N <= item_length[2] &
                         span >= span_min & span <= span_max)
}

top_quantile <- function(item_bank, quantile_cut = .95) {
  cut <- quantile(item_bank$log.freq, 1-quantile_cut)
  print(cut)
  # filt <- item_bank %>% filter(log.freq > cut)
  # ggplot(filt) +
  #   geom_histogram(aes(log.freq))
  cut
}


parse_item_bank_length <- function(specified_length, item_bank) {
  if (is.null(specified_length)) {
    specified_length <- c(1, max(item_bank$N))
  }
  else if (length(specified_length) == 1) {
    specified_length <- c(specified_length, specified_length)
  }
  else if (is.na(specified_length[2])) { # the NULL gets coerced to NA
    specified_length[2] <- max(item_bank$N)
  }
  else if(length(specified_length) > 2) {
    specified_length <- c(specified_length[1], specified_length[length(specified_length)])
  }
  else {
    stop('Unknown length format')
  }
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

