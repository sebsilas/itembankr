

#' List official itembankr item banks for use
#'
#' @return
#' @export
#'
#' @examples
list_official_item_banks <- function(default = "Berkowitz") {

  if(default == "Berkowitz") {

    list("Berkowitz" = "Berkowitz::Berkowitz",
         "WJD" = "WJD::WJD",
         "Slonimsky" = "Slonimsky::Slonimsky")

  } else if(default == "WJD") {

    list("WJD" = "WJD::WJD",
         "Berkowitz" = "Berkowitz::Berkowitz",
         "Slonimsky" = "Slonimsky::Slonimsky")

  } else if (default == "Slonimsky") {

    list("Slonimsky" = "Slonimsky::Slonimsky",
         "Berkowitz" = "Berkowitz::Berkowitz",
         "WJD" = "WJD::WJD")

  } else {
    stop("default not recognised.")
  }
}

#' List item bank types
#'
#' @return
#' @export
#'
#' @examples
list_item_bank_types <- function() {
  c("file",
    "item",
    "ngram",
    "phrase",
    "combined")
}
