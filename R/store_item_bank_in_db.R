
#' Store an item bank in the musicassessr database
#'
#' @param item_bank
#' @param item_bank_name
#' @param item_bank_description
#' @param db_name
#' @param db_host
#' @param db_username
#' @param db_password
#'
#' @return
#' @export
#'
#' @examples
store_item_bank_in_db <- function(item_bank,
                                  item_bank_name,
                                  item_bank_description,
                                  db_name = Sys.getenv("DB_NAME"),
                                  db_host = Sys.getenv("DB_HOST"),
                                  db_username = Sys.getenv("DB_USER"),
                                  db_password = Sys.getenv("DB_PASSWORD")) {

  con <- musicassessr::connect_to_db(db_host, db_name, db_username, db_password)



  if(item_bank_name %in% DBI::dbListTables(con)) stop("A table with this name already exists. Unique name needed.")

  logging::loginfo("Storing new item bank %s in %s database.", item_bank_name, db_name)


    DBI::dbWriteTable(con,
                      name = item_bank_name,
                      value = item_bank,
                      overwrite = FALSE,
                      append = FALSE)


    musicassessr::db_append_to_table(con, table = "item_banks",
                                     data = tibble::tibble(item_bank_name = item_bank_name,
                                                           item_bank_description = item_bank_description)
                                     )

    DBI::dbDisconnect(con)


}





# store_item_bank_in_db(Berkowitz::phrase_item_bank, item_bank_name = "Berkowitz_phrase_item_bank", item_bank_description = "The Berkowitz corpus as a phrase item bank.")
# store_item_bank_in_db(Berkowitz::ngram_item_bank, item_bank_name = "Berkowitz_ngram_item_bank", item_bank_description = "The Berkowitz corpus as a ngram item bank.")




