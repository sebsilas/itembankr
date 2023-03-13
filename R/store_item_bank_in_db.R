
store_item_bank_in_db <- function(item_bank,
                                  host = Sys.getenv("DB_HOST"),
                                  username = Sys.getenv("DB_USER"),
                                  password = Sys.getenv("DB_PASSWORD")) {

  item_bank_name <- paste0(substitute(item_bank))[3]

  con <- musicassessr::connect_to_db(host, username, password)


  logging::loginfo(paste0("Storing new item bank ", item_bank_name, " in DB."))

  purrr::walk(list_item_bank_types(), function(type) {

    print(type)

    tmp_df <- item_bank(type)

    if(!is.na(tmp_df)) {

      item_bank_type_name <- paste0(item_bank_name, "_", type)

      if(item_bank_type_name %in% DBI::dbListTables(con)) stop("A table with this name already exists. Unique name needed.")

      logging::loginfo(paste0("Storing new item bank ", item_bank_type_name, " in DB."))


      DBI::dbWriteTable(con,
                        name = item_bank_type_name,
                        value = tmp_df,
                        overwrite = FALSE,
                        append = FALSE)

      print('tables: ')
      print(DBI::dbListTables(con))
    }

    })

    DBI::dbDisconnect(con)


}











# store_item_bank_in_db(WJD::WJD, host, username, password)

# t <- musicassessr::get_table("WJD_ngram")
# t <- musicassessr::get_table("trials")


# library(dbplyr)
# Note that you don’t actually need to load dbplyr with library(dbplyr);
# dplyr automatically loads it for you when it sees you working with a database.


# normal_test <- system.time({
#
#   WJD::WJD('ngram') %>% dplyr::filter(N == 43)
#
# })
#
#
# dbplyr_test <- system.time({
#
#   con <- musicassessr::connect_to_db()
#
#   ngram <- dplyr::tbl(con, "WJD_ngram")
#
#   ngram %>% dplyr::filter(N == 43)
#
# })



get_user_trials_from_last_session <- function(test_username) {


  sessions <- musicassessr::get_table("sessions")

  user_sessions <- sessions %>% dplyr::filter(test_username == "Seb")

  latest_session <- user_sessions %>% dplyr::slice_max(time_completed) %>% dplyr::pull(session_id)

  trials <- musicassessr::get_table("trials")


  # latest trial for a given melody and user
  trials %>%
    dplyr::filter(test_username == !! test_username & session_id == latest_session) %>%
    dplyr::pull(abs_melody) %>%
    unique()

}



# get_user_trials_from_last_session("Seb")

