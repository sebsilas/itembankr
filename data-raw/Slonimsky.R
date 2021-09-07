## code to prepare `Slonimsky` dataset goes here

dat <- readLines('data-raw/Slonimsky.txt')

dat <- lapply(dat, function(x) as.integer(unlist(strsplit(unlist(strsplit(x, ","))[[2]], " "))))

dat <- lapply(dat, function(x) x[!is.na(x)])

dat_str <- lapply(dat, function(x) paste0(x, collapse = ","))


df <- data.frame(melody = unlist(dat_str))


Slonimsky <- corpus_to_item_bank(corpus_name = "Slonimsky",
                                 corpus_df = df,
                                 output_type = "ngram",
                                 launch_app = TRUE)


#usethis::use_data(Slonimsky, overwrite = TRUE)
