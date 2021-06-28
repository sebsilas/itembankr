
slonimsky <- readLines('R/www/item_banks/Slonimsky_Complete_no_semicolon.txt')

slonimsky <- lapply(slonimsky, function(x) as.integer(unlist(strsplit(unlist(strsplit(x, ","))[[2]], " "))))

slonimsky <- lapply(slonimsky, function(x) x[!is.na(x)])

saveRDS(slonimsky, "Slonimsky.RDS")
