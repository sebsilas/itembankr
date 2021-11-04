

library(readxl)

MAST21_df <- read_excel("data-raw/MAST-21.xlsx")

MAST21_df[MAST21_df$melody == "NA", "melody"] <- NA

# separate the long notes
MAST21_df <- MAST21_df[5:nrow(MAST21_df), ]


MAST21 <- corpus_to_item_bank(corpus_name = "MAST21",
                              corpus_df = MAST21_df,
                              phrases_db = MAST21_df,
                              output_type = "phrases")

t <- MAST21("phrases")


usethis::use_data(MAST21, overwrite = TRUE)
