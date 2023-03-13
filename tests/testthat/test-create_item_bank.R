

# TODO: Rewrite for new scheme

test_that("create_item_bank works", {

  # When going in as a phrase_df, you don't want an item or file DF
  item_bank <- create_item_bank(name = "Test",
                                   input = "phrase_df",
                                   output = "all",
                                   input_df = tibble::tibble(abs_melody = c("61,62,63,64,65,66",
                                                                            "72, 73, 75, 78"),
                                                             durations = c("1,2,3,4,5,6",
                                                                           "1, 1, 1, 1")))

  expect_setequal(class(item_bank), c("function", "item_bank"))

  file_item_bank <- item_bank("file")
  item_item_bank <- item_bank("item")
  ngram_item_bank <- item_bank("ngram")
  phrase_item_bank <- item_bank("phrase")
  combined_item_bank <- item_bank("combined")

  expect_identical(file_item_bank, NA)
  expect_identical(item_item_bank, NA)

  expect_setequal(class(ngram_item_bank), c("tbl_df", "tbl", "data.frame", "item_bank", "ngram_item_bank"))
  expect_setequal(class(phrase_item_bank), c("tbl_df", "tbl", "data.frame", "item_bank", "phrase_item_bank"))
  expect_setequal(class(combined_item_bank), c("tbl_df", "tbl", "data.frame", "item_bank", "combined_item_bank"))


  expect_setequal(names(ngram_item_bank),
                  c("durations", "abs_melody", "N", "parent_abs_melody", "parent_melody", "parent_durations",
                    "parent_N", "melody", "freq", "rel_freq", "log_freq", "ngrukkon",
                    "i.entropy", "span", "tonalness", "tonal.clarity", "tonal.spike",
                    "mode", "step.cont.glob.var", "step.cont.glob.dir", "step.cont.loc.var",
                    "mean_int_size", "int_range", "dir_change", "mean_dir_change",
                    "int_variety", "pitch_variety", "mean_run_length", "d.entropy",
                    "d.eq.trans", "mean_duration", "item_type", "item_id"))


  expect_setequal(names(phrase_item_bank),
                  c("abs_melody", "durations", "melody", "N", "i.entropy", "span",
                    "tonalness", "tonal.clarity", "tonal.spike", "mode", "step.cont.glob.var",
                    "step.cont.glob.dir", "step.cont.loc.var", "mean_int_size", "int_range",
                    "dir_change", "mean_dir_change", "int_variety", "pitch_variety",
                    "mean_run_length", "d.entropy", "d.eq.trans", "mean_duration",
                    "item_type", "item_id"))

  expect_identical(get_item_bank_name(item_bank), "Test")



  # Now going in with files and out with everything

  item_bank2 <- create_item_bank(name = "Test2",
                                    input = "files",
                                     output = "all",
                                     midi_file_dir = test_path("test_data/berkowitz_midi_rhythmic"),
                                     musicxml_file_dir = test_path("test_data/berkowitz_musicxml"))

  expect_identical(get_item_bank_name(item_bank2), "Test2")



  file_item_bank2 <- item_bank2("file")
  item_item_bank2 <- item_bank2("item")
  ngram_item_bank2 <- item_bank2("ngram")
  phrase_item_bank2 <- item_bank2("phrase")
  combined_item_bank2 <- item_bank2("combined")


  expect_setequal(class(file_item_bank2), c("tbl_df", "tbl", "data.frame", "item_bank", "file_item_bank"))
  expect_setequal(class(item_item_bank2), c("tbl_df", "tbl", "data.frame", "item_bank", "item_item_bank"))
  expect_setequal(class(ngram_item_bank2), c("tbl_df", "tbl", "data.frame", "item_bank", "ngram_item_bank"))
  expect_setequal(class(phrase_item_bank2), c("tbl_df", "tbl", "data.frame", "item_bank", "phrase_item_bank"))
  expect_setequal(class(combined_item_bank2), c("tbl_df", "tbl", "data.frame", "item_bank", "combined_item_bank"))




})




test_that("create_item_bank works", {


  # When going in as a phrase_df, you don't want an item or file DF
  item_bank <- create_item_bank(name = "Test",
                                input = "phrase_df",
                                output = "all",
                                input_df = tibble::tibble(abs_melody = c("61,62,63,64,65,66",
                                                                         "72, 73, 75, 78"),
                                                          durations = c("1,2,3,4,5,6",
                                                                        "1, 1, 1, 1")))

  expect_setequal(class(item_bank), c("function", "item_bank"))

  file_item_bank <- item_bank("file")
  item_item_bank <- item_bank("item")
  ngram_item_bank <- item_bank("ngram")
  phrase_item_bank <- item_bank("phrase")
  combined_item_bank <- item_bank("combined")

  expect_identical(file_item_bank, NA)
  expect_identical(item_item_bank, NA)

  expect_setequal(class(ngram_item_bank), c("tbl_df", "tbl", "data.frame", "item_bank", "ngram_item_bank"))
  expect_setequal(class(phrase_item_bank), c("tbl_df", "tbl", "data.frame", "item_bank", "phrase_item_bank"))
  expect_setequal(class(combined_item_bank), c("tbl_df", "tbl", "data.frame", "item_bank", "combined_item_bank"))


  expect_setequal(names(ngram_item_bank),
                  c("durations", "abs_melody", "N", "parent_abs_melody", "parent_melody", "parent_durations",
                    "parent_N", "melody", "freq", "rel_freq", "log_freq", "ngrukkon",
                    "i.entropy", "span", "tonalness", "tonal.clarity", "tonal.spike",
                    "mode", "step.cont.glob.var", "step.cont.glob.dir", "step.cont.loc.var",
                    "mean_int_size", "int_range", "dir_change", "mean_dir_change",
                    "int_variety", "pitch_variety", "mean_run_length", "d.entropy",
                    "d.eq.trans", "mean_duration", "item_type", "item_id"))


  expect_setequal(names(phrase_item_bank),
                  c("abs_melody", "durations", "melody", "N", "i.entropy", "span",
                    "tonalness", "tonal.clarity", "tonal.spike", "mode", "step.cont.glob.var",
                    "step.cont.glob.dir", "step.cont.loc.var", "mean_int_size", "int_range",
                    "dir_change", "mean_dir_change", "int_variety", "pitch_variety",
                    "mean_run_length", "d.entropy", "d.eq.trans", "mean_duration",
                    "item_type", "item_id"))

  expect_identical(get_item_bank_name(item_bank), "Test")



  # Now going in with files and out with everything

  item_bank2 <- create_item_bank(name = "Test2",
                                 input = "files",
                                 output = "all",
                                 midi_file_dir = test_path("test_data/berkowitz_midi_rhythmic"),
                                 musicxml_file_dir = test_path("test_data/berkowitz_musicxml"))

  expect_identical(get_item_bank_name(item_bank2), "Test2")



  file_item_bank2 <- item_bank2("file")
  item_item_bank2 <- item_bank2("item")
  ngram_item_bank2 <- item_bank2("ngram")
  phrase_item_bank2 <- item_bank2("phrase")
  combined_item_bank2 <- item_bank2("combined")


  expect_setequal(class(file_item_bank2), c("tbl_df", "tbl", "data.frame", "item_bank", "file_item_bank"))
  expect_setequal(class(item_item_bank2), c("tbl_df", "tbl", "data.frame", "item_bank", "item_item_bank"))
  expect_setequal(class(ngram_item_bank2), c("tbl_df", "tbl", "data.frame", "item_bank", "ngram_item_bank"))
  expect_setequal(class(phrase_item_bank2), c("tbl_df", "tbl", "data.frame", "item_bank", "phrase_item_bank"))
  expect_setequal(class(combined_item_bank2), c("tbl_df", "tbl", "data.frame", "item_bank", "combined_item_bank"))




})





