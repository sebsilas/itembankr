

test_that("create_item_bank_from_files works, musicxml only", {

  files_db <- create_item_bank_from_files(musicxml_file_dir = test_path("test_data/berkowitz_musicxml"))

  expect_setequal(names(files_db), c("abs_melody", "durations", "musicxml_file", "N", "file_key"))

  expect_equal(c(10, 5), dim(files_db))

})

test_that("create_item_bank_from_files works, midi only", {

  files_db <- create_item_bank_from_files(midi_file_dir = test_path("test_data/berkowitz_midi_rhythmic"))

  expect_setequal(names(files_db), c("abs_melody", "durations", "midi_file", "N", "file_key"))

  expect_equal(c(10, 5), dim(files_db))

})




test_that("create_item_bank_from_files works, midi and musicxml", {

  files_db <- create_item_bank_from_files(
                              midi_file_dir = test_path("test_data/berkowitz_midi_rhythmic"),
                              musicxml_file_dir = test_path("test_data/berkowitz_musicxml"))

  expect_setequal(names(files_db), c("abs_melody", "durations", "musicxml_file", "midi_file", "N", "file_key"))

  expect_equal(c(10, 6), dim(files_db))

})


test_that("midi_file_to_notes_and_durations works", {


  t <- itembankr::midi_file_to_notes_and_durations(testthat::test_path("test_data/true.mid"), string_df = FALSE)
  t2 <- itembankr::midi_file_to_notes_and_durations(testthat::test_path("test_data/true.mid"), string_df = TRUE)

  # expect...

})



