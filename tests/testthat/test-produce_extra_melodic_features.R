test_that("produce_extra_melodic_features works", {

  test <- tibble::tibble(
    note = c(60, 63, 52),
    freq = c(440, 550, 660),
    dur = c(1, 1, 2),
    onset = c(0, 1, 3)
  ) %>% produce_extra_melodic_features()

  should_be <- tibble::tibble(note = c(60, 63, 52),
                              freq = c(440, 550, 660),
                              dur = c(1, 1, 2),
                              onset = c(0, 1, 3),
                              sci_notation = c("C4", "Eb4", "E3"),
                              interval = c(NA, 3, -11),
                              ioi = c(0, 1, 2),
                              ioi_class = c(-2, 1, 2),
                              note_pos = 1:3,
                              phrasend = c(0, 1, 1),
                              phrasbeg = c(1, 0, 1),
                              cents_deviation_from_nearest_midi_pitch = c(0, 0, 0),
                              pitch_class = c("A", "Db", "E"),
                              pitch_class_numeric = c("A","Db", "E"),
                              interval_cents = c(NA, 386.313713864835, 315.641287000553)
  )



  expect_true(all.equal(test, should_be))
})
