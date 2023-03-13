
test_that("rel_bpm_to_seconds works", {
  expect_equal(rel_bpm_to_seconds(c(2, 2, 4, 2, 2), bpm = 120), c(0.250, 0.250, 0.125, 0.250, 0.250))
})


test_that("microseconds_per_beat_to_bpm works", {
  expect_equal(microseconds_per_beat_to_bpm(500000), 120)
})


test_that("sci_notation_to_midi works", {
  expect_equal(sci_notation_to_midi(c("C#4", "Eb4", "G#4", "Bb4", "A#4", "Db4", "E#4", "Gb2")),
                                    c(61, 63, 68, 70, 70, 61, 65, 42))
})

