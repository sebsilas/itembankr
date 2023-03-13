
test_that("subset_item_bank works", {

  test1 <- subset_item_bank(item_bank = WJD::WJD("phrase"), item_length = 5:15, span_min = 12, tonality = "minor")

  test2 <- subset_item_bank(item_bank = WJD::WJD("phrase"), item_length = 5:15, span_min = 12, tonality = "minor")

  test3 <- subset_item_bank(item_bank = WJD::WJD("phrase"), item_length = c(3, NA))

  test4 <- subset_item_bank(item_bank = WJD::WJD("phrase"), item_length = 3)


  expect_equal(dim(test1), c(2635, 24) )
  expect_equal(dim(test2), c(2635, 24) )
  expect_equal(dim(test3), c(9389, 24) )
  expect_equal(dim(test4), c(394, 24) )



})



