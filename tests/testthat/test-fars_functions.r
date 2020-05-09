test_that("FARS filenames are correctly created from a specified year.", {
  expect_equal(make_filename(2013), system.file("extdata","accident_2013.csv.bz2",package="fars"))
})
test_that("An error is thrown if an invalid year is specified", {
  expect_error(make_filename(2010), "'arg' should be one of \"2013\", \"2014\", \"2015\"")
})
