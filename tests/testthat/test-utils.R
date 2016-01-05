context("utils")

test_that("download_file", {
  url <- "https://raw.githubusercontent.com/richfitz/storr/master/DESCRIPTION"
  expect_warning(x <- download_file(url), NA)
  expect_true(file.exists(x))
  expect_equal(unname(read.dcf(x)[, "Package"]), "storr")
})
