context("jiffy")

test_that("jiffy_random", {
  res <- jiffy_random("cat", what="fixed_width_small", quiet=TRUE)
  expect_is("res", "character")
  expect_true(file.exists(res))
})

test_that("jiffy_translate", {
  res <- jiffy_translate("hell no", what="fixed_width_small", quiet=TRUE)
  expect_is("res", "character")
  expect_true(file.exists(res))
})
