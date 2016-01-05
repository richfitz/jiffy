context("api trending")

test_that("trending", {
  ret <- giphy_api_trending("cats", n=10)
  expect_equal(length(ret), 10L)
  expect_is(ret[[1]], "giphy")
})

test_that("pagination", {
  ## More than one call:
  n <- 0L
  suppressMessages(trace(httr::GET, function(x) n <<- n + 1L, print=FALSE))
  on.exit(untrace(httr::GET))
  ret <- giphy_api_trending("cats", n=120)
  expect_true(n >= 2L)
  expect_equal(length(ret), 120)
})
