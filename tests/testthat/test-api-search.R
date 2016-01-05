context("api search")

test_that("search", {
  ret <- giphy_api_search("cats")
  expect_equal(length(ret), 25L)
  expect_more_than(attr(ret, "total"), 3000)

  expect_is(ret[[1]], "giphy")
})

test_that("pagination", {
  ## More than one call:
  n <- 0L
  suppressMessages(trace(httr::GET, function(x) n <<- n + 1L, print=FALSE))
  on.exit(untrace(httr::GET))
  ret <- giphy_api_search("cats", n=120)
  expect_equal(n, 2L)
  expect_equal(length(ret), 120)
})
