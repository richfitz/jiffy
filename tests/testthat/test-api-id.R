context("api id")

test_that("id", {
  res <- giphy_api_id("JkCECae8F2sve")
  expect_equal(length(res), 1L)
  expect_is(res[[1]], "giphy")
  expect_equal(res[[1]]$size, 322483)
})
