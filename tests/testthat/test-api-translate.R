context("api translate")

test_that("translate", {
  res <- giphy_api_translate("hell no")
  expect_is(res, "giphy")
})
