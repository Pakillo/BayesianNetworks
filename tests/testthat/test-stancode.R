test_that("all argument combinations make valid Stan code", {
  expect_true(all(synt == "Stan program is syntactically correct"))
})
