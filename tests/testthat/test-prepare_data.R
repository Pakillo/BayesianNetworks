test_that("good matrices don't throw any errors", {
  expect_no_error(prepare_data(mat_int))
  expect_no_error(prepare_data(mat_num))
})

test_that("matrix warnings are caught", {
  expect_warning(prepare_data(mat_bin))
})

test_that("matrix errors are caught", {
  expect_error(prepare_data(""))
  expect_error(prepare_data(mat_dec))
  expect_error(prepare_data(mat_neg))
})

test_that("plant_effort problems are caught", {
  expect_warning(prepare_data(mat_int, plant_effort = 98:101))
  expect_error(prepare_data(mat_int, plant_effort = 1))
  expect_error(prepare_data(mat_int, plant_effort = rep(-1, 4)))
  expect_error(prepare_data(mat_int, plant_effort = rep("text", 4)))
})

test_that("plant_abun problems are caught", {
  expect_error(prepare_data(mat_int, plant_abun = 1))
  expect_error(prepare_data(mat_int, plant_abun = rep(-1, 4)))
  expect_error(prepare_data(mat_int, plant_abun = rep("text", 4)))
})

test_that("animal_abun problems are caught", {
  expect_error(prepare_data(mat_int, animal_abun = 1))
  expect_error(prepare_data(mat_int, animal_abun = rep(-1, 4)))
  expect_error(prepare_data(mat_int, animal_abun = rep("text", 4)))
})
