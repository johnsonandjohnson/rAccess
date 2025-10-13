pin_board <- pins::board_folder(path = ".", versioned = TRUE)

test_that("Check parameters", {
  expect_null(get_granted_units("nharida1", pin_board, NULL))
  expect_vector(get_granted_units("nharida1", pin_board, "demo"))
  expect_null(get_granted_units("wrong_name", pin_board, "demo"))
})
