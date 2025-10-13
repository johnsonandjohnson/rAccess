library(pins)

pin_board <- pins::board_folder(
  path = ".",
  versioned = TRUE
)

test_that("Test parameters", {
  expect_null(get_accesshistory(pin_board, NULL))
  expect_null(get_accesshistory(pin_board, "wrong_pin"))
  expect_no_error(get_accesshistory(pin_board, "demo"))
})
