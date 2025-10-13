library(pins)

pin_board <- pins::board_folder(path = ".", versioned = TRUE)

test_that("check if versioned", {
  pin_name <- "demo"
  act <- get_board(pin_board, pin_name)
  expect_true(
    length(act$access_list) > 0
  )
})

test_that("Pin name as null", {
  pin_name <- NULL
  expect_error(
    act <- get_board(pin_board, pin_name)
  )
})
