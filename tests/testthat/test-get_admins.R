library(pins)

pin_board <- pins::board_folder(path = ".", versioned = TRUE)

test_that("Pin name not NULL", {
  pin_name <- NULL
  expect_null(
    get_admins(pin_board, pin_name, "ADMIN")
  )
})

test_that("Wrong Admin Panel Name", {
  pin_name <- "demo"
  expect_true(
    length(get_admins(pin_board, pin_name, "WRONG")) == 0
  )
})

test_that("Wrong Pin name", {
  pin_name <- "demo_wrong"
  expect_null(
    get_admins(pin_board, pin_name, "ADMIN")
  )
})

test_that("Correct Pin name", {
  pin_name <- "demo"
  expect_true(
    length(get_admins(pin_board, pin_name, "ADMIN")) > 0
  )
})
