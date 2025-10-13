library(pins)

pin_board <- pins::board_folder(
  path = ".",
  versioned = TRUE
)

test_that("Check for dates", {
  date1 <- "06-29-2023"
  date2 <- "2023-06-29"
  date3 <- "2023/06/29"
  date4 <- "2023-07-29"
  date_time1 <- "2023-07-18 01:21:14 EDT"

  expect_error(get_accesslist(pin_board, "demo", date1))
  expect_no_error(get_accesslist(pin_board, "demo", date2))
  expect_error(get_accesslist(pin_board, "demo", date3))
  expect_no_error(get_accesslist(pin_board, "demo", date_time1))
  expect_no_error(get_accesslist(pin_board, "demo", date2, date4))
  expect_null(get_accesslist(pin_board, "demo", date4, date2))
})
