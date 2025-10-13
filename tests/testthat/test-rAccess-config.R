test_that("reading from a sample config file!!", {
  temp_dir <- tempdir()
  expect_message(use_config(file_name = "wrong_rAccess.yml", temp_dir),
                 "Config file NOT AVAILABLE")
  expect_true(use_config(file_name = "sample_rAccess.yml", temp_dir))
  expect_no_error(
    rAccess$new(
      user = "UserID",
      config = file.path(temp_dir, "sample_rAccess.yml")
    )
  )
})
