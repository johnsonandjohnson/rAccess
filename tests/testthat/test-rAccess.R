# pin board
pin_board <- pins::board_folder(
  path = "./data/",
  versioned = TRUE
)

# user list
user_df <- tibble::tribble(
  ~userid, ~username,
  "UserId1", "User Name 1",
  "UserId2", "User Name 2",
  "UserId3", "User Name 3",
  "UserId4", "User Name 4",
  "UserId5", "User Name 5"
)

test_that("board type asignment works!!", {
  expect_error(
    rAccess$new(
      user = "UserId1",
      board_type = "no type",
      pin_name = "demo",
      access_panels = c("Panel 1", "Panel 2", "Panel 3"),
      access_units = c("Unit 1", "Unit 2", "Unit 3"),
      access_mode = "default",
      user_df = user_df
    )
  )

  expect_no_error(
    rAccess$new(
      user = "UserId1",
      board_type = "local",
      app_name = "demo",
      access_panels = c("Panel 1", "Panel 2", "Panel 3"),
      access_units = c("Unit 1", "Unit 2", "Unit 3"),
      access_mode = "default",
      user_df = user_df
    )
  )

  expect_no_error(
    rAccess$new(
      user = "UserId1",
      board_type = "local",
      local_board_path = ".",
      app_name = "demo",
      access_panels = c("Panel 1", "Panel 2", "Panel 3"),
      access_units = c("Unit 1", "Unit 2", "Unit 3"),
      access_mode = NULL,
      user_df = user_df
    )
  )

  # rconnect should be used in interactive mode
  my_key <- Sys.getenv("CONNECT_API_KEY")
  my_server <- Sys.getenv("CONNECT_SERVER")
  Sys.setenv(CONNECT_API_KEY = "wrong_key")
  Sys.setenv(CONNECT_server = "wrong_server")
  expect_error(
    rAccess$new(
      user = "UserId1",
      board_type = "rconnect",
      app_name = "demo",
      access_panels = c("Panel 1", "Panel 2", "Panel 3"),
      access_units = c("Unit 1", "Unit 2", "Unit 3"),
      user_df = user_df
    )
  )
  Sys.setenv(CONNECT_API_KEY = my_key)
  Sys.setenv(CONNECT_server = my_server)
  expect_error(
    rAccess$new(
      user = "UserId1",
      board_type = "s3",
      app_name = "demo",
      access_panels = c("Panel 1", "Panel 2", "Panel 3"),
      access_units = c("Unit 1", "Unit 2", "Unit 3"),
      access_mode = "default",
      user_df = user_df
    )
  )

  expect_error(
    rAccess$new(
      user = "UserId1",
      board_type = "s3",
      s3_bucket = "",
      s3_access_key = "",
      s3_secret_key = "",
      pin_name = "demo",
      access_panels = c("Panel 1", "Panel 2", "Panel 3"),
      access_units = c("Unit 1", "Unit 2", "Unit 3"),
      access_mode = "default",
      user_df = user_df
    )
  )

  # # Please comment out when you want to test with valid s3 credentials
  # expect_no_error(
  #   rAccess$new(
  #     user = "UserId1",
  #     board_type = "s3",
  #     s3_bucket = ,
  #     s3_access_key = ,
  #     s3_secret_key = ,
  #     pin_name = "demoApp",
  #     access_panels = c("Panel 1", "Panel 2", "Panel 3"),
  #     access_units = c("Unit 1", "Unit 2", "Unit 3"),
  #     access_mode = "default",
  #     user_df = user_df
  #   )
  # )
})
test_that("Access panel assignment options", {
  panel_type1 <- c("panel1", "panel2", "panel3")
  unit_type1 <- c("unit1", "unit2", "unit3")
  panel_type2 <- list(
    `ADMIN` = NULL,
    `panel1` = c("unit1", "unit2"),
    `panel2` = c("unit3", "unit4")
  )
  expect_no_error(
    rAccess$new(
      user = "UserId1",
      board_type = "local",
      #local_board_path = "./data/",
      app_name = "test",
      access_panels = panel_type1,
      access_units = unit_type1,
      user_df = user_df
    )
  )
  expect_no_error(
    rAccess$new(
      user = "UserId1",
      board_type = "local",
      #local_board_path = "./data/",
      pin_name = "test",
      access_panels = panel_type2,
      user_df = user_df
    )
  )
})
test_that("user list assignment", {
  user_df1 <- tibble::tribble(
    ~userid, ~username,
    "UserId1", "User Name 1",
    "UserId2", "User Name 2",
    "UserId3", "User Name 3"
  )
  user_df2 <- tibble::tribble(
    ~userid, ~username,
    "UserId1", "User Name 1",
    "UserId2", "User Name 2",
    "UserId3", "User Name 3"
  )
  new_iam1 <- rAccess$new(
    user = "UserId1",
    board_type = "local",
    #local_board_path = "./data/",
    pin_name = "test",
    access_panels = c("Panel 1", "Panel 2", "Panel 3"),
    access_units = c("Unit 1", "Unit 2", "Unit 3"),
    access_mode = "default",
    user_df = user_df1
  )
  new_iam2 <- rAccess$new(
    user = "UserId1",
    board_type = "local",
    #local_board_path = "./data/",
    pin_name = "test",
    access_panels = c("Panel 1", "Panel 2", "Panel 3"),
    access_units = c("Unit 1", "Unit 2", "Unit 3"),
    access_mode = "default",
    user_df = user_df2
  )
  new_iam3 <- rAccess$new(
    user = "UserId1",
    board_type = "local",
    #local_board_path = "./data/",
    pin_name = "test",
    access_panels = c("Panel 1", "Panel 2", "Panel 3"),
    access_units = c("Unit 1", "Unit 2", "Unit 3"),
    access_mode = "default",
    user_df = user_df2
  )
  expect_identical(
    new_iam1$user_df, user_df1
  )
  expect_identical(
    new_iam2$user_df, user_df2
  )
})
test_that("testing rAccess methods..", {
  user_df <- tibble::tribble(
    ~userid, ~username,
    "NHarida1", "Nandukrishnan Haridas",
    "PEshghi ", "Peyman Eshghi",
    "NAbraha5", "Nadia Abraham"
  )
  access_panels <- list(
    `ADMIN` = NULL,
    `sum` = c("plot", "summary"),
    `data` = c("view")
  )
  new_iam <- rAccess$new(
    user = "NHarida1",
    app_name = "demo",
    board_type = "local",
    local_board_path = ".",
    access_panels = access_panels,
    user_df = user_df
  )
  expect_true(
    nrow(new_iam$check_access("nharida1", "ADMIN")) > 0
  )
  expect_true(
    new_iam$is_admin()
  )
  expect_false(
    new_iam$no_admin()
  )
})
