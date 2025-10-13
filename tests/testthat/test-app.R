test_that("app works if additional access unit is added to an stored board", {
  test_app1 <- list()
  test_app1$ui <- fluidPage(
    rAccess$public_methods$rAccessThemes(),
    module_iam_ui("iam")
  )
  # Define required server logic
  test_app1$server <- function(input, output, session) {
    user_id <- "UserID"
    session$user <- ifelse(is.null(session$user), user_id, session$user)
    access_panels <- list(
      `ADMIN` = NULL,
      `view-plots` = c("Study_A"),
      `view-summary` = c("Study_D"),
      `download_data` = c("Study_E", "Study_F", "Extra_Study")
    )
    user_df <- data.frame(
      userid = "UserID",
      username = "User Name"
    )
    new_iam <-
      rAccess$new(user = "UserID",
        board_type = "local",
        local_board_path = ".",
        app_name = "test1",
        access_panels = access_panels,
        access_mode = "default",
        switch_size = "small",
        unit_display = "dropdown",
        user_df = user_df,
        secure_mode = FALSE
      )
    module_iam_server("iam", new_iam)
  }
  shiny::testServer(as.shiny.appobj(test_app1), {
    session$setInputs(`iam-iam1-btnAddUsr` = "click")
  })
})
#######################################################################
test_that("user can add Everyone to an access unit.", {
  test_app2 <- list()
  test_app2$ui <- fluidPage(
    module_iam_ui("iam")
  )
  # Define required server logic
  test_app2$server <- function(input, output, session) {
    user_id <- "UserID"
    session$user <- ifelse(is.null(session$user), user_id, session$user)
    access_panels <- list(
      `ADMIN` = NULL,
      `Access Panel 1` = c("Unit 1", "Unit 2"),
      `Access Panel 2` = c("Unit 3", "Unit 4"),
      `Access Panel 3` = c("Unit 5", "Unit 6")
    )
    user_df <- data.frame(
      userid = c("UserID", "UserID2"),
      username = c("User Name", "User Name2")
    )
    new_iam <- rAccess$new(
      user = "UserID",
      board_type = "local",
      local_board_path = tempdir(),
      app_name = "test",
      access_panels = access_panels,
      access_mode = "default",
      switch_size = "small",
      unit_display = "dropdown",
      user_df = user_df
    )
    module_iam_server("iam", new_iam)
  }
  testServer(as.shiny.appobj(test_app2), {
    session$setInputs(`iam-iam1-btnDwnAccList` = "click")
    session$setInputs(`iam-iam1-btnaddUsrSubmit` = "click")
    session$setInputs(`iam-iam1-btnAddUsr` = "click")
    session$setInputs(`iam-iam1-txtUsername` = "")
    session$setInputs(`iam-iam1-txtUsername_reset` = "click")
    session$setInputs(`iam-iam1-txtUsername_search` = "click")
    session$setInputs(`iam-iam1-btnaddUsrSubmit` = "click")
    session$setInputs(`iam-iam1-txtUsername_text` = "")
    session$setInputs(`iam-iam3-txtUsername_text` = "Everyone")
    session$setInputs(`iam-iam1-txtUsername` = "Everyone")
    session$setInputs(`iam-iam1-txtUsername_search` = "click")
    session$setInputs(`iam-iam1-btnaddUsrSubmit` = "click")
    session$setInputs(`iam-iam1-btnAddUsr` = "click")
    session$setInputs(`iam-iam1-txtUsername` = "")
    session$setInputs(`iam-iam1-txtUsername_reset` = "click")
    session$setInputs(`iam-iam1-txtUsername_search` = "click")
    session$setInputs(`iam-iam1-btnaddUsrSubmit` = "click")
    session$setInputs(`iam-iam1-txtUsername_text` = "")
    session$setInputs(`iam-iam1-txtUsername_text` = "user")
    session$setInputs(`iam-iam1-txtUsername` = "user")
    session$setInputs(`iam-iam1-pduserList` = "UserID")
    session$setInputs(`iam-iam1-txtUsername_search` = "click")
    session$setInputs(`iam-iam1-btnaddUsrSubmit` = "click")
    session$setInputs(`iam-iam1-selUnits` = "ADMIN")
    session$setInputs(`iam-iam1-selUnits` = c("ADMIN", "Access Panel 1"))
    session$setInputs(`iam-iam1-selUnits` = c("ADMIN", "Access Panel 1",
                                              "Access Panel 2"))
    session$setInputs(`iam-iam1-selUnits` = c("ADMIN", "Access Panel 1",
                                              "Access Panel 2",
                                              "Access Panel 3"))
    session$setInputs(`iam-iam1-btnaddUsrSubmit` = "click")
    session$setInputs(`iam-btnRefreshAcess` = "click")
  })
})
#######################################################################
test_that("app works in single unit and switch display mode.", {
  test_app2 <- list()
  test_app2$ui <- fluidPage(
    module_iam_ui("iam")
  )
  # Define required server logic
  test_app2$server <- function(input, output, session) {
    user_id <- "UserID"
    session$user <- ifelse(is.null(session$user), user_id, session$user)
    access_panels <- list(
      `ADMIN` = NULL,
      `Access Panel 1` = c("Unit 1", "Unit 2"),
      `Access Panel 2` = c("Unit 3", "Unit 4"),
      `Access Panel 3` = c("Unit 5", "Unit 6")
    )
    user_df <- data.frame(
      userid = c("UserID", "UserID2"),
      username = c("User Name", "User Name2")
    )
    new_iam <- rAccess$new(
      user = "UserID",
      board_type = "local",
      local_board_path = tempdir(),
      app_name = "single_unit_test",
      access_panels = access_panels,
      access_mode = "single unit",
      switch_size = "small",
      unit_display = "switch",
      user_df = user_df
    )
    module_iam_server("iam", new_iam)
  }
  testServer(as.shiny.appobj(test_app2), {
    session$setInputs(`iam-iam1-btnAddUsr` = "click")
    session$setInputs(`iam-iam1-txtUsername` = "")
    session$setInputs(`iam-iam1-txtUsername_reset` = "click")
    session$setInputs(`iam-iam1-txtUsername_search` = "click")
    session$setInputs(`iam-iam1-btnaddUsrSubmit` = "click")
    session$setInputs(`iam-iam1-txtUsername_text` = "")
    session$setInputs(`iam-iam1-txtUsername_text` = "user")
    session$setInputs(`iam-iam1-txtUsername` = "user")
    session$setInputs(`iam-iam1-txtUsername_search` = "click")
    session$setInputs(`iam-iam1-pduserList` = "UserID")
    session$setInputs(`iam-iam1-btnaddUsrSubmit` = "click")
    session$setInputs(`iam-iam1-addbtnsrc1` = TRUE)
    session$setInputs(`iam-iam1-addbtnsrc2` = TRUE)
    session$setInputs(`iam-iam1-addbtnsrc3` = TRUE)
    session$setInputs(`iam-iam1-addbtnsrc4` = TRUE)
    session$setInputs(`iam-iam1-btnaddUsrSubmit` = "click")
    session$setInputs(`iam-iam1-accessDT_rows_selected` = 1,
                      allow_no_input_binding_ = TRUE)
    session$setInputs(`iam-iam1-accessDT_row_last_clicked` = 1,
                      allow_no_input_binding_ = TRUE, priority_ = "event")
    session$setInputs(`iam-iam1-accessDT_cell_clicked` = c("1", "2", "✓"),
                      allow_no_input_binding_ = TRUE, priority_ = "event")
    session$setInputs(`iam-iam1-btnEditUsr` = "click")
    session$setInputs(`iam-iam1-btndelUsr` = "click")
    # Update output value
    session$setInputs(`iam-iam1-editbtnsrc1` = FALSE)
    session$setInputs(`iam-iam1-editbtnsrc3` = FALSE)
    session$setInputs(`iam-iam1-editbtnsrc2` = FALSE)
    session$setInputs(`iam-iam1-editbtnsrc4` = FALSE)
    session$setInputs(`iam-iam1-btneditUsrSubmit` = "click")
    session$setInputs(`iam-btnRefreshAcess` = "click")
    session$setInputs(`iam-iam2-btnAddUsr` = "click",
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam2-txtUsername` = "",
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam2-txtUsername_reset` = "click",
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam2-txtUsername_search` = "click",
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam2-btnaddUsrSubmit` = "click",
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam2-txtUsername_text` = "",
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam2-txtUsername_text` = "user",
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam2-txtUsername` = "user",
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam2-txtUsername_search` = "click",
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam2-addbtnsrc1` = TRUE,
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam2-addbtnsrc2` = TRUE,
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam2-btnaddUsrSubmit` = "click",
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam2-addbtnsrc2` = FALSE,
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam2-btnaddUsrSubmit` = "click",
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam3-btnAddUsr` = "click",
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam3-txtUsername` = "", allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam3-txtUsername_reset` = "click",
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam3-txtUsername_search` = "click",
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam3-btnaddUsrSubmit` = "click",
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam3-txtUsername_text` = "",
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam3-txtUsername_text` = "user",
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam3-txtUsername` = "user",
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam3-txtUsername_search` = "click",
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam3-addbtnsrc2` = TRUE, allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam3-btnaddUsrSubmit` = "click",
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam2-accessDT_rows_selected` = 1,
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam2-accessDT_row_last_clicked` = 1,
                      allowInputNoBinding_ = TRUE, priority_ = "event")
    session$setInputs(`iam-iam2-accessDT_cell_clicked` = c("1", "1", "UserID"),
                      allowInputNoBinding_ = TRUE, priority_ = "event")
    session$setInputs(`iam-iam2-btnEditUsr` = "click",
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam2-btndelUsr` = "click",
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam2-editbtnsrc1` = FALSE,
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam2-editbtnsrc2` = TRUE,
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam2-btneditUsrSubmit` = "click",
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam1-accessDT_rows_selected` = 1,
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam1-accessDT_row_last_clicked` = 1,
                      allowInputNoBinding_ = TRUE, priority_ = "event")
    session$setInputs(`iam-iam1-accessDT_cell_clicked` = c("1", "1", "UserID"),
                      allowInputNoBinding_ = TRUE, priority_ = "event")
    session$setInputs(`iam-iam1-btnEditUsr` = "click",
                      allowInputNoBinding_ = TRUE,
                      priority_ = "event")
    session$setInputs(`iam-iam1-btndelUsr` = "click",
                      allowInputNoBinding_ = TRUE,
                      priority_ = "event")
    session$setInputs(`iam-iam1-editbtnsrc3` = FALSE,
                      allowInputNoBinding_ = TRUE,
                      priority_ = "event")
    session$setInputs(`iam-iam1-btneditUsrSubmit` = "click",
                      allowInputNoBinding_ = TRUE,
                      priority_ = "event")
  })
})
#######################################################################
test_that("app works in single unit and dropdown display mode.", {
  test_app2 <- list()
  test_app2$ui <- fluidPage(
    module_iam_ui("iam")
  )
  # Define required server logic
  test_app2$server <- function(input, output, session) {
    user_id <- "UserID"
    session$user <- ifelse(is.null(session$user), user_id, session$user)
    access_panels <- list(
      `ADMIN` = NULL,
      `Access Panel 1` = c("Unit 1", "Unit 2"),
      `Access Panel 2` = c("Unit 3", "Unit 4"),
      `Access Panel 3` = c("Unit 5", "Unit 6")
    )
    user_df <- data.frame(
      userid = c("UserID", "UserID2"),
      username = c("User Name", "User Name2")
    )
    new_iam <- rAccess$new(
      user = "UserID",
      board_type = "local",
      local_board_path = tempdir(),
      app_name = "single_unit_test2",
      access_panels = access_panels,
      access_mode = "single unit",
      switch_size = "small",
      unit_display = "dropdown",
      user_df = user_df
    )
    module_iam_server("iam", new_iam)
  }
  testServer(as.shiny.appobj(test_app2), {
    #Update output value
    session$setInputs(`iam-iam1-btnAddUsr` = "click")
    session$setInputs(`iam-iam1-btnEditUsr` = "click")
    session$setInputs(`iam-iam1-btnDwnAccList` = "click")
    session$setInputs(`iam-iam1-btnAddUsr` = "click")
    session$setInputs(`iam-iam1-txtUsername` = "")
    session$setInputs(`iam-iam1-txtUsername_reset` = "click")
    session$setInputs("iam-iam1-txtUsername_search" = "click")
    session$setInputs("iam-iam1-btnaddUsrSubmit" = "click")
    session$setInputs(`iam-iam1-txtUsername_text` = "")
    # Update output value
    session$setInputs(`iam-iam1-pduserList` = "")
    # Update output value
    session$setInputs(`iam-iam1-txtUsername_text` = "user")
    session$setInputs(`iam-iam1-txtUsername` = "user_wrong")
    session$setInputs("iam-iam1-txtUsername_search" = "click")
    session$setInputs(`iam-iam1-txtUsername` = "user")
    session$setInputs("iam-iam1-txtUsername_search" = "click")
    # Update output value
    session$setInputs(`iam-iam1-selUnits` = "ADMIN")
    session$setInputs(`iam-iam1-selUnits` = c("ADMIN", "Access Panel 1"))
    session$setInputs(`iam-iam1-selUnits` = c("ADMIN", "Access Panel 1",
                                              "Access Panel 2"))
    session$setInputs(`iam-iam1-selUnits` = c("ADMIN", "Access Panel 1",
                                              "Access Panel 2",
                                              "Access Panel 3"))
    session$setInputs("iam-iam1-btnaddUsrSubmit" = "click")
    # Update output value
    session$setInputs("iam-btnRefreshAcess" = "click")
    # Update output value
    session$setInputs("iam-iam2-btnAddUsr" = "click")
    session$setInputs(`iam-iam2-txtUsername` = "")
    session$setInputs("iam-iam2-txtUsername_reset" = "click")
    session$setInputs("iam-iam2-txtUsername_search" = "click")
    session$setInputs("iam-iam2-btnaddUsrSubmit" = "click")
    session$setInputs(`iam-iam2-txtUsername_text` = "")
    # Update output value
    session$setInputs(`iam-iam2-txtUsername_text` = "usere")
    session$setInputs(`iam-iam2-txtUsername_text` = "use")
    session$setInputs(`iam-iam2-txtUsername_text` = "user")
    session$setInputs(`iam-iam2-txtUsername` = "user")
    session$setInputs("iam-iam2-txtUsername_search" = "click")
    # Update output value
    session$setInputs(`iam-iam2-selUnits` = "Unit 1")
    session$setInputs(`iam-iam2-selUnits` = c("Unit 1", "Unit 2"))
    session$setInputs("iam-iam2-btnaddUsrSubmit" = "click")
    # Update output value
    session$setInputs(`iam-iam2-selUnits` = "Unit 1")
    session$setInputs("iam-iam2-btnaddUsrSubmit" = "click")
    # Update output value
    session$setInputs(`iam-iam2-accessDT_rows_selected` = 1,
                      allow_no_input_binding_ = TRUE)
    session$setInputs(`iam-iam2-accessDT_row_last_clicked` = 1,
                      allow_no_input_binding_ = TRUE, priority_ = "event")
    session$setInputs(`iam-iam2-accessDT_cell_clicked` =
                        c("1", "0", "User Name"),
                      allow_no_input_binding_ = TRUE, priority_ = "event")
    session$setInputs("iam-iam2-btnEditUsr" = "click")
    session$setInputs("iam-iam2-btndelUsr" = "click")
    # Update output value
    session$setInputs(`iam-iam2-edtUnits` = c("Unit 1", "Unit 2"))
    session$setInputs("iam-iam2-btneditUsrSubmit" = "click")
    session$setInputs(`iam-iam2-edtUnits` = "Unit 2")
    session$setInputs("iam-iam2-btneditUsrSubmit" = "click")
  })
})
#######################################################################
test_that(" rAccess methods work correctly after access boards edits", {
  user_id <- "UserID"
  access_panels <- list(
    `ADMIN` = NULL,
    `Access Panel 1` = c("Unit 1", "Unit 2"),
    `Access Panel 2` = c("Unit 3", "Unit 4"),
    `Access Panel 3` = c("Unit 5", "Unit 6")
  )
  user_df <- tibble::tribble(
    ~userid, ~username,
    "UserID", "User Name"
  )
  new_iam <- rAccess$new(user = "UserID",
    board_type = "local",
    local_board_path = ".",
    app_name = "test2",
    access_panels = access_panels,
    access_mode = "default",
    switch_size = "small",
    unit_display = "switch",
    user_df = user_df,
    secure_mode = TRUE
  )
  expect_named(
    new_iam$get_user_accesslist("UserID"),
    c("Access Panel 1", "Access Panel 2", "Access Panel 3", "ADMIN")
  )
  expect_error(new_iam$get_user_accesslist("Wrong_name"))
  expect_length(new_iam$get_userlist_unit("Access Panel 1", "Unit 1"), 1)
  expect_length(new_iam$get_superAdmins(), 1)
  expect_null(new_iam$check_access("wrong_name", "Access Panel 1"))
  expect_error(rAccess::s3_pinboard("wrong-bucket", "wrong-key",
                                    "wrong-secret-key", "wrong=prefix"))
  expect_error(rAccess::rconnect_pin_board("wrong-server", "wrong-key"))

})
#####################################################################
test_that("user access edits work on a saved access boards.", {
  test_app2 <- list()
  test_app2$ui <- fluidPage(
    module_iam_ui("iam")
  )
  # Define required server logic
  test_app2$server <- function(input, output, session) {
    user_id <- "UserID"
    session$user <- ifelse(is.null(session$user), user_id, session$user)
    access_panels <- list(
      `ADMIN` = NULL,
      `Access Panel 1` = c("Unit 1", "Unit 2"),
      `Access Panel 2` = c("Unit 3", "Unit 4"),
      `Access Panel 3` = c("Unit 5", "Unit 6", "Unit 7")
    )
    user_df <- data.frame(
      userid = "UserID",
      username = "User Name"
    )
    new_iam <- rAccess$new(
      user = "UserID",
      board_type = "local",
      local_board_path = ".",
      app_name = "test2",
      access_panels = access_panels,
      access_mode = "default",
      switch_size = "small",
      unit_display = "dropdown",
      user_df = user_df
    )
    if (new_iam$is_admin() | new_iam$no_admin())
      module_iam_server("iam", new_iam)
  }
  testServer(as.shiny.appobj(test_app2), {
    session$setInputs(`iam-iam2-accessDT_rows_selected` = 1,
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam2-accessDT_row_last_clicked` = 1,
                      allowInputNoBinding_ = TRUE, priority_ = "event")
    session$setInputs(`iam-iam2-accessDT_cell_clicked` =
                        c("1", "0", "User Name"),
                      allowInputNoBinding_ = TRUE,
                      priority_ = "event")
    session$setInputs(`iam-iam2-btnEditUsr` = "click",
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam2-btndelUsr` = "click",
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam2-btneditUsrSubmit` = "click",
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam2-edtUnits` = "Unit 1",
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam2-btneditUsrSubmit` = "click",
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam1-btnAddUsr` = "click",
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam1-txtUsername` = "", allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam1-txtUsername_reset` = "click",
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam1-txtUsername_search` = "click",
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam1-btnaddUsrSubmit` = "click",
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam1-txtUsername_text` = "wrong-user",
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam1-txtUsername` = "wrong-user",
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam1-txtUsername_search` = "click",
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam1-txtUsername_text` = "",
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam1-txtUsername_text` = "user",
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam1-pduserList` = "UserID")
    session$setInputs(`iam-iam1-txtUsername` = "user",
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam1-txtUsername_search` = "click",
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam1-selUnits` = "ADMIN",
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam1-selUnits` = c("ADMIN", "Access Panel 1"),
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam1-selUnits` = c("ADMIN",
                                              "Access Panel 1",
                                              "Access Panel 2"),
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam1-selUnits` = c("ADMIN",
                                              "Access Panel 1",
                                              "Access Panel 2",
                                              "Access Panel 3"),
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam1-btnaddUsrSubmit` = "click",
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-btnRefreshAcess` = "click",
                      allowInputNoBinding_ = TRUE)
  })
  testServer(test_app2, {
    session$setInputs(`iam-iam1-btnDwnAccList` = "click")
    session$setInputs(`iam-iam1-btnEditUsr` = "click",
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam1-accessDT_rows_selected` = 1,
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam1-accessDT_row_last_clicked` = 1,
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam1-accessDT_cell_clicked` = c("1",
                                                           "0",
                                                           "User Name"),
                      allowInputNoBinding_ = TRUE) #, priority_ = "event")
    session$setInputs(`iam-iam1-btnEditUsr` = "click",
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam1-btndelUsr` = "click",
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam1-edtUnits` = "ADMIN",
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam1-edtUnits` = character(0),
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam1-btneditUsrSubmit` = "click",
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam1-btnDelOK` = "click",
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam1-btnDelClose` = "click",
                      allowInputNoBinding_ = TRUE)
    session$setInputs(`iam-iam1-btnDelOK` = "click",
                      allowInputNoBinding_ = TRUE)
  })
})
#####################################################################
test_that("app works with no pre-existing access board", {
  test_app1 <- list()
  test_app1$ui <- fluidPage(
    rAccess$public_methods$rAccessThemes(),
    module_iam_ui("iam")
  )
  # Define required server logic
  test_app1$server <- function(input, output, session) {
    user_id <- "UserID"
    session$user <- ifelse(is.null(session$user), user_id, session$user)
    access_panels <- list(
      `ADMIN` = NULL,
      `view-plots` = c("Study_A"),
      `view-summary` = c("Study_D"),
      `download_data` = c("Study_E", "Study_F", "Extra_Study")
    )
    user_df <- data.frame(
      userid = "UserID",
      username = "User Name"
    )
    new_iam <- rAccess$new(user = "UserID",
      board_type = "local",
      local_board_path = tempdir(),
      app_name = "new_pin",
      access_panels = access_panels,
      access_mode = "default",
      switch_size = "small",
      unit_display = "dropdown",
      user_df = user_df,
      secure_mode = FALSE
    )
    if (new_iam$no_admin() | new_iam$is_admin())
      module_iam_server("iam", new_iam)
  }
  shiny::testServer(as.shiny.appobj(test_app1), {
    session$setInputs(`iam-iam1-btnAddUsr` = "click")
  })
})
