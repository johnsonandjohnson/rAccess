test_that("get_user_api works with non specific parameters", {
  expect_null(get_user_api(NULL, NULL, NULL))
  expect_null(get_user_api("", NULL, NULL))
  everyone_df <- tibble::tibble(userid = c("Everyone"),
                                username = c("Everyone"))
  act_everyone <- get_user_api("everyone", "shinyapps.io", "wrong")
  expect_identical(everyone_df, act_everyone)
})

## Uncomment when testing with valid user_id and valid Posit Connect details
# test_that("get_user_api works with specific parameters", {
#   user_id <-  # a valid user id
#   connect_server <-   # a valid posit connect server
#   connect_api_key <-  # a valid Posit Connect API key
#   expect_vector(get_user_api(user_id, connect_server, connect_api_key))
#   expect_null(get_user_api("no_user", connect_server, connect_api_key))
# })
