test_that("credentials are stored, listed and removed", {
  local_plug_keyring()

  expect_message(creds <- plug_list_credentials(), "No credentials found")
  expect_identical(creds, list())

  expect_message(plug_store_credentials("user", "secret"), "successfully stored")
  expect_identical(
    plug_list_credentials(),
    list(username = "user", password = "secret")
  )

  expect_message(removed <- plug_clear_credentials(), "removed")
  expect_true(removed)
  expect_message(plug_list_credentials(), "No credentials found")
  expect_message(removed <- plug_clear_credentials(), "Nothing to remove")
  expect_false(removed)
})

test_that("invalid credentials are rejected", {
  expect_error(plug_store_credentials(1, "secret"), "Username")
  expect_error(plug_store_credentials("", "secret"), "Username")
  expect_error(plug_store_credentials(c("a", "b"), "secret"), "Username")
  expect_error(plug_store_credentials("user", NULL), "Password")
  expect_error(plug_store_credentials("user", NA_character_), "Password")
})

test_that("storing credentials discards the cached token", {
  local_plug_keyring()
  suppressMessages(plug_store_credentials("user", "secret"))
  plug_cache_token("old-token", as.numeric(Sys.time()) + 3600, "https://example.com/auth")
  expect_named(plug_list_tokens(), c("token", "expiration"))

  suppressMessages(plug_store_credentials("other", "secret"))
  expect_message(tokens <- plug_list_tokens(), "No token found")
  expect_identical(tokens, list())
})

test_that("only the token is removed when credentials = FALSE", {
  local_plug_keyring()
  suppressMessages(plug_store_credentials("user", "secret"))
  plug_cache_token("token", as.numeric(Sys.time()) + 3600, "https://example.com/auth")

  suppressMessages(plug_clear_credentials(credentials = FALSE))
  expect_message(plug_list_tokens(), "No token found")
  expect_identical(plug_list_credentials()$username, "user")
})
