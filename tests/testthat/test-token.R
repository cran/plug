auth <- "https://example.com/auth"

test_that("token is NULL without credentials", {
  local_plug_keyring()
  httr2::local_mocked_responses(function(req) stop("no request expected"))

  expect_message(token <- plug_get_valid_token(endpoint = auth), "No valid credentials")
  expect_null(token)
})

test_that("token is requested, cached and reused", {
  local_plug_keyring()
  suppressMessages(plug_store_credentials("user", "secret"))

  calls <- 0
  httr2::local_mocked_responses(function(req) {
    calls <<- calls + 1
    expect_identical(request_body(req), list(UserName = "user", Password = "secret"))
    text_response(paste0("token-", calls))
  })

  expect_identical(plug_get_valid_token(endpoint = auth), "token-1")
  expect_identical(plug_get_valid_token(endpoint = auth), "token-1")
  expect_identical(calls, 1)

  tokens <- plug_list_tokens()
  expect_identical(tokens$token, "token-1")
  expect_s3_class(tokens$expiration, "POSIXct")
  expect_gt(as.numeric(tokens$expiration), as.numeric(Sys.time()) + 3500)

  expect_identical(plug_get_valid_token(endpoint = auth, force = TRUE), "token-2")
  expect_identical(plug_get_valid_token(endpoint = "https://example.com/other"), "token-3")
})

test_that("expired tokens are renewed", {
  local_plug_keyring()
  suppressMessages(plug_store_credentials("user", "secret"))
  plug_cache_token("expired", as.numeric(Sys.time()) - 10, auth)
  httr2::local_mocked_responses(function(req) json_response('{"token": "fresh"}'))

  expect_identical(plug_get_valid_token(endpoint = auth), "fresh")
})

test_that("tokens cached by previous versions are reused", {
  local_plug_keyring()
  plug_key_set(plug_services$token, "legacy")
  plug_key_set(plug_services$expiration, as.character(as.numeric(Sys.time()) + 3600))
  httr2::local_mocked_responses(function(req) stop("no request expected"))

  expect_identical(plug_get_valid_token(endpoint = auth), "legacy")
})

test_that("authentication failures are reported without error", {
  local_plug_keyring()
  suppressMessages(plug_store_credentials("user", "wrong"))

  httr2::local_mocked_responses(function(req) text_response("Invalid user", status = 401L))
  expect_message(token <- plug_get_valid_token(endpoint = auth), "HTTP 401.*Invalid user")
  expect_null(token)

  httr2::local_mocked_responses(function(req) stop("could not resolve host"))
  expect_message(token <- plug_get_valid_token(endpoint = auth), "could not resolve host")
  expect_null(token)

  httr2::local_mocked_responses(function(req) json_response("{}"))
  expect_message(token <- plug_get_valid_token(endpoint = auth), "did not contain a token")
  expect_null(token)

  expect_message(plug_list_tokens(), "No token found")
})

test_that("arguments are validated", {
  expect_error(plug_get_valid_token(validity_time = -1), "Validity time")
  expect_error(plug_get_valid_token(validity_time = "1"), "Validity time")
  expect_error(plug_get_valid_token(endpoint = NULL), "Endpoint")
})
