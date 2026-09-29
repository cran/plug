auth <- "https://example.com/auth"
api <- "https://example.com/query"

local_plug_api <- function(handler, env = parent.frame()) {
  local_plug_keyring(env)
  suppressMessages(plug_store_credentials("user", "secret"))
  httr2::local_mocked_responses(function(req) {
    if (identical(req$url, auth)) {
      return(text_response("token"))
    }
    handler(req)
  }, env = env)
}

test_that("queries are sent with the token and parsed as a tibble", {
  local_plug_api(function(req) {
    expect_identical(req$url, api)
    expect_identical(request_body(req), list(sqlQuery = "SELECT TOP 1 * FROM t"))
    json_response('[{"id": 1, "name": "a"}, {"id": 2, "name": "b"}]')
  })

  res <- plug_execute_query("SELECT TOP 1 * FROM t", endpoint = api, auth_endpoint = auth)
  expect_s3_class(res, "tbl_df")
  expect_identical(res$id, 1:2)
  expect_identical(res$name, c("a", "b"))
})

test_that("placeholders are replaced in the query", {
  local_plug_api(function(req) {
    expect_identical(
      request_body(req)$sqlQuery,
      "SELECT * FROM [t] WHERE a = 'O''Brien' AND b IN (1, 2)"
    )
    json_response("[]")
  })

  b <- 1:2
  res <- plug_execute_query(
    "SELECT * FROM {`tbl`} WHERE a = {a} AND b IN ({b*})",
    endpoint = api,
    tbl = "t",
    a = "O'Brien",
    auth_endpoint = auth
  )
  expect_s3_class(res, "tbl_df")
  expect_identical(nrow(res), 0L)
})

test_that("plug_download_base() selects everything from the base", {
  local_plug_api(function(req) {
    expect_identical(request_body(req)$sqlQuery, "SELECT * FROM dbo.Contratos_VIEW")
    json_response('[{"id": 1}]')
  })

  res <- plug_download_base("dbo.Contratos_VIEW", endpoint = api, auth_endpoint = auth)
  expect_identical(res$id, 1L)
})

test_that("a rejected token is renewed once", {
  tokens <- character()
  local_plug_api(function(req) {
    tokens <<- c(tokens, req$headers$Authorization)
    if (length(tokens) == 1) {
      text_response("expired", status = 401L)
    } else {
      json_response('[{"id": 1}]')
    }
  })
  plug_cache_token("stale", as.numeric(Sys.time()) + 3600, auth)

  res <- plug_execute_query("SELECT 1", endpoint = api, auth_endpoint = auth)
  expect_identical(res$id, 1L)
  expect_length(tokens, 2)
  expect_identical(plug_list_tokens()$token, "token")
})

test_that("API errors are reported", {
  local_plug_api(function(req) text_response("Invalid object name 't'.", status = 400L))
  expect_error(
    plug_execute_query("SELECT * FROM t", endpoint = api, auth_endpoint = auth),
    "HTTP 400.*Invalid object name"
  )

  local_plug_api(function(req) text_response("not json"))
  expect_error(
    plug_execute_query("SELECT * FROM t", endpoint = api, auth_endpoint = auth),
    "Unexpected content type: text/plain"
  )
})

test_that("queries fail without a token", {
  local_plug_keyring()
  httr2::local_mocked_responses(function(req) stop("no request expected"))

  expect_error(
    suppressMessages(plug_execute_query("SELECT 1", endpoint = api, auth_endpoint = auth)),
    "Failed to retrieve a valid token"
  )
})

test_that("arguments are validated", {
  expect_error(plug_execute_query(1), "SQL template")
  expect_error(plug_execute_query(""), "SQL template")
  expect_error(plug_execute_query(c("SELECT 1", "SELECT 2")), "SQL template")
  expect_error(plug_download_base(""), "Base name")
  expect_error(plug_download_base("t; DROP TABLE t"), "valid table or view name")
})
