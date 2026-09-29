# Use an in-memory keyring so tests never touch the system keyring
local_plug_keyring <- function(env = parent.frame()) {
  withr::local_options(keyring_backend = "env", .local_envir = env)
  withr::defer(
    suppressMessages(plug_clear_credentials()),
    envir = env
  )
  suppressMessages(plug_clear_credentials())
}

json_response <- function(body, status = 200L) {
  httr2::response(
    status_code = status,
    headers = list(`Content-Type` = "application/json"),
    body = charToRaw(body)
  )
}

text_response <- function(body, status = 200L) {
  httr2::response(
    status_code = status,
    headers = list(`Content-Type` = "text/plain; charset=utf-8"),
    body = charToRaw(body)
  )
}

request_body <- function(req) {
  req$body$data
}
