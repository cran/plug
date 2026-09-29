#' Store user credentials securely for Plug API
#'
#' This function securely stores the global username and password required to authenticate with the Plug application.
#' Any cached token is discarded, so the next request authenticates with the new credentials.
#'
#' @param username The username for the Plug.
#' @param password The password for the Plug.
#'
#' @return No return value. The credentials are securely stored.
#' @examples
#' # Use a temporary, in-memory keyring so the example does not touch
#' # the credentials stored in the system keyring
#' old <- options(keyring_backend = "env")
#'
#' plug_store_credentials("myusername", "mypassword")
#' plug_clear_credentials()
#'
#' options(old)
#' @export
plug_store_credentials <- function(username, password) {
  if (!is_string(username)) {
    stop("Username must be a valid string.")
  }
  if (!is_string(password)) {
    stop("Password must be a valid string.")
  }

  tryCatch(
    {
      plug_key_set(plug_services$username, username)
      plug_key_set(plug_services$password, password)
      # A token issued for the previous credentials must not be reused
      plug_clear_token()
      message("Global credentials successfully stored.")
    },
    error = function(e) {
      message(
        "Keyring not accessible. Credentials could not be securely stored. Error details: ",
        conditionMessage(e)
      )
    }
  )

  invisible(NULL)
}

#' Remove stored credentials and tokens for Plug API
#'
#' This function removes the username, password and cached token stored by this package from the keyring.
#'
#' @param credentials If `TRUE` (the default), removes the stored username and password.
#' If `FALSE`, only the cached token is removed.
#'
#' @return `TRUE` (invisibly) if something was removed, `FALSE` otherwise.
#' @examples
#' old <- options(keyring_backend = "env")
#'
#' plug_store_credentials("myusername", "mypassword")
#' plug_clear_credentials()
#'
#' options(old)
#' @export
plug_clear_credentials <- function(credentials = TRUE) {
  removed <- plug_clear_token()

  if (isTRUE(credentials)) {
    removed_username <- plug_key_delete(plug_services$username)
    removed_password <- plug_key_delete(plug_services$password)
    removed <- removed || removed_username || removed_password
  }

  if (removed) {
    message("Stored Plug API data removed.")
  } else {
    message("Nothing to remove.")
  }

  invisible(removed)
}

#' Get a valid token for Plug API
#'
#' This function checks if a valid global token exists for the Plug API. If no valid token is found,
#' it generates a new token using the stored global credentials by sending a properly formatted request.
#' If it fails to retrieve the token (for example, due to missing credentials or network issues),
#' it will not throw an error but will display a message explaining the problem and return `NULL`.
#'
#' @param validity_time The validity period of the token in seconds. Default is 3600 (1 hour).
#' @param endpoint The endpoint URL for generating the token.
#' @param force If `TRUE`, ignores the cached token and always requests a new one.
#'
#' @return The valid token as a string, or `NULL` if no valid credentials were found or an error occurred.
#' @examples
#' \dontrun{
#' token <- plug_get_valid_token(validity_time = 3600)
#' }
#' @export
plug_get_valid_token <- function(validity_time = 3600,
                                 endpoint = "https://plug.der.pe.gov.br/MadrixApi/authenticate/",
                                 force = FALSE) {
  if (!is.numeric(validity_time) || length(validity_time) != 1 ||
    is.na(validity_time) || validity_time <= 0) {
    stop("Validity time must be a positive number of seconds.")
  }
  if (!is_string(endpoint)) {
    stop("Endpoint must be a valid string.")
  }

  tryCatch(
    {
      # 1) Reuse the cached token if it is still valid for this endpoint
      if (!isTRUE(force)) {
        token <- plug_cached_token(endpoint)
        if (!is.null(token)) {
          return(token)
        }
      }

      # 2) Retrieve the stored credentials
      username <- plug_key_get(plug_services$username)
      password <- plug_key_get(plug_services$password)
      if (is.null(username) || is.null(password)) {
        stop("No valid credentials found. Use `plug_store_credentials()` to store them.")
      }

      # 3) Make the request to generate a new token
      response <- tryCatch(
        plug_request(endpoint) |>
          httr2::req_body_json(list(UserName = username, Password = password)) |>
          httr2::req_perform(),
        error = function(e) {
          stop(
            "Failed to generate a new token. Please check your network connection. Error details: ",
            conditionMessage(e)
          )
        }
      )
      if (httr2::resp_status(response) >= 400) {
        stop(
          "Failed to generate a new token. Please check your credentials. Error details: ",
          plug_http_error(response)
        )
      }

      # 4) Check the content type and parse accordingly
      content_type <- plug_content_type(response)
      if (content_type == "application/json") {
        new_token <- httr2::resp_body_json(response, simplifyVector = TRUE)$token
      } else if (content_type == "text/plain") {
        new_token <- trimws(httr2::resp_body_string(response))
      } else {
        stop("Unexpected content type: ", content_type)
      }
      if (!is_string(new_token)) {
        stop("The authentication response did not contain a token.")
      }

      # 5) Cache the new token and expiration
      plug_cache_token(new_token, as.numeric(Sys.time()) + validity_time, endpoint)

      new_token
    },
    error = function(e) {
      # If an error occurs at any point, show a message and return NULL
      message(conditionMessage(e))
      NULL
    }
  )
}

# Cached token for `endpoint`, or NULL if there is none or it is about to expire.
plug_cached_token <- function(endpoint) {
  token <- plug_key_get(plug_services$token)
  expiration <- plug_key_get(plug_services$expiration)
  if (is.null(token) || is.null(expiration) || !nzchar(token)) {
    return(NULL)
  }

  # Tokens cached by plug 0.1.0 have no endpoint registered
  token_endpoint <- plug_key_get(plug_services$token_endpoint)
  if (!is.null(token_endpoint) && !identical(token_endpoint, endpoint)) {
    return(NULL)
  }

  expiration <- suppressWarnings(as.numeric(expiration))
  # Keep a safety margin so the token does not expire during the request
  if (is.na(expiration) || as.numeric(Sys.time()) + 30 >= expiration) {
    return(NULL)
  }

  token
}

plug_cache_token <- function(token, expiration, endpoint) {
  tryCatch(
    {
      plug_key_set(plug_services$token, token)
      plug_key_set(plug_services$expiration, format(expiration, scientific = FALSE))
      plug_key_set(plug_services$token_endpoint, endpoint)
    },
    error = function(e) {
      # The token is still usable, it will just be requested again next time
      plug_clear_token()
    }
  )
  invisible(NULL)
}

#' List registered credentials for Plug API
#'
#' This function lists all globally stored credentials (username and password) for the Plug API.
#' If none are found or an error occurs, it displays a message in English ("No credentials found for Plug API.")
#' and returns an empty list.
#'
#' Note that the password is returned in plain text.
#'
#' @return A named list with `username` and `password` fields if credentials are found,
#' or an empty list if no credentials are stored.
#' @examples
#' \dontrun{
#' plug_list_credentials()
#' }
#' @export
plug_list_credentials <- function() {
  username <- plug_key_get(plug_services$username)
  password <- plug_key_get(plug_services$password)

  if (is.null(username) || is.null(password)) {
    message("No credentials found for Plug API.")
    return(list())
  }

  list(username = username, password = password)
}

#' List registered tokens for Plug API
#'
#' This function lists the stored API token and its expiration time for the Plug API.
#' If none are found or an error occurs, it displays a message in English ("No token found for Plug API.")
#' and returns an empty list.
#'
#' @return A named list with `token` and `expiration` fields if a token is found,
#' or an empty list if no token is stored.
#' @examples
#' \dontrun{
#' plug_list_tokens()
#' }
#' @export
plug_list_tokens <- function() {
  token <- plug_key_get(plug_services$token)
  expiration <- plug_key_get(plug_services$expiration)
  expiration <- suppressWarnings(as.numeric(expiration))

  if (is.null(token) || length(expiration) != 1 || is.na(expiration)) {
    message("No token found for Plug API.")
    return(list())
  }

  list(
    token = token,
    expiration = as.POSIXct(expiration, origin = "1970-01-01")
  )
}

#' Execute a custom SQL query on the Plug database
#'
#' This function executes a user-defined SQL query on the Plug database, with safe query construction
#' using the same placeholder syntax as [glue::glue_sql()].
#'
#' @details
#' Values are inserted in the query as Microsoft SQL Server literals:
#'
#' * `{x}` inserts a quoted value: strings are wrapped in single quotes (with embedded quotes escaped),
#'   dates and date-times are formatted, logical values become `1`/`0` and `NA` becomes `NULL`.
#' * `{x*}` inserts all the elements of `x` separated by commas, for use with `IN (...)`.
#' * ``{`x`}`` inserts a quoted identifier, such as a table or column name.
#' * `{I(x)}` inserts `x` as is, without any quoting.
#'
#' Use double braces (`{{` and `}}`) to insert literal braces in the query.
#'
#' If the API rejects the cached token, a new token is generated and the query is executed again.
#'
#' @param sql_template A SQL query template with placeholders for variables.
#' @param endpoint The endpoint URL for executing queries.
#' @param verbosity The verbosity level of the API request (0 = none, 1 = minimal, 2 = detailed).
#' @param ... Named arguments to replace placeholders in the SQL template.
#' @param auth_endpoint The endpoint URL for generating the token.
#' @param .envir The environment in which placeholders not supplied in `...` are evaluated.
#'
#' @return A tibble containing the query results.
#' @examples
#' \dontrun{
#' data <- plug_execute_query(sql_template = "SELECT TOP 1 * FROM Contratos_VIEW")
#'
#' # Using placeholders
#' data <- plug_execute_query(
#'   "SELECT TOP {n} * FROM {`base`} WHERE Ano IN ({years*})",
#'   n = 10,
#'   base = "Contratos_VIEW",
#'   years = c(2023, 2024)
#' )
#' }
#' @export
plug_execute_query <- function(sql_template,
                               endpoint = "https://plug.der.pe.gov.br/MadrixApi/executeQuery",
                               verbosity = 0,
                               ...,
                               auth_endpoint = "https://plug.der.pe.gov.br/MadrixApi/authenticate/",
                               .envir = parent.frame()) {
  # Ensure sql_template is valid
  if (!is_string(sql_template)) {
    stop("SQL template must be a valid string.")
  }

  # Construct the SQL query
  sql_query <- sql_interpolate(sql_template, ..., .envir = .envir)

  plug_run_query(
    sql_query,
    endpoint = endpoint,
    verbosity = verbosity,
    auth_endpoint = auth_endpoint
  )
}

# Send an already constructed query to the API and parse the result.
plug_run_query <- function(sql_query, endpoint, verbosity, auth_endpoint) {
  if (!is_string(endpoint)) {
    stop("Endpoint must be a valid string.")
  }

  perform <- function(token) {
    plug_request(endpoint) |>
      httr2::req_auth_bearer_token(token) |>
      httr2::req_body_json(list(sqlQuery = sql_query)) |>
      httr2::req_perform(verbosity = verbosity)
  }

  token <- plug_get_valid_token(endpoint = auth_endpoint)
  if (is.null(token)) {
    stop("Failed to retrieve a valid token.")
  }
  response <- perform(token)

  # The token may have been invalidated before its expected expiration
  if (httr2::resp_status(response) == 401) {
    token <- plug_get_valid_token(endpoint = auth_endpoint, force = TRUE)
    if (is.null(token)) {
      stop("Failed to retrieve a valid token.")
    }
    response <- perform(token)
  }

  if (httr2::resp_status(response) >= 400) {
    stop("Failed to execute the query. Error details: ", plug_http_error(response))
  }

  # Parse the response
  content_type <- plug_content_type(response)
  if (content_type != "application/json") {
    stop("Unexpected content type: ", content_type)
  }

  res <- httr2::resp_body_json(response, simplifyVector = TRUE)
  if (length(res) == 0) {
    return(tibble::tibble())
  }

  tibble::as_tibble(res)
}

#' Download all data from a specific base
#'
#' This function downloads all data from a specified base using the query `SELECT * FROM base_name`.
#'
#' @param base_name The name of the base from which to download all data.
#' It can include the schema, as in `"dbo.Contratos_VIEW"`.
#' @param endpoint The endpoint URL for executing queries.
#' @param verbosity The verbosity level of the API request (0 = none, 1 = minimal, 2 = detailed).
#' @param auth_endpoint The endpoint URL for generating the token.
#'
#' @return A tibble containing all data from the specified base.
#' @examples
#' \dontrun{
#' data <- plug_download_base(
#'   base_name = "Contratos_VIEW"
#' )
#' }
#' @export
plug_download_base <- function(base_name,
                               endpoint = "https://plug.der.pe.gov.br/MadrixApi/executeQuery",
                               verbosity = 0,
                               auth_endpoint = "https://plug.der.pe.gov.br/MadrixApi/authenticate/") {
  if (!is_string(base_name)) {
    stop("Base name must be a valid string.")
  }
  if (!is_sql_object_name(base_name)) {
    stop("Base name must be a valid table or view name.")
  }

  plug_run_query(
    paste("SELECT * FROM", base_name),
    endpoint = endpoint,
    verbosity = verbosity,
    auth_endpoint = auth_endpoint
  )
}
