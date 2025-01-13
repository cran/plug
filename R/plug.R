#' Store user credentials securely for Plug API
#'
#' This function securely stores the global username and password required to authenticate with the Plug application.
#'
#' @param username The username for the Plug.
#' @param password The password for the Plug.
#'
#' @return No return value. The credentials are securely stored.
#' @examples
#' plug_store_credentials("myusername", "mypassword")
#' @export
plug_store_credentials <- function(username, password) {
  if (!is.character(username) || !nzchar(username)) {
    stop("Username must be a valid string.")
  }
  if (!is.character(password) || !nzchar(password)) {
    stop("Password must be a valid string.")
  }

  tryCatch(
    {
      keyring::key_set_with_value(service = "PlugAPI_Username", username = "global", password = username)
      keyring::key_set_with_value(service = "PlugAPI_Password", username = "global", password = password)
      message("Global credentials successfully stored.")
    },
    error = function(e) {
      message("Keyring not accessible. Credentials could not be securely stored.")
    }
  )
}



#' Get a valid token for Plug API
#'
#' This function checks if a valid global token exists for the Plug API. If no valid token is found,
#' it generates a new token using the stored global credentials by sending a properly formatted request.
#' If it fails to retrieve the token (for example, due to missing credentials or network issues),
#' it will not throw an error but will display a message in English ("No valid credentials found.") and return `NULL`.
#'
#' @param validity_time The validity period of the token in seconds. Default is 3600 (1 hour).
#' @param endpoint The endpoint URL for generating the token.
#'
#' @return The valid token as a string, or `NULL` if no valid credentials were found or an error occurred.
#' @examples
#' \donttest{
#' \dontrun{
#' token <- plug_get_valid_token(validity_time = 3600)
#' }
#' }
#' @export
plug_get_valid_token <- function(validity_time = 3600,
                                 endpoint = "https://plug.der.pe.gov.br/MadrixApi/authenticate/") {
  tryCatch(
    {
      # 1) Retrieve cached token and expiration
      token <- tryCatch(
        keyring::key_get(service = "PlugAPI_Token", username = "global"),
        error = function(e) NULL
      )
      expiration <- tryCatch(
        as.numeric(keyring::key_get(service = "PlugAPI_Token_Expiration", username = "global")),
        error = function(e) NULL
      )

      # 2) Check if token is still valid
      if (!is.null(token) && !is.null(expiration) && Sys.time() < expiration) {
        return(token)
      }

      # 3) Generate a new token if none exists or if expired
      username <- keyring::key_get(service = "PlugAPI_Username", username = "global")
      password <- keyring::key_get(service = "PlugAPI_Password", username = "global")

      # 4) Create the JSON body for the authentication request
      body <- list(UserName = username, Password = password)

      # 5) Make the request to generate a new token with error handling
      response <- tryCatch(
        {
          httr2::request(endpoint) |>
            httr2::req_headers("Content-Type" = "application/json") |>
            httr2::req_body_json(body) |>
            httr2::req_perform()
        },
        error = function(e) {
          stop("Failed to generate a new token. Please check your credentials and network connection. Error details: ", conditionMessage(e))
        }
      )

      # 6) Check the content type and parse accordingly
      if (httr2::resp_content_type(response) == "application/json") {
        parsed_response <- httr2::resp_body_json(response, simplifyVector = TRUE)
      } else if (httr2::resp_content_type(response) == "text/plain") {
        parsed_response <- list(token = httr2::resp_body_string(response))
      } else {
        stop("Unexpected content type: ", httr2::resp_content_type(response))
      }

      # 7) Extract the token from the parsed response
      new_token <- parsed_response$token
      new_expiration <- as.numeric(Sys.time()) + validity_time

      # 8) Cache the new token and expiration
      keyring::key_set_with_value(
        service = "PlugAPI_Token",
        username = "global",
        password = new_token
      )
      keyring::key_set_with_value(
        service = "PlugAPI_Token_Expiration",
        username = "global",
        password = as.character(new_expiration)
      )

      return(new_token)
    },
    error = function(e) {
      # If an error occurs at any point, show a message in English and return NULL
      message("No valid credentials found.")
      return(NULL)
    }
  )
}

#' List registered credentials for Plug API
#'
#' This function lists all globally stored credentials (username and password) for the Plug API.
#' If none are found or an error occurs, it displays a message in English ("No credentials found for Plug API.")
#' and returns an empty list.
#'
#' @return A named list with `username` and `password` fields if credentials are found,
#' or an empty list if no credentials are stored.
#' @examples
#' \donttest{
#' \dontrun{
#' plug_list_credentials()
#' }
#' }
#' @export
plug_list_credentials <- function() {
  tryCatch(
    {
      # Retrieve the stored username and password
      username <- keyring::key_get(service = "PlugAPI_Username", username = "global")
      password <- keyring::key_get(service = "PlugAPI_Password", username = "global")

      list(username = username, password = password)
    },
    error = function(e) {
      message("No credentials found for Plug API.")
      return(list())
    }
  )
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
#' \donttest{
#' \dontrun{
#' plug_list_tokens()
#' }
#' }
#' @export
plug_list_tokens <- function() {
  tryCatch(
    {
      # Retrieve the stored token and expiration time
      token <- keyring::key_get(service = "PlugAPI_Token", username = "global")
      expiration <- keyring::key_get(service = "PlugAPI_Token_Expiration", username = "global")

      list(
        token = token,
        expiration = as.POSIXct(as.numeric(expiration), origin = "1970-01-01")
      )
    },
    error = function(e) {
      message("No token found for Plug API.")
      return(list())
    }
  )
}

#' Execute a custom SQL query on the Plug database
#'
#' This function executes a user-defined SQL query on the Plug database, with safe query construction
#' using `glue_sql`.
#'
#' @param sql_template A SQL query template with placeholders for variables.
#' @param endpoint The endpoint URL for executing queries.
#' @param verbosity The verbosity level of the API request (0 = none, 1 = minimal, 2 = detailed).
#' @param ... Named arguments to replace placeholders in the SQL template.
#'
#' @return A tibble containing the query results.
#' @examples
#' \donttest{
#' \dontrun{
#' data <- plug_execute_query(sql_template = "SELECT TOP 1 * FROM Contratos_VIEW")
#' }
#' }
#' @export
plug_execute_query <- function(sql_template,
                               endpoint = "https://plug.der.pe.gov.br/MadrixApi/executeQuery",
                               verbosity = 0,
                               ...) {

  # Ensure sql_template is valid
  if (!is.character(sql_template) || !nzchar(sql_template)) {
    stop("SQL template must be a valid string.")
  }

  # Get a valid token, with error handling
  token <- tryCatch(
    plug_get_valid_token(),
    error = function(e) {
      stop("Failed to retrieve a valid token: ", conditionMessage(e))
    }
  )

  # Construct the SQL query using glue_sql with .con = NULL
  sql_query <- glue::glue_sql(sql_template, .con = NULL, ...)

  # Execute the actual query
  response <- httr2::request(endpoint) |>
    httr2::req_headers("Content-Type" = "application/json", Authorization = paste("Bearer", token)) |>
    httr2::req_body_json(list(sqlQuery = sql_query)) |>
    httr2::req_perform(verbosity = verbosity)

  # Parse the response
  if (httr2::resp_content_type(response) == "application/json") {
    res <- httr2::resp_body_json(response, simplifyVector = TRUE) |> tibble::as_tibble()
  } else {
    stop("Unexpected content type: ", httr2::resp_content_type(response))
  }

  return(res)
}



#' Download all data from a specific base
#'
#' This function downloads all data from a specified base using the query `SELECT * FROM base_name`.
#'
#' @param base_name The name of the base from which to download all data.
#' @param endpoint The endpoint URL for executing queries.
#' @param verbosity The verbosity level of the API request (0 = none, 1 = minimal, 2 = detailed).
#'
#' @return A tibble containing all data from the specified base.
#' @examples
#' \donttest{
#' \dontrun{
#' data <- plug_download_base(
#'   base_name = "Contratos_VIEW"
#' )
#' }
#' }
#' @export
plug_download_base <- function(base_name,
                               endpoint = "https://plug.der.pe.gov.br/MadrixApi/executeQuery",
                               verbosity = 0) {
  if (!is.character(base_name) || !nzchar(base_name)) {
    stop("Base name must be a valid string.")
  }

  # Construct the SQL query using glue (not glue_sql)
  sql_query <- glue::glue("SELECT * FROM {base_name}")

  # Execute the query using plug_execute_query
  plug_execute_query(
    sql_template = sql_query,
    endpoint = endpoint,
    verbosity = verbosity
  )
}
