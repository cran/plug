# Internal helpers -----------------------------------------------------------

plug_keyring_user <- "global"

plug_services <- list(
  username = "PlugAPI_Username",
  password = "PlugAPI_Password",
  token = "PlugAPI_Token",
  expiration = "PlugAPI_Token_Expiration",
  token_endpoint = "PlugAPI_Token_Endpoint"
)

# Read a keyring entry, returning NULL when it does not exist or the keyring
# is not accessible.
plug_key_get <- function(service) {
  tryCatch(
    keyring::key_get(service = service, username = plug_keyring_user),
    error = function(e) NULL
  )
}

plug_key_set <- function(service, value) {
  keyring::key_set_with_value(
    service = service,
    username = plug_keyring_user,
    password = value
  )
}

# Delete a keyring entry. Returns TRUE if something was removed.
plug_key_delete <- function(service) {
  # Not every keyring backend fails when deleting a missing entry
  if (is.null(plug_key_get(service))) {
    return(FALSE)
  }
  tryCatch(
    {
      keyring::key_delete(service = service, username = plug_keyring_user)
      TRUE
    },
    error = function(e) FALSE
  )
}

plug_clear_token <- function() {
  removed <- vapply(
    plug_services[c("token", "expiration", "token_endpoint")],
    plug_key_delete,
    logical(1)
  )
  any(removed)
}

is_string <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
}

plug_request <- function(endpoint) {
  httr2::request(endpoint) |>
    httr2::req_user_agent("plug (https://github.com/StrategicProjects/plug)") |>
    httr2::req_error(is_error = function(resp) FALSE)
}

# Media type of a response, or "" when the server did not send one.
plug_content_type <- function(response) {
  type <- tryCatch(httr2::resp_content_type(response), error = function(e) NA_character_)
  if (is.na(type)) "" else tolower(type)
}

# Short description of a failed response, including whatever the API said.
plug_http_error <- function(response) {
  body <- tryCatch(httr2::resp_body_string(response), error = function(e) "")
  body <- trimws(body)
  if (nchar(body) > 500) {
    body <- paste0(substr(body, 1, 500), "...")
  }
  msg <- paste0("HTTP ", httr2::resp_status(response), " ", httr2::resp_status_desc(response))
  if (nzchar(body)) {
    msg <- paste0(msg, ": ", body)
  }
  msg
}

# SQL construction -----------------------------------------------------------

# Quote values as Microsoft SQL Server literals.
sql_quote_literal <- function(x) {
  if (inherits(x, "AsIs")) {
    return(as.character(unclass(x)))
  }
  if (is.null(x)) {
    return("NULL")
  }
  if (length(x) == 0) {
    return(character())
  }
  if (is.factor(x)) {
    x <- as.character(x)
  }

  if (is.character(x)) {
    x <- enc2utf8(x)
    prefix <- ifelse(grepl("[^\001-\177]", x, perl = TRUE), "N", "")
    out <- paste0(prefix, "'", gsub("'", "''", x, fixed = TRUE), "'")
  } else if (inherits(x, "Date")) {
    out <- paste0("'", format(x, "%Y-%m-%d"), "'")
  } else if (inherits(x, "POSIXt")) {
    out <- paste0("'", format(x, "%Y-%m-%d %H:%M:%S"), "'")
  } else if (is.logical(x)) {
    out <- as.character(as.integer(x))
  } else if (is.numeric(x)) {
    if (any(is.infinite(x) | is.nan(x))) {
      stop("Non-finite numbers cannot be used in a SQL query.", call. = FALSE)
    }
    out <- vapply(
      x,
      function(v) format(v, scientific = FALSE, digits = 15, trim = TRUE),
      character(1)
    )
  } else {
    stop(
      "Cannot convert an object of class '", class(x)[1], "' to a SQL value.",
      call. = FALSE
    )
  }

  out[is.na(x)] <- "NULL"
  unname(out)
}

# Quote names as Microsoft SQL Server identifiers.
sql_quote_identifier <- function(x) {
  if (!is.character(x) || anyNA(x) || !all(nzchar(x))) {
    stop("SQL identifiers must be non-empty strings.", call. = FALSE)
  }
  paste0("[", gsub("]", "]]", x, fixed = TRUE), "]")
}

# glue transformer implementing the `glue_sql()` placeholder syntax:
# `{value}`, `{values*}` and {`identifier`}.
sql_transformer <- function(text, envir) {
  collapse <- grepl("[*]\\s*$", text)
  if (collapse) {
    text <- sub("[*]\\s*$", "", text)
  }
  text <- trimws(text)

  is_identifier <- grepl("^`.*`$", text)
  if (is_identifier) {
    text <- substr(text, 2, nchar(text) - 1)
  }

  # As in glue, placeholders are R expressions written by the caller in the
  # template; the values they evaluate to are always quoted below.
  value <- eval(parse(text = text, keep.source = FALSE), envir)
  out <- if (is_identifier) sql_quote_identifier(value) else sql_quote_literal(value)

  if (collapse) {
    if (length(out) == 0) "NULL" else paste(out, collapse = ", ")
  } else {
    if (length(out) != 1) {
      stop(
        "Placeholder `{", text, "}` has length ", length(out),
        ". Use `{", text, "*}` to insert a comma-separated list.",
        call. = FALSE
      )
    }
    out
  }
}

sql_interpolate <- function(sql_template, ..., .envir = parent.frame()) {
  values <- list(...)
  if (length(values) > 0 && (is.null(names(values)) || !all(nzchar(names(values))))) {
    stop("All arguments passed in `...` must be named.", call. = FALSE)
  }

  envir <- list2env(values, parent = .envir)
  query <- glue::glue(sql_template, .envir = envir, .transformer = sql_transformer)
  as.character(query)
}

# A (possibly schema-qualified) table or view name: `name`, `dbo.name`,
# `[name with spaces]`, ...
is_sql_object_name <- function(x) {
  part <- "(\\[[^]]+\\]|[[:alnum:]_#@$]+)"
  grepl(paste0("^", part, "([.]", part, ")*$"), x)
}
