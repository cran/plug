# plug 0.2.0

## Bug fixes

* `plug_execute_query()` now replaces placeholders in the SQL template. The
  previous implementation failed for any template with placeholders, and
  exposed internal variables of the function to the template.
* `plug_execute_query()` and `plug_download_base()` now stop with a clear
  message when no valid token is available, instead of sending an
  unauthenticated request.
* `plug_store_credentials()` discards the cached token, so new credentials are
  used right away.
* `plug_get_valid_token()` reports the actual reason of a failure (missing
  credentials, network problem or response of the 'API') instead of always
  showing "No valid credentials found.".
* The example of `plug_store_credentials()` no longer overwrites credentials
  stored in the system keyring.
* The package now declares its dependency on R >= 4.1.0.

## New features

* New `plug_clear_credentials()` removes the stored credentials and tokens.
* `plug_execute_query()` supports the placeholders `{x}`, `{x*}`, `` {`x`} ``
  and `{I(x)}`, with values quoted according to Microsoft SQL Server syntax.
* `plug_execute_query()` and `plug_download_base()` gain the argument
  `auth_endpoint`, and request a new token when the cached one is rejected by
  the 'API'.
* `plug_get_valid_token()` gains the argument `force`.
* Errors of the 'API' now include the message returned by the server.
* `plug_download_base()` validates the name of the base.

# plug 0.1.0

* Initial CRAN submission.
