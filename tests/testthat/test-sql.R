test_that("values are quoted as SQL literals", {
  expect_identical(sql_quote_literal("abc"), "'abc'")
  expect_identical(sql_quote_literal("O'Brien"), "'O''Brien'")
  expect_identical(sql_quote_literal("São Paulo"), "N'São Paulo'")
  expect_identical(sql_quote_literal(factor("a")), "'a'")
  expect_identical(sql_quote_literal(c("a", NA)), c("'a'", "NULL"))
  expect_identical(sql_quote_literal(1L), "1")
  expect_identical(sql_quote_literal(c(1.5, NA)), c("1.5", "NULL"))
  expect_identical(sql_quote_literal(1e10), "10000000000")
  expect_identical(sql_quote_literal(c(TRUE, FALSE, NA)), c("1", "0", "NULL"))
  expect_identical(sql_quote_literal(as.Date("2024-01-31")), "'2024-01-31'")
  expect_identical(
    sql_quote_literal(as.POSIXct("2024-01-31 10:20:30", tz = "UTC")),
    "'2024-01-31 10:20:30'"
  )
  expect_identical(sql_quote_literal(NULL), "NULL")
  expect_identical(sql_quote_literal(I("GETDATE()")), "GETDATE()")
  expect_error(sql_quote_literal(Inf), "Non-finite")
  expect_error(sql_quote_literal(list(1)), "Cannot convert")
})

test_that("identifiers are quoted", {
  expect_identical(sql_quote_identifier("my table"), "[my table]")
  expect_identical(sql_quote_identifier("a]b"), "[a]]b]")
  expect_error(sql_quote_identifier(1), "identifiers")
  expect_error(sql_quote_identifier(""), "identifiers")
})

test_that("templates are interpolated", {
  expect_identical(sql_interpolate("SELECT 1"), "SELECT 1")
  expect_identical(
    sql_interpolate("SELECT * FROM t WHERE a = {a}", a = "x"),
    "SELECT * FROM t WHERE a = 'x'"
  )
  expect_identical(
    sql_interpolate("SELECT * FROM t WHERE a IN ({a*})", a = c("x", "y")),
    "SELECT * FROM t WHERE a IN ('x', 'y')"
  )
  expect_identical(
    sql_interpolate("SELECT * FROM t WHERE a IN ({a*})", a = character()),
    "SELECT * FROM t WHERE a IN (NULL)"
  )
  expect_identical(
    sql_interpolate("SELECT {`col`} FROM {`tbl`}", col = "a", tbl = "t"),
    "SELECT [a] FROM [t]"
  )
  expect_identical(
    sql_interpolate("SELECT TOP {I(n)} {{literal}} FROM t", n = 5),
    "SELECT TOP 5 {literal} FROM t"
  )
})

test_that("placeholders are looked up in the calling environment", {
  value <- "local"
  expect_identical(sql_interpolate("SELECT {value}"), "SELECT 'local'")
  expect_identical(sql_interpolate("SELECT {value}", value = "dots"), "SELECT 'dots'")
})

test_that("internal variables are not exposed to templates", {
  local_plug_keyring()
  expect_error(plug_execute_query("SELECT {token}"))
  expect_error(plug_execute_query("SELECT {sql_query}"))
})

test_that("invalid templates fail", {
  expect_error(sql_interpolate("SELECT {a}", a = 1:2), "comma-separated list")
  expect_error(sql_interpolate("SELECT {a}", "x"), "must be named")
  expect_error(sql_interpolate("SELECT {missing_value_xyz}"))
})

test_that("object names are validated", {
  expect_true(is_sql_object_name("Contratos_VIEW"))
  expect_true(is_sql_object_name("dbo.Contratos_VIEW"))
  expect_true(is_sql_object_name("[dbo].[My Table]"))
  expect_false(is_sql_object_name("t WHERE 1 = 1"))
  expect_false(is_sql_object_name("t; DROP TABLE t"))
  expect_false(is_sql_object_name("t--"))
  expect_false(is_sql_object_name("a..b"))
})
