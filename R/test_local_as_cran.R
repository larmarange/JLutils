#' Test a local source package
#'
#' Similar to `testthat::test_local()`, it runs all tests in a package
#' except those skipped by `testthat::skip_on_cran()`, using
#' `withr::local_envvar(NOT_CRAN = "false")`.
#' @inheritParams testthat::test_local
#' @export
test_local_as_cran <- function (path = ".", reporter = NULL, ..., load_package = "source")
{
  package <- pkgload::pkg_name(path)
  test_path <- file.path(pkgload::pkg_path(path), "tests",
                         "testthat")
  withr::local_envvar(NOT_CRAN = "false")
  testthat::test_dir(test_path, package = package, reporter = reporter,
           ..., load_package = load_package)
}
