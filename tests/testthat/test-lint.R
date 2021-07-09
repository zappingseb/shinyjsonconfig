test_that("linting", {
  
  expect_equal(length(lintr::lint_package(with_defaults(
    camel_case_linter=NULL
  ))), 0);
})
