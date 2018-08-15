if (requireNamespace('lintr', quietly = TRUE)) {
  context('lints')

  test_that('Package passes lintr', {
    skip_on_travis()
    lintr::expect_lint_free()
  })
}
