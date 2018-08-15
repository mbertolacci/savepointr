context('safe_saveRDS')

out_filename <- tempfile()
via_filename <- tempfile()

test_that('safe_saveRDS creates the file', {
  object_out <- 1 : 10
  safe_saveRDS(object_out, out_filename, via_filename)
  object_in <- readRDS(out_filename)
  expect_identical(object_out, object_in)
})

teardown({
  suppressWarnings(file.remove(out_filename, showWarnings = FALSE))
  suppressWarnings(file.remove(via_filename, showWarnings = FALSE))
})
