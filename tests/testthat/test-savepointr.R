context('savepointr')

# NOTE(mgnb): on Mac OS, tempdir() can return a path under /var, which is
# actually a symlink. savepointr called normalizePath, so it will get the true
# path. These next few functions attempt to normalize paths so they will match
# what the package gets.
normalized_tempdir <- function() {
  normalizePath(tempdir())
}

normalized_tempfile <- function() {
  filename <- tempfile()
  output <- file.path(normalizePath(dirname(filename)), basename(filename))
  teardown(suppressWarnings(file.remove(output)))
  output
}

savepointr_tempdir <- function() {
  name <- tempfile()
  output <- file.path(normalizePath(dirname(name)), basename(name))
  teardown(suppressWarnings(unlink(output, recursive = TRUE)))
  output
}

## savepointr

test_that('savepointr can be called with no arguments', {
  sptr <- savepointr()
  expect_is(sptr, 'savepointr')
  normalized_wd <- normalizePath(getwd())
  expect_equal(sptr$current_path, file.path(normalized_wd, 'savepoint.rds'))
  expect_equal(sptr$new_path, file.path(normalized_wd, 'new_savepoint.rds'))
})

test_that('savepointr creates the directory when create_directory = TRUE', {
  directory <- savepointr_tempdir()
  expect_false(dir.exists(directory))
  sptr <- savepointr(directory = directory, create_directory = TRUE)
  expect_true(dir.exists(directory))
  expect_equal(sptr$current_path, file.path(directory, 'savepoint.rds'))
  expect_equal(sptr$new_path, file.path(directory, 'new_savepoint.rds'))
  # Recreating the savepointr should not throw an error
  sptr <- savepointr(directory = directory, create_directory = TRUE)
})

test_that(paste(
  'savepointr throws an error when create_directory = FALSE and the directory',
  'does not exist'
), {
  directory <- savepointr_tempdir()
  expect_false(dir.exists(directory))
  expect_error(savepointr(directory = directory, create_directory = FALSE))
})

test_that('savepointr can have paths specified manually', {
  current_path_filename <- normalized_tempfile()
  new_path_filename <- normalized_tempfile()
  sptr <- savepointr(
    current_path = current_path_filename,
    new_path = new_path_filename
  )
  expect_equal(sptr$current_path, current_path_filename)
  expect_equal(sptr$new_path, new_path_filename)
})

## print.savepointr

test_that('print.savepointr doesn\'t fail', {
  output <- capture.output(print(savepointr()))
  expect_gt(nchar(output), 0)
})

## current_savepoint

test_that('current_savepoint returns NULL when there is no savepoint', {
  sptr <- savepointr(savepointr_tempdir())
  expect_null(current_savepoint(sptr))
})

test_that('current_savepoint returns the current savepoint', {
  sptr <- savepointr(savepointr_tempdir())
  save_current(sptr, 'dogs are the best')
  expect_identical(current_savepoint(sptr), 'dogs are the best')
})

## has_current_savepoint

test_that('has_current_savepoint returns FALSE when there is no savepoint', {
  sptr <- savepointr(savepointr_tempdir())
  expect_false(has_current_savepoint(sptr))
})

test_that('has_current_savepoint returns TRUE when there is a savepoint', {
  sptr <- savepointr(savepointr_tempdir())
  save_current(sptr, 'dogs are the best')
  expect_true(has_current_savepoint(sptr))
})

## clear_savepointr

test_that('clear_savepointr doesn\'t fail when there is no savepoint', {
  clear_savepointr(savepointr())
  # Dummy expectation; won't get here if there's an error
  expect_true(TRUE)
})

test_that('clear_savepointr removes the current savepoint', {
  sptr <- savepointr(savepointr_tempdir())
  save_current(sptr, 'dogs are the best')
  expect_true(has_current_savepoint(sptr))
  clear_savepointr(sptr)
  expect_false(has_current_savepoint(sptr))
})

test_that('clear_savepointr removes new_path, if it exists', {
  sptr <- savepointr(savepointr_tempdir())
  writeLines('dogs are the best', sptr$new_path)
  expect_true(file.exists(sptr$new_path))
  clear_savepointr(sptr)
  expect_false(file.exists(sptr$new_path))
})

## save_current

test_that('save_current updates the savepoint', {
  sptr <- savepointr(savepointr_tempdir())
  # First savepoint
  save_current(sptr, 'dogs are the best')
  expect_identical(current_savepoint(sptr), 'dogs are the best')
  # Overwrite last savepoint
  save_current(sptr, 'dogs are still the best')
  expect_identical(current_savepoint(sptr), 'dogs are still the best')
})
