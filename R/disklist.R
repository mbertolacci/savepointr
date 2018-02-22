.get_state <- function(dlist) {
  sptr <- attr(dlist, 'sptr')

  if (!has_current_savepoint(sptr)) {
    list(size = 0)
  } else {
    current_savepoint(sptr)
  }
}

.save_state <- function(dlist, state) {
  save_current(attr(dlist, 'sptr'), state)
}

#' @export
disklist <- function(
  directory = '.',
  create_directory = TRUE,
  value_path_format = file.path(directory, 'value%d.rds')
) {
  sptr <- savepointr(directory = directory, create_directory = create_directory)

  value_directory <- normalizePath(dirname(value_path_format))
  dlist <- structure(0,
    sptr = sptr,
    value_path_format = file.path(value_directory, basename(value_path_format))
  )
  class(dlist) <- 'disklist'
  return(dlist)
}

#' @export
clear_all <- function(dlist) {
  state <- .get_state(dlist)
  if (state$size > 0) {
    # Clear saved values
    for (index in 1 : state$size) {
      file.remove(sprintf(
        attr(dlist, 'value_path_format'),
        index
      ))
    }
  }
  .save_state(dlist, list(size = 0))

  invisible(dlist)
}

#' @export
push_value <- function(dlist, value) {
  # Procedure:
  # 1. Save to new location
  # 2. Update the state
  # If either fails, the list will be preserved (conditional on operating
  # system guarantees)
  state <- .get_state(dlist)
  saveRDS(value, sprintf(
    attr(dlist, 'value_path_format'),
    state$size + 1
  ))
  state$size <- state$size + 1
  .save_state(dlist, state)

  invisible(dlist)
}

#' @export
length.disklist <- function(dlist) {
  .get_state(dlist)$size
}

#' @export
`[[.disklist` <- function(dlist, index) {
  readRDS(sprintf(attr(dlist, 'value_path_format'), index))
}

#' @export
`[[<-.disklist` <- function(dlist, index, value) {
  stop('This operation is not supported')
}
