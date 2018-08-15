#' savepointr
#'
#' A \code{savepointr} is a pointer to a 'savepoint'. A savepoint is an R
#' object, saved to disk, which represents the state of a long running process.
#' For example, if you are running an iterative optimisation procedure, you
#' could use a savepoint to store the most recently found solution. Then, if the
#' R crashes for some reason, the process can be restarted from the last
#' savepoint.
#'
#' It is not particularly hard to implement a savepoint mechanism manually, by
#' saving out the current state of the computation from time to time, perhaps
#' using \code{\link[base:readRDS]{saveRDS}}. This can be dangerous if done
#' naively, since the computer could crash while conducting the save, which could leave
#' the savepoint in an unrecoverable state. \code{savepointr}s avoid this by
#' using \code{\link{safe_saveRDS}} to perform the save.
#'
#' \code{savepointr} creates a savepointr object. Called
#' with no arguments, the savepointr will reside in the current
#' directory. Alternatively, either a directory can be specified, or the paths
#' to use for the \code{safe_saveRDS} mechanism can be specified directly.
#'
#' \code{current_savepoint} returns the value of the current savepoint, or NULL
#' if you haven't saved anything yet. This function reads from disk, and
#' therefore should not be called too often or it may slow down your program.
#'
#' \code{has_current_savepoint} checks whether the savepoint has ever been saved
#' before, returning \code{TRUE} or \code{FALSE} for yes or no, respectively.
#'
#' \code{clear_savepointr} resets the savepoint, clearing any files - this is
#' destructive, so use it carefully.
#'
#' Finally, \code{save_current} is used to update the savepoint. It can save
#' any R object for which \code{saveRDS} works, so integers, vectors, lists,
#' are all acceptable objects. This method is guaranteed to either succeed,
#' updating the savepoint, or fail, leaving the current savepoint unchanged
#' (though see \code{\link{safe_saveRDS}} for details/caveats on this
#' guarantee).
#'
#' @param directory The directory in which to store the savepoint. Ignored if
#' current_path and new_path are provided.
#' @param create_directory If TRUE, and directory is not ignored, will create
#' the directory if it does not yet exist.
#' @param current_path Path at which the current savepoint will be stored.
#' Regardless of the file extension, will be stored in RDS format.
#' @param new_path Path where new savepoints will initially be saved, before
#' being safely renamed over the current path.
#' @param sptr A savepointr object.
#' @param savepoint An arbitrary R object (must be serializable).
#'
#' @export
savepointr <- function(
  directory = '.',
  create_directory = TRUE,
  current_path = file.path(directory, 'savepoint.rds'),
  new_path = file.path(directory, 'new_savepoint.rds')
) {
  if ((missing(current_path) || missing(new_path)) && !dir.exists(directory)) {
    if (create_directory) {
      dir.create(directory, recursive = TRUE)
    } else {
      stop(sprintf('Directory %s does not exist', directory))
    }
  }
  current_directory <- normalizePath(dirname(current_path))
  new_directory <- normalizePath(dirname(new_path))

  sptr <- list(
    current_path = file.path(current_directory, basename(current_path)),
    new_path = file.path(new_directory, basename(new_path))
  )
  class(sptr) <- 'savepointr'
  return(sptr)
}

#' Print a savepointr object.
#' @param x A \code{\link{savepointr}}.
#' @param ... Not used.
#' @export
print.savepointr <- function(x, ...) {
  cat(
    'Savepointr at',
    sptr[['current_path']],
    'with',
    ifelse(
      file.exists(sptr[['current_path']]),
      sprintf(
        'a current savepoint of size %d',
        file.size(sptr[['current_path']])
      ),
      'no current savepoint'
    ),
    '\n'
  )
  invisible(sptr)
}

#' @describeIn savepointr Return the current savepoint, if any.
#' @export
current_savepoint <- function(sptr) {
  if (!file.exists(sptr[['current_path']])) {
    NULL
  } else {
    readRDS(sptr[['current_path']])
  }
}

#' @describeIn savepointr Whether a current savepoint exists, TRUE/FALSE.
#' @export
has_current_savepoint <- function(sptr) {
  file.exists(sptr[['current_path']])
}

#' @describeIn savepointr Clear the current savepoint, if any, and any failed
#' saves.
#' @export
clear_savepointr <- function(sptr) {
  if (file.exists(sptr[['current_path']])) {
    file.remove(sptr[['current_path']])
  }
  if (file.exists(sptr[['new_path']])) {
    file.remove(sptr[['new_path']])
  }

  invisible(sptr)
}

#' @describeIn savepointr Create a new savepoint.
#' @export
save_current <- function(sptr, savepoint) {
  # Save to new path, then rename, because save is not atomic but rename is.
  # Thus, if the program crashes during the save / the save fails, the current
  # savepoint will not be lost
  saveRDS(savepoint, sptr[['new_path']])
  file.rename(sptr[['new_path']], sptr[['current_path']])

  invisible(sptr)
}
