#' Save an RDS file in a fault-tolerant manner
#'
#' Calls \code{\link[base:readRDS]{saveRDS}} in a fault-tolerant manner, such
#' that if the save fails, the existing file will not be overwritten. This is
#' done with the following scheme:
#' \enumerate{
#'    \item Call \code{saveRDS} to save the file to \code{via_filename}.
#'    \item If that succeeds, call \code{\link[base:files]{file.rename}} to rename
#'    the new file to overwrite \code{filename}.
#' }
#' The underlying implementation of \code{file.rename} uses the POSIX function
#' \code{rename}, which has the guarantee that it either succeeds or leaves
#' the destination file untouched (subject to hardware limitations). Therefore,
#' this function also either succeeds, or leaves \code{filename} untouched. In
#' reality, this guarantee is almost certainly dependent on the operating
#' system, the file system, and a host of other factors. Still, even if the
#' guarantee is not fully satisfied, renaming the file ought to be faster and
#' therefore a system failure during the rename is less likely than during
#' \code{saveRDS}.
#'
#' @param object R object to serialize.
#' @param filename Destination filename. Unlike
#' \code{\link[base:readRDS]{saveRDS}}, this can't be a connection.
#' @param via_filename Filename to save the file to before the rename step.
#' This should probably be on the same filesystem as \code{filename} to get the
#' atomicity guarantee described above.
#' @param ... Passed to \code{saveRDS}.
#' @export
safe_saveRDS <- function(object, filename, via_filename, ...) {
  saveRDS(object, via_filename, ...)
  file.rename(via_filename, filename)
  invisible(NULL)
}
