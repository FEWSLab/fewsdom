# Functions around setting up a processing environment
# Need to make these so it cleans up the processing code and makes passing
# function arguments much more clear.

.pkgenv <- new.env(parent = emptyenv())
.pkgenv$tracking_file <- ""

get_tracking_file <- function() {
  .pkgenv$tracking_file
}
set_tracking_file <- function(filepath) {
  old <- .pkgenv$tracking_file
  .pkgenv$tracking_file <- filepath
  invisible(old)
}
