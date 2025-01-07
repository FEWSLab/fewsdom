# Functions around setting up a processing environment
# Need to make these so it cleans up the processing code and makes passing
# function arguments much more clear.

# TODO PUT EVERY ARGUMENT IN HERE


# Create an empty environment to store EEMS processing arguments and parameters
.pkgenv <- new.env(parent = emptyenv())

# Project Path (prjpath) ----
# Initialize
# TODO Is there a saner default?
.pkgenv$prjpath <- ""
# Getter for prjpath
get_prjpath <- function() {
  .pkgenv$prjpath
}
# Setter for prjpath
# Setter invisibly returns the old value so we can assign it to a variable if we
# want to save it and reset to the old value later
set_prjpath <- function(filepath) {
 old <- .pkgenv$prjpath
 .pkgenv$prjpath <- filepath
 invisible(old)
}

# Metadata filename ----
# Initialize
.pkgenv$meta_name <- ""
# Getter for meta_name
get_meta_name <- function() {
  .pkgenv$meta_name
}
# Setter for meta_name
set_meta_name <- function(filename) {
  old <- .pkgenv$meta_name
  .pkgenv$meta_name <- filename
  invisible(old)
}

# DOC File info
# Initialize to NULL
.pkgenv$doc_file <- NULL
# Getter for DOC file


# TODO can I make the DOC file a class with all the required info like
# filename, sheet name, doc_column, name_column, nskip, doc_delim, etc.. so I don't
# have to individually have all those parameters in the environment?






# Tracking File ----
# Initialize the tracking file argument
# TODO: IS THERE A SANE DEFAULT?
.pkgenv$tracking_file <- ""

# Getter for the tracking file info
get_tracking_file <- function() {
  .pkgenv$tracking_file
}

#Setter
set_tracking_file <- function(filepath) {
  old <- .pkgenv$tracking_file
  .pkgenv$tracking_file <- filepath
  invisible(old)
}

