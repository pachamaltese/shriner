#!/usr/bin/env Rscript

# This script copies resources from AdminLTE to shriner's inst
# directory. The AdminLTE/ project directory should be on the same level
# as the shriner/ project directory.


# Returns the file currently being sourced or run with Rscript
thisFile <- function() {
  cmdArgs <- commandArgs(trailingOnly = FALSE)
  needle <- "--file="
  match <- grep(needle, cmdArgs)
  if (length(match) > 0) {
    # Rscript
    return(normalizePath(sub(needle, "", cmdArgs[match])))
  } else {
    # 'source'd via R console
    return(normalizePath(sys.frames()[[1]]$ofile))
  }
}

srcdir <- file.path(dirname(thisFile()), "../../AdminLTE/dist")
destdir <- file.path(dirname(thisFile()), "../inst/AdminLTE")


file.copy(file.path(srcdir, "/js/app.js"), "../srcjs/AdminLTE", overwrite = TRUE)
file.copy(file.path(srcdir, "/css/AdminLTE.css"), destdir, overwrite = TRUE)
file.copy(file.path(srcdir, "/css/skins/_all-skins.css"), destdir, overwrite = TRUE)
