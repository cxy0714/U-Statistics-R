# zzz.R - Package loading hooks for ustats
#
# This file contains .onLoad() and other startup functions.
# It is sourced automatically when the package is loaded.
#
# Author: Xingyu Chen

.onLoad <- function(libname, pkgname) {
  # Initialize the package-level environment for state caching
  # (e.g., Python availability checks)
  assign("ustats_env", new.env(parent = emptyenv()), envir = asNamespace(pkgname))

  # Optional: Set package options if needed
  # options(ustats.verbose = TRUE)

  invisible()
}

# .onAttach() - Optional: message on library(ustats)
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("ustats loaded. Use check_ustats_setup() to verify Python environment.")
}
