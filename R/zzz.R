# zzz.R - Package loading hooks for ustats
#
# This file contains .onLoad() and other startup functions.
# It is sourced automatically when the package is loaded.
#
# Author: Xingyu Chen

.onLoad <- function(libname, pkgname) {
  # Declare the Python dependencies of this package. With reticulate
  # (>= 1.41) these requirements are resolved automatically: the first
  # time Python is initialized (e.g. on the first call to ustat()),
  # reticulate provisions a cached ephemeral environment containing
  # them, unless the user has already configured a Python environment
  # (e.g. via RETICULATE_PYTHON, use_virtualenv(), or use_condaenv()).
  #
  # py_require() only records the requirements -- nothing is downloaded
  # or installed at load time, so this is safe on CRAN check machines.
  reticulate::py_require(c("u-stats", "numpy", "torch"))
  reticulate::py_require(python_version = ">=3.11")

  invisible()
}

# .onAttach() - message on library(ustats)
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "ustats: Python dependencies (u-stats, numpy, torch) are provisioned ",
    "automatically on first use.\n",
    "Run check_ustats_setup() to verify the environment, ",
    "or see ?setup_ustats for manual setup."
  )
}
