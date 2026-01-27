#' Internal package environment
#'
#' A dedicated environment for storing ustats package state,
#' such as Python availability checks. Parent is emptyenv() for isolation.
#'
#' @keywords internal
ustats_env <- new.env(parent = emptyenv())
