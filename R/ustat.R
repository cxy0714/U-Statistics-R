
#'
#' @author Xingyu Chen
#' @date 2026-01-23


#' Convert Index List to Einstein Summation Notation
#'
#' Converts a nested list of index pairs (or single indices) into an
#' Einstein summation expression understood by the Python \code{u_stats} backend.
#'
#' For example, a structure like \code{list(c(1,2), c(2,3), c(3,4))} will be
#' converted to the string \code{"ab,bc,cd->"}.
#'
#' Each unique numeric index is mapped to a lowercase letter in alphabetical
#' order. A maximum of 26 unique indices is supported.
#'
#' @param expr_list A list of numeric vectors. Each element must be of length
#'   1 (rank-1 tensor) or 2 (rank-2 tensor).
#'
#' @return A character string representing the Einstein summation expression.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' expr_list_to_einstein(list(c(1, 2), c(2, 3), c(3, 4)))
#' # Returns "ab,bc,cd->"
#' }
expr_list_to_einstein <- function(expr_list) {
  # Collect and SORT indices
  all_indices <- sort(unique(as.integer(unlist(expr_list))))
  n_unique <- length(all_indices)

  if (n_unique > 26) {
    stop("Too many unique indices (>26)")
  }

  index_to_letter <- setNames(letters[seq_len(n_unique)], all_indices)

  # Convert each tensor
  terms <- vapply(expr_list, function(idx) {
    idx <- as.integer(idx)

    if (length(idx) == 1) {
      index_to_letter[as.character(idx)]

    } else if (length(idx) == 2) {
      paste0(index_to_letter[as.character(idx[1])],
             index_to_letter[as.character(idx[2])])

    } else {
      stop("Only rank-1 or rank-2 tensors supported")
    }
  }, character(1))

  paste0(paste(terms, collapse = ","), "->")
}



#' Compute a Higher-Order U-Statistic via Python
#'
#' Computes a higher-order U-statistic from precomputed kernel tensors using
#' the Python package \code{u_stats}. This function serves as an R interface
#' and handles automatic data conversion via \pkg{reticulate}.
#'
#' The U-statistic structure can be specified using either:
#' \itemize{
#'   \item An Einstein summation string (e.g. \code{"ab,bc->"}), or
#'   \item A nested list of index vectors (e.g. \code{list(c(1,2), c(2,3))})
#' }
#'
#' @param tensors A list of numeric vectors, matrices, or arrays representing
#'   kernel evaluations. All tensors must have compatible dimensions.
#' @param expression Either a character string in Einstein notation or a list
#'   of numeric vectors of length 1 or 2 describing index structure.
#' @param backend Character string specifying the computation backend:
#'   \code{"torch"} (default) or \code{"numpy"}.
#' @param average Logical; if \code{TRUE} (default), return the averaged
#'   U-statistic. Otherwise returns the raw sum.
#' @param dtype Optional character string specifying numeric precision for
#'   tensors converted from R. Must be one of \code{"float32"} or
#'   \code{"float64"}. If \code{NULL} (default), precision is chosen
#'   automatically:
#'   \itemize{
#'     \item \code{float32} when using the Torch backend with CUDA available
#'     \item \code{float64} otherwise
#'   }
#'
#' @details
#' This function requires a working Python environment with the
#' \code{u_stats} package installed. Use \code{setup_ustat()} to install
#' dependencies and \code{check_ustat_setup()} to verify configuration.
#'
#' R numeric objects are converted to NumPy arrays using the selected
#' precision. If Python tensors (e.g., Torch tensors) are supplied directly,
#' they are passed through unchanged.
#'
#' @return A numeric scalar containing the computed U-statistic.
#'
#' @examples
#' \dontrun{
#' setup_ustat()
#'
#' v1 <- runif(100)
#' H1 <- matrix(runif(100), 10, 10)
#' H2 <- matrix(runif(100), 10, 10)
#'
#' ustat(list(H1, H2), "ab,bc->")
#' ustat(list(H1, H2), "ab,bc->", dtype = "float32")
#' ustat(list(H1, H2), "ab,bc->", dtype = NULL)  # auto precision
#' }
#' @export

ustat <- function(tensors,
                   expression,
                   backend = c("torch", "numpy"),
                   average = TRUE,
                   dtype = NULL) {

  if (!check_python_env()) {
    stop(
      "Python environment not properly configured.\n",
      "Please run: setup_ustat()\n",
      "Or check setup status with: check_ustat_setup()",
      call. = FALSE
    )
  }

  backend <- match.arg(backend)

  if (backend == "torch" && !reticulate::py_module_available("torch")) {
    warning(
      "Torch backend not available; falling back to numpy.\n",
      "Install torch with: reticulate::py_install('torch')",
      call. = FALSE
    )
    backend <- "numpy"
  }

  ustat_mod <- tryCatch({
    reticulate::import("u_stats", delay_load = TRUE)
  }, error = function(e) {
    stop(
      "Failed to import u_stats module.\n",
      "Please run: setup_ustat()\n",
      "Error: ", e$message,
      call. = FALSE
    )
  })

  ustat_mod$set_backend(backend)
  np <- reticulate::import("numpy", convert = FALSE)

  # ==========================================================
  # 🔢 DTYPE AUTO-SELECTION
  # ==========================================================
  if (!is.null(dtype)) {
    if (!dtype %in% c("float32", "float64")) {
      stop("dtype must be NULL, 'float32', or 'float64'", call. = FALSE)
    }
    np_dtype <- dtype

  } else {
    # Auto নির্বাচন
    if (backend == "torch") {
      torch <- reticulate::import("torch", convert = FALSE)
      use_cuda <- FALSE
      try({
        use_cuda <- torch$cuda$is_available()
      }, silent = TRUE)

      np_dtype <- if (isTRUE(use_cuda)) "float32" else "float64"

    } else {
      np_dtype <- "float64"
    }
  }

  # ==========================================================
  # 🔁 Expression auto-conversion
  # ==========================================================
  if (is.list(expression)) {

    valid_structure <- all(vapply(expression, function(x) {
      is.numeric(x) && length(x) %in% c(1, 2)
    }, logical(1)))

    if (!valid_structure) {
      stop("Expression list must contain numeric vectors of length 1 or 2.",
           call. = FALSE)
    }

    expression <- expr_list_to_einstein(expression)

  } else if (!is.character(expression)) {
    stop("Expression must be either a character string or a nested list",
         call. = FALSE)
  }

  # ==========================================================
  # 🔁 Tensor auto-conversion
  # ==========================================================
  tensors <- lapply(tensors, function(x) {

    # Already a Python object → do not touch dtype
    if (inherits(x, "python.builtin.object")) {
      return(x)
    }

    if (!is.numeric(x)) {
      stop("All tensors must be numeric (vector, matrix, or array).",
           call. = FALSE)
    }

    if (is.null(dim(x))) {
      return(np$array(as.numeric(x), dtype = np_dtype))
    }

    return(np$array(x, dtype = np_dtype))
  })

  # ==========================================================
  # 🚀 Call Python ustat
  # ==========================================================
  result <- tryCatch({
    ustat_mod$ustat(
      tensors = tensors,
      expression = expression,
      average = average
    )
  }, error = function(e) {
    stop(
      "Error computing U-statistic: ", e$message, "\n",
      "Expression used: ", expression,
      call. = FALSE
    )
  })

  as.numeric(result)
}
