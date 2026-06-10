#' Python Environment Setup Utilities for ustats
#'
#' Helper functions for configuring and verifying the Python environment
#' required by \code{ustat()}.
#'
#' These functions install and validate Python dependencies including
#' \code{u_stats}, \code{numpy}, and \code{torch} (recommended for numerical
#' stability and performance).
#'
#' @author Xingyu Chen


#' Check Python and u_stats availability
#'
#' Internal helper that checks whether Python and the required
#' \code{u_stats} module are available.
#'
#' @return Logical scalar
#' @keywords internal
check_python_env <- function() {
  if (!is.null(ustats_env$python_available)) {
    return(ustats_env$python_available && ustats_env$ustats_available)
  }

  py_ok <- tryCatch({
    reticulate::py_available(initialize = TRUE)
  }, error = function(e) FALSE)

  ustats_env$python_available <- py_ok

  if (!py_ok) {
    ustats_env$ustats_available <- FALSE
    return(FALSE)
  }

  ustats_ok <- tryCatch({
    reticulate::py_module_available("u_stats")
  }, error = function(e) FALSE)

  ustats_env$ustats_available <- ustats_ok

  py_ok && ustats_ok
}



#' Set Up Python Environment for ustats
#'
#' Installs and configures the Python environment required to run
#' \code{ustat()}, including \code{u_stats}, \code{numpy}, and
#' \code{torch}.
#'
#' \strong{Most users do not need to call this function.} With
#' \pkg{reticulate} (>= 1.41), the Python dependencies declared by this
#' package are provisioned automatically in a cached environment the first
#' time Python is used (e.g. on the first call to \code{ustat()}). Call
#' \code{setup_ustats()} only if you prefer a persistent, dedicated
#' environment, or if you want to control how PyTorch is installed (see
#' the \code{gpu} argument).
#'
#' \strong{Note:} PyTorch is strongly recommended. The NumPy backend is slower
#' and may be numerically less stable for higher-order U-statistics.
#'
#' @param method Installation method for Python:
#'   \itemize{
#'     \item \code{"auto"} (default): use existing Python or install Miniconda
#'     \item \code{"virtualenv"}: create a virtual environment
#'     \item \code{"conda"}: create a conda environment
#'     \item \code{"system"}: use system Python
#'   }
#' @param envname Name of the virtualenv/conda environment (default: \code{"r-ustats"})
#' @param gpu Logical; if \code{FALSE} (default), install the CPU-only build
#'   of PyTorch from the official PyTorch wheel index
#'   (\verb{https://download.pytorch.org/whl/cpu}). The CPU build is much
#'   smaller (roughly 200 MB instead of more than 2 GB with bundled CUDA
#'   libraries on Linux) and is sufficient for machines without an NVIDIA
#'   GPU. Set \code{gpu = TRUE} to install the default PyPI build of
#'   PyTorch, which includes CUDA support on Linux; for GPU builds on
#'   Windows, or for a specific CUDA version, see
#'   \url{https://pytorch.org/get-started/locally/}.
#' @param restart Logical; whether to restart the R session after setup
#' @param persist Logical; whether to attempt persisting the configuration
#'   by adding RETICULATE_PYTHON to the project-level .Rprofile (default: FALSE)
#'
#' @return Invisibly returns \code{TRUE} if setup completed and the
#'   environment verifies, \code{FALSE} otherwise.
#'
#' @examples
#' \dontrun{
#' setup_ustats()                # CPU-only PyTorch (small, default)
#' setup_ustats(gpu = TRUE)      # default PyPI PyTorch (CUDA on Linux)
#' setup_ustats(method = "conda", envname = "ustats-env")
#' }
#' @export
setup_ustats <- function(method = c("auto", "virtualenv", "conda", "system"),
                         envname = "r-ustats",
                         gpu = FALSE,
                         restart = FALSE,
                         persist = FALSE) {

  method <- match.arg(method)

  message("=== ustats Python Environment Setup ===\n")

  # ----------------------------------------------------------
  # Step 1: Ensure Python exists
  # ----------------------------------------------------------
  if (method == "auto") {
    if (reticulate::py_available(initialize = FALSE)) {
      py_config <- reticulate::py_config()
      message("[OK] Found existing Python: ", py_config$python)
    } else {
      # Must ask user before installing anything on their system (CRAN policy)
      if (!interactive()) {
        stop(
          "No Python found and session is non-interactive. ",
          "Please install Python manually, then call setup_ustats() again.",
          call. = FALSE
        )
      }
      ans <- readline("No Python found. Install Miniconda now? [y/N]: ")
      if (!tolower(trimws(ans)) %in% c("y", "yes")) {
        message("Aborted. Please install Python manually and try again.")
        return(invisible(FALSE))
      }
      message("Installing Miniconda (may take a few minutes)...")
      tryCatch({
        reticulate::install_miniconda()
        message("[OK] Miniconda installed")
      }, error = function(e) {
        stop("Failed to install Miniconda: ", e$message, call. = FALSE)
      })
    }
  } else if (method == "virtualenv") {
    message("Creating virtualenv: ", envname)
    reticulate::virtualenv_create(envname, version = ">=3.11")
    reticulate::use_virtualenv(envname, required = TRUE)

  } else if (method == "conda") {
    message("Creating conda environment: ", envname)
    reticulate::conda_create(envname, python_version = "3.11")
    reticulate::use_condaenv(envname, required = TRUE)

  } else if (method == "system") {
    if (!reticulate::py_available(initialize = TRUE)) {
      stop("System Python not found. Please install Python first.", call. = FALSE)
    }
    message("[OK] Using system Python")
  }

  # ----------------------------------------------------------
  # Step 2: Install required Python packages
  # ----------------------------------------------------------
  message("\nInstalling required Python packages...")

  core_pkgs <- c("u-stats", "numpy")

  tryCatch({
    reticulate::py_install(core_pkgs, pip = TRUE)
    message("[OK] Installed: ", paste(core_pkgs, collapse = ", "))
  }, error = function(e) {
    warning("Package installation issue: ", e$message, call. = FALSE)
    message("Try manual install:\n  pip install u-stats numpy")
  })

  # PyTorch is installed separately: by default the CPU-only wheel index is
  # used, because the default PyPI build bundles CUDA libraries on Linux
  # (> 2 GB download) that are useless on machines without an NVIDIA GPU.
  if (isTRUE(gpu)) {
    message("Installing PyTorch (default PyPI build; includes CUDA on Linux)...")
    torch_pip_options <- character()
  } else {
    message("Installing PyTorch (CPU-only build; use gpu = TRUE for CUDA)...")
    torch_pip_options <- c("--index-url", "https://download.pytorch.org/whl/cpu")
  }

  tryCatch({
    reticulate::py_install("torch", pip = TRUE, pip_options = torch_pip_options)
    message("[OK] Installed: torch")
  }, error = function(e) {
    warning("PyTorch installation issue: ", e$message, call. = FALSE)
    message(
      "Try manual install:\n  pip install torch",
      if (!isTRUE(gpu)) " --index-url https://download.pytorch.org/whl/cpu"
    )
  })

  # ----------------------------------------------------------
  # Step 3: Verify
  # ----------------------------------------------------------
  message("\n=== Verifying Installation ===")

  ustats_env$python_available <- NULL
  ustats_env$ustats_available <- NULL

  ok <- check_python_env()

  if (ok) {
    message("[OK] Python and u_stats are ready")

    if (reticulate::py_module_available("torch")) {
      message("[OK] PyTorch available (recommended backend)")
    } else {
      message("[WARN] PyTorch not detected -- computations may be slower and less stable")
    }

    message("\n Setup complete! You can now call ustat().")

  } else {
    warning("Verification failed. A restart may be required.", call. = FALSE)
  }

  # Persist configuration if requested and conditions are met
  if (ok && method %in% c("virtualenv", "conda") && isTRUE(persist)) {
    persist_project_config()
  }

  if (restart) {
    message("\nRestarting R session...")
    if (requireNamespace("rstudioapi", quietly = TRUE)) {
      rstudioapi::restartSession()
    } else {
      message("Please restart R manually.")
    }
  }

  invisible(ok)
}

persist_project_config <- function() {
  # Get the current Python executable path (reliable after use_*())
  py_path <- normalizePath(reticulate::py_config()$python, winslash = "/", mustWork = TRUE)

  # Project-level .Rprofile (in current working directory)
  project_rprofile <- file.path(getwd(), ".Rprofile")

  # Avoid writing if current directory is home or root (safety check)
  home_dir <- normalizePath(Sys.getenv("HOME"), winslash = "/")
  if (identical(normalizePath(getwd(), winslash = "/"), home_dir)) {
    message("Current working directory is home directory. Skipping project-level .Rprofile write.")
    message("To persist, manually add to ~/.Rprofile:")
    message(sprintf('Sys.setenv(RETICULATE_PYTHON = "%s")', py_path))
    return(invisible())
  }

  # Non-interactive session: skip automatic write
  if (!interactive()) {
    message("Non-interactive session. Skipping automatic .Rprofile write.")
    message(sprintf("Manually add to .Rprofile in this project:\nSys.setenv(RETICULATE_PYTHON = \"%s\")", py_path))
    return(invisible())
  }

  # Ask user for confirmation
  cat("\nSetup successful! Would you like to make this Python environment persistent for the current project?\n")
  cat("This will add a line to .Rprofile in the current working directory.\n")
  choice <- utils::menu(
    c("Yes, add to project .Rprofile", "No, skip"),
    title = "Persist configuration?"
  )

  if (choice != 1L) {
    message("Skipping persistence. You can manually add:")
    message(sprintf('  Sys.setenv(RETICULATE_PYTHON = "%s")', py_path))
    return(invisible())
  }

  # Backup existing file if it exists
  if (file.exists(project_rprofile)) {
    backup_path <- sprintf("%s.bak-%s", project_rprofile, format(Sys.time(), "%Y%m%d-%H%M%S"))
    file.copy(project_rprofile, backup_path)
    message(sprintf("Backed up existing .Rprofile to: %s", basename(backup_path)))
  }

  # Read existing lines or start empty
  if (file.exists(project_rprofile)) {
    lines <- readLines(project_rprofile, warn = FALSE)
  } else {
    lines <- character()
  }

  # Comment out any previous RETICULATE_PYTHON lines to avoid conflicts
  lines <- gsub(
    "^[[:space:]]*Sys\\.setenv\\([[:space:]]*RETICULATE_PYTHON.*$",
    "# \\0   # commented by ustats setup (previous setting)",
    lines
  )

  # Append new configuration with clear comments
  new_lines <- c(
    lines,
    "",
    "# === ustats configuration: persistent Python environment ===",
    paste0("# Added by setup_ustats() on ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    sprintf('Sys.setenv(RETICULATE_PYTHON = "%s")', py_path),
    "# This ensures reticulate uses the dedicated environment for ustats (with torch, etc.)",
    "# To change or disable: comment out or delete this line",
    "# ====================================================="
  )

  # Write to file
  writeLines(new_lines, project_rprofile)
  message(sprintf("\nSuccessfully added configuration to project .Rprofile: %s", project_rprofile))
  message("Please restart R / RStudio for the change to take effect in new sessions.")
}


#' Check ustats Python Environment Status
#'
#' Reports whether Python and required modules for \code{ustat()} are
#' available, including the detected PyTorch version and whether CUDA
#' (GPU acceleration) can be used.
#'
#' Note that with \pkg{reticulate} (>= 1.41), calling this function may
#' initialize Python and trigger the automatic, one-time provisioning of
#' the declared Python dependencies if no Python environment is configured
#' yet (this can involve a sizeable download the first time).
#'
#' @return Invisibly returns TRUE if environment is ready
#'
#' @examples
#' \dontrun{
#' check_ustats_setup()
#' }
#' @export
check_ustats_setup <- function() {
  message("=== ustats Environment Status ===\n")

  py_ok <- tryCatch(reticulate::py_available(initialize = TRUE), error = function(e) FALSE)

  if (!py_ok) {
    message("[FAIL] Python not available")
    message("Run setup_ustats() to install dependencies.")
    return(invisible(FALSE))
  }

  py_config <- reticulate::py_config()
  message("[OK] Python: ", py_config$python)
  message("  Version: ", py_config$version)

  ustats_ok <- reticulate::py_module_available("u_stats")
  numpy_ok  <- reticulate::py_module_available("numpy")
  torch_ok  <- reticulate::py_module_available("torch")

  message(if (ustats_ok) "[OK] u_stats available" else "[FAIL] u_stats missing")
  message(if (numpy_ok)  "[OK] NumPy available"  else "[FAIL] NumPy missing")

  if (torch_ok) {
    torch_info <- tryCatch({
      torch <- reticulate::import("torch")
      cuda_ok <- tryCatch(isTRUE(torch$cuda$is_available()),
                          error = function(e) FALSE)
      paste0("version ", torch$`__version__`,
             if (cuda_ok) ", CUDA available" else ", CPU only")
    }, error = function(e) NULL)

    message("[OK] PyTorch available",
            if (!is.null(torch_info)) paste0(" (", torch_info, ")"))
  } else {
    message("[FAIL] PyTorch not installed -- computations may be slow and less stable")
    message("  Install it with setup_ustats(), or manually:")
    message("    pip install torch --index-url https://download.pytorch.org/whl/cpu")
  }

  message("\n---------------------------------")

  basic_ok <- py_ok && ustats_ok && numpy_ok
  full_ok  <- basic_ok && torch_ok

  if (full_ok) {
    message(" Environment fully ready (Torch backend available)")
  } else if (basic_ok) {
    message("[OK] Basic environment ready, but Torch is strongly recommended")
  } else {
    message("[FAIL] Setup incomplete. Run setup_ustats().")
  }

  invisible(full_ok)
}


