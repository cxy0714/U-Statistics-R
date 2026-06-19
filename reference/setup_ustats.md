# Set Up Python Environment for ustats

Installs and configures the Python environment required to run
[`ustat()`](https://cxy0714.github.io/U-Statistics-R/reference/ustat.md),
including `u_stats`, `numpy`, and `torch`.

## Usage

``` r
setup_ustats(
  method = c("auto", "virtualenv", "conda", "system"),
  envname = "r-ustats",
  gpu = FALSE,
  restart = FALSE,
  persist = FALSE
)
```

## Arguments

- method:

  Installation method for Python:

  - `"auto"` (default): use existing Python or install Miniconda

  - `"virtualenv"`: create a virtual environment

  - `"conda"`: create a conda environment

  - `"system"`: use system Python

- envname:

  Name of the virtualenv/conda environment (default: `"r-ustats"`)

- gpu:

  Logical; if `FALSE` (default), install the CPU-only build of PyTorch
  from the official PyTorch wheel index
  (`https://download.pytorch.org/whl/cpu`). The CPU build is much
  smaller (roughly 200 MB instead of more than 2 GB with bundled CUDA
  libraries on Linux) and is sufficient for machines without an NVIDIA
  GPU. Set `gpu = TRUE` to install the default PyPI build of PyTorch,
  which includes CUDA support on Linux; for GPU builds on Windows, or
  for a specific CUDA version, see
  <https://pytorch.org/get-started/locally/>.

- restart:

  Logical; whether to restart the R session after setup

- persist:

  Logical; if `TRUE`, print the `RETICULATE_PYTHON` configuration line
  that you can add to your `.Rprofile` yourself to make the environment
  persist across sessions. The function never writes to your files
  (default: FALSE)

## Value

Invisibly returns `TRUE` if setup completed and the environment
verifies, `FALSE` otherwise.

## Details

**Most users do not need to call this function.** With reticulate (\>=
1.41), the Python dependencies declared by this package are provisioned
automatically in a cached environment the first time Python is used
(e.g. on the first call to
[`ustat()`](https://cxy0714.github.io/U-Statistics-R/reference/ustat.md)).
Call `setup_ustats()` only if you prefer a persistent, dedicated
environment, or if you want to control how PyTorch is installed (see the
`gpu` argument).

**Note:** PyTorch is strongly recommended. The NumPy backend is slower
and may be numerically less stable for higher-order U-statistics.

## Examples

``` r
if (FALSE) { # \dontrun{
setup_ustats()                # CPU-only PyTorch (small, default)
setup_ustats(gpu = TRUE)      # default PyPI PyTorch (CUDA on Linux)
setup_ustats(method = "conda", envname = "ustats-env")
} # }
```
