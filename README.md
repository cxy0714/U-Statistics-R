# ustats (R Package)

`ustats` provides an R interface to the Python package [`u-stats`](https://pypi.org/project/u-stats/) for computing **higher-order U-statistics** efficiently. Heavy numerical computation is handled in Python (via [`numpy.einsum`](https://numpy.org/doc/stable/reference/generated/numpy.einsum.html) and [`torch.einsum`](https://pytorch.org/docs/stable/generated/torch.einsum.html)), while R serves as a convenient front-end for data handling and statistical workflows.

---

## Background

This package accompanies our paper:

> [Xingyu Chen, Ruiqi Zhang, Lin Liu. On computing and the complexity of computing higher-order U-statistics, exactly. arXiv preprint arXiv:2508.12627, 2025.](https://arxiv.org/abs/2508.12627)

The underlying Python implementation is the PyPI package [`u-stats`](https://pypi.org/project/u-stats/) (source code: [U-Statistics-python](https://github.com/zrq1706/U-Statistics-python)). This R package is a lightweight interface built on [`reticulate`](https://rstudio.github.io/reticulate/).

---

## Installation

```r
# From CRAN (once accepted):
# install.packages("ustats")

# Development version from GitHub:
# install.packages("devtools")
devtools::install_github("cxy0714/U-Statistics-R")
```

The package needs **Python (>= 3.11)** with the Python packages `u-stats`, `numpy`, and `torch`. There are three ways to get them — pick the one that fits you:

### Option 1 — Do nothing (recommended)

With `reticulate` (>= 1.41), the Python dependencies are declared by the package and provisioned **automatically** the first time Python is needed. Just call `ustat()`:

```r
library(ustats)

H <- matrix(rnorm(100), 10, 10)
ustat(list(H, H), "ab,bc->")   # first call sets up Python automatically
```

The downloaded environment is cached and reused across sessions.

> **Note:** the first call downloads PyTorch. On Linux the default build
> bundles CUDA libraries (~2.5 GB); if you prefer a small CPU-only build
> or want full control, use Option 2 or 3.

### Option 2 — One-shot managed setup: `setup_ustats()`

Creates a persistent environment and installs all dependencies. By default it installs the **CPU-only** build of PyTorch (~200 MB instead of ~2.5 GB):

```r
library(ustats)
setup_ustats()                                     # CPU-only PyTorch (default)
setup_ustats(gpu = TRUE)                           # default PyPI PyTorch (CUDA on Linux)
setup_ustats(method = "virtualenv", envname = "r-ustats")
```

### Option 3 — Use your own Python/conda environment

If you already have an environment with a configured PyTorch (e.g. a specific CUDA version), just add `u-stats`:

```bash
pip install u-stats
```

and point reticulate to that environment **before Python initializes**:

```r
library(ustats)
reticulate::use_condaenv("your_env_name", required = TRUE)  # or use_virtualenv()
# Alternatively, set the RETICULATE_PYTHON environment variable
# (e.g. in .Rprofile or .Renviron) to the path of your python binary.
```

### Verify the setup

Whichever option you chose:

```r
check_ustats_setup()
#> === ustats Environment Status ===
#> [OK] Python: /path/to/python
#> [OK] u_stats available
#> [OK] NumPy available
#> [OK] PyTorch available (version 2.x, CUDA available)
```

See `vignette("ustats")` for a complete installation guide and troubleshooting tips.

---

## Quick Example

```r
library(ustats)

set.seed(1)
n <- 300

H1 <- rnorm(n)
H2 <- matrix(rnorm(n * n), n, n)
H3 <- rnorm(n)

# U-statistic defined by the Einstein summation "a,ab,bc,c->"
result <- ustat(
  tensors    = list(H1, H2, H2, H3),
  expression = "a,ab,bc,c->",
  backend    = "torch",  # falls back to numpy if torch is unavailable
  dtype      = NULL      # auto: float32 on GPU, float64 on CPU
)
print(result)
```

The structure can also be given as an index list instead of a string:

```r
ustat(list(H1, H2, H2, H3), list(1, c(1, 2), c(2, 3), 3))
```

---

## GPU acceleration

When PyTorch detects a CUDA GPU, `ustat(..., backend = "torch")` uses it automatically:

```r
torch <- reticulate::import("torch")
torch$cuda$is_available()
```

To get a CUDA-enabled PyTorch, run `setup_ustats(gpu = TRUE)` (Linux), or install a wheel matching your CUDA version from [pytorch.org](https://pytorch.org/get-started/locally/) into the environment reticulate uses (Option 3 above).
