# ustats (R Package)

`ustats` provides an R interface to our high-performance Python implementation for computing **higher-order U-statistics**. Heavy numerical computation is handled in Python (via [`numpy.einsum`](https://numpy.org/doc/stable/reference/generated/numpy.einsum.html) and [`torch.einsum`](https://pytorch.org/docs/stable/generated/torch.einsum.html)), while R serves as a convenient front-end for data handling and statistical workflows.

---

##  Background

This package accompanies our paper:

> [Xingyu Chen, Ruiqi Zhang, Lin Liu. On computing and the complexity of computing higher-order U-statistics, exactly.arXiv preprint arXiv:2508.12627, 2025.](https://arxiv.org/abs/2508.12627)  

The underlying Python implementation is available in `PyPi` named [`u-stats-python`](https://github.com/zrq1706/U-Statistics-python/tree/main)
:

This R package is a lightweight interface built using `reticulate` to call this backend efficiently.

---

##  Installation (Current)

This package is not yet on CRAN. Install locally:


```r
# install.packages("devtools")
devtools::install_github("cxy0714/U-Statistics-R")
```

The package requires Python ≥ 3.11 and the following Python packages:

- u-stats

- numpy

- torch 

If you already have a conda environment with PyTorch configured, you can activate it manually
```r
library(ustats)
library(reticulate)
use_condaenv("your_env_name", required = TRUE)
# Ensure Python is initialized
py_config()
setup_ustats()
check_ustats_setup()
```


## Quick Example


```r
devtools::load_all()

library(ustats)
library(reticulate)

# Ensure Python is initialized
py_config()
# setup_ustats()
check_ustats_setup()

torch <- import("torch")
cat("Torch CUDA available:", torch$cuda$is_available(), "\n\n")

set.seed(1)

n <- 4000
cat("Matrix size:", n, "x", n, "\n\n")

H1 <- rnorm(n)
H2 <- matrix(rnorm(n * n), n, n)
H3 <- rnorm(n)

tensors <- list(H1, H2, H2, H3)
expr <- "a,ab,bc,c->"

ustats(
  tensors = tensors,
  expression = expr,
  backend = "torch",
  dtype = NULL  # auto float32(GPU) / float64(CPU)
)

```

Also see a benchmarking example script here:  
[`test/manual/benchmark.R`](test/manual/benchmark.R)
