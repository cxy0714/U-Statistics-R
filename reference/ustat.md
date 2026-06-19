# Compute a Higher-Order U-Statistic via Python

Computes a higher-order U-statistic from precomputed kernel tensors
using the Python package `u_stats`. This function serves as an R
interface and handles automatic data conversion via reticulate.

## Usage

``` r
ustat(
  tensors,
  expression,
  backend = c("torch", "numpy"),
  average = TRUE,
  dtype = NULL
)
```

## Arguments

- tensors:

  A list of numeric vectors, matrices, or arrays representing kernel
  evaluations. All tensors must have compatible dimensions.

- expression:

  Either a character string in Einstein notation or a list of numeric
  vectors of length 1 or 2 describing index structure.

- backend:

  Character string specifying the computation backend: `"torch"`
  (default) or `"numpy"`.

- average:

  Logical; if `TRUE` (default), return the averaged U-statistic.
  Otherwise returns the raw sum.

- dtype:

  Optional character string specifying numeric precision for tensors
  converted from R. Must be one of `"float32"` or `"float64"`. If `NULL`
  (default), precision is chosen automatically:

  - `float32` when using the Torch backend with CUDA available

  - `float64` otherwise

## Value

A numeric scalar containing the computed U-statistic.

## Details

The U-statistic structure can be specified using either:

- An Einstein summation string (e.g. `"ab,bc->"`), or

- A nested list of index vectors (e.g. `list(c(1,2), c(2,3))`)

This function requires a working Python environment with the `u_stats`
package installed. With reticulate (\>= 1.41) the required Python
packages are provisioned automatically the first time Python is used, so
no manual setup is needed in most cases. To create a persistent
environment instead (or to choose between the CPU-only and CUDA builds
of PyTorch), use
[`setup_ustats()`](https://cxy0714.github.io/U-Statistics-R/reference/setup_ustats.md);
use
[`check_ustats_setup()`](https://cxy0714.github.io/U-Statistics-R/reference/check_ustats_setup.md)
to verify the configuration.

R numeric objects are converted to NumPy arrays using the selected
precision. If Python tensors (e.g., Torch tensors) are supplied
directly, they are passed through unchanged.

## Examples

``` r
if (FALSE) { # \dontrun{
setup_ustats()

v1 <- runif(100)
H1 <- matrix(runif(100), 10, 10)
H2 <- matrix(runif(100), 10, 10)

ustat(list(H1, H2), "ab,bc->")
ustat(list(H1, H2), "ab,bc->", dtype = "float32")
ustat(list(H1, H2), "ab,bc->", dtype = NULL)  # auto precision
} # }
```
