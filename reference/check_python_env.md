# Python Environment Setup Utilities for ustats

Helper functions for configuring and verifying the Python environment
required by
[`ustat()`](https://cxy0714.github.io/U-Statistics-R/reference/ustat.md).

## Usage

``` r
check_python_env()
```

## Value

Logical scalar

## Details

These functions install and validate Python dependencies including
`u_stats`, `numpy`, and `torch` (recommended for numerical stability and
performance).

## Author

Xingyu Chen Check Python and u_stats availability

Internal helper that checks whether Python and the required `u_stats`
module are available.
