# Check ustats Python Environment Status

Reports whether Python and required modules for
[`ustat()`](https://cxy0714.github.io/U-Statistics-R/reference/ustat.md)
are available, including the detected PyTorch version and whether CUDA
(GPU acceleration) can be used.

## Usage

``` r
check_ustats_setup()
```

## Value

Invisibly returns TRUE if environment is ready

## Details

Note that with reticulate (\>= 1.41), calling this function may
initialize Python and trigger the automatic, one-time provisioning of
the declared Python dependencies if no Python environment is configured
yet (this can involve a sizeable download the first time).

## Examples

``` r
if (FALSE) { # \dontrun{
check_ustats_setup()
} # }
```
