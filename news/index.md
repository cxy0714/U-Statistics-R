# Changelog

## ustats (development version)

- Added a `CITATION` file: `citation("ustats")` now returns both the
  package reference and the paper describing the implemented methods
  (Chen, Zhang, and Liu, 2025, <doi:10.48550/arXiv.2508.12627>).
- Added a package-level help page, accessible via
  [`?ustats`](https://cxy0714.github.io/U-Statistics-R/reference/ustats-package.md)
  or
  [`package?ustats`](https://cxy0714.github.io/U-Statistics-R/reference/ustats-package.md),
  summarising the package and linking to its documentation site.

## ustats 0.1.5

CRAN release: 2026-06-18

- The DESCRIPTION now cites the paper describing the implemented
  methods: Chen, Zhang, and Liu (2025) <doi:10.48550/arXiv.2508.12627>.
- `setup_ustats(persist = TRUE)` no longer offers to write to the
  project `.Rprofile`. It now only prints the `RETICULATE_PYTHON` line
  for the user to add manually, so the package never writes to the
  user’s file space (CRAN policy).

## ustats 0.1.4

### Python environment handling

- Python dependencies (u-stats, numpy, torch) are now declared with
  [`reticulate::py_require()`](https://rstudio.github.io/reticulate/reference/py_require.html)
  in `.onLoad()`. With reticulate (\>= 1.41) they are provisioned
  automatically in a cached environment on first use, so most users need
  no manual setup at all. reticulate (\>= 1.41) is now required.
- [`setup_ustats()`](https://cxy0714.github.io/U-Statistics-R/reference/setup_ustats.md)
  gains a `gpu` argument. By default (`gpu = FALSE`) the much smaller
  CPU-only PyTorch build is installed from the official PyTorch wheel
  index; `gpu = TRUE` installs the default PyPI build (CUDA-enabled on
  Linux).
- [`check_ustats_setup()`](https://cxy0714.github.io/U-Statistics-R/reference/check_ustats_setup.md)
  now reports the detected PyTorch version and whether CUDA is
  available, and prints actionable installation hints when something is
  missing.
- Added a “Getting started” vignette with a complete installation guide
  (automatic setup,
  [`setup_ustats()`](https://cxy0714.github.io/U-Statistics-R/reference/setup_ustats.md),
  and bring-your-own-environment) and troubleshooting tips; the README
  installation section was rewritten accordingly.

### Bug fixes

- [`setup_ustats()`](https://cxy0714.github.io/U-Statistics-R/reference/setup_ustats.md)
  now invisibly returns `TRUE`/`FALSE` as documented (it previously
  returned `NULL` because the persistence step was unreachable dead code
  placed after the return value).
- Fixed a duplicated “Description:” prefix in the DESCRIPTION file.

### Internal

- Python-dependent tests are now additionally guarded with
  `skip_on_cran()`, so the test suite never initializes Python (or
  triggers automatic dependency downloads) on CRAN machines.
- `expr_list_to_einstein()` is internal and no longer generates a help
  page.

## ustats 0.1.3

- Fixed NEWS.md filename casing (News.md -\> NEWS.md).

## ustats 0.1.2

- Quoted software names (‘NumPy’, ‘PyTorch’, ‘CUDA’) in DESCRIPTION per
  CRAN policy.

## ustats 0.1.1

- Minor improvements and CRAN compliance fixes.

- Fixed issues identified during CRAN pre-checks, including:

  - Proper handling of external dependencies
  - Removal of non-ASCII characters
  - Namespace and import adjustments
  - Compliance with CRAN policies on user system modifications

## ustats 0.1.0

- Initial CRAN release.
