## Resubmission (0.1.2)

* Quoted all software names in DESCRIPTION ('NumPy', 'PyTorch', 'CUDA')
  as requested by CRAN reviewer Uwe Ligges.
  
## Test environments

* Windows (R-devel, x86_64-w64-mingw32) via rhub
* macOS arm64 / aarch64-apple-darwin23 (R-devel) via rhub
* Linux Ubuntu 24.04 x86_64 (R-devel) via rhub
* Linux Debian x86_64 (R-devel) via CRAN incoming pretest

## R CMD check results

0 errors | 0 warnings | 3 notes

### NOTE 1: New submission

This is the first submission of this package to CRAN.

### NOTE 2: Possibly misspelled words in DESCRIPTION: CUDA, NumPy

These are not misspelled. 'NumPy' and 'CUDA' are established proper nouns
referring to the NumPy scientific computing library (<https://numpy.org>)
and NVIDIA's CUDA parallel computing platform respectively. Both have been
added to inst/WORDLIST.

### NOTE 3: CPU time / elapsed time ratio in tests

The ratio arises from Python and reticulate initialization during the test
suite. All tests that require Python are guarded with skip_if_not() and
will be skipped on CRAN check servers where Python is not available.
The thread count has been limited via OMP_NUM_THREADS, OPENBLAS_NUM_THREADS,
and MKL_NUM_THREADS to reduce this ratio.

## Changes made during pre-submission checks (rhub )

* Removed non-ASCII characters (Unicode symbols and emoji) from R/setup.R
  and R/ustat.R
* Added `Suggests: rstudioapi, testthat (>= 3.0.0)` to DESCRIPTION
* Added `importFrom(stats, setNames)` via roxygen2 to NAMESPACE
* Fixed bare `conda_create()` call to `reticulate::conda_create()`
* Added `^\.github$` to .Rbuildignore to exclude CI configuration
* Added user confirmation prompt before installing Miniconda in
  setup_ustats() to comply with CRAN policy on modifying user systems
* Added `inst/WORDLIST` with CUDA and NumPy
* Added `Language: en-US` to DESCRIPTION
* Renamed News.md to NEWS.md
* Added thread limit environment variables in tests/testthat.R
* Added URL and BugReports fields to DESCRIPTION
* Added testthat test suite covering pure-R functions and
  Python-dependent functions (with skip_if_not guards)

## Downstream dependencies

None. This is a new package.
