## Resubmission (0.1.5)

This version addresses all remarks from the manual review of 0.1.3
(Benjamin Altmann, 2026-03-24):

* **References in DESCRIPTION:** the Description field now cites the
  paper describing the implemented methods, in the requested form:
  Chen, Zhang, and Liu (2025) <doi:10.48550/arXiv.2508.12627>.

* **Writing to the user's home filespace (R/setup.R):** the package no
  longer writes to the user's file space at all. The previous option of
  setup_ustats() to append RETICULATE_PYTHON to a project .Rprofile
  (in getwd()) has been removed; with persist = TRUE the function now
  only prints the configuration line for the user to add manually.
  Installing Miniconda still requires explicit interactive confirmation,
  and virtual/conda environments are only created when the user
  explicitly calls setup_ustats(). Examples, tests and vignettes do not
  write anywhere (the vignette code is not evaluated).

* **\dontrun{} in examples:** the example of the unexported pure-R
  helper (which was executable) has been removed together with its help
  page. The remaining examples of the three exported functions
  (ustat(), setup_ustats(), check_ustats_setup()) are kept in
  \dontrun{} because they really cannot be executed without additional
  software: they require a Python installation with the Python packages
  'u-stats', 'numpy' and 'torch'. Since the package declares these via
  reticulate::py_require(), running the examples (e.g. under
  \donttest{} with --run-donttest) would trigger an automatic download
  and installation of several hundred MB of Python dependencies on the
  check machines and exceed check time limits.

* **Examples for unexported functions:** fixed. expr_list_to_einstein()
  is internal and no longer has a help page or examples
  (expr_list_to_einstein.Rd was removed).

Note: this submission supersedes the 0.1.4 upload of 2026-06-10, which
was submitted before we had re-checked the 0.1.3 review and did not yet
address the first two remarks above. Please review only 0.1.5; apologies
for the extra upload.

Other changes since the reviewed 0.1.3 (also in NEWS.md): Python
dependencies are declared via reticulate::py_require() and provisioned
automatically on first use (reticulate >= 1.41 is now required; nothing
is downloaded at load time on check machines); setup_ustats() gains a
gpu argument and installs the CPU-only PyTorch build by default; a
"Getting started" vignette documents installation; a duplicated
"Description:" prefix in DESCRIPTION was fixed; Python-dependent tests
additionally call skip_on_cran(), so the test suite never initializes
Python on CRAN machines.

## Earlier resubmissions

* 0.1.3: fixed NEWS.md filename casing (News.md -> NEWS.md).
* 0.1.2: quoted software names in DESCRIPTION ('NumPy', 'PyTorch',
  'CUDA'), as requested by CRAN reviewer Uwe Ligges.

## Test environments

* Windows Server 2022 x64 (R-devel, x86_64-w64-mingw32) via win-builder
* Windows (R-devel) via rhub
* macOS arm64 (R-devel) via rhub
* Linux Ubuntu x86_64 (R-devel) via rhub

## R CMD check results

0 errors | 0 warnings | 1 note

### NOTE: New submission

This is the first submission of this package to CRAN. This was the only
NOTE reported by win-builder (R-devel) and rhub.

Depending on the spell-check dictionaries used, the author names
('Chen', 'Zhang', 'Liu') and 'CUDA', 'NumPy', 'PyTorch' in the
DESCRIPTION may additionally be flagged; these are proper nouns and are
listed in inst/WORDLIST.

## Downstream dependencies

None. This is a new package.
