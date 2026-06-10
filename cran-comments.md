## Resubmission (0.1.4)

* Fixed a duplicated "Description:" prefix in the Description field of
  DESCRIPTION.
* Python dependencies are now declared via reticulate::py_require() (which
  only records requirements at load time; nothing is downloaded or
  installed on CRAN machines) and are provisioned automatically on the
  user's machine on first use. reticulate (>= 1.41) is now required.
* setup_ustats() gains a `gpu` argument and installs the CPU-only PyTorch
  build by default; it also now returns TRUE/FALSE invisibly as documented.
* Added a "Getting started" vignette documenting the installation paths.
  The vignette sets eval = FALSE globally because the code requires a
  Python runtime that is not available on build machines.

## Resubmission (0.1.3)

* Fixed NEWS.md filename casing (News.md -> NEWS.md) which caused a
  "Non-standard file/directory found at top level" NOTE on CRAN's
  Linux check servers.

## Resubmission (0.1.2)

* Quoted all software names in DESCRIPTION ('NumPy', 'PyTorch', 'CUDA')
  as requested by CRAN reviewer Uwe Ligges.

## Test environments

* Windows Server 2022 x64 (R-devel, x86_64-w64-mingw32) via win-builder
* Windows (R-devel) via rhub
* macOS arm64 and x86_64 (R-devel) via rhub
* Linux Ubuntu x86_64 (R-devel) via rhub

## R CMD check results

0 errors | 0 warnings | 1 note

### NOTE: New submission

This is the first submission of this package to CRAN. This is the only
NOTE reported by win-builder (R-devel) and rhub.

Depending on the spell-check dictionaries used, 'CUDA', 'NumPy' and
'PyTorch' in the DESCRIPTION may additionally be flagged; these are
established proper nouns (NVIDIA's CUDA platform, the NumPy library
<https://numpy.org> and the PyTorch framework <https://pytorch.org>)
and are listed in inst/WORDLIST.

## Comments on examples and tests

* All examples of exported functions are wrapped in \dontrun{} because
  they require a Python runtime with the 'u-stats', 'numpy' and 'torch'
  packages. These are not available on CRAN check machines, and running
  the examples would trigger a large (hundreds of MB) one-time download
  of Python dependencies, modify the user's file space, and exceed check
  time limits.
* For the same reason, all Python-dependent tests call skip_on_cran() in
  addition to runtime availability checks, so the test suite never
  initializes Python on CRAN machines. Pure-R functionality is tested
  unconditionally.

## Downstream dependencies

None. This is a new package.
