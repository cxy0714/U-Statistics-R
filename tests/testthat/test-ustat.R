# tests/testthat/test-ustat.R

# ----------------------------------------------------------------
# expr_list_to_einstein: pure R, no Python needed, always runs
# ----------------------------------------------------------------

test_that("expr_list_to_einstein: basic two-tensor chain", {
  result <- ustats:::expr_list_to_einstein(list(c(1, 2), c(2, 3)))
  expect_equal(result, "ab,bc->")
})

test_that("expr_list_to_einstein: three-tensor chain", {
  result <- ustats:::expr_list_to_einstein(list(c(1, 2), c(2, 3), c(3, 4)))
  expect_equal(result, "ab,bc,cd->")
})

test_that("expr_list_to_einstein: rank-1 tensor (single index)", {
  result <- ustats:::expr_list_to_einstein(list(c(1), c(1, 2)))
  expect_equal(result, "a,ab->")
})

test_that("expr_list_to_einstein: non-contiguous indices are remapped", {
  # Indices 1, 3, 5 should map to a, b, c
  result <- ustats:::expr_list_to_einstein(list(c(1, 3), c(3, 5)))
  expect_equal(result, "ab,bc->")
})

test_that("expr_list_to_einstein: errors on too many unique indices", {
  # 27 unique indices exceeds the 26-letter limit
  big_list <- lapply(1:27, function(i) c(i))
  expect_error(
    ustats:::expr_list_to_einstein(big_list),
    "Too many unique indices"
  )
})

test_that("expr_list_to_einstein: errors on rank-3 tensor", {
  expect_error(
    ustats:::expr_list_to_einstein(list(c(1, 2, 3))),
    "Only rank-1 or rank-2 tensors supported"
  )
})

# ----------------------------------------------------------------
# ustat: requires Python + u_stats, skip if not available
# ----------------------------------------------------------------

skip_msg <- "Python/u_stats not available"

has_ustats <- function() {
  reticulate::py_available(initialize = TRUE) &&
    reticulate::py_module_available("u_stats") &&
    reticulate::py_module_available("numpy")
}

test_that("ustat: basic matrix contraction with numpy backend", {
  skip_if_not(has_ustats(), skip_msg)

  set.seed(42)
  H <- matrix(rnorm(100), 10, 10)
  result <- ustat(list(H, H), "ab,bc->", backend = "numpy")

  expect_length(result, 1)
  expect_true(is.numeric(result))
  expect_false(is.nan(result))
})

test_that("ustat: expression as list produces same result as string", {
  skip_if_not(has_ustats(), skip_msg)

  set.seed(42)
  H <- matrix(rnorm(100), 10, 10)

  r_string <- ustat(list(H, H), "ab,bc->", backend = "numpy")
  r_list   <- ustat(list(H, H), list(c(1, 2), c(2, 3)), backend = "numpy")

  expect_equal(r_string, r_list)
})

test_that("ustat: average=FALSE returns larger value than average=TRUE", {
  skip_if_not(has_ustats(), skip_msg)

  set.seed(1)
  H <- matrix(abs(rnorm(100)), 10, 10)  # positive entries

  avg   <- ustat(list(H), "ab->", backend = "numpy", average = TRUE)
  total <- ustat(list(H), "ab->", backend = "numpy", average = FALSE)

  expect_gt(abs(total), abs(avg))
})

test_that("ustat: dtype float32 vs float64 give close results", {
  skip_if_not(has_ustats(), skip_msg)

  set.seed(7)
  H <- matrix(rnorm(100), 10, 10)

  r32 <- ustat(list(H), "ab->", backend = "numpy", dtype = "float32")
  r64 <- ustat(list(H), "ab->", backend = "numpy", dtype = "float64")

  expect_equal(r32, r64, tolerance = 1e-4)
})

test_that("ustat: errors on invalid dtype", {
  skip_if_not(has_ustats(), skip_msg)

  H <- matrix(1:4, 2, 2)
  expect_error(
    ustat(list(H), "ab->", dtype = "float16"),
    "dtype must be"
  )
})

test_that("ustat: errors on non-numeric tensor", {
  skip_if_not(has_ustats(), skip_msg)

  expect_error(
    ustat(list(matrix("a", 2, 2)), "ab->"),
    "numeric"
  )
})

test_that("ustat: errors without Python setup", {
  # This test mocks the check_python_env to return FALSE
  # to verify the error message is correct
  local_mocked_bindings(
    check_python_env = function() FALSE,
    .package = "ustats"
  )
  expect_error(
    ustat(list(matrix(1:4, 2, 2)), "ab->"),
    "setup_ustats"
  )
})
