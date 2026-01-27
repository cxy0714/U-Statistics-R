devtools::load_all()

library(ustats)
library(reticulate)

# use_condaenv("r_env", required = TRUE)
# If the condaenv already exist and have python

# Manually trigger Python initialization once so py_available can detect it
py_config() # or import("sys")$version, etc.

cat("==== ustats Benchmark ====\n\n")

# setup_ustats()
check_ustats_setup()

torch <- import("torch")
cat("Torch CUDA available:", torch$cuda$is_available(), "\n\n")


# -------------------------------------------------
# Generate test data
# -------------------------------------------------
set.seed(1)

n <- 4000
cat("Matrix size:", n, "x", n, "\n\n")

H1 <- rnorm(n)
H2 <- matrix(rnorm(n * n), n, n)
H3 <- rnorm(n)
tensors <- list(H1, H2, H2, H3)
expr <- "a,ab,bc,c->"

# -------------------------------------------------
# Direct calculation of the ground truth value.
# -------------------------------------------------

A <- sweep(H2, 1, H1, "*")
AA <- sweep(H2, 2, H3, "*")
diag(A) <- 0
diag(AA) <- 0
AAA <- A %*% AA
true <- (sum(AAA) - sum(diag(AAA))) / (n * (n - 1) * (n - 2))

# -------------------------------------------------
# Warm up for torch
# -------------------------------------------------

ustat(
  tensors = tensors,
  expression = expr,
  backend = "torch",
  dtype = NULL # auto float32(GPU) / float64(CPU)
)

# -------------------------------------------------
# Torch (auto → GPU if available)
# -------------------------------------------------
cat("Running Torch backend (auto dtype)...\n")
t1 <- system.time({
  res_torch_gpu <- ustat(
    tensors = tensors,
    expression = expr,
    backend = "torch",
    dtype = NULL # auto float32(GPU) / float64(CPU)
  )
})
print(t1)
cat("Result (torch auto):", res_torch_gpu, "\n\n")

# -------------------------------------------------
# Torch forced CPU float64
# -------------------------------------------------
cat("Running Torch backend (CPU float64)...\n")
t2 <- system.time({
  res_torch_cpu <- ustat(
    tensors = tensors,
    expression = expr,
    backend = "torch",
    dtype = "float64"
  )
})
print(t2)
cat("Result (torch cpu):", res_torch_cpu, "\n\n")

# -------------------------------------------------
# NumPy float64
# -------------------------------------------------
cat("Running NumPy backend (float64)...\n")
t3 <- system.time({
  res_numpy <- ustat(
    tensors = tensors,
    expression = expr,
    backend = "numpy",
    dtype = "float64"
  )
})
print(t3)
cat("Result (numpy):", res_numpy, "\n\n")

# -------------------------------------------------
# Numerical difference comparison
# -------------------------------------------------
cat("==== Result Differences ====\n")
cat("torch(auto_dection) vs true:", abs(res_torch_gpu - true), "\n")
cat("torch(float64) vs true:", abs(res_torch_cpu - true), "\n")
cat("numpy vs true:", abs(res_numpy - true), "\n")

cat("\n==== Speed Summary (seconds) ====\n")
print(rbind(
  torch_auto = t1[3],
  torch_cpu  = t2[3],
  numpy      = t3[3]
))

cat("\nDone.\n")
