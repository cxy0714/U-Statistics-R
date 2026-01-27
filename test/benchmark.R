devtools::load_all()

library(ustats)
library(reticulate)

cat("==== ustats GPU Benchmark ====\n\n")

setup_ustats(method = "conda", envname = "r-ustats", restart = TRUE, persist = TRUE)
check_ustats_setup()

torch <- import("torch")
cat("Torch CUDA available:", torch$cuda$is_available(), "\n\n")

set.seed(1)

# -------------------------------------------------
# 构造测试数据（可以调大 n 测压力）
# -------------------------------------------------
n <- 400   # GPU 可以试 8000 / 10000
cat("Matrix size:", n, "x", n, "\n\n")

H1 <- matrix(rnorm(n * n), n, n)
H2 <- matrix(rnorm(n * n), n, n)
H3 <- matrix(rnorm(n * n), n, n)

expr <- "ab,bc,cd->"

# -------------------------------------------------
# Torch (auto → GPU if available)
# -------------------------------------------------
cat("Running Torch backend (auto dtype)...\n")
t1 <- system.time({
  res_torch_gpu <- ustats(
    tensors = list(H1, H2, H3),
    expression = expr,
    backend = "torch",
    dtype = NULL   # 自动 float32(GPU) / float64(CPU)
  )
})
print(t1)
cat("Result (torch auto):", res_torch_gpu, "\n\n")

# -------------------------------------------------
# Torch forced CPU float64
# -------------------------------------------------
cat("Running Torch backend (CPU float64)...\n")
t2 <- system.time({
  res_torch_cpu <- ustats(
    tensors = list(H1, H2, H3),
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
  res_numpy <- ustats(
    tensors = list(H1, H2, H3),
    expression = expr,
    backend = "numpy",
    dtype = "float64"
  )
})
print(t3)
cat("Result (numpy):", res_numpy, "\n\n")

# -------------------------------------------------
# 数值差异对比
# -------------------------------------------------
cat("==== Result Differences ====\n")
cat("torch(GPU) vs torch(CPU):", abs(res_torch_gpu - res_torch_cpu), "\n")
cat("torch(GPU) vs numpy     :", abs(res_torch_gpu - res_numpy), "\n")
cat("torch(CPU) vs numpy     :", abs(res_torch_cpu - res_numpy), "\n")

cat("\n==== Speed Summary (seconds) ====\n")
print(rbind(
  torch_auto = t1[3],
  torch_cpu  = t2[3],
  numpy      = t3[3]
))

cat("\nDone.\n")
