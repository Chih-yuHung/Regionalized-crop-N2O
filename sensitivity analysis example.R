# File: sobol_sa.R
library(tidyverse)
library(sensitivity)

# ---------- 1. Define the model ----------
# vector of parameter names in **exact** sampling order
param_names <- c("ef_n2o", "n_fert", "co2_plant", "eta_conv", "e_trans")

ghgi_model <- function(X) {
  ## --- always work with a matrix -----------------------------------------
  X <- as.matrix(X)                      # catches data frame or vector
  if (is.null(colnames(X)))              # <- happens inside sobolSalt
    colnames(X) <- param_names           # restore names
  
  ## --- algebra ------------------------------------------------------------
  a <- X[, "ef_n2o"]  * X[, "n_fert"]
  b <- X[, "co2_plant"] / X[, "eta_conv"]
  c <- X[, "e_trans"]
  
  a + b + c                              # vectorised result
}
# ---------- 2. Describe input distributions ----------
k      <- 5
N      <- 2000                # → (k+2)*N = 14 000 model runs
dists  <- list(
  ef_n2o   = function(n) triangle::rtriangle(n, 0.005, 0.025, 0.015),
  n_fert   = function(n) rnorm(n, mean = 140, sd = 20),
  co2_plant = function(n) runif(n, 1600, 2000),
  eta_conv  = function(n) rbeta(n, 25, 30),
  e_trans   = function(n) rlnorm(n, log(4), 0.15)
)

# helper to sample an N × k matrix
sample_matrix <- function(n) purrr::map_dfc(dists, ~ .x(n)) |>
  set_names(names(dists)) |>
  as.matrix()

A <- sample_matrix(N)
B <- sample_matrix(N)

# ---------- 3. Run Sobol' SA ----------
Y <- ghgi_model(rbind(A, B))              # (k+2)*N rows
sob <- sobolSalt(model = NULL, X1 = A, X2 = B, y = Y, nboot = 100)


sob <- sobolSalt(model = ghgi_model, X1 = A, X2 = B, nboot = 100)

# ---------- 4. Summarise & plot ----------
print(sob$S)   # first-order
#X means the parameters and the values are the % of contribution to the uncertainties. 
#The difference of S and T suggests interactions. Small difference means interaction negligible. 


print(sob$T)   # total
plot(sob, main = "Sobol' first- & total-order indices")

# Save tidy summary for Snakemake downstream rule
sob_df <- tibble(param = names(dists),
                 S  = sob$S$original,
                 T  = sob$T$original)
write_csv(sob_df, "results/sobol_indices.csv")
