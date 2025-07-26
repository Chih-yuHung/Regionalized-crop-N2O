# ---------- 1. USER-EDITABLE INPUT  ---------------------------------------
n_sim <- 10         # final Monte-Carlo size

bounds <- tribble(
  ~var,            ~low,  ~high,   # multiplicative factor range
  "Fert_LCA_kg",     0.9,   1.10,  #  –10 % … +10 %
  "P",               0.9,   1.10,
  "PE",              0.9,   1.10,
  "Topo",            0.9,   1.10,
  "Frac_Fine",       0.8,   1.20,  # slightly wider so Dirichlet has room
  "Frac_Medium",     0.8,   1.20,
  "Frac_Coarse",     0.8,   1.20,
  "coef1",           0.9,   1.10,
  "coef2",           0.9,   1.10,
  "NSEF",            0.6,   1.60   # Peltster study: ±60 %
)




library(lhs);  library(tidyverse)

scalar_vars <- bounds$var[!str_detect(bounds$var, "Frac_")]
u_scalar    <- randomLHS(n_sim, length(scalar_vars))   # ∈ (0,1)

# 2a. scalar variables (identical to the fixed version we just did)
scalar_df <- map2_dfc(
  as.data.frame(u_scalar), scalar_vars,
  ~ { b <- filter(bounds, var == .y)
  tibble(!!.y := b$low + .x * (b$high - b$low)) }
)

# 2b. three texture fractions – NO renormalisation here
u_frac   <- randomLHS(n_sim, 3)
frac_df  <- map2_dfc(
  as_tibble(u_frac),                     # still in [0,1]
  c("Frac_Fine","Frac_Medium","Frac_Coarse"),
  ~ { b <- filter(bounds, var == .y)
  tibble(!!.y := b$low + .x * (b$high - b$low)) }
)

# final Δ-factor table
lhs_df <- tibble(sim_id = 1:n_sim) %>% bind_cols(scalar_df, frac_df)
