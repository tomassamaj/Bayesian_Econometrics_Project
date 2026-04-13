# ============================================================
# BE_Group3_Code.R
# Bayesian VAR Analysis: Global Volatility Transmission
# Group 3 - Bayesian Econometrics, WU Vienna
# ============================================================


# 0. SETUP ---------------------------------------------------

library(BVAR)       # Bayesian VAR with hierarchical prior selection
library(BVARverse)  # ggplot2 wrappers for BVAR objects
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)  # combine ggplot2 panels
library(coda)      # MCMC diagnostics (Geweke test)

set.seed(42)


# 1. DATA PREPARATION ----------------------------------------

load("df_indices_wide.RData")

# Rename columns for clean labels
df_indices_wide <- df_indices_wide %>%
  rename(
    VIX    = `VIX Index`,
    V2X    = `V2X Index`,
    IVIUK  = `IVIUK Index`,
    VNKY   = `VNKY Index`,
    VHSI   = `VHSI Index`,
    INVIXN = `INVIXN Index`
  )

var_names  <- c("VIX", "V2X", "IVIUK", "VNKY", "VHSI", "INVIXN")
date_col   <- df_indices_wide$date

# Sub-sample matrices (BVAR expects numeric matrix with column names)
make_mat <- function(rows) {
  m <- as.matrix(df_indices_wide[rows, var_names])
  colnames(m) <- var_names
  m
}

data_full   <- make_mat(rep(TRUE, nrow(df_indices_wide)))
data_calm   <- make_mat(date_col < as.Date("2020-01-01"))
data_crisis <- make_mat(date_col >= as.Date("2020-01-01") &
                          date_col <= as.Date("2021-12-31"))

cat("Observations — Full:", nrow(data_full),
    "| Calm:", nrow(data_calm),
    "| Crisis:", nrow(data_crisis), "\n")

# --- Summary statistics (Slide 4) ---
summary(data_full)

# --- Time series plot: all 6 indices with COVID band (Slide 3) ---
ts_long <- df_indices_wide %>%
  pivot_longer(all_of(var_names), names_to = "Index", values_to = "Value") %>%
  mutate(Index = factor(Index, levels = var_names))

covid_band <- data.frame(
  xmin = as.Date("2020-01-01"), xmax = as.Date("2021-12-31"),
  ymin = -Inf, ymax = Inf
)

p_ts <- ggplot(ts_long, aes(x = date, y = Value)) +
  geom_rect(data = covid_band, inherit.aes = FALSE,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "tomato", alpha = 0.15) +
  geom_line(colour = "steelblue", linewidth = 0.4) +
  facet_wrap(~ Index, scales = "free_y", ncol = 2) +
  labs(title = "Global Implied Volatility Indices (2015–2025)",
       subtitle = "Red band = COVID-19 crisis period (2020–2021)",
       x = NULL, y = "Index Level") +
  theme_bw()

print(p_ts)


# 2. SHARED BVAR SETTINGS ------------------------------------

# Minnesota prior: hierarchically estimated lambda, alpha, psi
# lambda: overall tightness (shrinkage strength)
# alpha:  lag-decay speed
# psi:    per-variable variance scaling
mn <- bv_minnesota(
  lambda = bv_lambda(mode = 0.2, sd = 0.4, min = 0.0001, max = 5),
  alpha  = bv_alpha(mode = 2),
  psi    = bv_psi(scale = 0.004, shape = 0.004),
  var    = 1
)
priors <- bv_priors(hyper = c("lambda", "alpha", "psi"), mn = mn)

# IRF: Cholesky identification
# Ordering: VIX → V2X → IVIUK → VNKY → VHSI → INVIXN
# Rationale: US market leads global volatility (time zone + market cap)
irf_setup <- bv_irf(horizon = 20, fevd = TRUE, identification = TRUE)

# Forecast setup (unconditional)
fcast_setup <- bv_fcast(horizon = 20)

# MCMC: Metropolis-Hastings tuning (target acceptance 25–45 %)
mh     <- bv_metropolis(scale_hess = 0.01, adjust_acc = TRUE,
                        acc_lower = 0.25, acc_upper = 0.45)
n_draw <- 25000
n_burn <- 10000


# 3. FULL SAMPLE BVAR (2015–2025) ----------------------------

bvar_full <- bvar(
  data    = data_full,
  lags    = 5,
  n_draw  = n_draw, n_burn  = n_burn,
  priors  = priors,  mh     = mh,
  irf     = irf_setup, fcast = fcast_setup,
  verbose = TRUE
)

# --- 3.1 Convergence diagnostics (Slide 7) ---
summary(bvar_full)          # acceptance rate, posterior means, ESS
plot(bvar_full)             # trace + density plots for lambda, alpha, psi

# Geweke test via coda interface
geweke.diag(as.mcmc(bvar_full))

# --- 3.2 Lambda posterior vs. prior (Slide 8) ---
# Illustrates Occam's razor: data tightens the prior on shrinkage
lambda_draws <- bvar_full$hyper[, "lambda"]

p_lambda <- ggplot(data.frame(x = lambda_draws), aes(x = x)) +
  geom_density(fill = "steelblue", alpha = 0.4, colour = "steelblue4",
               linewidth = 0.8) +
  stat_function(
    fun  = function(x) dnorm(x, mean = mn$lambda$mode, sd = mn$lambda$sd),
    colour = "tomato", linetype = "dashed", linewidth = 1
  ) +
  labs(
    title    = expression("Shrinkage Hyperparameter " * lambda *
                            ": Posterior vs. Prior"),
    subtitle = "Blue = Posterior | Red dashed = Prior",
    x = expression(lambda), y = "Density"
  ) +
  coord_cartesian(xlim = c(0, 1)) +
  theme_bw()

print(p_lambda)

# --- 3.3 Impulse Response Functions – VIX shock (Slides 9–10) ---
irf_full <- irf(bvar_full, horizon = 20, conf_bands = c(0.05, 0.16))

# Full IRF grid (all shocks × all responses)
plot(irf_full)

# Focus: responses to VIX shock (impulse = 1) — main result
BVARverse::bv_ggplot(irf_full, vars_impulse = 1) +
  labs(title = "IRF: Response to 1 SD VIX Shock (Full Sample 2015–2025)",
       subtitle = "68% and 90% credible bands")

# --- 3.4 FEVD: Forecast Error Variance Decomposition (Slide 11) ---
# Shows share of each market's forecast variance attributable to VIX shocks
plot(irf_full, type = "fevd")

# --- 3.5 Unconditional Forecast — 20 trading days ahead (Slide 16) ---
pred_full <- predict(bvar_full, conf_bands = c(0.05, 0.16))
plot(pred_full)


# 4. SUB-SAMPLE COMPARISON -----------------------------------

# --- 4.1 Calm period BVAR (2015–2019) ---
bvar_calm <- bvar(
  data    = data_calm,
  lags    = 5,
  n_draw  = n_draw, n_burn  = n_burn,
  priors  = priors,  mh     = mh,
  irf     = irf_setup,
  verbose = TRUE
)

# --- 4.2 Crisis period BVAR (2020–2021) ---
bvar_crisis <- bvar(
  data    = data_crisis,
  lags    = 5,
  n_draw  = n_draw, n_burn  = n_burn,
  priors  = priors,  mh     = mh,
  irf     = irf_setup,
  verbose = TRUE
)

# --- 4.3 IRF comparison: VIX shock, calm vs. crisis (Slides 13–14) ---
irf_calm   <- irf(bvar_calm,   horizon = 20, conf_bands = c(0.05, 0.16))
irf_crisis <- irf(bvar_crisis, horizon = 20, conf_bands = c(0.05, 0.16))

p_irf_calm <- BVARverse::bv_ggplot(irf_calm, vars_impulse = 1) +
  labs(title = "IRF: VIX Shock — Calm Period (2015–2019)")

p_irf_crisis <- BVARverse::bv_ggplot(irf_crisis, vars_impulse = 1) +
  labs(title = "IRF: VIX Shock — Crisis Period (2020–2021)")

# Side-by-side comparison
p_irf_calm / p_irf_crisis   # patchwork stacks vertically

# --- 4.4 FEVD comparison: calm vs. crisis (Slide 15) ---
# base R plot — bv_ggplot does not support type = "fevd"
plot(irf_calm,   type = "fevd"); title("FEVD — Calm (2015–2019)")
plot(irf_crisis, type = "fevd"); title("FEVD — Crisis (2020–2021)")

# Sub-sample hyperparameter comparison (optional — shows lambda shifts)
cat("Full sample lambda (median):",
    median(bvar_full$hyper[, "lambda"]), "\n")
cat("Calm lambda (median):       ",
    median(bvar_calm$hyper[, "lambda"]), "\n")
cat("Crisis lambda (median):     ",
    median(bvar_crisis$hyper[, "lambda"]), "\n")


# 5. CONDITIONAL FORECASTING — STRESS SCENARIO ---------------

# Scenario: moderate VIX stress episode (comparable to Q4 2018 selloff)
# VIX rises gradually to ~50 over 10 days, then levels off
# Other markets: unconditioned — BVAR propagates the shock endogenously

vix_path <- c(25, 30, 35, 40, 45, 48, 50, 50, 48, 45)   # 10-day VIX path

# Conditions matrix: nrow = forecast horizon, ncol = M
# NA = unconditioned; numeric = conditioned at that value
cond_mat <- matrix(NA_real_, nrow = 20, ncol = length(var_names))
colnames(cond_mat) <- var_names
cond_mat[seq_along(vix_path), "VIX"] <- vix_path

fcast_cond <- bv_fcast(horizon = 20)
pred_cond  <- predict(bvar_full, fcast = fcast_cond,
                      conditional = cond_mat, conf_bands = c(0.05, 0.16))

# Comparison: unconditional vs. conditional (Slide 17)
p_unc  <- BVARverse::bv_ggplot(pred_full) + labs(title = "Unconditional Forecast")
p_cond <- BVARverse::bv_ggplot(pred_cond) + labs(title = "Conditional: VIX Stress Scenario")

p_unc / p_cond
