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
library(coda)       # MCMC diagnostics (Geweke test)

set.seed(42)

# Output directory for all saved plots
dir.create("plots", showWarnings = FALSE)

# Helper: save a ggplot to the plots/ directory
save_gg <- function(p, filename, width = 12, height = 7) {
  ggsave(file.path("plots", filename), plot = p,
         width = width, height = height, dpi = 150)
  invisible(p)
}

# Helper: extract posterior forecast quantiles into a tidy data frame.
# Works on any bvar_fcast object (unconditional or conditional).
# pred_obj$fcast is an array of dimension [n_draw × horizon × n_vars].
extract_fcast_df <- function(pred_obj, var_names) {
  arr     <- pred_obj$fcast        # [n_draw × horizon × n_vars]
  horizon <- dim(arr)[2]
  do.call(rbind, lapply(seq_along(var_names), function(v) {
    mat <- arr[, , v]              # [n_draw × horizon] matrix
    q   <- apply(mat, 2, quantile,
                 probs = c(0.05, 0.16, 0.50, 0.84, 0.95), na.rm = TRUE)
    data.frame(
      h        = seq_len(horizon),
      variable = factor(var_names[v], levels = var_names),
      q05      = q[1, ],
      q16      = q[2, ],
      median   = q[3, ],
      q84      = q[4, ],
      q95      = q[5, ],
      stringsAsFactors = FALSE
    )
  }))
}


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

var_names <- c("VIX", "V2X", "IVIUK", "VNKY", "VHSI", "INVIXN")
date_col  <- df_indices_wide$date

# Sub-sample matrices (BVAR expects numeric matrix with column names)
make_mat <- function(rows) {
  m <- as.matrix(df_indices_wide[rows, var_names])
  colnames(m) <- var_names
  m
}

data_full   <- make_mat(rep(TRUE, nrow(df_indices_wide)))
data_calm   <- make_mat(date_col < as.Date("2020-01-01"))

# COVID acute shock: March 2020 – December 2020
# Rationale: March 2020 captures the onset of the market crash (VIX peaked
# at ~85 in mid-March); December 2020 marks the end of the acute phase before
# vaccination-driven normalisation. This is more precise than using the full
# 2020-2021 period, which also includes the recovery phase.
data_crisis <- make_mat(date_col >= as.Date("2020-03-01") &
                          date_col <= as.Date("2020-12-31"))

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
  xmin = as.Date("2020-03-01"), xmax = as.Date("2020-12-31"),
  ymin = -Inf, ymax = Inf
)

p_ts <- ggplot(ts_long, aes(x = date, y = Value)) +
  geom_rect(data = covid_band, inherit.aes = FALSE,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "tomato", alpha = 0.15) +
  geom_line(colour = "steelblue", linewidth = 0.4) +
  facet_wrap(~ Index, scales = "free_y", ncol = 2) +
  labs(title    = "Global Implied Volatility Indices (2015–2025)",
       subtitle = "Red band = COVID-19 acute crisis (March–December 2020)",
       x = NULL, y = "Index Level") +
  theme_bw()

print(p_ts)
save_gg(p_ts, "01_timeseries.png", width = 12, height = 8)


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

# Forecast setup (unconditional, shared with conditional calls)
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
summary(bvar_full)   # acceptance rate, posterior means, ESS

# Trace + density plots for hyperparameters (lambda, alpha, psi)
# NOTE on interpretation:
#   - Trending/drifting traces indicate slow mixing or insufficient burn-in.
#     If observed, consider increasing n_burn to 20 000 or n_draw to 50 000.
#   - Multimodal marginal densities can reflect genuine posterior multimodality
#     (e.g. two plausible shrinkage levels) rather than a sampling artefact.
#     This is not necessarily pathological but warrants scrutiny — check whether
#     both modes are economically plausible and whether the chain visits them
#     regularly.
png(file.path("plots", "03_convergence_diagnostics.png"),
    width = 1400, height = 900, res = 100)
plot(bvar_full)
dev.off()
plot(bvar_full)   # display inline

# Geweke z-test: values within ±1.96 indicate chain stationarity
geweke.diag(as.mcmc(bvar_full))

# --- 3.2 Lambda posterior vs. prior (Slide 8) ---
# Illustrates Occam's razor: data tightens the prior on shrinkage
lambda_draws <- bvar_full$hyper[, "lambda"]

p_lambda <- ggplot(data.frame(x = lambda_draws), aes(x = x)) +
  geom_density(fill = "steelblue", alpha = 0.4, colour = "steelblue4",
               linewidth = 0.8) +
  stat_function(
    fun    = function(x) dnorm(x, mean = mn$lambda$mode, sd = mn$lambda$sd),
    colour = "tomato", linetype = "dashed", linewidth = 1
  ) +
  labs(
    title    = expression("Shrinkage Hyperparameter " * lambda *
                            ": Posterior vs. Prior"),
    subtitle = "Blue = Posterior  |  Red dashed = Prior (Normal approximation)",
    x = expression(lambda), y = "Density"
  ) +
  # Lower-bound at 0 (lambda must be positive); upper auto-scales to data
  coord_cartesian(xlim = c(0, NA)) +
  theme_bw()

print(p_lambda)
save_gg(p_lambda, "03_lambda_prior_posterior.png")

# --- 3.3 Impulse Response Functions – VIX shock (Slides 9–10) ---
irf_full <- irf(bvar_full, horizon = 20, conf_bands = c(0.05, 0.16))

# Focused plot: responses of all indices to 1 SD VIX shock (impulse = 1)
p_irf_full_vix <- BVARverse::bv_ggplot(irf_full, vars_impulse = 1) +
  labs(title    = "IRF: Response to 1 SD VIX Shock — Full Sample (2015–2025)",
       subtitle = "68% and 90% posterior credible bands | Cholesky identification")

print(p_irf_full_vix)
save_gg(p_irf_full_vix, "03_irf_full_vix_shock.png", width = 12, height = 8)

# --- 3.4 FEVD: Forecast Error Variance Decomposition — Full Sample (Slide 11) ---
# par(oma) adds outer margin for a proper multi-panel title
png(file.path("plots", "03_fevd_full.png"),
    width = 1400, height = 1000, res = 100)
par(oma = c(0, 0, 3, 0))
plot(irf_full, type = "fevd")
mtext("FEVD — Full Sample (2015–2025)", outer = TRUE, cex = 1.3, font = 2)
par(oma = c(0, 0, 0, 0))
dev.off()

par(oma = c(0, 0, 3, 0))
plot(irf_full, type = "fevd")
mtext("FEVD — Full Sample (2015–2025)", outer = TRUE, cex = 1.3, font = 2)
par(oma = c(0, 0, 0, 0))

# --- 3.5 Unconditional Forecast — 20 trading days ahead (Slide 16) ---
pred_full <- predict(bvar_full, conf_bands = c(0.05, 0.16))

p_fcast_unc <- BVARverse::bv_ggplot(pred_full) +
  labs(title    = "Unconditional Forecast — Full Sample BVAR (20 trading days ahead)",
       subtitle = "68% and 90% posterior credible bands")

print(p_fcast_unc)
save_gg(p_fcast_unc, "03_forecast_unconditional.png", width = 12, height = 8)


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

# --- 4.2 Crisis period BVAR (March–December 2020) ---
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
  labs(title    = "IRF: VIX Shock — Calm Period (2015–2019)",
       subtitle = "68% and 90% posterior credible bands")

p_irf_crisis <- BVARverse::bv_ggplot(irf_crisis, vars_impulse = 1) +
  labs(title    = "IRF: VIX Shock — COVID Crisis (March–December 2020)",
       subtitle = "68% and 90% posterior credible bands")

print(p_irf_calm)
print(p_irf_crisis)

# Side-by-side comparison (patchwork stacks vertically)
p_irf_compare <- p_irf_calm / p_irf_crisis
print(p_irf_compare)
save_gg(p_irf_compare, "04_irf_calm_vs_crisis.png", width = 12, height = 14)

# --- 4.4 FEVD comparison: all three regimes (Slide 15) ---

# Calm period
png(file.path("plots", "04_fevd_calm.png"),
    width = 1400, height = 1000, res = 100)
par(oma = c(0, 0, 3, 0))
plot(irf_calm, type = "fevd")
mtext("FEVD — Calm Period (2015–2019)", outer = TRUE, cex = 1.3, font = 2)
par(oma = c(0, 0, 0, 0))
dev.off()

# Crisis period
png(file.path("plots", "04_fevd_crisis.png"),
    width = 1400, height = 1000, res = 100)
par(oma = c(0, 0, 3, 0))
plot(irf_crisis, type = "fevd")
mtext("FEVD — COVID Crisis (March–December 2020)", outer = TRUE, cex = 1.3, font = 2)
par(oma = c(0, 0, 0, 0))
dev.off()

# Display all three inline for comparison
par(oma = c(0, 0, 3, 0))
plot(irf_full, type = "fevd")
mtext("FEVD — Full Sample (2015–2025)", outer = TRUE, cex = 1.3, font = 2)
par(oma = c(0, 0, 0, 0))

par(oma = c(0, 0, 3, 0))
plot(irf_calm, type = "fevd")
mtext("FEVD — Calm Period (2015–2019)", outer = TRUE, cex = 1.3, font = 2)
par(oma = c(0, 0, 0, 0))

par(oma = c(0, 0, 3, 0))
plot(irf_crisis, type = "fevd")
mtext("FEVD — COVID Crisis (March–December 2020)", outer = TRUE, cex = 1.3, font = 2)
par(oma = c(0, 0, 0, 0))

# Sub-sample hyperparameter comparison (shows lambda shifts across regimes)
cat("Full sample lambda (median):", median(bvar_full$hyper[, "lambda"]), "\n")
cat("Calm lambda (median):       ", median(bvar_calm$hyper[, "lambda"]), "\n")
cat("Crisis lambda (median):     ", median(bvar_crisis$hyper[, "lambda"]), "\n")


# 5. CONDITIONAL FORECASTING — STRESS SCENARIOS --------------

# --- 5.1 VIX Stress Scenario ---
# Scenario: moderate VIX stress episode comparable to the Q4 2018 selloff.
# VIX rises gradually from ~25 to 50 over 10 trading days, then partially
# reverses. All other indices are unconditioned — the BVAR propagates the
# shock endogenously through the estimated cross-variable dynamics.
#
# What this answers: "If VIX behaves like this, what does the model expect
# European and Asian volatility to do?"

vix_path <- c(25, 30, 35, 40, 45, 48, 50, 50, 48, 45)   # 10-day VIX path

# Conditions matrix: nrow = forecast horizon, ncol = M
# NA = unconditioned; numeric = conditioned at that value
cond_mat_vix <- matrix(NA_real_, nrow = 20, ncol = length(var_names))
colnames(cond_mat_vix) <- var_names
cond_mat_vix[seq_along(vix_path), "VIX"] <- vix_path

pred_cond_vix <- predict(bvar_full, fcast = fcast_setup,
                          conditional = cond_mat_vix,
                          conf_bands = c(0.05, 0.16))

# Build tidy data frames for the overlay plot
df_unc      <- extract_fcast_df(pred_full,      var_names)
df_unc$scenario <- "Unconditional"

df_vix_cond <- extract_fcast_df(pred_cond_vix,  var_names)
df_vix_cond$scenario <- "VIX Stress"

df_compare_vix <- rbind(df_unc, df_vix_cond)
df_compare_vix$scenario <- factor(df_compare_vix$scenario,
                                   levels = c("Unconditional", "VIX Stress"))

# Overlay plot: base scenario vs. VIX-conditional scenario on the same axes
p_compare_vix <- ggplot(df_compare_vix,
                         aes(x = h, colour = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = q16, ymax = q84), alpha = 0.18, colour = NA) +
  geom_line(aes(y = median), linewidth = 0.8) +
  facet_wrap(~ variable, scales = "free_y", ncol = 3) +
  scale_colour_manual(values = c("Unconditional" = "steelblue",
                                  "VIX Stress"   = "tomato")) +
  scale_fill_manual(  values = c("Unconditional" = "steelblue",
                                  "VIX Stress"   = "tomato")) +
  labs(
    title    = "Conditional vs. Unconditional Forecast: VIX Stress Scenario",
    subtitle = paste("VIX stress path: rises gradually to 50 over 10 days",
                     "(Q4 2018-style selloff) | 68% credible bands"),
    x        = "Horizon (trading days)",
    y        = "Index Level",
    colour   = "Scenario", fill = "Scenario"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

print(p_compare_vix)
save_gg(p_compare_vix, "05_forecast_vix_stress_vs_base.png", width = 14, height = 9)

# --- 5.2 V2X (Euro Stoxx Volatility) Stress Scenario ---
# Scenario: a European-origin volatility shock (e.g. sovereign debt crisis,
# geopolitical event) where V2X (EURO STOXX 50 implied vol) rises to ~50.
# All other indices — including VIX — are left unconditioned, allowing the
# model to show how a euro-area shock propagates to US and Asian markets.

v2x_path <- c(28, 32, 37, 42, 46, 49, 50, 50, 47, 44)   # 10-day V2X path

cond_mat_v2x <- matrix(NA_real_, nrow = 20, ncol = length(var_names))
colnames(cond_mat_v2x) <- var_names
cond_mat_v2x[seq_along(v2x_path), "V2X"] <- v2x_path

pred_cond_v2x <- predict(bvar_full, fcast = fcast_setup,
                          conditional = cond_mat_v2x,
                          conf_bands = c(0.05, 0.16))

df_v2x_cond <- extract_fcast_df(pred_cond_v2x, var_names)
df_v2x_cond$scenario <- "V2X Stress"

df_compare_v2x <- rbind(df_unc, df_v2x_cond)
df_compare_v2x$scenario <- factor(df_compare_v2x$scenario,
                                   levels = c("Unconditional", "V2X Stress"))

p_compare_v2x <- ggplot(df_compare_v2x,
                         aes(x = h, colour = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = q16, ymax = q84), alpha = 0.18, colour = NA) +
  geom_line(aes(y = median), linewidth = 0.8) +
  facet_wrap(~ variable, scales = "free_y", ncol = 3) +
  scale_colour_manual(values = c("Unconditional" = "steelblue",
                                  "V2X Stress"   = "darkorange")) +
  scale_fill_manual(  values = c("Unconditional" = "steelblue",
                                  "V2X Stress"   = "darkorange")) +
  labs(
    title    = "Conditional vs. Unconditional Forecast: V2X (Euro Vola) Stress Scenario",
    subtitle = paste("V2X stress path: rises gradually to 50 over 10 days",
                     "(European-origin shock) | 68% credible bands"),
    x        = "Horizon (trading days)",
    y        = "Index Level",
    colour   = "Scenario", fill = "Scenario"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

print(p_compare_v2x)
save_gg(p_compare_v2x, "05_forecast_v2x_stress_vs_base.png", width = 14, height = 9)

# Combined 3-way comparison: unconditional, VIX stress, V2X stress
df_all_scenarios <- rbind(df_unc, df_vix_cond, df_v2x_cond)
df_all_scenarios$scenario <- factor(df_all_scenarios$scenario,
                                     levels = c("Unconditional",
                                                "VIX Stress", "V2X Stress"))

p_all_scenarios <- ggplot(df_all_scenarios,
                           aes(x = h, colour = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = q16, ymax = q84), alpha = 0.15, colour = NA) +
  geom_line(aes(y = median), linewidth = 0.8) +
  facet_wrap(~ variable, scales = "free_y", ncol = 3) +
  scale_colour_manual(values = c("Unconditional" = "steelblue",
                                  "VIX Stress"   = "tomato",
                                  "V2X Stress"   = "darkorange")) +
  scale_fill_manual(  values = c("Unconditional" = "steelblue",
                                  "VIX Stress"   = "tomato",
                                  "V2X Stress"   = "darkorange")) +
  labs(
    title    = "Stress Scenario Comparison: VIX Shock vs. V2X Shock",
    subtitle = "Both stress paths rise to ~50 over 10 trading days | 68% credible bands",
    x        = "Horizon (trading days)",
    y        = "Index Level",
    colour   = "Scenario", fill = "Scenario"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

print(p_all_scenarios)
save_gg(p_all_scenarios, "05_forecast_all_scenarios.png", width = 14, height = 9)


# 6. LAG SENSITIVITY ANALYSIS --------------------------------
# Estimates full-sample BVARs with lags = 1, 3, and 5 (baseline) and
# compares the resulting IRF profiles for the VIX shock. This tests whether
# the impulse responses are robust to the choice of lag length.

bvar_lag1 <- bvar(
  data    = data_full,
  lags    = 1,
  n_draw  = n_draw, n_burn  = n_burn,
  priors  = priors,  mh     = mh,
  irf     = irf_setup,
  verbose = TRUE
)

bvar_lag3 <- bvar(
  data    = data_full,
  lags    = 3,
  n_draw  = n_draw, n_burn  = n_burn,
  priors  = priors,  mh     = mh,
  irf     = irf_setup,
  verbose = TRUE
)

# bvar_full (lags = 5) already estimated in Section 3 — no re-estimation needed

irf_lag1 <- irf(bvar_lag1, horizon = 20, conf_bands = c(0.05, 0.16))
irf_lag3 <- irf(bvar_lag3, horizon = 20, conf_bands = c(0.05, 0.16))
# irf_full already computed in Section 3.3

p_irf_lag1 <- BVARverse::bv_ggplot(irf_lag1, vars_impulse = 1) +
  labs(title    = "IRF: VIX Shock — 1 Lag",
       subtitle = "Full sample 2015–2025 | 68% and 90% credible bands")

p_irf_lag3 <- BVARverse::bv_ggplot(irf_lag3, vars_impulse = 1) +
  labs(title    = "IRF: VIX Shock — 3 Lags",
       subtitle = "Full sample 2015–2025 | 68% and 90% credible bands")

p_irf_lag5 <- BVARverse::bv_ggplot(irf_full, vars_impulse = 1) +
  labs(title    = "IRF: VIX Shock — 5 Lags (Baseline)",
       subtitle = "Full sample 2015–2025 | 68% and 90% credible bands")

print(p_irf_lag1)
print(p_irf_lag3)
print(p_irf_lag5)

# Stacked comparison panel
p_lag_sensitivity <- p_irf_lag1 / p_irf_lag3 / p_irf_lag5
print(p_lag_sensitivity)
save_gg(p_lag_sensitivity, "06_irf_lag_sensitivity.png",
        width = 12, height = 20)

# Lambda comparison across lag specifications
cat("Lag 1 — lambda (median):", median(bvar_lag1$hyper[, "lambda"]), "\n")
cat("Lag 3 — lambda (median):", median(bvar_lag3$hyper[, "lambda"]), "\n")
cat("Lag 5 — lambda (median):", median(bvar_full$hyper[, "lambda"]), "\n")
