# ============================================================
# BE_Group3_Code.R
# Bayesian VAR Analysis: Global Volatility Transmission
# Group 3 - Bayesian Econometrics, WU Vienna
# ============================================================


# 0. SETUP ---------------------------------------------------

library(BVAR)       # Bayesian VAR with hierarchical prior selection
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)  # combine ggplot2 panels
library(coda)       # MCMC diagnostics

set.seed(42)

# Output directory for all saved plots
dir.create("plots old", showWarnings = FALSE)

# Clean labels for all six volatility indices
series_labels <- c(
  VIX    = "VIX (US)",
  V2X    = "V2X (Euro Area)",
  IVIUK  = "IVIUK (UK)",
  VNKY   = "VNKY (Japan)",
  VHSI   = "VHSI (Hong Kong)",
  INVIXN = "INVIXN (India)"
)

# Helper: save a ggplot or patchwork object to the plots/ directory
save_gg <- function(p, filename, width = 12, height = 7) {
  ggsave(
    file.path("plots old", filename),
    plot = p,
    width = width,
    height = height,
    dpi = 150,
    bg = "white"
  )
  invisible(p)
}

# Helper: save a base-R plot to the plots/ directory
save_base_plot <- function(filename, expr, width = 12, height = 7, res = 150) {
  png(
    file.path("plots old", filename),
    width = width * res,
    height = height * res,
    res = res
  )
  on.exit(dev.off(), add = TRUE)
  force(expr)
  invisible(filename)
}

# Helper: compact date range label
date_range_label <- function(dates) {
  dates <- sort(as.Date(dates[!is.na(dates)]))
  if (!length(dates)) {
    return("No dates available")
  }
  paste(format(min(dates), "%Y-%m-%d"), "to", format(max(dates), "%Y-%m-%d"))
}

# Helper: colour palette with stable ordering
scenario_palette <- function(levels) {
  base_cols <- c(
    "#2C7FB8", "#D95F0E", "#31A354",
    "#756BB1", "#636363", "#E6550D",
    "#1B9E77", "#A6761D"
  )
  setNames(base_cols[seq_along(levels)], levels)
}

# Helper: resolve variable names for plotting and subtitles
label_var <- function(x) {
  out <- unname(series_labels[x])
  out[is.na(out)] <- x[is.na(out)]
  out
}

# Helper: extract posterior forecast quantiles into a tidy data frame.
# Works on any bvar_fcast object (unconditional or conditional).
# pred_obj$fcast is an array of dimension [n_draw x horizon x n_vars].
extract_fcast_df <- function(pred_obj, var_names) {
  arr <- pred_obj$fcast
  horizon <- dim(arr)[2]

  do.call(rbind, lapply(seq_along(var_names), function(v) {
    mat <- arr[, , v]
    if (is.null(dim(mat))) {
      mat <- matrix(mat, nrow = 1)
    }

    q <- apply(
      mat, 2, quantile,
      probs = c(0.05, 0.16, 0.50, 0.84, 0.95),
      na.rm = TRUE
    )

    data.frame(
      h = seq_len(horizon),
      variable = factor(var_names[v], levels = var_names, labels = label_var(var_names)),
      q05 = q[1, ],
      q16 = q[2, ],
      median = q[3, ],
      q84 = q[4, ],
      q95 = q[5, ],
      stringsAsFactors = FALSE
    )
  }))
}

# Helper: extract IRF quantiles into a tidy data frame.
# irf_obj$irf is an array of dimension [n_draw x response x horizon x impulse].
extract_irf_df <- function(irf_obj, var_names) {
  arr <- irf_obj$irf
  horizon <- dim(arr)[3]

  do.call(rbind, lapply(seq_along(var_names), function(r) {
    do.call(rbind, lapply(seq_along(var_names), function(i) {
      mat <- arr[, r, , i]
      if (is.null(dim(mat))) {
        mat <- matrix(mat, nrow = 1)
      }

      q <- apply(
        mat, 2, quantile,
        probs = c(0.05, 0.16, 0.50, 0.84, 0.95),
        na.rm = TRUE
      )

      data.frame(
        h = seq_len(horizon),
        response = factor(var_names[r], levels = var_names, labels = label_var(var_names)),
        impulse = factor(var_names[i], levels = var_names, labels = label_var(var_names)),
        q05 = q[1, ],
        q16 = q[2, ],
        median = q[3, ],
        q84 = q[4, ],
        q95 = q[5, ],
        stringsAsFactors = FALSE
      )
    }))
  }))
}

# Helper: extract FEVD/GFEVD median shares into a tidy data frame.
# Accepts any object with a $fevd array of dimension
# [n_draw x response x horizon x impulse].
extract_fevd_df <- function(fevd_obj, var_names) {
  arr <- fevd_obj$fevd
  horizon <- dim(arr)[3]

  do.call(rbind, lapply(seq_along(var_names), function(r) {
    share_mat <- sapply(seq_along(var_names), function(i) {
      apply(arr[, r, , i], 2, median, na.rm = TRUE)
    })

    if (is.null(dim(share_mat))) {
      share_mat <- matrix(share_mat, ncol = length(var_names))
    }

    row_sums <- rowSums(share_mat, na.rm = TRUE)
    row_sums[!is.finite(row_sums) | row_sums <= 0] <- 1
    share_mat <- share_mat / row_sums
    share_vals <- pmin(pmax(as.vector(share_mat), 0), 1)
    share_vals[!is.finite(share_vals)] <- 0

    data.frame(
      h = rep(seq_len(horizon), times = length(var_names)),
      response = factor(rep(var_names[r], horizon * length(var_names)),
                        levels = var_names, labels = label_var(var_names)),
      shock = factor(rep(var_names, each = horizon),
                     levels = var_names, labels = label_var(var_names)),
      share = share_vals,
      stringsAsFactors = FALSE
    )
  }))
}

# Helper: Generalised FEVD (Pesaran & Shin 1998).
# Uses reduced-form (non-orthogonalised) IRF draws together with the posterior
# draws of the residual covariance matrix to produce an ordering-invariant
# variance decomposition.
#
# irf_obj   : object returned by irf() with identification = TRUE or FALSE;
#             only the $irf array [n_draw x n_vars x horizon x n_vars] is used.
# sigma_draws: posterior draws of the residual covariance [n_draw x n_vars x n_vars],
#              i.e. bvar_obj$sigma.
# var_names  : character vector of variable names (length n_vars).
# horizon    : number of horizons to compute (must be <= dim(irf_obj$irf)[3]).
#
# Returns a list with element $fevd dimensioned identically to the array that
# fevd() produces, so extract_fevd_df() and plot_fevd_facets() work unchanged.
compute_gfevd <- function(irf_obj, sigma_draws, var_names, horizon = 20) {
  n_draw <- dim(irf_obj$irf)[1]
  n_vars <- length(var_names)

  # Output array: same layout as bvar fevd() — [n_draw x response x horizon x impulse]
  gfevd_arr <- array(
    NA_real_,
    dim = c(n_draw, n_vars, horizon, n_vars),
    dimnames = list(NULL, var_names, NULL, var_names)
  )

  for (d in seq_len(n_draw)) {
    Sigma    <- sigma_draws[d, , ]          # n_vars x n_vars residual covariance
    sigma_jj <- diag(Sigma)                 # own-variance of each impulse variable

    # Accumulate squared generalised impulse numerators [response x horizon x impulse]
    numer <- array(0, dim = c(n_vars, horizon, n_vars))

    for (h in seq_len(horizon)) {
      # Psi_h: MA coefficient matrix at horizon h  [n_vars x n_vars]
      # irf array is [draw x response x horizon x impulse]
      Psi_h <- matrix(irf_obj$irf[d, , h, ], nrow = n_vars, ncol = n_vars)

      for (j in seq_len(n_vars)) {          # impulse j
        # Generalised impulse of variable r to shock j:
        #   GI_{r,j,h} = (e_r' Psi_h Sigma e_j) / sqrt(sigma_jj[j])
        # GFEVD numerator contribution at horizon h is GI^2 (without the
        # sqrt, because we square immediately):
        #   = (Psi_h[r, ] %*% Sigma[, j])^2 / sigma_jj[j]
        gi_vec <- (Psi_h %*% Sigma[, j])^2 / sigma_jj[j]  # length n_vars
        numer[, h, j] <- gi_vec
      }
    }

    # Cumulate over horizon (GFEVD is based on cumulative sums)
    cum_numer <- apply(numer, c(1, 3), cumsum)  # horizon x response x impulse
    # aperm to [response x horizon x impulse]
    cum_numer <- aperm(cum_numer, c(2, 1, 3))

    # Normalise each (response, horizon) cell so shares sum to 1 across impulses
    for (r in seq_len(n_vars)) {
      row_sums <- rowSums(cum_numer[r, , ])   # length horizon
      row_sums[!is.finite(row_sums) | row_sums <= 0] <- 1
      cum_numer[r, , ] <- cum_numer[r, , ] / row_sums
    }

    gfevd_arr[d, , , ] <- cum_numer
  }

  list(fevd = gfevd_arr)   # named $fevd so extract_fevd_df() works unchanged
}

# Helper: plot forecast panels with 68% and 90% bands
plot_forecast_facets <- function(df, title, subtitle, colours = NULL) {
  levels_use <- unique(as.character(df$scenario))
  if (is.null(colours)) {
    colours <- scenario_palette(levels_use)
  }

  ggplot(df, aes(x = h, colour = scenario, fill = scenario)) +
    geom_ribbon(aes(ymin = q05, ymax = q95), alpha = 0.08, colour = NA) +
    geom_ribbon(aes(ymin = q16, ymax = q84), alpha = 0.18, colour = NA) +
    geom_line(aes(y = median), linewidth = 0.8) +
    facet_wrap(~ variable, scales = "free_y", ncol = 3) +
    scale_colour_manual(values = colours) +
    scale_fill_manual(values = colours) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Forecast horizon (trading days)",
      y = "Index level",
      colour = "Scenario",
      fill = "Scenario"
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold"),
      strip.text = element_text(face = "bold")
    )
}

# Helper: plot IRF panels for one impulse variable
plot_irf_facets <- function(irf_obj, var_names, impulse_var, title, subtitle) {
  df_irf <- extract_irf_df(irf_obj, var_names) %>%
    filter(as.character(impulse) == label_var(impulse_var))

  ggplot(df_irf, aes(x = h)) +
    geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.4) +
    geom_ribbon(aes(ymin = q05, ymax = q95), fill = "grey75", alpha = 0.35) +
    geom_ribbon(aes(ymin = q16, ymax = q84), fill = "steelblue", alpha = 0.25) +
    geom_line(aes(y = median), colour = "steelblue4", linewidth = 0.8) +
    facet_wrap(~ response, scales = "free_y", ncol = 3) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Horizon (trading days)",
      y = "Response"
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(face = "bold"),
      strip.text = element_text(face = "bold")
    )
}

# Helper: plot GFEVD (or FEVD) panels as stacked bar charts
plot_fevd_facets <- function(fevd_obj, var_names, title, subtitle) {
  fevd_df <- extract_fevd_df(fevd_obj, var_names)
  shock_cols <- setNames(hcl.colors(length(var_names), "Set 2"), label_var(var_names))

  ggplot(fevd_df, aes(x = h, y = share, fill = shock)) +
    geom_col(width = 0.9) +
    facet_wrap(~ response, ncol = 2) +
    scale_y_continuous(
      expand = c(0, 0),
      labels = scales::percent_format(accuracy = 1)
    ) +
    coord_cartesian(ylim = c(0, 1)) +
    scale_fill_manual(values = shock_cols) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Forecast horizon (trading days)",
      y = "Median GFEVD share",
      fill = "Shock"
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold"),
      strip.text = element_text(face = "bold")
    )
}

# Helper: count the number of visible modes in a smoothed density estimate
count_density_modes <- function(x, adjust = 1.2) {
  x <- x[is.finite(x)]
  if (length(x) < 10) {
    return(NA_integer_)
  }

  dens <- density(x, adjust = adjust)
  peaks <- which(diff(sign(diff(dens$y))) == -2L) + 1L
  length(peaks)
}

# Helper: summarise hyperparameter diagnostics
summarise_hyper_diagnostics <- function(hyper_draws) {
  hyper_mcmc <- as.mcmc(hyper_draws)
  geweke_z <- tryCatch(geweke.diag(hyper_mcmc)$z, error = function(e) rep(NA_real_, ncol(hyper_draws)))
  ess <- tryCatch(effectiveSize(hyper_mcmc), error = function(e) rep(NA_real_, ncol(hyper_draws)))

  data.frame(
    hyperparameter = colnames(hyper_draws),
    posterior_mean = colMeans(hyper_draws, na.rm = TRUE),
    posterior_sd = apply(hyper_draws, 2, sd, na.rm = TRUE),
    effective_size = as.numeric(ess),
    geweke_z = as.numeric(geweke_z),
    density_modes = vapply(seq_len(ncol(hyper_draws)), function(i) {
      count_density_modes(hyper_draws[, i])
    }, integer(1)),
    stationarity_flag = ifelse(
      is.na(geweke_z), "Check manually",
      ifelse(abs(geweke_z) <= 1.96, "OK", "Investigate")
    ),
    mixing_flag = ifelse(
      is.na(ess), "Check manually",
      ifelse(ess >= 200, "OK", "Low ESS")
    ),
    stringsAsFactors = FALSE
  )
}

# Helper: trace and density plots for all hyperparameters
plot_hyper_diagnostics <- function(hyper_draws, title, subtitle = NULL) {
  trace_df <- as.data.frame(hyper_draws) %>%
    mutate(draw = seq_len(n())) %>%
    pivot_longer(-draw, names_to = "hyperparameter", values_to = "value")

  density_df <- do.call(rbind, lapply(colnames(hyper_draws), function(nm) {
    dens <- density(hyper_draws[, nm], adjust = 1.2, na.rm = TRUE)
    data.frame(
      hyperparameter = nm,
      x = dens$x,
      density = dens$y,
      stringsAsFactors = FALSE
    )
  }))

  p_trace <- ggplot(trace_df, aes(x = draw, y = value)) +
    geom_line(colour = "steelblue", linewidth = 0.25) +
    facet_wrap(~ hyperparameter, scales = "free_y", ncol = 4) +
    labs(title = "Trace plots", x = "Saved MCMC draw", y = "Value") +
    theme_bw() +
    theme(
      plot.title = element_text(face = "bold"),
      strip.text = element_text(face = "bold")
    )

  p_density <- ggplot(density_df, aes(x = x, y = density)) +
    geom_area(fill = "tomato", alpha = 0.25) +
    geom_line(colour = "tomato4", linewidth = 0.6) +
    facet_wrap(~ hyperparameter, scales = "free", ncol = 4) +
    labs(title = "Posterior densities", x = "Value", y = "Density") +
    theme_bw() +
    theme(
      plot.title = element_text(face = "bold"),
      strip.text = element_text(face = "bold")
    )

  (p_trace / p_density) +
    plot_annotation(title = title, subtitle = subtitle)
}

# Helper: build a +1 forecast-standard-deviation path for one variable
build_plus_sd_path <- function(df_forecast, scenario_var, fallback_sd) {
  df_var <- df_forecast %>%
    filter(as.character(variable) == label_var(scenario_var)) %>%
    arrange(h)

  sigma <- (df_var$q84 - df_var$q16) / 2
  sigma[!is.finite(sigma) | sigma <= 0] <- fallback_sd

  data.frame(
    h = df_var$h,
    baseline_median = df_var$median,
    forecast_sd = sigma,
    conditioned_value = pmax(df_var$median + sigma, 1e-6),
    stringsAsFactors = FALSE
  )
}

# Helper: run and tidy one conditional forecast scenario
run_conditional_scenario <- function(model, baseline_df, data_mat,
                                     scenario_var, var_names, horizon, conf_bands) {
  fallback_sd <- stats::sd(data_mat[, scenario_var], na.rm = TRUE)
  if (!is.finite(fallback_sd) || fallback_sd <= 0) {
    fallback_sd <- 1
  }

  path_df <- build_plus_sd_path(
    df_forecast = baseline_df,
    scenario_var = scenario_var,
    fallback_sd = fallback_sd
  )

  cond_setup <- bv_fcast(
    horizon = horizon,
    cond_path = path_df$conditioned_value,
    cond_vars = scenario_var
  )

  pred_cond <- predict(model, cond_setup, conf_bands = conf_bands)

  scenario_name <- paste0(label_var(scenario_var), " +1 Forecast SD")
  df_cond <- extract_fcast_df(pred_cond, var_names)
  df_cond$scenario <- scenario_name

  df_base <- baseline_df
  df_base$scenario <- "Baseline"

  path_df$scenario_var <- scenario_var
  path_df$scenario_label <- scenario_name

  list(
    pred = pred_cond,
    plot_df = bind_rows(df_base, df_cond),
    path_df = path_df,
    scenario_name = scenario_name
  )
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
date_col <- df_indices_wide$date

# Crisis-window choice:
# - acute_2020 isolates the initial COVID volatility shock.
# - full_2020_2021 also includes the recovery/re-opening phase.
# Default = acute_2020 because the assignment asks for a crisis regime and
# the acute shock window is conceptually tighter.
covid_windows <- list(
  acute_2020 = c(as.Date("2020-03-01"), as.Date("2020-12-31")),
  full_2020_2021 = c(as.Date("2020-01-01"), as.Date("2021-12-31"))
)
covid_window <- Sys.getenv("COVID_WINDOW", unset = "acute_2020")
if (!covid_window %in% names(covid_windows)) {
  stop("Unknown COVID_WINDOW. Use one of: ", paste(names(covid_windows), collapse = ", "))
}
crisis_bounds <- covid_windows[[covid_window]]

# Sub-sample matrices (BVAR expects a numeric matrix with column names)
make_mat <- function(rows) {
  m <- as.matrix(df_indices_wide[rows, var_names])
  colnames(m) <- var_names
  m
}

full_rows <- rep(TRUE, nrow(df_indices_wide))
calm_rows <- date_col < as.Date("2020-01-01")
crisis_rows <- date_col >= crisis_bounds[1] & date_col <= crisis_bounds[2]

data_full <- make_mat(full_rows)
data_calm <- make_mat(calm_rows)
data_crisis <- make_mat(crisis_rows)

full_sample_label <- date_range_label(date_col[full_rows])
calm_sample_label <- date_range_label(date_col[calm_rows])
crisis_sample_label <- date_range_label(date_col[crisis_rows])
crisis_regime_title <- if (covid_window == "acute_2020") {
  "COVID Acute Crisis"
} else {
  "COVID Crisis and Recovery"
}

cat("Observations - Full:", nrow(data_full),
    "| Calm:", nrow(data_calm),
    "| Crisis:", nrow(data_crisis), "\n")
cat("Using crisis window:", crisis_regime_title, "|", crisis_sample_label, "\n")
if (covid_window == "acute_2020") {
  cat("Note: acute_2020 is the default because it isolates the initial market stress.\n",
      "Set COVID_WINDOW=full_2020_2021 if you want the broader 2020-2021 regime.\n",
      sep = "")
}

# --- Summary statistics (Slide 4) ---
summary(data_full)

# --- Time series plot: all 6 indices with COVID band (Slide 3) ---
ts_long <- df_indices_wide %>%
  pivot_longer(all_of(var_names), names_to = "Index", values_to = "Value") %>%
  mutate(Index = factor(Index, levels = var_names, labels = label_var(var_names)))

covid_band <- data.frame(
  xmin = crisis_bounds[1], xmax = crisis_bounds[2],
  ymin = -Inf, ymax = Inf
)

p_ts <- ggplot(ts_long, aes(x = date, y = Value)) +
  geom_rect(
    data = covid_band, inherit.aes = FALSE,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = "tomato", alpha = 0.15
  ) +
  geom_line(colour = "steelblue", linewidth = 0.4) +
  facet_wrap(~ Index, scales = "free_y", ncol = 2) +
  labs(
    title = "Global Implied Volatility Indices",
    subtitle = paste(
      "Full sample:", full_sample_label,
      "| Shaded band:", crisis_regime_title, "(", crisis_sample_label, ")"
    ),
    x = NULL,
    y = "Index level"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold")
  )

print(p_ts)
save_gg(p_ts, "01_timeseries.png", width = 12, height = 8)


# 2. SHARED BVAR SETTINGS ------------------------------------

# Minnesota prior: hierarchically estimated lambda, alpha, psi
# lambda: overall tightness (shrinkage strength)
# alpha:  lag-decay speed
# psi:    per-variable variance scaling
mn <- bv_minnesota(
  lambda = bv_lambda(mode = 0.2, sd = 0.4, min = 0.0001, max = 5),
  alpha = bv_alpha(mode = 2),
  psi = bv_psi(scale = 0.004, shape = 0.004),
  var = 1
)
priors <- bv_priors(hyper = c("lambda", "alpha", "psi"), mn = mn)

irf_setup <- bv_irf(horizon = 20, fevd = TRUE, identification = FALSE)

# Forecast setup
fcast_horizon <- 20L
fcast_setup <- bv_fcast(horizon = fcast_horizon)

# MCMC settings (defaults kept high for the final run; can be lowered for a
# quick check by setting environment variables before sourcing the script)
mh <- bv_metropolis(
  scale_hess = 0.01,
  adjust_acc = TRUE,
  acc_lower = 0.25,
  acc_upper = 0.45
)
n_draw <- as.integer(Sys.getenv("BVAR_N_DRAW", unset = "25000"))
n_burn <- as.integer(Sys.getenv("BVAR_N_BURN", unset = "10000"))
verbose_bvar <- tolower(Sys.getenv("BVAR_VERBOSE", unset = "true")) != "false"

if (!is.finite(n_draw) || !is.finite(n_burn) || n_draw <= n_burn) {
  stop("Need n_draw > n_burn. Current values are n_draw = ", n_draw,
       " and n_burn = ", n_burn, ".")
}


# 3. FULL SAMPLE BVAR ----------------------------------------

bvar_full <- bvar(
  data = data_full,
  lags = 5,
  n_draw = n_draw,
  n_burn = n_burn,
  priors = priors,
  mh = mh,
  irf = irf_setup,
  fcast = fcast_setup,
  verbose = verbose_bvar
)

# --- 3.1 Convergence diagnostics (Slide 7) ---
summary(bvar_full)

save_base_plot("03_convergence_overview.png", {
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)
  par(oma = c(0, 0, 4, 0))
  plot(bvar_full)
  mtext("Convergence Diagnostics - Full Sample", outer = TRUE, line = 2.5, cex = 1.3, font = 2)
}, width = 14, height = 9)

hyper_diag_full <- summarise_hyper_diagnostics(bvar_full$hyper)
print(hyper_diag_full)
write.csv(
  hyper_diag_full,
  file.path("plots old", "03_hyperparameter_diagnostics.csv"),
  row.names = FALSE
)

cat("\nHyperparameter diagnostics note:\n")
if (all(hyper_diag_full$stationarity_flag == "OK") &&
    all(hyper_diag_full$mixing_flag == "OK")) {
  cat("Geweke and ESS do not flag a major convergence problem in the saved draws.\n",
      "If some densities look multimodal, that can still be economically meaningful rather than pathological.\n",
      sep = "")
} else {
  cat("At least one hyperparameter is flagged by Geweke or ESS.\n",
      "If that persists in the full run, consider increasing n_burn and n_draw before finalising the slides.\n",
      sep = "")
}

p_hyper_diag <- plot_hyper_diagnostics(
  bvar_full$hyper,
  title = "Hyperparameter Trace and Density Diagnostics - Full Sample",
  subtitle = paste(
    "Sample:", full_sample_label,
    "| Geweke z outside +/-1.96 suggests non-stationarity.",
    "Multimodality alone is not necessarily a problem."
  )
)

print(p_hyper_diag)
save_gg(p_hyper_diag, "03_hyperparameter_trace_density.png", width = 14, height = 12)

# --- 3.2 Lambda posterior vs. prior (Slide 8) ---
lambda_draws <- bvar_full$hyper[, "lambda"]
lambda_shape <- mn$lambda$coef$k
lambda_scale <- mn$lambda$coef$theta
lambda_upper <- min(
  mn$lambda$max,
  max(
    quantile(lambda_draws, 0.995, na.rm = TRUE) * 1.15,
    qgamma(0.995, shape = lambda_shape, scale = lambda_scale)
  )
)
lambda_grid <- seq(mn$lambda$min, lambda_upper, length.out = 500)

prior_lambda_df <- data.frame(
  x = lambda_grid,
  density = dgamma(lambda_grid, shape = lambda_shape, scale = lambda_scale)
)

p_lambda <- ggplot(data.frame(x = lambda_draws), aes(x = x)) +
  geom_density(
    fill = "steelblue", alpha = 0.35,
    colour = "steelblue4", linewidth = 0.8
  ) +
  geom_line(
    data = prior_lambda_df,
    aes(x = x, y = density),
    inherit.aes = FALSE,
    colour = "tomato",
    linetype = "dashed",
    linewidth = 1
  ) +
  coord_cartesian(xlim = c(mn$lambda$min, lambda_upper)) +
  labs(
    title = "Shrinkage Hyperparameter Lambda: Posterior vs. Prior",
    subtitle = "Blue = posterior density | Red dashed = Minnesota gamma prior",
    x = expression(lambda),
    y = "Density"
  ) +
  theme_bw() +
  theme(plot.title = element_text(face = "bold"))

print(p_lambda)
save_gg(p_lambda, "03_lambda_prior_posterior.png")

# --- 3.3 Impulse Response Functions: VIX shock (Slides 9-10) ---
irf_full <- irf(bvar_full, horizon = 20, conf_bands = c(0.05, 0.16))

p_irf_full_vix <- plot_irf_facets(
  irf_obj = irf_full,
  var_names = var_names,
  impulse_var = "VIX",
  title = "IRF: Response to a 1 SD VIX Shock - Full Sample",
  subtitle = paste(
    "Sample:", full_sample_label,
    "| 68% and 90% posterior credible bands"
  )
)

print(p_irf_full_vix)
save_gg(p_irf_full_vix, "03_irf_full_vix_shock.png", width = 12, height = 8)

# --- 3.4 Generalised FEVD - Full Sample ---
# GFEVD is computed from the reduced-form IRF draws and the posterior residual
# covariance draws. It is ordering-invariant (Pesaran & Shin 1998).
gfevd_full <- compute_gfevd(irf_full, bvar_full$sigma, var_names, horizon = 20)

p_gfevd_full <- plot_fevd_facets(
  fevd_obj = gfevd_full,
  var_names = var_names,
  title = "GFEVD - Full Sample",
  subtitle = paste(
    "Sample:", full_sample_label,
    "| Pesaran & Shin (1998) | Ordering-invariant"
  )
)

print(p_gfevd_full)
save_gg(p_gfevd_full, "03_gfevd_full.png", width = 14, height = 10)

# --- 3.5 Unconditional Forecast - 20 trading days ahead (Slide 16) ---
pred_full <- predict(bvar_full, conf_bands = c(0.05, 0.16))

df_unc <- extract_fcast_df(pred_full, var_names)
df_unc$scenario <- "Baseline"

p_fcast_unc <- plot_forecast_facets(
  df_unc,
  title = "Unconditional Forecast - Full Sample BVAR",
  subtitle = paste(
    "Sample:", full_sample_label,
    "| 20 trading days ahead | 68% and 90% posterior credible bands"
  ),
  colours = c(Baseline = "#2C7FB8")
)

print(p_fcast_unc)
save_gg(p_fcast_unc, "03_forecast_unconditional.png", width = 12, height = 8)


# 4. SUB-SAMPLE COMPARISON -----------------------------------

# --- 4.1 Calm period BVAR ---
bvar_calm <- bvar(
  data = data_calm,
  lags = 5,
  n_draw = n_draw,
  n_burn = n_burn,
  priors = priors,
  mh = mh,
  irf = irf_setup,
  verbose = verbose_bvar
)

# --- 4.2 Crisis period BVAR ---
bvar_crisis <- bvar(
  data = data_crisis,
  lags = 5,
  n_draw = n_draw,
  n_burn = n_burn,
  priors = priors,
  mh = mh,
  irf = irf_setup,
  verbose = verbose_bvar
)

# --- 4.3 IRF comparison: VIX shock, calm vs crisis ---
irf_calm <- irf(bvar_calm, horizon = 20, conf_bands = c(0.05, 0.16))
irf_crisis <- irf(bvar_crisis, horizon = 20, conf_bands = c(0.05, 0.16))

p_irf_calm <- plot_irf_facets(
  irf_obj = irf_calm,
  var_names = var_names,
  impulse_var = "VIX",
  title = "IRF: Response to a 1 SD VIX Shock - Calm Period",
  subtitle = paste(
    "Sample:", calm_sample_label,
    "| 68% and 90% posterior credible bands"
  )
)

p_irf_crisis <- plot_irf_facets(
  irf_obj = irf_crisis,
  var_names = var_names,
  impulse_var = "VIX",
  title = paste("IRF: Response to a 1 SD VIX Shock -", crisis_regime_title),
  subtitle = paste(
    "Sample:", crisis_sample_label,
    "| 68% and 90% posterior credible bands"
  )
)

print(p_irf_calm)
print(p_irf_crisis)
save_gg(p_irf_calm, "04_irf_calm_vix_shock.png", width = 12, height = 8)
save_gg(p_irf_crisis, "04_irf_crisis_vix_shock.png", width = 12, height = 8)

p_irf_compare <- (p_irf_calm / p_irf_crisis) +
  plot_annotation(
    title = "IRF Comparison Across Regimes",
    subtitle = paste(
      "Calm sample:", calm_sample_label,
      "| Crisis sample:", crisis_sample_label
    )
  )

print(p_irf_compare)
save_gg(p_irf_compare, "04_irf_calm_vs_crisis.png", width = 12, height = 14)

# --- 4.4 Generalised FEVD comparison: calm vs crisis ---
gfevd_calm   <- compute_gfevd(irf_calm,   bvar_calm$sigma,   var_names, horizon = 20)
gfevd_crisis <- compute_gfevd(irf_crisis, bvar_crisis$sigma, var_names, horizon = 20)

p_gfevd_calm <- plot_fevd_facets(
  fevd_obj = gfevd_calm,
  var_names = var_names,
  title = "GFEVD - Calm Period",
  subtitle = paste(
    "Sample:", calm_sample_label,
    "| Pesaran & Shin (1998) | Ordering-invariant"
  )
)

p_gfevd_crisis <- plot_fevd_facets(
  fevd_obj = gfevd_crisis,
  var_names = var_names,
  title = paste("GFEVD -", crisis_regime_title),
  subtitle = paste(
    "Sample:", crisis_sample_label,
    "| Pesaran & Shin (1998) | Ordering-invariant"
  )
)

print(p_gfevd_calm)
print(p_gfevd_crisis)
save_gg(p_gfevd_calm,   "04_gfevd_calm.png",   width = 14, height = 10)
save_gg(p_gfevd_crisis, "04_gfevd_crisis.png", width = 14, height = 10)

# Sub-sample hyperparameter comparison
cat("Full sample lambda (median):", median(bvar_full$hyper[, "lambda"]), "\n")
cat("Calm lambda (median):       ", median(bvar_calm$hyper[, "lambda"]), "\n")
cat("Crisis lambda (median):     ", median(bvar_crisis$hyper[, "lambda"]), "\n")


# 5. CONDITIONAL FORECASTING - +1 FORECAST-SD SCENARIOS -----

# The original script used an unsupported `conditional=` argument in predict(),
# so the scenario forecasts were effectively not being conditioned.
# Here we use BVAR's actual conditional interface:
#   bv_fcast(horizon, cond_path = ..., cond_vars = ...)
# and build one scenario for each of the six indices.

scenario_results <- lapply(var_names, function(v) {
  run_conditional_scenario(
    model = bvar_full,
    baseline_df = df_unc,
    data_mat = data_full,
    scenario_var = v,
    var_names = var_names,
    horizon = fcast_horizon,
    conf_bands = c(0.05, 0.16)
  )
})
names(scenario_results) <- var_names

scenario_paths <- bind_rows(lapply(scenario_results, `[[`, "path_df"))
write.csv(
  scenario_paths,
  file.path("plots old", "05_conditional_scenario_paths.csv"),
  row.names = FALSE
)

for (v in var_names) {
  scen <- scenario_results[[v]]
  scen_label <- scen$scenario_name
  plot_df <- scen$plot_df
  plot_df$scenario <- factor(plot_df$scenario, levels = c("Baseline", scen_label))

  p_scenario <- plot_forecast_facets(
    plot_df,
    title = paste("Baseline vs Conditional Forecast:", label_var(v), "Stress Scenario"),
    subtitle = paste(
      label_var(v), "is fixed at its baseline median + 1 forecast SD at each horizon;",
      "all other indices are forecast endogenously | 68% and 90% bands"
    ),
    colours = c(Baseline = "#2C7FB8", setNames("#D95F0E", scen_label))
  )

  print(p_scenario)
  save_gg(
    p_scenario,
    paste0("05_conditional_forecast_", tolower(v), "_plus1sd.png"),
    width = 14,
    height = 9
  )
}


# 6. LAG SENSITIVITY ANALYSIS --------------------------------

# Estimates full-sample BVARs with lags = 1, 3, and 5 (baseline) and
# compares the resulting IRFs for a VIX shock.

bvar_lag1 <- bvar(
  data = data_full,
  lags = 1,
  n_draw = n_draw,
  n_burn = n_burn,
  priors = priors,
  mh = mh,
  irf = irf_setup,
  verbose = verbose_bvar
)

bvar_lag3 <- bvar(
  data = data_full,
  lags = 3,
  n_draw = n_draw,
  n_burn = n_burn,
  priors = priors,
  mh = mh,
  irf = irf_setup,
  verbose = verbose_bvar
)

irf_lag1 <- irf(bvar_lag1, horizon = 20, conf_bands = c(0.05, 0.16))
irf_lag3 <- irf(bvar_lag3, horizon = 20, conf_bands = c(0.05, 0.16))

p_irf_lag1 <- plot_irf_facets(
  irf_obj = irf_lag1,
  var_names = var_names,
  impulse_var = "VIX",
  title = "IRF: Response to a 1 SD VIX Shock - 1 Lag",
  subtitle = paste(
    "Sample:", full_sample_label,
    "| 68% and 90% posterior credible bands"
  )
)

p_irf_lag3 <- plot_irf_facets(
  irf_obj = irf_lag3,
  var_names = var_names,
  impulse_var = "VIX",
  title = "IRF: Response to a 1 SD VIX Shock - 3 Lags",
  subtitle = paste(
    "Sample:", full_sample_label,
    "| 68% and 90% posterior credible bands"
  )
)

p_irf_lag5 <- plot_irf_facets(
  irf_obj = irf_full,
  var_names = var_names,
  impulse_var = "VIX",
  title = "IRF: Response to a 1 SD VIX Shock - 5 Lags",
  subtitle = paste(
    "Sample:", full_sample_label,
    "| 68% and 90% posterior credible bands"
  )
)

print(p_irf_lag1)
print(p_irf_lag3)
print(p_irf_lag5)
save_gg(p_irf_lag1, "06_irf_lag1.png", width = 12, height = 8)
save_gg(p_irf_lag3, "06_irf_lag3.png", width = 12, height = 8)
save_gg(p_irf_lag5, "06_irf_lag5.png", width = 12, height = 8)

p_lag_sensitivity <- (p_irf_lag1 / p_irf_lag3 / p_irf_lag5) +
  plot_annotation(
    title = "Lag Sensitivity of VIX-Shock IRFs",
    subtitle = paste("Full sample:", full_sample_label, "| Comparing 1, 3, and 5 lags")
  )

print(p_lag_sensitivity)
save_gg(p_lag_sensitivity, "06_irf_lag_sensitivity.png", width = 12, height = 20)

cat("Lag 1 - lambda (median):", median(bvar_lag1$hyper[, "lambda"]), "\n")
cat("Lag 3 - lambda (median):", median(bvar_lag3$hyper[, "lambda"]), "\n")
cat("Lag 5 - lambda (median):", median(bvar_full$hyper[, "lambda"]), "\n")