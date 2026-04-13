# Code README — Bayesian VAR: Global Volatility Transmission
**Group 3 | Bayesian Econometrics, WU Vienna**

---

## Overview

The project investigates how volatility shocks originating in the US equity market (captured by the VIX) propagate across global implied-volatility indices. Two R scripts handle, respectively, data collection and the full econometric analysis.

---

## File 1 — `BE_Group3_Indices.R` (Data Collection)

**Purpose:** Pull raw daily closing levels for six implied-volatility indices from Bloomberg and save a clean wide-format dataset.

| Index | Market |
|-------|--------|
| VIX | US (S&P 500) |
| V2X | Euro-zone (EuroStoxx 50) |
| IVIUK | UK (FTSE 100) |
| VNKY | Japan (Nikkei 225) |
| VHSI | Hong Kong (Hang Seng) |
| INVIXN | India (Nifty 50) |

**Steps:**
1. Connect to Bloomberg via `Rblpapi::blpConnect()`.
2. Download `PX_LAST` (last price) for each ticker from 2015-01-01 to 2026-03-31 using `bdh()`.
3. Pivot to wide format (one row per date, one column per index).
4. **Clean missing data:** rows with three or more simultaneous NAs are dropped (non-overlapping trading calendars); isolated NAs are forward-filled; any remaining leading NAs are dropped with `drop_na()`.
5. Save the result as `df_indices_wide.RData` for use in the main analysis script.

**Why this approach makes sense:** Forward-filling is the standard convention for implied-volatility series across different trading calendars (e.g., a US holiday does not represent a genuine gap in European volatility). Dropping rows with three or more NAs avoids carrying observations where most markets are closed.

---

## File 2 — `BE_Group3_Code.R` (Main Analysis)

The main script is organised into five numbered sections that correspond directly to the presentation slides.

---

### Section 0 — Setup

Loads required packages:

| Package | Role |
|---------|------|
| `BVAR` | Core Bayesian VAR estimation with hierarchical Minnesota prior |
| `BVARverse` | `ggplot2`-based plotting wrappers for BVAR objects |
| `ggplot2` / `dplyr` / `tidyr` | Data manipulation and base graphics |
| `patchwork` | Composing multi-panel ggplot figures |
| `coda` | MCMC diagnostics (Geweke convergence test) |

`set.seed(42)` ensures full reproducibility of all MCMC draws.

---

### Section 1 — Data Preparation

1. Loads `df_indices_wide.RData` and renames columns to short, clean labels.
2. Defines three analysis windows via a helper `make_mat()` that subsets rows and returns a named numeric matrix (the format required by `bvar()`):
   - **Full sample** — 2015-01-01 to 2026-03-31 (all available data)
   - **Calm period** — before 2020-01-01 (pre-COVID baseline)
   - **Crisis period** — 2020-01-01 to 2021-12-31 (COVID-19 shock)
3. Produces a faceted time-series plot with a highlighted COVID band, corresponding to **Slide 3** of the presentation.
4. Prints summary statistics (**Slide 4**).

**Why this split makes sense:** Estimating separate BVARs on calm and crisis sub-samples lets us test whether volatility-transmission dynamics changed structurally during the crisis — a core empirical question of the project.

---

### Section 2 — Shared BVAR Settings

All three models share the same prior and MCMC configuration to ensure comparability.

**Minnesota Prior (`bv_minnesota`)**

The Minnesota prior shrinks VAR coefficients toward a random walk (own lags) and zero (cross-variable lags). Three hyperparameters are estimated hierarchically from the data:

- `lambda` — overall tightness: lower values mean stronger shrinkage toward the prior. Sampled with a Normal prior centred at 0.2.
- `alpha` — lag-decay speed: higher values shrink distant lags more aggressively. Fixed-mode prior at 2.
- `psi` — per-variable variance scaling: accounts for heterogeneous volatility levels across indices.

**Why hierarchical estimation?** Rather than fixing the prior tightness by hand, the model selects `lambda`, `alpha`, and `psi` by maximising the marginal likelihood. This implements Occam's razor automatically: if the data contain genuine cross-variable dynamics, the posterior for `lambda` will be larger (less shrinkage); if dynamics are sparse, shrinkage increases.

**IRF identification (`bv_irf`)**

Impulse responses are identified via a recursive Cholesky decomposition with the ordering:

```
VIX → V2X → IVIUK → VNKY → VHSI → INVIXN
```

This ordering reflects the assumption that the US market leads global volatility — motivated by US market size, global benchmark status, and the time-zone sequence (US closes after Europe, before Asia). A shock to a variable in position `k` can affect all variables in positions `k+1, …, 6` within the same period, but not those ranked above it. FEVD is computed alongside the IRF.

**Forecast setup:** 20-trading-day unconditional horizon.

**MCMC configuration:**

- 25,000 draws with a 10,000-draw burn-in.
- Metropolis-Hastings with adaptive scaling targeting a 25–45% acceptance rate — the standard efficient range for MH algorithms.

---

### Section 3 — Full-Sample BVAR (2015–2025)

#### 3.1 Convergence diagnostics (Slide 7)
`summary()` reports posterior means, credible intervals, effective sample sizes, and the MH acceptance rate. `plot()` produces trace and density plots for the three hyperparameters. `geweke.diag()` from `coda` performs a formal z-test comparing early and late MCMC draws — values within ±1.96 indicate stationarity of the chain.

#### 3.2 Lambda posterior vs. prior (Slide 8)
Overlays the posterior density of `lambda` (blue) against its Normal prior (red dashed). A posterior that is tighter and shifted relative to the prior demonstrates that the data are informative about shrinkage strength — the hallmark of a well-identified Bayesian model.

#### 3.3 Impulse Response Functions — VIX shock (Slides 9–10)
`irf()` computes posterior IRFs at horizons 1–20 with 68% (±1 SD) and 90% credible bands. The main result is the response of all six indices to a one-standard-deviation VIX shock, illustrating the speed and magnitude of US-to-global volatility transmission.

#### 3.4 FEVD (Slide 11)
Forecast Error Variance Decomposition quantifies what fraction of each market's *h*-step-ahead forecast uncertainty is attributable to VIX shocks versus its own shocks and other markets. High VIX shares confirm that US volatility is a dominant driver of global implied-volatility fluctuations.

#### 3.5 Unconditional 20-day Forecast (Slide 16)
`predict()` produces fan-chart forecasts from the end of the sample with 68% and 90% credible bands.

---

### Section 4 — Sub-sample Comparison

Two separate BVARs are estimated — one on the calm period and one on the crisis period — using identical prior and MCMC settings.

#### 4.1–4.2 Calm and crisis models
Estimated with `bvar()` on `data_calm` and `data_crisis` respectively.

#### 4.3 IRF comparison — calm vs. crisis (Slides 13–14)
Side-by-side `patchwork` panels show how the IRF profile of a VIX shock changed between the two regimes. The crisis-period model is expected to show larger, faster-spreading, and more persistent responses — consistent with the financial-contagion hypothesis.

#### 4.4 FEVD comparison (Slide 15)
Compares the variance shares across regimes. An increase in VIX's FEVD share during the crisis would indicate that the US market became a relatively more dominant source of global volatility uncertainty.

**Lambda comparison:** The median posterior `lambda` is printed for all three models. A lower crisis-period `lambda` implies the data favoured stronger cross-variable dynamics and less shrinkage — i.e., the crisis period was econometrically richer in inter-market linkages.

---

### Section 5 — Conditional Forecasting: Stress Scenario (Slide 17)

A moderate stress scenario is constructed: VIX rises gradually from 25 to 50 over ten trading days (comparable to the Q4 2018 market sell-off), then levels off and partially reverses.

**Implementation:**
- A conditions matrix (`cond_mat`) is built with rows = forecast horizon (20 days) and columns = variables. Only the VIX column is pinned to the stress path for the first 10 days; all other entries are `NA` (unconditioned).
- `predict(..., conditional = cond_mat)` runs Bayesian conditional forecasting: the model endogenously propagates the VIX shock to European, Asian, and Indian volatility indices through the estimated VAR dynamics.

**Why this is useful:** Conditional forecasting is a practical risk-management tool. It answers: "If VIX behaves like this, what does the model expect European and Asian volatility to do?" The comparison of unconditional vs. conditional fan charts on Slide 17 illustrates the economic magnitude of the assumed stress.

---

## Workflow Summary

```
BE_Group3_Indices.R          BE_Group3_Code.R
        |                            |
  Bloomberg API             df_indices_wide.RData
        |                            |
  df_indices_wide.RData      Section 1: Data prep + sub-samples
                             Section 2: Shared prior + MCMC settings
                             Section 3: Full-sample BVAR
                                  - Convergence diagnostics
                                  - Lambda posterior
                                  - IRF (VIX shock)
                                  - FEVD
                                  - Unconditional forecast
                             Section 4: Sub-sample BVARs (calm / crisis)
                                  - IRF comparison
                                  - FEVD comparison
                                  - Lambda comparison
                             Section 5: Conditional stress-scenario forecast
```
