################################################################################
# BVAR Shiny app for flexible climate-macro model specification
# - Loads .RData objects from disk
# - Lets users choose a dataset and variables
# - Runs your BVAR model only when the user clicks "Run model"
# - Visualises summary, IRFs, forecasts, and transformed data
################################################################################

required_pkgs <- c(
  "shiny", "dplyr", "DT", "BVAR", "coda", "rstudioapi", "bslib",
  "readxl", "ggplot2", "plotly"
)

installed_pkgs <- rownames(installed.packages())
missing_pkgs <- setdiff(required_pkgs, installed_pkgs)
if (length(missing_pkgs) > 0) install.packages(missing_pkgs)

invisible(lapply(required_pkgs, function(pkg) {
  library(pkg, character.only = TRUE)
}))

################################################################################
# PATHS + DATA LOAD
################################################################################
try({
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}, silent = TRUE)

DATA_FILE <- "all_df_regions_quarterly_vf_final.RData"
if (!file.exists(DATA_FILE)) {
  stop("Could not find '", DATA_FILE, "' in the working directory. Place app.R next to the .RData file.")
}

loaded_env <- new.env(parent = emptyenv())
load(DATA_FILE, envir = loaded_env)

extract_datasets <- function(env) {
  objs <- ls(env, all.names = TRUE)
  out <- list()
  
  for (nm in objs) {
    obj <- get(nm, envir = env)
    
    if (is.data.frame(obj)) {
      out[[nm]] <- obj
    }
    
    if (is.list(obj) && length(obj) > 0) {
      is_df_list <- all(vapply(obj, is.data.frame, logical(1), USE.NAMES = FALSE))
      if (is_df_list) {
        child_names <- names(obj)
        if (is.null(child_names) || any(child_names == "")) {
          child_names <- paste0("dataset_", seq_along(obj))
        }
        for (i in seq_along(obj)) {
          out[[paste0(nm, "::", child_names[i])]] <- obj[[i]]
        }
      }
    }
  }
  
  out
}

all_datasets <- extract_datasets(loaded_env)
if (length(all_datasets) == 0) {
  stop("No data.frames or lists of data.frames were found in the .RData file.")
}

normalize_dataset <- function(dat) {
  dat <- as.data.frame(dat)
  
  # Standardise common date column names to a dedicated quarter column if possible
  if (!"quarter" %in% names(dat)) {
    for (cand in c("Date", "date", "Quarter", "quarter_date")) {
      if (cand %in% names(dat)) {
        names(dat)[names(dat) == cand] <- "quarter"
        break
      }
    }
  }
  
  if ("quarter" %in% names(dat)) {
    suppressWarnings(dat$quarter <- as.Date(dat$quarter))
  }
  
  dat
}

all_datasets <- lapply(all_datasets, normalize_dataset)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

allowed_macro_vars <- c(
  "Cost of Borrowing for Corporations",
  "Current Account Exports",
  "Current Account Imports q",
  "Government Debt",
  "Government Primary Deficit or Surplus",
  "HICP Inflation Energy",
  "HICP Inflation Food Incl Alcohol and Tobacco",
  "HICP Inflation Services",
  "HICP Inflation Total",
  "House Price Index",
  "Real GDP",
  "Unemployment Rate",
  "Agriculture, Forestry and Fishing (GVA)",
  "Industry excl. Construction (GVA)",
  "Manufacturing (GVA)",
  "Wholesale & Retail Trade (GVA)"
)

allowed_climate_vars <- c(
  "Sea Level (mm)",
  "spread_component_mean_bot10",
  "spread_component_std",
  "swe_std",
  "swi_mean",
  "swi_std",
  "build_up_index_mean_top25",
  "drought_code_mean_top10",
  "energy_release_component_std",
  "Extreme Precipitation Total",
  "Extreme Wind Speed Days",
  "fine_fuel_moisture_code_mean_top25",
  "Frost Days",
  "Heat Waves (Climatological)",
  "High UTCI Days",
  "Mean Wind Speed",
  "runoff_6h_mean_top25",
  "runoff_6h_std",
  "runoff_6h_sum",
  "Total Precipitation",
  "days of flooding",
  "river_coastal_count"
)

default_logdiff_vars <- c(
  "drought_code_mean_top10",
  "days of flooding",
  "Current Account Exports",
  "runoff_6h_sum"
)

dataset_label_overrides <- c(
  "all_df_dach_excl_swiss_quar" = "DACH Excluding Switzerland (Quarterly)",
  "all_df_dach_quar" = "DACH (Quarterly)",
  "all_df_south_europe_quar" = "South Europe (Quarterly)"
)

variable_label_overrides <- c(
  "Cost of Borrowing for Corporations" = "Corporate borrowing cost",
  "Current Account Exports" = "Current account exports",
  "Current Account Imports q" = "Current Account Imports (Quarterly)",
  "Government Debt" = "Government debt (% GDP)",
  "Government Primary Deficit or Surplus" = "Government primary balance (% GDP)",
  "HICP Inflation Energy" = "HICP energy inflation",
  "HICP Inflation Food Incl Alcohol and Tobacco" = "HICP food inflation (incl. alcohol & tobacco)",
  "HICP Inflation Services" = "HICP services inflation",
  "HICP Inflation Total" = "HICP total inflation",
  "House Price Index" = "House price index",
  "Real GDP" = "Real GDP (volume) growth",
  "Unemployment Rate" = "Unemployment rate",
  "Agriculture, Forestry and Fishing (GVA)" = "GVA: Agriculture, forestry & fishing",
  "Industry excl. Construction (GVA)" = "GVA: Industry excl. construction",
  "Manufacturing (GVA)" = "GVA: Manufacturing",
  "Wholesale & Retail Trade (GVA)" = "GVA: Wholesale & retail trade",
  "days of flooding" = "Days of Flooding",
  "river_coastal_count" = "River Coastal Count",
  "runoff_6h_mean_top25" = "Runoff 6h Mean Top 25",
  "runoff_6h_std" = "Runoff 6h Std. Dev.",
  "runoff_6h_sum" = "Runoff 6h Sum",
  "spread_component_mean_bot10" = "Spread Component Mean Bottom 10",
  "spread_component_std" = "Spread Component Std. Dev.",
  "build_up_index_mean_top25" = "Build Up Index Mean Top 25",
  "drought_code_mean_top10" = "Drought Code Mean Top 10",
  "energy_release_component_std" = "Energy Release Component Std. Dev.",
  "fine_fuel_moisture_code_mean_top25" = "Fine Fuel Moisture Code Mean Top 25",
  "swe_std" = "SWE Std. Dev.",
  "swi_mean" = "SWI Mean",
  "swi_std" = "SWI Std. Dev."
)

resolve_existing_path <- function(paths) {
  existing <- paths[file.exists(paths)]
  if (!length(existing)) {
    return(NULL)
  }
  existing[1]
}

DATA_DICTIONARY_FILE <- resolve_existing_path(c(
  "variable_data_dictionary.xlsx",
  file.path("..", "variable_data_dictionary.xlsx"),
  file.path("..", "Other output", "data_dictionary.xlsx")
))

load_variable_dictionary <- function(path) {
  if (is.null(path) || !nzchar(path)) {
    return(data.frame(
      variable_name = character(0),
      full_variable_name = character(0),
      description = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  out <- tryCatch(
    as.data.frame(readxl::read_excel(path), stringsAsFactors = FALSE),
    error = function(e) {
      warning("Could not load the variable data dictionary: ", conditionMessage(e))
      data.frame(
        variable_name = character(0),
        full_variable_name = character(0),
        description = character(0),
        stringsAsFactors = FALSE
      )
    }
  )
  
  needed_cols <- c("variable_name", "full_variable_name", "description")
  if (!all(needed_cols %in% names(out))) {
    warning("Variable data dictionary is missing one or more required columns: ", paste(needed_cols, collapse = ", "))
    return(data.frame(
      variable_name = character(0),
      full_variable_name = character(0),
      description = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  out <- out[, needed_cols, drop = FALSE]
  out[] <- lapply(out, function(x) trimws(as.character(x)))
  out
}

categorize_variable <- function(var_name) {
  if (identical(var_name, "Date") || identical(var_name, "quarter")) {
    return("Time")
  }
  if (var_name %in% allowed_climate_vars) {
    return("Climate")
  }
  if (var_name %in% allowed_macro_vars) {
    return("Macro")
  }
  "Other"
}

variable_dictionary <- load_variable_dictionary(DATA_DICTIONARY_FILE)
if (nrow(variable_dictionary)) {
  variable_dictionary$category <- vapply(
    variable_dictionary$variable_name,
    categorize_variable,
    character(1)
  )
  variable_dictionary$display_name <- vapply(seq_len(nrow(variable_dictionary)), function(i) {
    raw_name <- variable_dictionary$variable_name[i]
    if (raw_name %in% names(variable_label_overrides)) {
      return(unname(variable_label_overrides[[raw_name]]))
    }
    full_name <- variable_dictionary$full_variable_name[i]
    if (!is.na(full_name) && nzchar(full_name)) {
      return(full_name)
    }
    raw_name
  }, character(1))
} else {
  variable_dictionary$category <- character(0)
  variable_dictionary$display_name <- character(0)
}

variable_dictionary_aliases <- c("quarter" = "Date")
variable_dictionary_display_lookup <- if (nrow(variable_dictionary)) {
  stats::setNames(variable_dictionary$display_name, variable_dictionary$variable_name)
} else {
  character(0)
}

safe_named_lookup <- function(x, key, default = NULL) {
  if (is.null(key) || !length(key) || is.na(key) || !key %in% names(x)) {
    return(default)
  }
  unname(x[key][1])
}

prior_choice_labels <- c(
  "minnesota" = "Minnesota only",
  "minnesota_soc" = "Minnesota + SOC",
  "minnesota_sur" = "Minnesota + SUR",
  "minnesota_soc_sur" = "Minnesota + SOC + SUR"
)

scenario_template_labels <- c(
  "baseline" = "Baseline forecast",
  "plus_sd" = "Higher path (+1 sigma)",
  "minus_sd" = "Lower path (-1 sigma)",
  "vol_high" = "Higher volatility",
  "vol_low" = "Lower volatility"
)

################################################################################
# BACKEND HELPERS
################################################################################
finite_complete_rows <- function(df) {
  if (!ncol(df)) return(rep(TRUE, nrow(df)))
  ok_list <- lapply(df, function(x) {
    if (is.numeric(x)) !is.na(x) & !is.nan(x) & is.finite(x) else !is.na(x)
  })
  Reduce(`&`, ok_list)
}

safe_log <- function(x, eps = 1e-6) {
  x <- as.numeric(x)
  
  if (any(is.nan(x) | is.infinite(x), na.rm = TRUE)) {
    stop("NaN or Inf values before log transform.")
  }
  
  if (any(x <= 0, na.rm = TRUE)) {
    shift <- abs(min(x, na.rm = TRUE)) + eps
    return(log(x + shift))
  }
  
  log(x)
}

compute_log_shift <- function(x, eps = 1e-6) {
  x <- as.numeric(x)
  
  if (any(is.nan(x) | is.infinite(x), na.rm = TRUE)) {
    stop("NaN or Inf values before log transform.")
  }
  
  if (any(x <= 0, na.rm = TRUE)) {
    return(abs(min(x, na.rm = TRUE)) + eps)
  }
  
  0
}

safe_log_with_shift <- function(x, eps = 1e-6) {
  shift <- compute_log_shift(x, eps = eps)
  list(values = log(as.numeric(x) + shift), shift = shift)
}

to_title_word <- function(token) {
  token_lower <- tolower(token)
  token_map <- c(
    "gdp" = "GDP",
    "hicp" = "HICP",
    "swe" = "SWE",
    "swi" = "SWI",
    "utci" = "UTCI",
    "sd" = "SD",
    "std" = "Std. Dev.",
    "bot10" = "Bottom 10",
    "top10" = "Top 10",
    "top25" = "Top 25",
    "6h" = "6h"
  )
  
  if (token_lower %in% names(token_map)) {
    return(unname(token_map[[token_lower]]))
  }
  
  if (grepl("^[0-9]+$", token_lower)) {
    return(token_lower)
  }
  
  paste0(toupper(substr(token_lower, 1, 1)), substr(token_lower, 2, nchar(token_lower)))
}

display_label <- function(x) {
  x <- as.character(x)
  
  if (!length(x)) {
    return(character(0))
  }
  
  out <- vapply(x, function(item) {
    if (item %in% names(dataset_label_overrides)) {
      return(unname(dataset_label_overrides[[item]]))
    }
    
    lookup_key <- safe_named_lookup(variable_dictionary_aliases, item, default = item)
    if (lookup_key %in% names(variable_dictionary_display_lookup)) {
      return(unname(variable_dictionary_display_lookup[[lookup_key]]))
    }
    
    if (item %in% names(variable_label_overrides)) {
      return(unname(variable_label_overrides[[item]]))
    }
    
    if (!grepl("_", item, fixed = TRUE) && !grepl("^[a-z]", item)) {
      return(item)
    }
    
    cleaned <- gsub("_", " ", item, fixed = TRUE)
    parts <- unlist(strsplit(cleaned, "\\s+"))
    paste(vapply(parts, to_title_word, character(1)), collapse = " ")
  }, character(1))
  
  unname(out)
}

lookup_variable_dictionary <- function(vars) {
  vars <- as.character(vars %||% character(0))
  if (!length(vars)) {
    return(data.frame(
      variable_name = character(0),
      full_variable_name = character(0),
      description = character(0),
      category = character(0),
      display_name = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  out <- lapply(vars, function(var_name) {
    lookup_key <- safe_named_lookup(variable_dictionary_aliases, var_name, default = var_name)
    row <- variable_dictionary[variable_dictionary$variable_name == lookup_key, , drop = FALSE]
    
    if (!nrow(row)) {
      row <- data.frame(
        variable_name = lookup_key,
        full_variable_name = display_label(var_name),
        description = "No definition was found in the data dictionary file for this variable.",
        category = categorize_variable(var_name),
        display_name = display_label(var_name),
        stringsAsFactors = FALSE
      )
    }
    
    row$variable_name <- var_name
    row$display_name <- display_label(var_name)
    row$category <- categorize_variable(var_name)
    row
  })
  
  dplyr::bind_rows(out)
}

named_choices <- function(values) {
  values <- values %||% character(0)
  stats::setNames(values, display_label(values))
}

pretty_var_text <- function(values, collapse = ", ") {
  values <- values %||% character(0)
  if (!length(values)) {
    return("None")
  }
  paste(display_label(values), collapse = collapse)
}

blank_transform_spec <- function(var) {
  list(
    variable = var,
    label = display_label(var),
    log = FALSE,
    log_shift = 0,
    logdiff = FALSE,
    scaled = FALSE,
    scale_center = 0,
    scale_scale = 1,
    last_raw_value = NA_real_
  )
}

unscale_values <- function(x, info) {
  x <- as.numeric(x)
  
  if (!isTRUE(info$scaled)) {
    return(x)
  }
  
  scale_scale <- as.numeric(info$scale_scale %||% 1)
  scale_center <- as.numeric(info$scale_center %||% 0)
  
  if (!is.finite(scale_scale) || isTRUE(all.equal(scale_scale, 0))) {
    scale_scale <- 1
  }
  
  x * scale_scale + scale_center
}

scale_values <- function(x, info) {
  x <- as.numeric(x)
  
  if (!isTRUE(info$scaled)) {
    return(x)
  }
  
  scale_scale <- as.numeric(info$scale_scale %||% 1)
  scale_center <- as.numeric(info$scale_center %||% 0)
  
  if (!is.finite(scale_scale) || isTRUE(all.equal(scale_scale, 0))) {
    scale_scale <- 1
  }
  
  (x - scale_center) / scale_scale
}

back_transform_values <- function(model_values, info, previous_raw = info$last_raw_value) {
  x <- unscale_values(model_values, info)
  
  if (isTRUE(info$logdiff)) {
    prev_log <- log1p(as.numeric(previous_raw))
    out <- numeric(length(x))
    
    for (i in seq_along(x)) {
      prev_log <- prev_log + x[i]
      out[i] <- exp(prev_log) - 1
    }
    
    return(out)
  }
  
  if (isTRUE(info$log)) {
    return(exp(x) - as.numeric(info$log_shift %||% 0))
  }
  
  x
}

transform_values_for_conditioning <- function(raw_values, info, previous_raw = info$last_raw_value) {
  x <- as.numeric(raw_values)
  
  if (isTRUE(info$logdiff)) {
    prev_log <- log1p(as.numeric(previous_raw))
    out <- numeric(length(x))
    
    for (i in seq_along(x)) {
      current_log <- log1p(x[i])
      out[i] <- current_log - prev_log
      prev_log <- current_log
    }
    
    return(scale_values(out, info))
  }
  
  if (isTRUE(info$log)) {
    return(scale_values(log(x + as.numeric(info$log_shift %||% 0)), info))
  }
  
  scale_values(x, info)
}

estimate_innovation_scale <- function(x, order, differences = 0) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  
  if (length(x) <= (order + differences + 2)) {
    return(NA_real_)
  }
  
  ar_order <- max(1L, min(as.integer(order), length(x) - differences - 2L))
  if (!is.finite(ar_order) || ar_order < 1) {
    return(NA_real_)
  }
  
  fit <- suppressWarnings(
    tryCatch(
      stats::arima(
        x,
        order = c(ar_order, as.integer(differences), 0),
        include.mean = differences == 0,
        method = "ML"
      ),
      error = function(e) NULL
    )
  )
  
  if (is.null(fit)) {
    return(NA_real_)
  }
  
  sigma2 <- suppressWarnings(tryCatch(as.numeric(fit$sigma2), error = function(e) NA_real_))
  if (!is.finite(sigma2) || sigma2 <= 0) {
    return(NA_real_)
  }
  
  sqrt(sigma2)
}

estimate_ols_ar_scale <- function(x, order) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  
  if (length(x) <= (order + 2)) {
    return(NA_real_)
  }
  
  ar_order <- max(1L, min(as.integer(order), length(x) - 2L))
  emb <- tryCatch(stats::embed(x, ar_order + 1L), error = function(e) NULL)
  if (is.null(emb) || !nrow(emb)) {
    return(NA_real_)
  }
  
  y <- emb[, 1]
  xreg <- cbind(1, emb[, -1, drop = FALSE])
  fit <- suppressWarnings(tryCatch(stats::lm.fit(x = xreg, y = y), error = function(e) NULL))
  if (is.null(fit) || is.null(fit$residuals)) {
    return(NA_real_)
  }
  
  resid_sd <- stats::sd(fit$residuals, na.rm = TRUE)
  if (!is.finite(resid_sd) || resid_sd <= 0) {
    return(NA_real_)
  }
  
  resid_sd
}

compute_manual_psi <- function(Ymat, lags, min_ratio = 0.01, max_ratio = 100, floor_value = 1e-4) {
  if (!is.matrix(Ymat)) {
    Ymat <- as.matrix(Ymat)
  }
  
  if (!ncol(Ymat)) {
    return(data.frame())
  }
  
  var_names <- colnames(Ymat) %||% paste0("V", seq_len(ncol(Ymat)))
  base_order <- max(1L, min(as.integer(lags), max(nrow(Ymat) - 2L, 1L)))
  
  out <- lapply(seq_len(ncol(Ymat)), function(j) {
    x <- as.numeric(Ymat[, j])
    x <- x[is.finite(x)]
    
    order_j <- max(1L, min(base_order, max(length(x) - 2L, 1L)))
    level_scale <- estimate_innovation_scale(x, order = order_j, differences = 0)
    diff_scale <- estimate_innovation_scale(x, order = order_j, differences = 1)
    ols_resid_scale <- estimate_ols_ar_scale(x, order = order_j)
    diff_sd <- if (length(x) > 1) stats::sd(diff(x), na.rm = TRUE) else NA_real_
    raw_sd <- if (length(x) > 1) stats::sd(x, na.rm = TRUE) else NA_real_
    
    candidates <- c(
      "AR(p) innovation" = level_scale,
      "ARIMA(p,1,0) innovation" = diff_scale,
      "AR OLS residual" = ols_resid_scale,
      "First-difference sd" = diff_sd,
      "Series sd" = raw_sd
    )
    
    valid_idx <- which(is.finite(candidates) & candidates > 0)
    chosen_scale <- if (length(valid_idx)) unname(candidates[valid_idx[1]]) else NA_real_
    chosen_source <- if (length(valid_idx)) names(candidates)[valid_idx[1]] else "Median fallback"
    
    data.frame(
      variable = var_names[j],
      psi_source = chosen_source,
      psi_mode_raw = chosen_scale,
      level_scale = level_scale,
      diff_scale = diff_scale,
      ols_resid_scale = ols_resid_scale,
      diff_sd = diff_sd,
      raw_sd = raw_sd,
      stringsAsFactors = FALSE
    )
  })
  
  out <- dplyr::bind_rows(out)
  valid_modes <- out$psi_mode_raw[is.finite(out$psi_mode_raw) & out$psi_mode_raw > 0]
  fallback_mode <- if (length(valid_modes)) stats::median(valid_modes) else 1
  use_fallback <- !is.finite(out$psi_mode_raw) | out$psi_mode_raw <= 0
  
  out$psi_source[use_fallback] <- "Median fallback"
  out$psi_mode <- ifelse(use_fallback, fallback_mode, out$psi_mode_raw)
  out$psi_mode <- pmax(out$psi_mode, floor_value)
  out$psi_min <- pmax(out$psi_mode * min_ratio, floor_value)
  out$psi_max <- pmax(out$psi_mode * max_ratio, out$psi_min * 1.01)
  
  out[, c(
    "variable", "psi_source", "psi_mode", "psi_min", "psi_max",
    "level_scale", "diff_scale", "ols_resid_scale", "diff_sd", "raw_sd"
  )]
}

prepare_dataset <- function(dat, date_col = NULL) {
  stopifnot(is.data.frame(dat))
  
  out <- dat
  if (!is.null(date_col) && nzchar(date_col) && date_col %in% names(out)) {
    if (date_col != "quarter" && !"quarter" %in% names(out)) {
      names(out)[names(out) == date_col] <- "quarter"
      date_col <- "quarter"
    }
    if (date_col %in% names(out)) {
      suppressWarnings(out[[date_col]] <- as.Date(out[[date_col]]))
    }
  }
  
  info <- data.frame(
    column = names(out),
    class = vapply(out, function(x) paste(class(x), collapse = ", "), character(1)),
    n_missing = vapply(out, function(x) sum(is.na(x)), integer(1)),
    stringsAsFactors = FALSE
  )
  
  list(
    data = out,
    info = info,
    numeric_columns = names(out)[vapply(out, is.numeric, logical(1))],
    all_columns = names(out)
  )
}

format_period_value <- function(x) {
  if (inherits(x, "Date")) {
    if (is.na(x)) {
      return(NA_character_)
    }
    month_num <- suppressWarnings(as.integer(format(x, "%m")))
    quarter_num <- ((month_num - 1L) %/% 3L) + 1L
    return(paste0(format(x, "%Y"), " Q", quarter_num))
  }
  as.character(x)
}

format_numeric_value <- function(x, digits = 3L) {
  x <- suppressWarnings(as.numeric(x))
  if (!length(x)) {
    return(character(0))
  }
  
  out <- rep(NA_character_, length(x))
  ok <- is.finite(x)
  
  if (any(ok)) {
    out[ok] <- formatC(x[ok], format = "f", digits = digits)
  }
  
  out
}

make_time_index <- function(dat, date_col = "quarter") {
  if (!is.null(date_col) && nzchar(date_col) && date_col %in% names(dat)) {
    return(dat[[date_col]])
  }
  seq_len(nrow(dat))
}

variable_role_label <- function(var_name, climate_vars, macro_vars) {
  roles <- c(
    if (var_name %in% (climate_vars %||% character(0))) "Climate",
    if (var_name %in% (macro_vars %||% character(0))) "Macro"
  )
  
  if (!length(roles)) {
    return(categorize_variable(var_name))
  }
  
  paste(roles, collapse = " + ")
}

variable_transformation_label <- function(var_name, log_vars, logdiff_vars, scale_vars) {
  transforms <- c(
    if (var_name %in% (log_vars %||% character(0))) "Log",
    if (var_name %in% (logdiff_vars %||% character(0))) "Log-difference",
    if (var_name %in% (scale_vars %||% character(0))) "Scaled"
  )
  
  if (!length(transforms)) {
    return("None")
  }
  
  paste(transforms, collapse = "; ")
}

variable_transformation_reason <- function(var_name, log_vars, logdiff_vars, scale_vars) {
  reasons <- c()
  
  if (var_name %in% (log_vars %||% character(0))) {
    reasons <- c(reasons, "Log transform selected to interpret movements more proportionally and compress scale.")
  }
  if (var_name %in% (logdiff_vars %||% character(0))) {
    if (var_name %in% default_logdiff_vars) {
      reasons <- c(reasons, "Automatic default log-difference: this is a persistent positive series where change-based dynamics are typically easier to model and forecast.")
    } else {
      reasons <- c(reasons, "User-selected log-difference: useful for persistent positive series when growth-like movements are more informative than levels.")
    }
  }
  if (var_name %in% (scale_vars %||% character(0))) {
    reasons <- c(reasons, "Scaling selected to standardize units and improve numerical stability across differently sized variables.")
  }
  
  if (!length(reasons)) {
    return("No transformation is currently selected for this variable.")
  }
  
  paste(reasons, collapse = " ")
}

build_transformation_summary <- function(
    selected_vars,
    climate_vars,
    macro_vars,
    log_vars,
    logdiff_vars,
    scale_vars
) {
  selected_vars <- selected_vars %||% character(0)
  if (!length(selected_vars)) {
    return(empty_message_table("No model variables are currently selected."))
  }
  
  dict_tbl <- lookup_variable_dictionary(selected_vars)
  
  data.frame(
    Variable = dict_tbl$display_name,
    Role = vapply(selected_vars, variable_role_label, character(1), climate_vars = climate_vars, macro_vars = macro_vars),
    `Current transformation` = vapply(selected_vars, variable_transformation_label, character(1), log_vars = log_vars, logdiff_vars = logdiff_vars, scale_vars = scale_vars),
    Why = vapply(selected_vars, variable_transformation_reason, character(1), log_vars = log_vars, logdiff_vars = logdiff_vars, scale_vars = scale_vars),
    stringsAsFactors = FALSE
  )
}

single_variable_summary_table <- function(dat, var_name, date_col = "quarter") {
  if (is.null(var_name) || !nzchar(var_name) || !var_name %in% names(dat)) {
    return(empty_message_table("Choose a variable to see its summary statistics."))
  }
  
  x <- suppressWarnings(as.numeric(dat[[var_name]]))
  idx <- make_time_index(dat, date_col = date_col)
  finite_idx <- which(is.finite(x))
  latest_idx <- if (length(finite_idx)) utils::tail(finite_idx, 1) else NA_integer_
  
  stats_tbl <- data.frame(
    Metric = c(
      "Observations",
      "Missing values",
      "Latest available period",
      "Latest available value",
      "Mean",
      "Median",
      "Standard deviation",
      "Minimum",
      "Maximum"
    ),
    Value = c(
      length(x),
      sum(!is.finite(x)),
      if (is.finite(latest_idx)) format_period_value(idx[latest_idx]) else NA_character_,
      if (is.finite(latest_idx)) format_numeric_value(x[latest_idx]) else NA_character_,
      format_numeric_value(mean(x, na.rm = TRUE)),
      format_numeric_value(stats::median(x, na.rm = TRUE)),
      format_numeric_value(stats::sd(x, na.rm = TRUE)),
      format_numeric_value(suppressWarnings(min(x, na.rm = TRUE))),
      format_numeric_value(suppressWarnings(max(x, na.rm = TRUE)))
    ),
    stringsAsFactors = FALSE
  )
  
  stats_tbl
}

selected_variable_overview_table <- function(
    dat,
    vars,
    climate_vars,
    macro_vars,
    log_vars,
    logdiff_vars,
    scale_vars,
    date_col = "quarter"
) {
  vars <- vars %||% character(0)
  if (!length(vars)) {
    return(empty_message_table("Choose one or more selected model variables, then click Update selected variable plots."))
  }
  
  idx <- make_time_index(dat, date_col = date_col)
  dict_tbl <- lookup_variable_dictionary(vars)
  
  out <- lapply(seq_along(vars), function(i) {
    var_name <- vars[i]
    x <- suppressWarnings(as.numeric(dat[[var_name]]))
    finite_idx <- which(is.finite(x))
    latest_idx <- if (length(finite_idx)) utils::tail(finite_idx, 1) else NA_integer_
    
    data.frame(
      Variable = dict_tbl$display_name[i],
      Role = variable_role_label(var_name, climate_vars = climate_vars, macro_vars = macro_vars),
      Definition = dict_tbl$description[i],
      `Current transformation` = variable_transformation_label(var_name, log_vars = log_vars, logdiff_vars = logdiff_vars, scale_vars = scale_vars),
      `Latest period` = if (is.finite(latest_idx)) format_period_value(idx[latest_idx]) else NA_character_,
      `Latest value` = if (is.finite(latest_idx)) format_numeric_value(x[latest_idx]) else NA_character_,
      `Missing values` = sum(!is.finite(x)),
      stringsAsFactors = FALSE
    )
  })
  
  dplyr::bind_rows(out)
}

selected_variable_correlation_table <- function(dat, vars) {
  vars <- vars %||% character(0)
  if (length(vars) < 2) {
    return(empty_message_table("Select at least two variables to show pairwise correlations."))
  }
  
  num_df <- dat[, vars, drop = FALSE]
  cor_mat <- suppressWarnings(stats::cor(num_df, use = "pairwise.complete.obs"))
  
  if (all(!is.finite(cor_mat))) {
    return(empty_message_table("Pairwise correlations could not be computed for the chosen variables."))
  }
  
  upper_idx <- which(upper.tri(cor_mat), arr.ind = TRUE)
  if (!nrow(upper_idx)) {
    return(empty_message_table("Pairwise correlations could not be computed for the chosen variables."))
  }
  
  data.frame(
    `Variable 1` = display_label(colnames(cor_mat)[upper_idx[, 1]]),
    `Variable 2` = display_label(colnames(cor_mat)[upper_idx[, 2]]),
    Correlation = round(cor_mat[upper_idx], 3),
    stringsAsFactors = FALSE
  )
}

single_variable_plot_frame <- function(dat, var_name, date_col = "quarter") {
  if (is.null(var_name) || !nzchar(var_name) || !var_name %in% names(dat)) {
    return(NULL)
  }
  
  idx <- make_time_index(dat, date_col = date_col)
  x <- suppressWarnings(as.numeric(dat[[var_name]]))
  out <- data.frame(
    index = idx,
    index_label = vapply(idx, format_period_value, character(1)),
    value = x,
    variable = display_label(var_name),
    raw_variable = var_name,
    stringsAsFactors = FALSE
  )
  
  out[is.finite(out$value), , drop = FALSE]
}

selected_variable_plot_frame <- function(dat, vars, date_col = "quarter") {
  vars <- vars %||% character(0)
  if (!length(vars)) {
    return(NULL)
  }
  
  dplyr::bind_rows(lapply(vars, function(var_name) {
    out <- single_variable_plot_frame(dat, var_name, date_col = date_col)
    if (is.null(out)) {
      return(NULL)
    }
    out$variable <- display_label(var_name)
    out
  }))
}

theme_bvar_plot <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(color = "#d9e2ec"),
      panel.grid.major.y = ggplot2::element_line(color = "#d9e2ec"),
      strip.text = ggplot2::element_text(face = "bold"),
      plot.title = ggplot2::element_text(face = "bold"),
      legend.position = "bottom"
    )
}

as_interactive_plot <- function(plot_obj, tooltip = "text") {
  plotly_obj <- withCallingHandlers(
    plotly::ggplotly(plot_obj, tooltip = tooltip),
    warning = function(w) {
      if (grepl("Ignoring unknown aesthetics: text", conditionMessage(w), fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )
  
  plotly::config(
    plotly_obj,
    displaylogo = FALSE,
    modeBarButtonsToRemove = c("lasso2d", "select2d")
  )
}

validate_bvar_inputs <- function(
    dat,
    climate_vars,
    macro_vars,
    impulse_vars,
    response_vars,
    log_vars,
    logdiff_vars,
    scale_vars,
    lags,
    date_col = "quarter"
) {
  messages <- character()
  warnings <- character()
  
  all_vars <- unique(c(climate_vars, macro_vars))
  
  if (!length(climate_vars)) messages <- c(messages, "Select at least one climate variable.")
  if (!length(macro_vars)) messages <- c(messages, "Select at least one macro variable.")
  if (!length(all_vars)) messages <- c(messages, "Select model variables before running.")
  
  missing_cols <- setdiff(all_vars, names(dat))
  if (length(missing_cols)) messages <- c(messages, paste("Variables not found in dataset:", paste(missing_cols, collapse = ", ")))
  
  existing_vars <- intersect(all_vars, names(dat))
  non_numeric <- existing_vars[vapply(dat[existing_vars], function(x) !is.numeric(x), logical(1))]
  if (length(non_numeric)) messages <- c(messages, paste("These selected model variables are not numeric:", paste(non_numeric, collapse = ", ")))
  
  bad_log <- setdiff(log_vars, all_vars)
  if (length(bad_log)) messages <- c(messages, paste("log_vars must be selected model variables:", paste(bad_log, collapse = ", ")))
  
  bad_logdiff <- setdiff(logdiff_vars, all_vars)
  if (length(bad_logdiff)) {
    messages <- c(messages, paste("logdiff_vars must be selected model variables:", paste(bad_logdiff, collapse = ", ")))
  }
  
  overlap_transform <- intersect(log_vars, logdiff_vars)
  if (length(overlap_transform)) {
    messages <- c(messages, paste(
      "Variables cannot be selected in both log_vars and logdiff_vars:",
      paste(overlap_transform, collapse = ", ")
    ))
  }
  
  bad_scale <- setdiff(scale_vars, all_vars)
  if (length(bad_scale)) messages <- c(messages, paste("scale_vars must be selected model variables:", paste(bad_scale, collapse = ", ")))
  
  if (!length(impulse_vars)) {
    messages <- c(messages, "Select at least one impulse variable.")
  }
  
  if (!length(response_vars)) {
    messages <- c(messages, "Select at least one response variable.")
  }
  
  bad_impulse <- setdiff(impulse_vars, all_vars)
  if (length(bad_impulse)) {
    messages <- c(messages, paste("impulse_vars must be selected model variables:", paste(bad_impulse, collapse = ", ")))
  }
  
  bad_response <- setdiff(response_vars, all_vars)
  if (length(bad_response)) {
    messages <- c(messages, paste("response_vars must be selected model variables:", paste(bad_response, collapse = ", ")))
  }
  
  Yraw <- dat[, intersect(all_vars, names(dat)), drop = FALSE]
  keep <- finite_complete_rows(Yraw)
  n_complete <- sum(keep)
  k <- ncol(Yraw)
  
  log_problems <- character()
  for (v in intersect(log_vars, names(dat))) {
    x <- suppressWarnings(as.numeric(dat[[v]][keep]))
    
    if (length(x) == 0 || all(is.na(x))) {
      log_problems <- c(log_problems, paste0(v, " (all usable values NA after filtering)"))
    } else if (any(is.nan(x) | is.infinite(x), na.rm = TRUE)) {
      log_problems <- c(log_problems, paste0(v, " (NaN or Inf values present in usable sample)"))
    } else if (any(x <= 0, na.rm = TRUE)) {
      warnings <- c(warnings, paste0(v, ": contains non-positive values in usable sample, so safe_log() will shift before logging."))
    }
  }
  if (length(log_problems)) {
    messages <- c(messages, paste("These log variables are problematic:", paste(log_problems, collapse = "; ")))
  }
  
  logdiff_problems <- character()
  for (v in intersect(logdiff_vars, names(dat))) {
    x <- suppressWarnings(as.numeric(dat[[v]][keep]))
    
    if (length(x) == 0 || all(is.na(x))) {
      logdiff_problems <- c(logdiff_problems, paste0(v, " (all usable values NA after filtering)"))
    } else if (any(is.nan(x) | is.infinite(x), na.rm = TRUE)) {
      logdiff_problems <- c(logdiff_problems, paste0(v, " (NaN or Inf values present in usable sample)"))
    } else if (any(x <= -1, na.rm = TRUE)) {
      logdiff_problems <- c(logdiff_problems, paste0(v, " (contains values <= -1 in usable sample, so log1p is undefined)"))
    }
  }
  if (length(logdiff_problems)) {
    messages <- c(messages, paste("These log-difference variables are problematic:", paste(logdiff_problems, collapse = "; ")))
  }
  
  recommended_logdiff_missing <- setdiff(intersect(default_logdiff_vars, all_vars), logdiff_vars)
  if (length(recommended_logdiff_missing)) {
    warnings <- c(
      warnings,
      paste(
        "Recommended log-difference variables for more stable forecast scaling are not selected:",
        pretty_var_text(recommended_logdiff_missing)
      )
    )
  }
  
  constant_vars <- character()
  for (v in existing_vars) {
    x <- suppressWarnings(as.numeric(dat[[v]][keep]))
    x <- x[is.finite(x)]
    
    if (length(unique(x)) <= 1) {
      constant_vars <- c(constant_vars, v)
    }
  }
  
  if (length(constant_vars)) {
    warnings <- c(
      warnings,
      paste(
        "These variables are constant in the usable sample and can cause singularity problems:",
        pretty_var_text(constant_vars)
      )
    )
  }
  
  if ("Current Account Exports" %in% macro_vars &&
      any(c("runoff_6h_sum", "river_coastal_count") %in% climate_vars)) {
    warnings <- c(
      warnings,
      paste(
        "Current Account Exports can become singular with",
        pretty_var_text(intersect(c("runoff_6h_sum", "river_coastal_count"), climate_vars)),
        "in some datasets. This is especially likely when River Coastal Count is constant."
      )
    )
  }
  
  if ("House Price Index" %in% macro_vars) {
    house_price_vars <- intersect(c("House Price Index", climate_vars), names(dat))
    hpi_complete <- sum(finite_complete_rows(dat[, house_price_vars, drop = FALSE]))
    
    if (length(climate_vars) > 2) {
      warnings <- c(
        warnings,
        paste0(
          "House Price Index has only ",
          hpi_complete,
          " usable observations with the current climate selection. Forecasts become fragile when more than two climate variables are included."
        )
      )
    }
  }
  
 
  
  if (n_complete < (lags + 5)) {
    messages <- c(messages, paste0("Too few complete observations after NA filtering: ", n_complete,
                                   ". Need at least lags + 5 = ", lags + 5, "."))
  }
  
  rough_ratio <- NA_real_
  if (k > 0 && n_complete > 0) {
    rough_ratio <- n_complete / max(1, k * lags)
    if (rough_ratio < 3) {
      warnings <- c(
        warnings,
        paste0(
          "Sample is small relative to model size (T / (K * lags) = ",
          round(rough_ratio, 2),
          "). Recommendation: reduce lags or reduce the number of selected variables."
        )
      )
    } else if (rough_ratio < 5) {
      warnings <- c(
        warnings,
        paste0(
          "Model size is somewhat aggressive relative to usable sample (T / (K * lags) = ",
          round(rough_ratio, 2),
          "). Results may still be usable, but caution is recommended."
        )
      )
    }
  }
  
  list(
    ok = length(messages) == 0,
    messages = unique(messages),
    warnings = unique(warnings),
    n_total = nrow(dat),
    n_complete = n_complete,
    k = k,
    lags = lags,
    date_col = date_col,
    rough_ratio = rough_ratio
  )
}

run_bvar_climate_macro <- function(
    dat,
    climate_vars,
    macro_vars,
    date_col = "quarter",
    log_vars = macro_vars,
    logdiff_vars = NULL,
    scale_vars = climate_vars,
    safe_log_shift_eps = 1e-6,
    lags   = 5,
    n_draw = 15000,
    n_burn = 5000,
    n_thin = 1,
    prior_spec = "minnesota",
    hyper_mode = "auto",
    alpha_sd = 0.25,
    alpha_min = 1,
    alpha_max = 3,
    soc_mode = 1,
    soc_sd = 1,
    soc_min = 1e-4,
    soc_max = 50,
    sur_mode = 1,
    sur_sd = 1,
    sur_min = 1e-4,
    sur_max = 50,
    lambda_mode = 0.2,
    lambda_sd   = 0.4,
    lambda_min  = 1e-4,
    lambda_max  = 5,
    alpha_mode  = 2,
    intercept_var = 1e7,
    scale_hess = 0.05,
    adjust_acc = TRUE,
    acc_lower  = 0.25,
    acc_upper  = 0.45,
    do_irf = TRUE,
    irf_horizon = 12,
    conf_bands = c(0.05, 0.16),
    identification = TRUE,
    impulse_vars = NULL,
    response_vars = macro_vars,
    do_forecast = TRUE,
    forecast_horizon = 8,
    conditional_forecast = FALSE,
    scenario_var = NULL,
    scenario_values = NULL,
    run_multichain_diag = FALSE,
    n_chains = 4L,
    parallel_chains = TRUE,
    seed_base = 1234L,
    verbose = FALSE
) {
  all_vars <- unique(c(climate_vars, macro_vars))
  missing_cols <- setdiff(all_vars, names(dat))
  if (length(missing_cols) > 0) {
    stop("These variables were not found in dat: ", paste(missing_cols, collapse = ", "))
  }
  
  if (!is.null(date_col) && date_col %in% names(dat)) {
    if (!inherits(dat[[date_col]], "Date")) {
      suppressWarnings(dat[[date_col]] <- as.Date(dat[[date_col]]))
    }
  }
  
  Yraw <- dat %>% dplyr::select(dplyr::all_of(all_vars))
  keep <- finite_complete_rows(Yraw)
  dat_clean <- dat[keep, , drop = FALSE]
  Y_clean <- Yraw[keep, , drop = FALSE]
  
  if (nrow(Y_clean) < (lags + 5)) {
    stop("Too few usable observations after removing NAs for the chosen lag length.")
  }
  
  Y <- Y_clean
  transform_info <- stats::setNames(lapply(names(Y), blank_transform_spec), names(Y))
  
  if (!is.null(logdiff_vars) && length(logdiff_vars) > 0) {
    bad <- setdiff(logdiff_vars, names(Y))
    if (length(bad) > 0) stop("logdiff_vars not in selected variables: ", paste(bad, collapse = ", "))
    
    for (v in logdiff_vars) {
      x <- as.numeric(Y[[v]])
      if (any(is.nan(x) | is.infinite(x), na.rm = TRUE)) {
        stop("logdiff_vars contains NaN or Inf for variable: ", v)
      }
      if (any(x <= -1, na.rm = TRUE)) {
        stop("logdiff_vars contains values <= -1 for variable: ", v, ". log1p is not defined there.")
      }
      transform_info[[v]]$logdiff <- TRUE
      Y[[v]] <- c(NA_real_, diff(log1p(x)))
    }
  }
  
  if (!is.null(log_vars) && length(log_vars) > 0) {
    bad <- setdiff(log_vars, names(Y))
    if (length(bad) > 0) stop("log_vars not in selected variables: ", paste(bad, collapse = ", "))
    
    overlap <- intersect(log_vars, logdiff_vars %||% character(0))
    if (length(overlap) > 0) {
      stop("Variables cannot be in both log_vars and logdiff_vars: ", paste(overlap, collapse = ", "))
    }
    
    for (v in log_vars) {
      log_out <- safe_log_with_shift(Y[[v]], eps = safe_log_shift_eps)
      transform_info[[v]]$log <- TRUE
      transform_info[[v]]$log_shift <- log_out$shift
      Y[[v]] <- log_out$values
    }
  }
  
  if (!is.null(scale_vars) && length(scale_vars) > 0) {
    bad <- setdiff(scale_vars, names(Y))
    if (length(bad) > 0) stop("scale_vars not in selected variables: ", paste(bad, collapse = ", "))
    for (v in scale_vars) {
      scale_out <- scale(Y[[v]])
      scale_center <- as.numeric(attr(scale_out, "scaled:center"))
      scale_scale <- as.numeric(attr(scale_out, "scaled:scale"))
      
      if (!is.finite(scale_scale) || isTRUE(all.equal(scale_scale, 0))) {
        stop("Cannot scale variable with zero or undefined variance: ", v)
      }
      
      transform_info[[v]]$scaled <- TRUE
      transform_info[[v]]$scale_center <- scale_center
      transform_info[[v]]$scale_scale <- scale_scale
      Y[[v]] <- as.numeric(scale_out)
    }
  }
  
  keep2 <- finite_complete_rows(Y)
  Y <- Y[keep2, , drop = FALSE]
  dat_clean <- dat_clean[keep2, , drop = FALSE]
  for (v in names(transform_info)) {
    transform_info[[v]]$last_raw_value <- as.numeric(utils::tail(dat_clean[[v]], 1))
  }
  
  if (nrow(Y) < (lags + 5)) {
    stop("Too few usable observations after transformations for the chosen lag length.")
  }
  
  ordered_vars <- c(climate_vars, macro_vars)
  ordered_vars <- ordered_vars[ordered_vars %in% names(Y)]
  Y <- Y %>% dplyr::select(dplyr::all_of(ordered_vars))
  
  Ymat <- as.matrix(Y)
  storage.mode(Ymat) <- "double"
  if (any(!is.finite(Ymat))) stop("Non-finite values after transformations.")
  psi_summary <- compute_manual_psi(Ymat, lags = lags)
  
  fitted_vars <- colnames(Ymat)
  climate_vars_fit <- intersect(climate_vars, fitted_vars)
  macro_vars_fit   <- intersect(macro_vars, fitted_vars)
  response_vars_fit <- intersect(response_vars, fitted_vars)
  
  climate_idx  <- match(climate_vars_fit, fitted_vars)
  macro_idx    <- match(macro_vars_fit, fitted_vars)
  response_idx <- match(response_vars_fit, fitted_vars)
  
  if (is.null(impulse_vars) || length(impulse_vars) == 0) {
    impulse_vars_fit <- climate_vars_fit
    impulse_idx <- climate_idx
  } else {
    impulse_vars_fit <- intersect(impulse_vars, fitted_vars)
    impulse_idx <- match(impulse_vars_fit, fitted_vars)
  }
  
  if (length(impulse_idx) == 0 || any(is.na(impulse_idx))) {
    stop("No valid impulse variables were found among fitted variables.")
  }
  
  priors <- build_bvar_priors(
    prior_spec = prior_spec,
    hyper_mode = hyper_mode,
    lambda_mode = lambda_mode,
    lambda_sd   = lambda_sd,
    lambda_min  = lambda_min,
    lambda_max  = lambda_max,
    alpha_mode  = alpha_mode,
    alpha_sd    = alpha_sd,
    alpha_min   = alpha_min,
    alpha_max   = alpha_max,
    psi_mode    = psi_summary$psi_mode,
    psi_min     = psi_summary$psi_min,
    psi_max     = psi_summary$psi_max,
    intercept_var = intercept_var,
    soc_mode = soc_mode,
    soc_sd   = soc_sd,
    soc_min  = soc_min,
    soc_max  = soc_max,
    sur_mode = sur_mode,
    sur_sd   = sur_sd,
    sur_min  = sur_min,
    sur_max  = sur_max
  )
  
  mh <- BVAR::bv_metropolis(
    scale_hess = scale_hess,
    adjust_acc = adjust_acc,
    acc_lower  = acc_lower,
    acc_upper  = acc_upper
  )
  
  fit <- BVAR::bvar(
    Ymat,
    lags    = lags,
    n_draw  = n_draw,
    n_burn  = n_burn,
    n_thin  = n_thin,
    priors  = priors,
    mh      = mh,
    verbose = verbose
  )
  
  hyper_summary <- summarize_hyperparameters(fit)
  optim_summary <- summarize_optimizer(fit)
  
  irf_obj <- NULL
  fevd_obj <- NULL
  
  if (isTRUE(do_irf)) {
    opt_irf <- BVAR::bv_irf(
      horizon = irf_horizon,
      fevd = TRUE,
      identification = identification
    )
    
    BVAR::irf(fit) <- BVAR::irf(fit, opt_irf, conf_bands = conf_bands)
    irf_obj <- BVAR::irf(fit)
    fevd_obj <- BVAR::fevd(fit)
  }
  
  forecast_obj <- NULL
  forecast_cond_obj <- NULL
  
  if (isTRUE(do_forecast)) {
    BVAR::predict(fit) <- stats::predict(
      fit,
      horizon = forecast_horizon,
      conf_bands = conf_bands
    )
    forecast_obj <- stats::predict(fit)
    
    if (isTRUE(conditional_forecast) && !is.null(scenario_var) && nzchar(scenario_var)) {
      if (!scenario_var %in% fitted_vars) {
        stop("Scenario variable must be one of the fitted model variables.")
      }
    }
    
    if (isTRUE(conditional_forecast) && !is.null(scenario_var)) {
      cond_setup <- make_conditional_path(
        fitted_vars = fitted_vars,
        forecast_horizon = forecast_horizon,
        scenario_var = scenario_var,
        scenario_values = scenario_values
      )
      
      if (!is.null(cond_setup)) {
        cond_opt <- BVAR::bv_fcast(
          horizon = forecast_horizon,
          cond_path = cond_setup$cond_path,
          cond_vars = cond_setup$cond_vars
        )
        
        forecast_cond_obj <- stats::predict(
          fit,
          cond_path = cond_setup$cond_path,
          cond_vars = cond_setup$cond_vars,
          horizon = forecast_horizon,
          conf_bands = conf_bands
        )
      }
    }
  }
  
  summ <- capture.output(summary(fit))
  convergence <- compute_convergence_diagnostics(
    fit = fit,
    Ymat = Ymat,
    lags = lags,
    n_draw = n_draw,
    n_burn = n_burn,
    n_thin = n_thin,
    priors = priors,
    mh = mh,
    run_multichain_diag = run_multichain_diag,
    n_chains = n_chains,
    parallel_chains = parallel_chains,
    seed_base = seed_base
  )
  
  list(
    fit = fit,
    data_used = dat_clean,
    Y = Y,
    Ymat = Ymat,
    irf = irf_obj,
    fevd = fevd_obj,
    forecast = forecast_obj,
    forecast_conditional = forecast_cond_obj,
    summary_text = summ,
    hyper_summary = hyper_summary,
    optim_summary = optim_summary,
    psi_summary = psi_summary,
    convergence = convergence,
    transform_info = transform_info,
    meta = list(
      fitted_vars = fitted_vars,
      climate_vars_fit = climate_vars_fit,
      macro_vars_fit = macro_vars_fit,
      response_vars_fit = response_vars_fit,
      climate_idx = climate_idx,
      macro_idx = macro_idx,
      response_idx = response_idx,
      impulse_idx = impulse_idx,
      impulse_vars_fit = impulse_vars_fit,
      conf_bands = conf_bands,
      date_col = date_col,
      prior_spec = prior_spec,
      hyper_mode = hyper_mode
    )
  )
}

safe_run_bvar <- function(...) {
  tryCatch(
    {
      list(ok = TRUE, result = run_bvar_climate_macro(...), error = NULL)
    },
    error = function(e) {
      list(ok = FALSE, result = NULL, error = conditionMessage(e))
    }
  )
}

info_control <- function(id, label, input_tag, what_it_is,
                         increase_text = NULL, decrease_text = NULL) {
  info_btn_id <- paste0(id, "_info_btn")
  
  tagList(
    div(
      style = "display:flex; align-items:center; justify-content:space-between; gap:8px; margin-bottom:6px;",
      tags$label(
        `for` = id,
        style = "margin-bottom:0; font-weight:600;",
        label
      ),
      actionLink(
        inputId = info_btn_id,
        label = NULL,
        icon = icon("circle-info"),
        class = "info-icon-btn",
        title = paste("More information about", label)
      )
    ),
    input_tag,
    conditionalPanel(
      condition = sprintf("input['%s'] %% 2 == 1", info_btn_id),
      div(
        class = "info-panel",
        # tags$div(tags$strong("What it means: "), what_it_is),
        tags$div(tags$strong(""), what_it_is),
        if (!is.null(increase_text) && nzchar(increase_text)) {
          tags$div(
            style = "margin-top:6px; color:#0b6b2f;",
            tags$strong("Increasing it: "), increase_text
          )
        },
        if (!is.null(decrease_text) && nzchar(decrease_text)) {
          tags$div(
            style = "margin-top:6px; color:#8a5a00;",
            tags$strong("Decreasing it: "), decrease_text
          )
        }
      )
    )
  )
}

info_select_input <- function(id, label, choices, selected = NULL, what_it_is) {
  info_control(
    id = id,
    label = label,
    input_tag = selectInput(id, label = NULL, choices = choices, selected = selected),
    what_it_is = what_it_is
  )
}

info_selectize_input <- function(id, label, choices, selected = NULL, multiple = FALSE,
                                 options = NULL, what_it_is) {
  info_control(
    id = id,
    label = label,
    input_tag = selectizeInput(
      id, label = NULL, choices = choices, selected = selected,
      multiple = multiple, options = options
    ),
    what_it_is = what_it_is
  )
}

info_numeric_input <- function(id, label, value, min = NULL, max = NULL, step = NULL,
                               what_it_is, increase_text = NULL, decrease_text = NULL) {
  
  numeric_args <- list(
    inputId = id,
    label = NULL,
    value = value
  )
  
  if (!is.null(min))  numeric_args$min  <- min
  if (!is.null(max))  numeric_args$max  <- max
  if (!is.null(step)) numeric_args$step <- step
  
  info_control(
    id = id,
    label = label,
    input_tag = do.call(numericInput, numeric_args),
    what_it_is = what_it_is,
    increase_text = increase_text,
    decrease_text = decrease_text
  )
}

info_checkbox_input <- function(id, label, value = FALSE,
                                what_it_is, increase_text = NULL, decrease_text = NULL) {
  info_control(
    id = id,
    label = label,
    input_tag = checkboxInput(id, label = NULL, value = value),
    what_it_is = what_it_is,
    increase_text = increase_text,
    decrease_text = decrease_text
  )
}

plot_info_header <- function(id, label, what_to_look_for) {
  info_btn_id <- paste0(id, "_info_btn")
  
  tagList(
    div(
      style = "display:flex; align-items:center; justify-content:space-between; gap:8px; margin:10px 0 6px 0;",
      tags$h4(style = "margin:0;", label),
      actionLink(
        inputId = info_btn_id,
        label = NULL,
        icon = icon("circle-info"),
        class = "info-icon-btn",
        title = paste("More information about", label)
      )
    ),
    conditionalPanel(
      condition = sprintf("input['%s'] %% 2 == 1", info_btn_id),
      div(
        class = "info-panel",
        tags$div(tags$strong("What to look for: "), what_to_look_for)
      )
    )
  )
}

plot_info_section <- function(id, label, plot_tag, what_to_look_for, caption = NULL) {
  tagList(
    plot_info_header(id = id, label = label, what_to_look_for = what_to_look_for),
    if (!is.null(caption) && nzchar(caption)) {
      div(class = "tab-caption", caption)
    },
    plot_tag
  )
}

plot_help_text <- list(
  hyper_trace = paste(
    "Look for a chain that moves around a stable band without a persistent trend, long flat sticking periods, or sudden shifts between regimes.",
    "Slow drifting or sticky behavior usually means more burn-in or more retained draws may be needed."
  ),
  hyper_density = paste(
    "Look for a smooth posterior shape without extreme spikes, heavy truncation at parameter bounds, or clearly separated modes unless you expect them.",
    "Very rough or unstable densities can point to weak identification or too few effective draws."
  ),
  convergence_trace = paste(
    "Look for stable wandering around a constant range rather than a clear upward or downward trend.",
    "Frequent sticking or very slow movement suggests strong autocorrelation and weaker mixing."
  ),
  convergence_density = paste(
    "Look for a reasonably smooth posterior density that is not changing shape because of a handful of draws.",
    "Multiple isolated peaks or very jagged shapes are signals to review the sampler settings."
  ),
  geweke = paste(
    "Look for the early and late parts of the chain to agree reasonably well.",
    "Large or repeated departures from the reference range suggest the start and end of the chain are behaving differently."
  ),
  gelman = paste(
    "Look for the shrink factors to move toward 1.00 as more iterations are included.",
    "Values above about 1.01 deserve review and values above about 1.05 are a poor sign for convergence."
  ),
  irf = paste(
    "Look at the sign, size, timing, and persistence of each response after the shock.",
    "Responses that stay near zero or whose uncertainty bands overlap zero are weaker, while explosive or non-decaying paths deserve extra caution."
  ),
  fevd = paste(
    "Look for which shocks explain most of the forecast error variance at short and long horizons.",
    "The shares should sum to one, and abrupt flips or extremely concentrated shares can indicate strong identification assumptions."
  ),
  forecast = paste(
    "Compare the median forecast and uncertainty band with recent history.",
    "Look for plausible levels, turning points, and band widths rather than sudden jumps or implausibly explosive uncertainty."
  ),
  forecast_median = paste(
    "Use this to focus on the central forecast path only.",
    "Look for turning points and persistence, but remember this plot hides uncertainty and should be read together with the main forecast plot."
  ),
  scenario_forecast = paste(
    "Compare the baseline and scenario paths by looking at when they diverge, how large the gap becomes, and whether the scenario remains plausible relative to the historical scale.",
    "Also check whether the uncertainty bands still overlap heavily or separate meaningfully."
  ),
  scenario_median = paste(
    "Use this to compare the baseline and scenario median paths when the uncertainty bands are wide.",
    "Focus on the direction, size, and duration of the scenario effect, but remember this view omits uncertainty."
  )
)

find_first_array <- function(x) {
  if (is.array(x) && length(dim(x)) >= 3) return(x)
  if (is.list(x)) {
    for (nm in names(x)) {
      out <- find_first_array(x[[nm]])
      if (!is.null(out)) return(out)
    }
  }
  NULL
}

print_fevd_structure <- function(x) {
  cat("Class:\n")
  print(class(x))
  cat("\nTop-level names:\n")
  print(names(x))
  
  arr <- find_first_array(x)
  cat("\nFirst array dims:\n")
  print(dim(arr))
  cat("\nFirst array dimnames:\n")
  print(dimnames(arr))
  
  invisible(arr)
}

forecast_diagnostic_table <- function(run_result) {
  fc <- run_result$forecast
  if (is.null(fc)) return(data.frame(message = "No forecast object found."))
  
  # Try common storage patterns
  obj_names <- names(fc)
  
  # Case 1: forecast stored in fc$fcast as named list of matrices
  if (!is.null(fc$fcast) && is.list(fc$fcast) && length(fc$fcast) > 0) {
    out <- data.frame()
    
    for (nm in names(fc$fcast)) {
      obj <- fc$fcast[[nm]]
      vals <- as.numeric(run_result$Y[[nm]])
      hist_min <- min(vals, na.rm = TRUE)
      hist_max <- max(vals, na.rm = TRUE)
      
      med <- tryCatch(as.numeric(obj[, "50%"]), error = function(e) rep(NA_real_, nrow(obj)))
      lo  <- tryCatch(as.numeric(obj[, "5%"]),  error = function(e) rep(NA_real_, nrow(obj)))
      hi  <- tryCatch(as.numeric(obj[, "95%"]), error = function(e) rep(NA_real_, nrow(obj)))
      
      out <- rbind(
        out,
        data.frame(
          variable = nm,
          hist_min = hist_min,
          hist_max = hist_max,
          fc_median_min = min(med, na.rm = TRUE),
          fc_median_max = max(med, na.rm = TRUE),
          fc_lower_min = min(lo, na.rm = TRUE),
          fc_upper_max = max(hi, na.rm = TRUE)
        )
      )
    }
    
    return(out)
  }
  
  # Case 2: forecast stored as array in fc$forecast or fc$fcast
  arr <- NULL
  if (!is.null(fc$forecast) && is.array(fc$forecast)) arr <- fc$forecast
  if (is.null(arr) && !is.null(fc$fcast) && is.array(fc$fcast)) arr <- fc$fcast
  
  if (!is.null(arr)) {
    d <- dim(arr)
    var_names <- colnames(run_result$Y)
    
    # Common candidate: draws x horizon x variable
    if (length(d) == 3 && d[3] == length(var_names)) {
      out <- do.call(rbind, lapply(seq_along(var_names), function(j) {
        vals <- as.numeric(run_result$Y[[var_names[j]]])
        hist_min <- min(vals, na.rm = TRUE)
        hist_max <- max(vals, na.rm = TRUE)
        
        draw_h <- arr[, , j, drop = TRUE]
        med <- apply(draw_h, 2, median, na.rm = TRUE)
        lo  <- apply(draw_h, 2, quantile, probs = 0.05, na.rm = TRUE)
        hi  <- apply(draw_h, 2, quantile, probs = 0.95, na.rm = TRUE)
        
        data.frame(
          variable = var_names[j],
          hist_min = hist_min,
          hist_max = hist_max,
          fc_median_min = min(med, na.rm = TRUE),
          fc_median_max = max(med, na.rm = TRUE),
          fc_lower_min = min(lo, na.rm = TRUE),
          fc_upper_max = max(hi, na.rm = TRUE)
        )
      }))
      
      return(out)
    }
  }
  
  data.frame(
    message = paste(
      "Forecast object structure not recognized.",
      "Top-level names:",
      paste(obj_names, collapse = ", ")
    )
  )
}

print_forecast_structure <- function(x) {
  cat("Forecast class:\n")
  print(class(x))
  cat("\nTop-level names:\n")
  print(names(x))
  cat("\nStructure:\n")
  str(x, max.level = 2)
  invisible(x)
}

build_bvar_priors <- function(
    prior_spec = "minnesota",
    hyper_mode = "auto",
    lambda_mode = 0.2,
    lambda_sd   = 0.4,
    lambda_min  = 1e-4,
    lambda_max  = 5,
    alpha_mode  = 2,
    alpha_sd    = 0.25,
    alpha_min   = 1,
    alpha_max   = 3,
    psi_mode    = "auto",
    psi_min     = "auto",
    psi_max     = "auto",
    intercept_var = 1e7,
    soc_mode = 1,
    soc_sd   = 1,
    soc_min  = 1e-4,
    soc_max  = 50,
    sur_mode = 1,
    sur_sd   = 1,
    sur_min  = 1e-4,
    sur_max  = 50
) {
  mn <- BVAR::bv_mn(
    lambda = BVAR::bv_lambda(
      mode = lambda_mode, sd = lambda_sd, min = lambda_min, max = lambda_max
    ),
    alpha = BVAR::bv_alpha(
      mode = alpha_mode, sd = alpha_sd, min = alpha_min, max = alpha_max
    ),
    psi = if (is.character(psi_mode) && identical(psi_mode, "auto")) {
      BVAR::bv_psi(mode = "auto")
    } else {
      BVAR::bv_psi(mode = psi_mode, min = psi_min, max = psi_max)
    },
    var = intercept_var
  )
  
  if (identical(prior_spec, "minnesota")) {
    return(BVAR::bv_priors(hyper = hyper_mode, mn = mn))
  }
  
  if (identical(prior_spec, "minnesota_soc")) {
    soc <- BVAR::bv_soc(mode = soc_mode, sd = soc_sd, min = soc_min, max = soc_max)
    return(BVAR::bv_priors(hyper = hyper_mode, mn = mn, soc = soc))
  }
  
  if (identical(prior_spec, "minnesota_sur")) {
    sur <- BVAR::bv_sur(mode = sur_mode, sd = sur_sd, min = sur_min, max = sur_max)
    return(BVAR::bv_priors(hyper = hyper_mode, mn = mn, sur = sur))
  }
  
  if (identical(prior_spec, "minnesota_soc_sur")) {
    soc <- BVAR::bv_soc(mode = soc_mode, sd = soc_sd, min = soc_min, max = soc_max)
    sur <- BVAR::bv_sur(mode = sur_mode, sd = sur_sd, min = sur_min, max = sur_max)
    return(BVAR::bv_priors(hyper = hyper_mode, mn = mn, soc = soc, sur = sur))
  }
  
  stop("Unknown prior_spec: ", prior_spec)
}

summarize_hyperparameters <- function(fit) {
  hp <- fit$hyper
  if (is.null(hp)) {
    return(data.frame(
      parameter = character(0),
      mean = numeric(0),
      median = numeric(0),
      sd = numeric(0),
      q05 = numeric(0),
      q95 = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  
  hp <- as.matrix(hp)
  if (is.null(colnames(hp))) {
    colnames(hp) <- paste0("hyper_", seq_len(ncol(hp)))
  }
  
  out <- lapply(seq_len(ncol(hp)), function(j) {
    x <- hp[, j]
    data.frame(
      parameter = colnames(hp)[j],
      mean = mean(x, na.rm = TRUE),
      median = median(x, na.rm = TRUE),
      sd = stats::sd(x, na.rm = TRUE),
      q05 = stats::quantile(x, 0.05, na.rm = TRUE),
      q95 = stats::quantile(x, 0.95, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  
  dplyr::bind_rows(out)
}

summarize_optimizer <- function(fit) {
  opt <- fit$optim
  if (is.null(opt)) {
    return(data.frame(item = "optimizer", value = "No optimizer output found"))
  }
  
  vals <- unlist(opt, recursive = FALSE)
  vals <- vals[vapply(vals, function(x) length(x) == 1, logical(1))]
  
  data.frame(
    item = names(vals),
    value = vapply(vals, as.character, character(1)),
    stringsAsFactors = FALSE
  )
}

empty_message_table <- function(message) {
  data.frame(message = as.character(message), stringsAsFactors = FALSE)
}

mcmc_metadata <- function(x) {
  mcpar <- attr(x, "mcpar")
  list(
    start = if (!is.null(mcpar) && length(mcpar) >= 1) as.numeric(mcpar[1]) else 1,
    thin = if (!is.null(mcpar) && length(mcpar) >= 3) as.numeric(mcpar[3]) else 1
  )
}

rebuild_mcmc_from_matrix <- function(mat, template = NULL, start = 1L, thin = 1L) {
  if (!is.null(template) && inherits(template, "mcmc")) {
    meta <- mcmc_metadata(template)
    start <- meta$start
    thin <- meta$thin
  }
  
  coda::mcmc(mat, start = start, thin = thin)
}

subset_mcmc_columns <- function(x, vars = NULL) {
  if (is.null(x) || is.null(vars) || !length(vars)) {
    return(x)
  }
  
  if (inherits(x, "mcmc.list")) {
    chain_list <- lapply(x, subset_mcmc_columns, vars = vars)
    chain_list <- Filter(Negate(is.null), chain_list)
    if (!length(chain_list)) return(NULL)
    return(do.call(coda::mcmc.list, chain_list))
  }
  
  if (!inherits(x, "mcmc")) {
    return(NULL)
  }
  
  mat <- as.matrix(x)
  available <- intersect(vars, colnames(mat) %||% character(0))
  if (!length(available)) return(NULL)
  
  rebuild_mcmc_from_matrix(mat[, available, drop = FALSE], template = x)
}

normalize_hyper_draws <- function(draws) {
  if (is.null(draws)) return(NULL)
  
  mat <- as.matrix(draws)
  if (!nrow(mat) || !ncol(mat)) return(NULL)
  storage.mode(mat) <- "double"
  
  row_keep <- apply(mat, 1, function(x) all(is.finite(x)))
  mat <- mat[row_keep, , drop = FALSE]
  if (!nrow(mat) || !ncol(mat)) return(NULL)
  
  if (is.null(colnames(mat))) {
    colnames(mat) <- paste0("hyper_", seq_len(ncol(mat)))
  }
  
  mat
}

extract_mcmc_from_bvar <- function(fit, prefer_bvar_method = TRUE) {
  hyper_names <- NULL
  if (!is.null(fit$hyper)) {
    hyper_names <- colnames(as.matrix(fit$hyper))
  }
  
  if (isTRUE(prefer_bvar_method) && exists("as.mcmc", where = asNamespace("BVAR"), inherits = FALSE)) {
    bvar_as_mcmc <- get("as.mcmc", envir = asNamespace("BVAR"))
    mcmc_obj <- tryCatch(
      bvar_as_mcmc(fit),
      error = function(e) NULL
    )
    
    if (!is.null(mcmc_obj)) {
      if (!is.null(hyper_names) && length(hyper_names)) {
        subset_obj <- subset_mcmc_columns(mcmc_obj, hyper_names)
        if (!is.null(subset_obj)) {
          mcmc_obj <- subset_obj
        }
      }
      
      if (inherits(mcmc_obj, "mcmc.list") && length(mcmc_obj) == 1L) {
        mcmc_obj <- mcmc_obj[[1L]]
      }
      
      if (inherits(mcmc_obj, "mcmc") && ncol(as.matrix(mcmc_obj)) > 0) {
        return(mcmc_obj)
      }
    }
  }
  
  draws <- normalize_hyper_draws(fit$hyper)
  if (is.null(draws)) {
    stop("No sampled hyperparameter draws were found in the fitted BVAR object.")
  }
  
  start_iter <- if (!is.null(fit$meta$n_burn)) as.integer(fit$meta$n_burn) + 1L else 1L
  thin_iter <- if (!is.null(fit$meta$n_thin)) as.integer(fit$meta$n_thin) else 1L
  
  coda::mcmc(draws, start = start_iter, thin = thin_iter)
}

align_named_numeric <- function(x, params) {
  out <- stats::setNames(rep(NA_real_, length(params)), params)
  if (is.null(x) || !length(params)) return(out)
  
  nms <- names(x)
  x_num <- as.numeric(x)
  
  if (!is.null(nms) && length(nms)) {
    common <- intersect(params, nms)
    names(x_num) <- nms
    out[common] <- x_num[common]
    return(out)
  }
  
  if (length(x_num) == length(params)) {
    out[] <- x_num
  } else if (length(x_num) == 1L && length(params) == 1L) {
    out[1] <- x_num[1]
  }
  
  out
}

classify_geweke <- function(z_value) {
  if (!is.finite(z_value)) {
    return("Unavailable")
  }
  
  if (abs(z_value) > 2) "Review" else "OK"
}

classify_psrf <- function(psrf_value) {
  if (!is.finite(psrf_value)) {
    return("Unavailable")
  }
  
  if (psrf_value > 1.05) {
    "Poor"
  } else if (psrf_value > 1.01) {
    "Review"
  } else {
    "OK"
  }
}

classify_heidel_component <- function(x) {
  if (!is.finite(x)) {
    return("Unavailable")
  }
  
  if (x >= 1) "Pass" else "Review"
}

classify_heidel <- function(stest, htest) {
  if (!is.finite(stest) && !is.finite(htest)) {
    return("Unavailable")
  }
  
  if (is.finite(stest) && stest < 1) return("Review")
  if (is.finite(htest) && htest < 1) return("Review")
  "OK"
}

compute_single_chain_diagnostics <- function(fit) {
  mcmc_obj <- tryCatch(
    extract_mcmc_from_bvar(fit),
    error = function(e) e
  )
  
  if (inherits(mcmc_obj, "error")) {
    return(list(
      ok = FALSE,
      mcmc = NULL,
      table = NULL,
      geweke = NULL,
      heidel = NULL,
      error = conditionMessage(mcmc_obj),
      messages = character(0)
    ))
  }
  
  params <- colnames(as.matrix(mcmc_obj))
  if (is.null(params)) {
    params <- paste0("hyper_", seq_len(ncol(as.matrix(mcmc_obj))))
  }
  
  ess_try <- tryCatch(coda::effectiveSize(mcmc_obj), error = function(e) e)
  ess <- stats::setNames(rep(NA_real_, length(params)), params)
  messages <- character(0)
  if (inherits(ess_try, "error")) {
    messages <- c(messages, paste("Effective sample size could not be computed:", conditionMessage(ess_try)))
  } else {
    ess <- align_named_numeric(ess_try, params)
  }
  
  geweke_try <- tryCatch(coda::geweke.diag(mcmc_obj), error = function(e) e)
  geweke_z <- stats::setNames(rep(NA_real_, length(params)), params)
  if (inherits(geweke_try, "error")) {
    messages <- c(messages, paste("Geweke diagnostic could not be computed:", conditionMessage(geweke_try)))
    geweke_obj <- NULL
  } else {
    geweke_obj <- geweke_try
    if (!is.null(geweke_try$z)) {
      geweke_z <- align_named_numeric(geweke_try$z, params)
    }
  }
  
  heidel_try <- tryCatch(coda::heidel.diag(mcmc_obj), error = function(e) e)
  heidel_stationarity <- stats::setNames(rep("Unavailable", length(params)), params)
  heidel_halfwidth <- stats::setNames(rep("Unavailable", length(params)), params)
  heidel_status <- stats::setNames(rep("Unavailable", length(params)), params)
  if (inherits(heidel_try, "error")) {
    messages <- c(messages, paste("Heidelberger-Welch diagnostic could not be computed:", conditionMessage(heidel_try)))
    heidel_obj <- NULL
  } else {
    heidel_obj <- heidel_try
    heidel_rows <- rownames(heidel_try) %||% params
    common_rows <- intersect(params, heidel_rows)
    if (length(common_rows)) {
      stest_vals <- heidel_try[common_rows, "stest"]
      htest_vals <- heidel_try[common_rows, "htest"]
      heidel_stationarity[common_rows] <- vapply(stest_vals, classify_heidel_component, character(1))
      heidel_halfwidth[common_rows] <- vapply(htest_vals, classify_heidel_component, character(1))
      heidel_status[common_rows] <- mapply(classify_heidel, stest_vals, htest_vals, USE.NAMES = FALSE)
    }
  }
  
  overall_status <- vapply(params, function(param) {
    g_status <- classify_geweke(geweke_z[[param]])
    h_status <- heidel_status[[param]]
    
    if (identical(g_status, "Review") || identical(h_status, "Review")) {
      return("Review")
    }
    if (identical(g_status, "OK") || identical(h_status, "OK")) {
      return("OK")
    }
    "Unavailable"
  }, character(1))
  
  tbl <- data.frame(
    parameter = params,
    ess = as.numeric(ess[params]),
    geweke_z = as.numeric(geweke_z[params]),
    geweke_abs_z = abs(as.numeric(geweke_z[params])),
    status = as.character(overall_status),
    heidel_stationarity = as.character(heidel_stationarity[params]),
    heidel_halfwidth = as.character(heidel_halfwidth[params]),
    heidel_status = as.character(heidel_status[params]),
    stringsAsFactors = FALSE
  )
  
  list(
    ok = TRUE,
    mcmc = mcmc_obj,
    table = tbl,
    geweke = geweke_obj,
    heidel = heidel_obj,
    error = NULL,
    messages = unique(messages)
  )
}

safe_detect_cores <- function() {
  detected <- tryCatch(parallel::detectCores(logical = FALSE), error = function(e) NA_integer_)
  if (!is.finite(detected) || is.na(detected) || detected < 1) {
    return(1L)
  }
  
  as.integer(detected)
}

with_preserved_seed <- function(seed, expr) {
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }
  
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  
  set.seed(as.integer(seed))
  eval.parent(substitute(expr))
}

run_multichain_bvar <- function(Ymat, lags, n_draw, n_burn, n_thin, priors, mh,
                                n_chains = 4L, parallel_chains = TRUE, seed_base = 1234L) {
  n_chains <- max(2L, as.integer(n_chains))
  seed_base <- max(1L, as.integer(seed_base))
  notes <- character(0)
  
  if (isTRUE(parallel_chains) && exists("par_bvar", where = asNamespace("BVAR"), inherits = FALSE)) {
    cl <- NULL
    parallel_out <- tryCatch(
      {
        n_workers <- min(n_chains, safe_detect_cores())
        cl <- parallel::makeCluster(n_workers)
        on.exit({
          if (!is.null(cl)) {
            try(parallel::stopCluster(cl), silent = TRUE)
          }
        }, add = TRUE)
        parallel::clusterSetRNGStream(cl, iseed = seed_base)
        
        chain_fits <- BVAR::par_bvar(
          cl = cl,
          n_runs = n_chains,
          data = Ymat,
          lags = lags,
          n_draw = n_draw,
          n_burn = n_burn,
          n_thin = n_thin,
          priors = priors,
          mh = mh
        )
        
        list(
          ok = TRUE,
          fits = unclass(chain_fits),
          method = "BVAR::par_bvar",
          notes = notes,
          error = NULL
        )
      },
      error = function(e) {
        list(
          ok = FALSE,
          fits = NULL,
          method = "BVAR::par_bvar",
          notes = notes,
          error = conditionMessage(e)
        )
      }
    )
    
    if (isTRUE(parallel_out$ok)) {
      return(parallel_out)
    }
    
    notes <- c(notes, paste("Parallel multi-chain run fell back to sequential reruns:", parallel_out$error))
  } else if (isTRUE(parallel_chains)) {
    notes <- c(notes, "Parallel multi-chain fitting is not available in the installed BVAR version, so the app used sequential reruns instead.")
  }
  
  fits <- vector("list", n_chains)
  for (i in seq_len(n_chains)) {
    fit_i <- tryCatch(
      with_preserved_seed(
        seed_base + i - 1L,
        BVAR::bvar(
          Ymat,
          lags = lags,
          n_draw = n_draw,
          n_burn = n_burn,
          n_thin = n_thin,
          priors = priors,
          mh = mh,
          verbose = FALSE
        )
      ),
      error = function(e) e
    )
    
    if (inherits(fit_i, "error")) {
      return(list(
        ok = FALSE,
        fits = NULL,
        method = if (length(notes)) "Sequential fallback" else "Sequential reruns",
        notes = unique(notes),
        error = paste0("Chain ", i, " failed: ", conditionMessage(fit_i))
      ))
    }
    
    fits[[i]] <- fit_i
  }
  
  list(
    ok = TRUE,
    fits = fits,
    method = if (length(notes)) "Sequential fallback" else "Sequential reruns",
    notes = unique(notes),
    error = NULL
  )
}

align_mcmc_chains <- function(chain_mcmcs) {
  chain_mcmcs <- Filter(function(x) inherits(x, "mcmc"), chain_mcmcs)
  if (length(chain_mcmcs) < 2L) {
    stop("At least two valid MCMC chains are required for Gelman-Rubin diagnostics.")
  }
  
  var_sets <- lapply(chain_mcmcs, function(x) colnames(as.matrix(x)) %||% character(0))
  common_vars <- Reduce(intersect, var_sets)
  if (!length(common_vars)) {
    stop("No common sampled hyperparameters were found across chains.")
  }
  
  aligned <- lapply(chain_mcmcs, function(x) {
    mat <- as.matrix(x)
    rebuild_mcmc_from_matrix(mat[, common_vars, drop = FALSE], template = x)
  })
  
  do.call(coda::mcmc.list, aligned)
}

compute_multichain_diagnostics <- function(chain_fits, n_chains = length(chain_fits),
                                           method = NULL, notes = character(0)) {
  chain_mcmcs <- tryCatch(
    {
      lapply(chain_fits, extract_mcmc_from_bvar)
    },
    error = function(e) e
  )
  
  if (inherits(chain_mcmcs, "error")) {
    return(list(
      requested = TRUE,
      ok = FALSE,
      mcmc_list = NULL,
      table = NULL,
      gelman = NULL,
      error = conditionMessage(chain_mcmcs),
      method = method,
      notes = unique(notes),
      n_chains = as.integer(n_chains)
    ))
  }
  
  mcmc_list <- tryCatch(
    align_mcmc_chains(chain_mcmcs),
    error = function(e) e
  )
  
  if (inherits(mcmc_list, "error")) {
    return(list(
      requested = TRUE,
      ok = FALSE,
      mcmc_list = NULL,
      table = NULL,
      gelman = NULL,
      error = conditionMessage(mcmc_list),
      method = method,
      notes = unique(notes),
      n_chains = as.integer(n_chains)
    ))
  }
  
  gelman_try <- tryCatch(
    coda::gelman.diag(mcmc_list, autoburnin = FALSE, multivariate = FALSE),
    error = function(e) e
  )
  
  if (inherits(gelman_try, "error")) {
    return(list(
      requested = TRUE,
      ok = FALSE,
      mcmc_list = mcmc_list,
      table = NULL,
      gelman = NULL,
      error = conditionMessage(gelman_try),
      method = method,
      notes = unique(notes),
      n_chains = as.integer(n_chains)
    ))
  }
  
  psrf_mat <- gelman_try$psrf
  if (is.null(psrf_mat) || !nrow(psrf_mat)) {
    return(list(
      requested = TRUE,
      ok = FALSE,
      mcmc_list = mcmc_list,
      table = NULL,
      gelman = gelman_try,
      error = "Gelman-Rubin output did not contain parameter-level PSRF values.",
      method = method,
      notes = unique(notes),
      n_chains = as.integer(n_chains)
    ))
  }
  
  psrf_point <- as.numeric(psrf_mat[, "Point est."])
  psrf_upper <- as.numeric(psrf_mat[, "Upper C.I."])
  class_metric <- pmax(psrf_point, psrf_upper)
  class_metric[!is.finite(class_metric)] <- NA_real_
  param_names <- rownames(psrf_mat)
  if (is.null(param_names)) {
    param_names <- colnames(as.matrix(mcmc_list[[1L]]))
  }
  
  tbl <- data.frame(
    parameter = param_names,
    psrf_point_estimate = psrf_point,
    psrf_upper_ci = psrf_upper,
    status = vapply(class_metric, classify_psrf, character(1)),
    stringsAsFactors = FALSE
  )
  
  list(
    requested = TRUE,
    ok = TRUE,
    mcmc_list = mcmc_list,
    table = tbl,
    gelman = gelman_try,
    error = NULL,
    method = method,
    notes = unique(notes),
    n_chains = as.integer(n_chains)
  )
}

get_optimizer_convergence_code <- function(fit) {
  code <- tryCatch(fit$optim$convergence, error = function(e) NA_integer_)
  if (!length(code) || !is.finite(code[1])) {
    return(NA_integer_)
  }
  
  as.integer(code[1])
}

optimizer_convergence_text <- function(fit) {
  code <- get_optimizer_convergence_code(fit)
  if (!is.finite(code)) {
    return("Optimizer initialization status is unavailable.")
  }
  
  if (identical(code, 0L)) {
    "Optimizer initialization reported successful convergence."
  } else {
    paste(
      "Optimizer initialization returned convergence code", code,
      "and should be reviewed separately from the posterior MCMC diagnostics below."
    )
  }
}

convergence_interpretation_text <- function(convergence, fit = NULL) {
  if (is.null(convergence)) {
    return("No convergence diagnostics are available yet.")
  }
  
  lines <- character(0)
  if (!is.null(fit)) {
    lines <- c(lines, optimizer_convergence_text(fit))
  }
  
  single <- convergence$single
  if (is.null(single) || !isTRUE(single$ok) || is.null(single$table)) {
    lines <- c(
      lines,
      paste(
        "Single-chain coda diagnostics could not be computed:",
        single$error %||% "No sampled hyperparameters were available."
      )
    )
  } else {
    single_review <- single$table$parameter[single$table$status == "Review"]
    if (length(single_review)) {
      lines <- c(
        lines,
        paste("Single-chain diagnostics suggest review for", paste(single_review, collapse = ", "), ".")
      )
    } else {
      lines <- c(lines, "Single-chain diagnostics look acceptable.")
    }
  }
  
  multi <- convergence$multi
  if (is.null(multi) || !isTRUE(multi$requested)) {
    lines <- c(lines, "Multi-chain checks were not run.")
  } else if (!isTRUE(multi$ok) || is.null(multi$table)) {
    lines <- c(
      lines,
      paste(
        "Multi-chain diagnostics were requested but could not be completed:",
        multi$error %||% "Unknown multi-chain error."
      )
    )
  } else {
    multi_review <- multi$table$parameter[multi$table$status %in% c("Review", "Poor")]
    if (length(multi_review)) {
      lines <- c(
        lines,
        paste0(
          "Gelman-Rubin diagnostics across ", multi$n_chains,
          " chains suggest review for ", paste(multi_review, collapse = ", "), "."
        )
      )
    } else {
      lines <- c(
        lines,
        paste0(
          "Gelman-Rubin diagnostics across ", multi$n_chains,
          " chains are acceptable for ", paste(multi$table$parameter, collapse = ", "), "."
        )
      )
    }
  }
  
  single_flag <- !is.null(single$table) && any(single$table$status == "Review", na.rm = TRUE)
  multi_flag <- !is.null(multi$table) && any(multi$table$status %in% c("Review", "Poor"), na.rm = TRUE)
  if (!is.null(fit) && identical(get_optimizer_convergence_code(fit), 0L) && (single_flag || multi_flag)) {
    lines <- c(lines, "Optimizer convergence was successful, but at least one MCMC convergence diagnostic suggests more draws or more burn-in may be needed.")
  }
  
  extra_notes <- c(single$messages %||% character(0), multi$notes %||% character(0))
  if (length(extra_notes)) {
    lines <- c(lines, unique(extra_notes))
  }
  
  paste(lines, collapse = "\n")
}

compute_convergence_diagnostics <- function(fit, Ymat, lags, n_draw, n_burn, n_thin,
                                            priors, mh, run_multichain_diag = FALSE,
                                            n_chains = 4L, parallel_chains = TRUE,
                                            seed_base = 1234L) {
  single <- compute_single_chain_diagnostics(fit)
  
  multi <- list(
    requested = isTRUE(run_multichain_diag),
    ok = FALSE,
    mcmc_list = NULL,
    table = NULL,
    gelman = NULL,
    error = NULL,
    method = NULL,
    notes = character(0),
    n_chains = max(2L, as.integer(n_chains))
  )
  
  if (isTRUE(run_multichain_diag)) {
    multichain_run <- run_multichain_bvar(
      Ymat = Ymat,
      lags = lags,
      n_draw = n_draw,
      n_burn = n_burn,
      n_thin = n_thin,
      priors = priors,
      mh = mh,
      n_chains = n_chains,
      parallel_chains = parallel_chains,
      seed_base = seed_base
    )
    
    if (isTRUE(multichain_run$ok)) {
      multi <- compute_multichain_diagnostics(
        chain_fits = multichain_run$fits,
        n_chains = length(multichain_run$fits),
        method = multichain_run$method,
        notes = multichain_run$notes
      )
    } else {
      multi$error <- multichain_run$error
      multi$method <- multichain_run$method
      multi$notes <- multichain_run$notes
    }
  }
  
  out <- list(single = single, multi = multi)
  out$summary_text <- convergence_interpretation_text(out, fit = fit)
  out
}

prior_recommendation <- function(climate_vars, macro_vars, logdiff_vars) {
  selected_vars <- unique(c(climate_vars, macro_vars))
  level_like_vars <- setdiff(selected_vars, logdiff_vars %||% character(0))
  persistent_level_vars <- intersect(
    level_like_vars,
    c(
      "Sea Level (mm)",
      "Government Debt",
      "House Price Index",
      "Real GDP",
      "Current Account Imports q",
      "Current Account Exports"
    )
  )
  
  recommended <- if (length(persistent_level_vars) >= 2) {
    "minnesota_soc"
  } else {
    "minnesota"
  }
  
  list(
    recommended = recommended,
    headline = if (identical(recommended, "minnesota_soc")) {
      "Recommended default: Minnesota + SOC"
    } else {
      "Recommended default: Minnesota only"
    },
    body = if (identical(recommended, "minnesota_soc")) {
      paste(
        "Several selected variables still behave like persistent levels.",
        "Adding SOC usually stabilises long-run dynamics in that setup."
      )
    } else {
      paste(
        "The current selection is relatively compact or already partly differenced.",
        "Starting with a plain Minnesota prior is usually the clearest benchmark."
      )
    },
    follow_up = "Add SUR only when you want stronger shrinkage around near-unit-root or cointegrated behaviour; it is useful, but more opinionated."
  )
}

hyperparameter_insight_text <- function(run_result) {
  hp <- run_result$hyper_summary
  opt <- run_result$optim_summary
  
  lines <- c(
    "Tuning approach: the BVAR package uses continuous nonlinear optimisation for hyperparameter initialisation, which is the recommended default here. Grid search is better used only as a sensitivity check over a few preset prior configurations."
  )
  
  method_row <- opt[grepl("method", opt$item, ignore.case = TRUE), , drop = FALSE]
  if (nrow(method_row)) {
    lines <- c(lines, paste("Optimizer method:", method_row$value[1]))
  } else {
    lines <- c(lines, "Optimizer method: nonlinear optimisation inside BVAR (typically L-BFGS-B).")
  }
  
  lambda_row <- hp[grepl("lambda", hp$parameter, ignore.case = TRUE), , drop = FALSE]
  if (nrow(lambda_row)) {
    lambda_mean <- lambda_row$mean[1]
    lambda_msg <- if (lambda_mean < 0.15) {
      "tight shrinkage, favouring stability over flexibility."
    } else if (lambda_mean < 0.35) {
      "moderate shrinkage, a balanced middle ground."
    } else {
      "looser shrinkage, allowing richer dynamics but with more overfitting risk."
    }
    lines <- c(lines, paste0("Lambda posterior mean: ", round(lambda_mean, 3), " - ", lambda_msg))
  }
  
  alpha_row <- hp[grepl("alpha", hp$parameter, ignore.case = TRUE), , drop = FALSE]
  if (nrow(alpha_row)) {
    alpha_mean <- alpha_row$mean[1]
    alpha_msg <- if (alpha_mean < 1.5) {
      "higher-order lags are decaying fairly quickly."
    } else if (alpha_mean < 2.5) {
      "lag decay is moderate."
    } else {
      "higher-order lags are being kept relatively alive."
    }
    lines <- c(lines, paste0("Alpha posterior mean: ", round(alpha_mean, 3), " - ", alpha_msg))
  }
  
  psi_tbl <- run_result$psi_summary
  if (!is.null(psi_tbl) && nrow(psi_tbl)) {
    psi_counts <- sort(table(psi_tbl$psi_source), decreasing = TRUE)
    psi_text <- paste(paste(names(psi_counts), psi_counts), collapse = "; ")
    lines <- c(
      lines,
      paste(
        "Psi calibration: the app now computes variable-specific cross-lag prior scales before estimation.",
        "This avoids BVAR's automatic psi fallback warnings and keeps runs more reproducible.",
        "Sources used:",
        psi_text
      )
    )
  }
  
  lines <- c(
    lines,
    paste("Selected prior family:", prior_choice_labels[[run_result$meta$prior_spec]] %||% run_result$meta$prior_spec),
    paste("Hierarchical mode:", paste(run_result$meta$hyper_mode %||% "auto", collapse = ", "))
  )
  
  paste(lines, collapse = "\n")
}

compute_model_complexity_summary <- function(run_result, ess_table = NULL) {
  make_message <- function(message) {
    list(
      ok = FALSE,
      table = data.frame(
        Metric = "Message",
        Value = as.character(message),
        stringsAsFactors = FALSE
      ),
      note = NULL
    )
  }
  
  fmt_num <- function(x, digits = 3L, integer = FALSE, na = "") {
    if (!length(x)) return(na)
    x <- suppressWarnings(as.numeric(x[1]))
    if (!is.finite(x)) return(na)
    if (isTRUE(integer)) {
      return(format(as.integer(round(x)), trim = TRUE, scientific = FALSE))
    }
    formatC(x, format = "f", digits = digits)
  }
  
  safe_ratio <- function(num, den) {
    num <- suppressWarnings(as.numeric(num[1]))
    den <- suppressWarnings(as.numeric(den[1]))
    if (!is.finite(num) || !is.finite(den) || den == 0) {
      return(NA_real_)
    }
    num / den
  }
  
  summarize_effective_coefficients <- function(fit, conf_band = 0.95) {
    coef_quantiles <- tryCatch(
      stats::coef(fit, type = "quantile", conf_bands = conf_band),
      error = function(e) NULL
    )
    
    if (is.null(coef_quantiles) || length(dim(coef_quantiles)) != 3) {
      return(list(count = NA_real_, share = NA_real_))
    }
    
    lower <- as.numeric(coef_quantiles[1, , ])
    upper <- as.numeric(coef_quantiles[dim(coef_quantiles)[1], , ])
    valid <- is.finite(lower) & is.finite(upper)
    
    if (!any(valid)) {
      return(list(count = NA_real_, share = NA_real_))
    }
    
    non_zero <- valid & ((lower > 0 & upper > 0) | (lower < 0 & upper < 0))
    
    list(
      count = sum(non_zero),
      share = sum(non_zero) / sum(valid)
    )
  }
  
  if (is.null(run_result) || is.null(run_result$fit)) {
    return(make_message("Model size and complexity will appear after a successful run."))
  }
  
  fit <- run_result$fit
  Y <- run_result$Y
  Ymat <- run_result$Ymat
  meta <- run_result$meta %||% list()
  hyper_summary <- run_result$hyper_summary
  
  if (is.null(ess_table)) {
    ess_table <- run_result$convergence$single$table %||% NULL
  }
  
  usable_obs <- suppressWarnings(tryCatch(as.numeric(fit$meta$N), error = function(e) NA_real_))
  if (!is.finite(usable_obs)) {
    usable_obs <- if (!is.null(Ymat) && nrow(as.matrix(Ymat)) > 0) {
      nrow(as.matrix(Ymat))
    } else if (!is.null(Y) && nrow(as.data.frame(Y)) > 0) {
      nrow(as.data.frame(Y))
    } else {
      NA_real_
    }
  }
  
  K <- suppressWarnings(tryCatch(as.numeric(fit$meta$M), error = function(e) NA_real_))
  if (!is.finite(K)) {
    K <- if (!is.null(Ymat)) ncol(as.matrix(Ymat)) else NA_real_
  }
  
  p <- suppressWarnings(tryCatch(as.numeric(fit$meta$lags), error = function(e) NA_real_))
  
  coefficient_parameters <- if (is.finite(K) && is.finite(p)) K * (K * p + 1) else NA_real_
  covariance_parameters <- if (is.finite(K)) K * (K + 1) / 2 else NA_real_
  effective_coef_summary <- summarize_effective_coefficients(fit, conf_band = 0.95)
  
  hyper_draws <- tryCatch(normalize_hyper_draws(fit$hyper), error = function(e) NULL)
  sampled_hyperparameters <- if (!is.null(hyper_summary) && nrow(hyper_summary) > 0 && "parameter" %in% names(hyper_summary)) {
    length(unique(as.character(hyper_summary$parameter)))
  } else if (!is.null(hyper_draws)) {
    ncol(hyper_draws)
  } else {
    0L
  }
  
  total_raw_unknowns <- coefficient_parameters + covariance_parameters + sampled_hyperparameters
  t_per_kp <- safe_ratio(usable_obs, K * p)
  t_per_raw_unknowns <- safe_ratio(usable_obs, total_raw_unknowns)
  
  impulse_variables <- length(meta$impulse_vars_fit %||% integer(0))
  response_variables <- length(meta$response_vars_fit %||% integer(0))
  
  accepted_draw_rate <- NA_real_
  accepted_draws <- suppressWarnings(tryCatch(as.numeric(fit$meta$accepted), error = function(e) NA_real_))
  draw_denom <- suppressWarnings(tryCatch(as.numeric(fit$meta$n_draw - fit$meta$n_burn), error = function(e) NA_real_))
  if (is.finite(accepted_draws) && is.finite(draw_denom) && draw_denom > 0) {
    accepted_draw_rate <- accepted_draws / draw_denom
  }
  
  retained_draws <- suppressWarnings(tryCatch(as.numeric(fit$meta$n_save), error = function(e) NA_real_))
  if (!is.finite(retained_draws) && !is.null(hyper_draws)) {
    retained_draws <- nrow(hyper_draws)
  }
  
  lambda_mean <- NA_real_
  if (!is.null(hyper_summary) && nrow(hyper_summary) > 0 && all(c("parameter", "mean") %in% names(hyper_summary))) {
    lambda_idx <- which(tolower(as.character(hyper_summary$parameter)) == "lambda")
    if (!length(lambda_idx)) {
      lambda_idx <- grep("lambda", as.character(hyper_summary$parameter), ignore.case = TRUE)
    }
    if (length(lambda_idx)) {
      lambda_mean <- suppressWarnings(as.numeric(hyper_summary$mean[lambda_idx[1]]))
    }
  }
  if (!is.finite(lambda_mean) && !is.null(hyper_draws) && ncol(hyper_draws) > 0) {
    hyper_names <- colnames(hyper_draws) %||% paste0("hyper_", seq_len(ncol(hyper_draws)))
    lambda_idx <- which(tolower(hyper_names) == "lambda")
    if (!length(lambda_idx)) {
      lambda_idx <- grep("lambda", hyper_names, ignore.case = TRUE)
    }
    if (length(lambda_idx)) {
      lambda_mean <- mean(hyper_draws[, lambda_idx[1]], na.rm = TRUE)
    }
  }
  
  ess_ratio_display <- ""
  if (!is.null(ess_table) && nrow(ess_table) > 0 && is.finite(retained_draws) && retained_draws > 0) {
    param_col <- if ("parameter" %in% names(ess_table)) "parameter" else names(ess_table)[1]
    ess_col <- if ("ess" %in% names(ess_table)) {
      "ess"
    } else if ("ESS" %in% names(ess_table)) {
      "ESS"
    } else {
      NULL
    }
    
    if (!is.null(ess_col)) {
      param_names <- as.character(ess_table[[param_col]])
      ess_vals <- suppressWarnings(as.numeric(ess_table[[ess_col]]))
      
      lambda_idx <- which(tolower(param_names) == "lambda")
      if (!length(lambda_idx)) {
        lambda_idx <- grep("lambda", param_names, ignore.case = TRUE)
      }
      
      if (length(lambda_idx) && is.finite(ess_vals[lambda_idx[1]])) {
        ess_ratio_display <- paste0(fmt_num(ess_vals[lambda_idx[1]] / retained_draws, digits = 3), " (lambda)")
      } else {
        ess_vals <- ess_vals[is.finite(ess_vals)]
        if (length(ess_vals)) {
          ess_ratio_display <- paste0(
            fmt_num(stats::median(ess_vals) / retained_draws, digits = 3),
            " (median across sampled hyperparameters)"
          )
        }
      }
    }
  }
  
  coef_threshold_rows <- NULL
  coef_medians <- tryCatch(
    stats::coef(fit, type = "quantile", conf_bands = 0.5),
    error = function(e) NULL
  )
  if (!is.null(coef_medians)) {
    coef_vals <- as.numeric(coef_medians)
    coef_vals <- coef_vals[is.finite(coef_vals)]
    if (length(coef_vals)) {
      coef_threshold_rows <- data.frame(
        Metric = c(
          "Share of median coefficients with |value| < 0.05",
          "Share of median coefficients with |value| < 0.10"
        ),
        Value = c(
          fmt_num(mean(abs(coef_vals) < 0.05), digits = 3),
          fmt_num(mean(abs(coef_vals) < 0.10), digits = 3)
        ),
        stringsAsFactors = FALSE
      )
    }
  }
  
  complexity_note <- if (!is.finite(t_per_raw_unknowns)) {
    "Complexity note: Raw model dimension could not be benchmarked cleanly against the available sample."
  } else if (t_per_raw_unknowns < 1.5) {
    "Complexity note: Raw model dimension is high relative to the available sample; posterior shrinkage is especially important for stabilization."
  } else if (t_per_raw_unknowns <= 3) {
    "Complexity note: Raw model dimension is moderate relative to sample size; posterior shrinkage remains important for stabilization."
  } else {
    "Complexity note: Raw model dimension is relatively comfortable for the available sample, though posterior shrinkage still helps stabilize estimation."
  }
  
  if (is.finite(lambda_mean)) {
    lambda_note <- if (lambda_mean < 0.15) {
      "Posterior mean lambda suggests tighter shrinkage."
    } else if (lambda_mean < 0.35) {
      "Posterior mean lambda suggests moderate shrinkage."
    } else {
      "Posterior mean lambda suggests looser shrinkage."
    }
    complexity_note <- paste(complexity_note, lambda_note)
  }
  
  tbl <- data.frame(
    Metric = c(
      "Usable observations",
      "Variables (K)",
      "Lags (p)",
      "Coefficient parameters",
      "Effective coefficient parameters (95% CrI excludes 0)",
      "Effective coefficient share",
      "Covariance parameters",
      "Sampled hyperparameters",
      "Total raw unknowns",
      "T / (K x p)",
      "T / raw unknowns",
      "Impulse variables",
      "Response variables",
      "Accepted draw rate",
      "ESS / retained draws ratio",
      "Posterior mean of lambda"
    ),
    Value = c(
      fmt_num(usable_obs, integer = TRUE),
      fmt_num(K, integer = TRUE),
      fmt_num(p, integer = TRUE),
      fmt_num(coefficient_parameters, integer = TRUE),
      fmt_num(effective_coef_summary$count, integer = TRUE),
      fmt_num(effective_coef_summary$share, digits = 3),
      fmt_num(covariance_parameters, integer = TRUE),
      fmt_num(sampled_hyperparameters, integer = TRUE),
      fmt_num(total_raw_unknowns, integer = TRUE),
      fmt_num(t_per_kp, digits = 3),
      fmt_num(t_per_raw_unknowns, digits = 3),
      fmt_num(impulse_variables, integer = TRUE),
      fmt_num(response_variables, integer = TRUE),
      fmt_num(accepted_draw_rate, digits = 3),
      ess_ratio_display,
      if (is.finite(lambda_mean)) fmt_num(lambda_mean, digits = 3) else "Not sampled"
    ),
    stringsAsFactors = FALSE
  )
  
  if (!is.null(coef_threshold_rows)) {
    tbl <- dplyr::bind_rows(tbl, coef_threshold_rows)
  }
  
  list(ok = TRUE, table = tbl, note = complexity_note)
}

parse_probability_labels <- function(labels) {
  if (is.null(labels)) {
    return(rep(NA_real_, 0))
  }
  
  out <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", labels)))
  has_pct <- grepl("%", labels)
  out[has_pct] <- out[has_pct] / 100
  out[!has_pct & out > 1] <- out[!has_pct & out > 1] / 100
  out
}

quantile_frame_from_matrix <- function(mat, probs = c(0.05, 0.16, 0.5, 0.84, 0.95)) {
  if (is.null(dim(mat))) {
    mat <- matrix(mat, ncol = 1)
  }
  
  qmat <- apply(mat, 2, stats::quantile, probs = probs, na.rm = TRUE)
  data.frame(
    horizon = seq_len(ncol(mat)),
    q05 = as.numeric(qmat[1, ]),
    q16 = as.numeric(qmat[2, ]),
    q50 = as.numeric(qmat[3, ]),
    q84 = as.numeric(qmat[4, ]),
    q95 = as.numeric(qmat[5, ])
  )
}

extract_forecast_draw_matrix <- function(fcast, variable) {
  if (is.null(fcast)) {
    return(NULL)
  }
  
  if (!is.null(fcast$fcast) && is.array(fcast$fcast) && length(dim(fcast$fcast)) == 3) {
    arr <- fcast$fcast
    var_names <- dimnames(arr)[[3]] %||% fcast$variables
    var_idx <- match(variable, var_names)
    
    if (!is.na(var_idx)) {
      draw_h <- arr[, , var_idx, drop = TRUE]
      if (is.null(dim(draw_h))) {
        draw_h <- matrix(draw_h, ncol = 1)
      }
      return(draw_h)
    }
  }
  
  NULL
}

back_transform_draw_matrix <- function(draw_mat, info, previous_raw = info$last_raw_value) {
  if (is.null(draw_mat)) {
    return(NULL)
  }
  
  if (is.null(dim(draw_mat))) {
    draw_mat <- matrix(draw_mat, ncol = 1)
  }
  
  if (isTRUE(info$scaled)) {
    scale_scale <- as.numeric(info$scale_scale %||% 1)
    scale_center <- as.numeric(info$scale_center %||% 0)
    
    if (!is.finite(scale_scale) || isTRUE(all.equal(scale_scale, 0))) {
      scale_scale <- 1
    }
    
    draw_mat <- draw_mat * scale_scale + scale_center
  }
  
  if (isTRUE(info$logdiff)) {
    start_log <- log1p(as.numeric(previous_raw))
    out <- t(apply(draw_mat, 1, function(path) exp(cumsum(path) + start_log) - 1))
    if (is.null(dim(out))) {
      out <- matrix(out, nrow = 1)
    }
    return(out)
  }
  
  if (isTRUE(info$log)) {
    return(exp(draw_mat) - as.numeric(info$log_shift %||% 0))
  }
  
  draw_mat
}

extract_forecast_quantile_frame <- function(fcast, variable) {
  if (is.null(fcast)) {
    return(NULL)
  }
  
  if (!is.null(fcast$quants) && is.array(fcast$quants) && length(dim(fcast$quants)) == 3) {
    qarr <- fcast$quants
    var_names <- dimnames(qarr)[[3]] %||% fcast$variables %||% dimnames(fcast$fcast)[[3]]
    var_idx <- match(variable, var_names)
    
    if (!is.na(var_idx)) {
      probs <- parse_probability_labels(dimnames(qarr)[[1]])
      q_cols <- c(q05 = 0.05, q16 = 0.16, q50 = 0.5, q84 = 0.84, q95 = 0.95)
      out <- data.frame(horizon = seq_len(dim(qarr)[2]))
      
      for (nm in names(q_cols)) {
        if (length(probs) && any(!is.na(probs))) {
          idx <- which.min(abs(probs - q_cols[[nm]]))
          out[[nm]] <- as.numeric(qarr[idx, , var_idx])
        }
      }
      
      return(out)
    }
  }
  
  if (!is.null(fcast$fcast) && is.array(fcast$fcast) && length(dim(fcast$fcast)) == 3) {
    arr <- fcast$fcast
    var_names <- dimnames(arr)[[3]] %||% fcast$variables
    var_idx <- match(variable, var_names)
    
    if (!is.na(var_idx)) {
      draw_h <- arr[, , var_idx, drop = TRUE]
      if (is.null(dim(draw_h))) {
        draw_h <- matrix(draw_h, ncol = 1)
      }
      return(quantile_frame_from_matrix(draw_h))
    }
  }
  
  if (!is.null(fcast$fcast) && is.list(fcast$fcast) && variable %in% names(fcast$fcast)) {
    obj <- as.data.frame(fcast$fcast[[variable]])
    probs <- parse_probability_labels(colnames(obj))
    q_cols <- c(q05 = 0.05, q16 = 0.16, q50 = 0.5, q84 = 0.84, q95 = 0.95)
    out <- data.frame(horizon = seq_len(nrow(obj)))
    
    for (nm in names(q_cols)) {
      if (length(probs) && any(!is.na(probs))) {
        idx <- which.min(abs(probs - q_cols[[nm]]))
        out[[nm]] <- as.numeric(obj[[idx]])
      }
    }
    
    return(out)
  }
  
  NULL
}

forecast_path_on_original_scale <- function(run_result, variable, forecast_obj) {
  if (is.null(run_result) || is.null(forecast_obj)) {
    return(NULL)
  }
  
  info <- run_result$transform_info[[variable]] %||% blank_transform_spec(variable)
  draw_mat <- extract_forecast_draw_matrix(forecast_obj, variable)
  
  if (!is.null(draw_mat)) {
    raw_draws <- back_transform_draw_matrix(draw_mat, info, previous_raw = info$last_raw_value)
    return(quantile_frame_from_matrix(raw_draws))
  }
  
  q_frame <- extract_forecast_quantile_frame(forecast_obj, variable)
  
  if (is.null(q_frame)) {
    return(NULL)
  }
  
  out <- q_frame
  q_cols <- setdiff(names(out), "horizon")
  
  for (col in q_cols) {
    out[[col]] <- back_transform_values(out[[col]], info, previous_raw = info$last_raw_value)
  }
  
  out
}

estimate_forecast_sigma <- function(path_df, history_values) {
  sigma <- (path_df$q84 - path_df$q16) / 2
  hist_sigma <- stats::sd(as.numeric(history_values), na.rm = TRUE)
  
  if (!is.finite(hist_sigma) || hist_sigma <= 0) {
    hist_sigma <- median(abs(path_df$q50), na.rm = TRUE) * 0.05
  }
  
  sigma[!is.finite(sigma) | sigma <= 0] <- hist_sigma
  sigma
}

scenario_suggestion_values <- function(path_df, last_raw_value, template, sd_multiple, vol_multiplier, history_values) {
  baseline <- path_df$q50
  sigma <- estimate_forecast_sigma(path_df, history_values)
  
  if (identical(template, "plus_sd")) {
    return(baseline + sd_multiple * sigma)
  }
  
  if (identical(template, "minus_sd")) {
    return(baseline - sd_multiple * sigma)
  }
  
  increments <- diff(c(last_raw_value, baseline))
  
  if (identical(template, "vol_high")) {
    return(last_raw_value + cumsum(increments * vol_multiplier))
  }
  
  if (identical(template, "vol_low")) {
    return(last_raw_value + cumsum(increments / max(vol_multiplier, 1.01)))
  }
  
  baseline
}

build_scenario_frame <- function(run_result, scenario_var, template, sd_multiple, vol_multiplier, manual_values = NULL) {
  if (is.null(run_result) || is.null(scenario_var) || !nzchar(scenario_var)) {
    return(NULL)
  }
  
  path_df <- forecast_path_on_original_scale(run_result, scenario_var, run_result$forecast)
  if (is.null(path_df)) {
    return(NULL)
  }
  
  info <- run_result$transform_info[[scenario_var]]
  history_values <- run_result$data_used[[scenario_var]]
  suggested <- scenario_suggestion_values(
    path_df = path_df,
    last_raw_value = info$last_raw_value,
    template = template,
    sd_multiple = sd_multiple,
    vol_multiplier = vol_multiplier,
    history_values = history_values
  )
  
  scenario_values <- manual_values
  if (is.null(scenario_values) || length(scenario_values) != nrow(path_df)) {
    scenario_values <- suggested
  }
  
  lower_bound <- -Inf
  if (isTRUE(info$logdiff)) {
    lower_bound <- -0.999999
  } else if (isTRUE(info$log)) {
    lower_bound <- -as.numeric(info$log_shift %||% 0) + 1e-6
  }
  
  suggested <- pmax(suggested, lower_bound)
  scenario_values <- pmax(as.numeric(scenario_values), lower_bound)
  
  scenario_model <- transform_values_for_conditioning(
    raw_values = scenario_values,
    info = info,
    previous_raw = info$last_raw_value
  )
  
  data.frame(
    Horizon = path_df$horizon,
    Baseline = round(path_df$q50, 3),
    `1 Sigma` = round(estimate_forecast_sigma(path_df, history_values), 3),
    Suggested = round(suggested, 3),
    `Scenario Path` = round(scenario_values, 3),
    `Delta vs Baseline` = round(scenario_values - path_df$q50, 3),
    scenario_model = scenario_model,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

run_conditional_forecast <- function(run_result, scenario_var, scenario_model_values) {
  cond_setup <- make_conditional_path(
    fitted_vars = run_result$meta$fitted_vars,
    forecast_horizon = length(scenario_model_values),
    scenario_var = scenario_var,
    scenario_values = scenario_model_values
  )
  
  if (is.null(cond_setup)) {
    stop("Could not build the conditional forecast path.")
  }
  
  stats::predict(
    run_result$fit,
    cond_path = cond_setup$cond_path,
    cond_vars = cond_setup$cond_vars,
    horizon = length(scenario_model_values),
    conf_bands = run_result$meta$conf_bands %||% c(0.05, 0.16)
  )
}

safe_run_conditional_forecast <- function(run_result, scenario_var, scenario_model_values) {
  tryCatch(
    {
      list(
        ok = TRUE,
        result = run_conditional_forecast(run_result, scenario_var, scenario_model_values),
        error = NULL
      )
    },
    error = function(e) {
      list(ok = FALSE, result = NULL, error = conditionMessage(e))
    }
  )
}

build_forecast_plot_data <- function(run_result, forecast_obj, comparison_obj = NULL, t_back = 16) {
  vars_to_plot <- run_result$meta$fitted_vars %||% character(0)
  if (!length(vars_to_plot)) {
    return(NULL)
  }
  
  has_dates <- "quarter" %in% names(run_result$data_used) &&
    inherits(run_result$data_used$quarter, "Date") &&
    any(!is.na(run_result$data_used$quarter))
  
  history_list <- list()
  baseline_band_list <- list()
  baseline_line_list <- list()
  scenario_band_list <- list()
  scenario_line_list <- list()
  split_list <- list()
  x_axis_label <- "Quarter index"
  
  for (v in vars_to_plot) {
    base_df <- forecast_path_on_original_scale(run_result, v, forecast_obj)
    comp_df <- if (!is.null(comparison_obj)) {
      forecast_path_on_original_scale(run_result, v, comparison_obj)
    } else {
      NULL
    }
    
    if (is.null(base_df)) {
      next
    }
    
    hist_all_values <- as.numeric(run_result$data_used[[v]])
    hist_n <- min(t_back, length(hist_all_values))
    hist_values <- utils::tail(hist_all_values, hist_n)
    
    if (has_dates) {
      hist_index <- utils::tail(run_result$data_used$quarter, hist_n)
      last_hist_index <- utils::tail(hist_index, 1)
      fc_index <- seq.Date(from = last_hist_index, by = "quarter", length.out = nrow(base_df) + 1L)[-1]
      x_axis_label <- "Quarter"
    } else {
      hist_index <- seq_len(hist_n)
      last_hist_index <- utils::tail(hist_index, 1)
      fc_index <- last_hist_index + base_df$horizon
      x_axis_label <- "Quarter index"
    }
    
    panel_name <- display_label(v)
    history_text <- paste0(
      panel_name,
      "<br>Period: ",
      vapply(hist_index, format_period_value, character(1)),
      "<br>History: ",
      vapply(hist_values, format_numeric_value, character(1))
    )
    history_list[[v]] <- data.frame(
      panel_label = panel_name,
      x = hist_index,
      value = hist_values,
      text = history_text,
      stringsAsFactors = FALSE
    )
    
    base_text <- paste0(
      panel_name,
      "<br>Period: ",
      vapply(fc_index, format_period_value, character(1)),
      "<br>Baseline median: ",
      vapply(base_df$q50, format_numeric_value, character(1)),
      "<br>68% band: [",
      vapply(base_df$q16, format_numeric_value, character(1)),
      ", ",
      vapply(base_df$q84, format_numeric_value, character(1)),
      "]"
    )
    baseline_band_list[[v]] <- data.frame(
      panel_label = panel_name,
      x = fc_index,
      q16 = base_df$q16,
      q84 = base_df$q84,
      text = base_text,
      stringsAsFactors = FALSE
    )
    baseline_line_list[[v]] <- data.frame(
      panel_label = panel_name,
      x = c(last_hist_index, fc_index),
      value = c(utils::tail(hist_values, 1), base_df$q50),
      text = c(
        paste0(panel_name, "<br>Period: ", format_period_value(last_hist_index), "<br>History: ", format_numeric_value(utils::tail(hist_values, 1))),
        base_text
      ),
      stringsAsFactors = FALSE
    )
    
    if (!is.null(comp_df)) {
      comp_text <- paste0(
        panel_name,
        "<br>Period: ",
        vapply(fc_index, format_period_value, character(1)),
        "<br>Scenario median: ",
        vapply(comp_df$q50, format_numeric_value, character(1)),
        "<br>68% band: [",
        vapply(comp_df$q16, format_numeric_value, character(1)),
        ", ",
        vapply(comp_df$q84, format_numeric_value, character(1)),
        "]"
      )
      scenario_band_list[[v]] <- data.frame(
        panel_label = panel_name,
        x = fc_index,
        q16 = comp_df$q16,
        q84 = comp_df$q84,
        text = comp_text,
        stringsAsFactors = FALSE
      )
      scenario_line_list[[v]] <- data.frame(
        panel_label = panel_name,
        x = c(last_hist_index, fc_index),
        value = c(utils::tail(hist_values, 1), comp_df$q50),
        text = c(
          paste0(panel_name, "<br>Period: ", format_period_value(last_hist_index), "<br>History: ", format_numeric_value(utils::tail(hist_values, 1))),
          comp_text
        ),
        stringsAsFactors = FALSE
      )
    }
    
    split_list[[v]] <- data.frame(
      panel_label = panel_name,
      x = last_hist_index,
      stringsAsFactors = FALSE
    )
  }
  
  list(
    history = dplyr::bind_rows(history_list),
    baseline_band = dplyr::bind_rows(baseline_band_list),
    baseline_line = dplyr::bind_rows(baseline_line_list),
    scenario_band = dplyr::bind_rows(scenario_band_list),
    scenario_line = dplyr::bind_rows(scenario_line_list),
    split = dplyr::bind_rows(split_list),
    x_axis_label = x_axis_label %||% "Quarter index"
  )
}

plot_forecast_panels <- function(run_result, forecast_obj, comparison_obj = NULL, title_prefix = "Forecast", t_back = 16) {
  plot_data <- build_forecast_plot_data(
    run_result = run_result,
    forecast_obj = forecast_obj,
    comparison_obj = comparison_obj,
    t_back = t_back
  )
  
  if (is.null(plot_data) || !nrow(plot_data$baseline_band)) {
    return(as_interactive_plot(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 1, y = 1, label = "No forecast data available.") +
        ggplot2::theme_void()
    ))
  }
  
  p <- suppressWarnings({
    p0 <- ggplot2::ggplot() +
      ggplot2::geom_line(
        data = plot_data$history,
        ggplot2::aes(x = x, y = value, group = panel_label, color = "History", text = text),
        linewidth = 0.9
      ) +
      ggplot2::geom_point(
        data = plot_data$history,
        ggplot2::aes(x = x, y = value, group = panel_label, color = "History", text = text),
        size = 1.25,
        show.legend = FALSE
      ) +
      ggplot2::geom_ribbon(
        data = plot_data$baseline_band,
        ggplot2::aes(x = x, ymin = q16, ymax = q84, group = panel_label, fill = "Baseline 68% band", text = text),
        alpha = 0.28
      ) +
      ggplot2::geom_line(
        data = plot_data$baseline_line,
        ggplot2::aes(x = x, y = value, group = panel_label, color = "Baseline median", text = text),
        linewidth = 1.0
      ) +
      ggplot2::geom_point(
        data = plot_data$baseline_line,
        ggplot2::aes(x = x, y = value, group = panel_label, color = "Baseline median", text = text),
        size = 1.25,
        show.legend = FALSE
      ) +
      ggplot2::geom_vline(
        data = plot_data$split,
        ggplot2::aes(xintercept = x),
        color = "grey70",
        linetype = "dashed"
      ) +
      ggplot2::facet_wrap(~panel_label, ncol = 1, scales = "free_y") +
      ggplot2::scale_color_manual(
        values = c(
          "History" = "#243b53",
          "Baseline median" = "#2f6fb0",
          "Scenario median" = "#d1495b"
        ),
        name = NULL
      ) +
      ggplot2::scale_fill_manual(
        values = c(
          "Baseline 68% band" = "#9ec5ea",
          "Scenario 68% band" = "#f2a7af"
        ),
        name = NULL
      ) +
      ggplot2::labs(
        title = title_prefix,
        x = plot_data$x_axis_label,
        y = NULL
      ) +
      theme_bvar_plot()
    
    if (nrow(plot_data$scenario_band)) {
      p0 <- p0 +
        ggplot2::geom_ribbon(
          data = plot_data$scenario_band,
          ggplot2::aes(x = x, ymin = q16, ymax = q84, group = panel_label, fill = "Scenario 68% band", text = text),
          alpha = 0.22
        ) +
        ggplot2::geom_line(
          data = plot_data$scenario_line,
          ggplot2::aes(x = x, y = value, group = panel_label, color = "Scenario median", text = text),
          linewidth = 1.0,
          linetype = "dashed"
        ) +
        ggplot2::geom_point(
          data = plot_data$scenario_line,
          ggplot2::aes(x = x, y = value, group = panel_label, color = "Scenario median", text = text),
          size = 1.25,
          show.legend = FALSE
        )
    }
    
    p0
  })
  
  as_interactive_plot(p)
}

plot_forecast_median_panels <- function(run_result, forecast_obj, comparison_obj = NULL, title_prefix = "Forecast Median Detail", t_back = 16) {
  plot_data <- build_forecast_plot_data(
    run_result = run_result,
    forecast_obj = forecast_obj,
    comparison_obj = comparison_obj,
    t_back = t_back
  )
  
  if (is.null(plot_data) || !nrow(plot_data$baseline_line)) {
    return(as_interactive_plot(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 1, y = 1, label = "No forecast data available.") +
        ggplot2::theme_void()
    ))
  }
  
  p <- suppressWarnings({
    p0 <- ggplot2::ggplot() +
      ggplot2::geom_line(
        data = plot_data$history,
        ggplot2::aes(x = x, y = value, group = panel_label, color = "History", text = text),
        linewidth = 0.9
      ) +
      ggplot2::geom_point(
        data = plot_data$history,
        ggplot2::aes(x = x, y = value, group = panel_label, color = "History", text = text),
        size = 1.25,
        show.legend = FALSE
      ) +
      ggplot2::geom_line(
        data = plot_data$baseline_line,
        ggplot2::aes(x = x, y = value, group = panel_label, color = "Baseline median", text = text),
        linewidth = 1.0
      ) +
      ggplot2::geom_point(
        data = plot_data$baseline_line,
        ggplot2::aes(x = x, y = value, group = panel_label, color = "Baseline median", text = text),
        size = 1.25,
        show.legend = FALSE
      ) +
      ggplot2::geom_vline(
        data = plot_data$split,
        ggplot2::aes(xintercept = x),
        color = "grey70",
        linetype = "dashed"
      ) +
      ggplot2::facet_wrap(~panel_label, ncol = 1, scales = "free_y") +
      ggplot2::scale_color_manual(
        values = c(
          "History" = "#243b53",
          "Baseline median" = "#2f6fb0",
          "Scenario median" = "#d1495b"
        ),
        name = NULL
      ) +
      ggplot2::labs(
        title = title_prefix,
        x = plot_data$x_axis_label,
        y = NULL
      ) +
      theme_bvar_plot()
    
    if (nrow(plot_data$scenario_line)) {
      p0 <- p0 +
        ggplot2::geom_line(
          data = plot_data$scenario_line,
          ggplot2::aes(x = x, y = value, group = panel_label, color = "Scenario median", text = text),
          linewidth = 1.0,
          linetype = "dashed"
        ) +
        ggplot2::geom_point(
          data = plot_data$scenario_line,
          ggplot2::aes(x = x, y = value, group = panel_label, color = "Scenario median", text = text),
          size = 1.25,
          show.legend = FALSE
        )
    }
    
    p0
  })
  
  as_interactive_plot(p)
}

make_conditional_path <- function(
    fitted_vars,
    forecast_horizon,
    scenario_var,
    scenario_values
) {
  if (is.null(scenario_var) || !nzchar(scenario_var)) return(NULL)
  scenario_idx <- match(scenario_var, fitted_vars)
  if (is.na(scenario_idx)) return(NULL)
  
  cond_path <- matrix(NA_real_, nrow = forecast_horizon, ncol = 1)
  vals <- suppressWarnings(as.numeric(scenario_values))
  vals <- vals[seq_len(min(length(vals), forecast_horizon))]
  cond_path[seq_along(vals), 1] <- vals
  
  list(
    cond_path = cond_path,
    cond_vars = scenario_idx
  )
}

fevd_color_palette <- function(n) {
  # A muted, print-friendly palette inspired by the reference images
  base_colors <- c(
    "#4c78a8",  # steel blue
    "#e45756",  # coral red
    "#72b7b2",  # teal
    "#f58518",  # orange
    "#eeca3b",  # gold
    "#b279a2",  # mauve
    "#54a24b",  # green
    "#ff9da6",  # pink
    "#9d755d",  # brown
    "#bab0ac",  # grey
    "#d67195",  # rose
    "#88d27a",  # light green
    "#b6992d",  # dark gold
    "#439894",  # dark teal
    "#d1495b"   # deep red
  )
  if (n <= length(base_colors)) {
    return(base_colors[seq_len(n)])
  }
  # Fall back to a generated palette for very large models
  grDevices::colorRampPalette(base_colors)(n)
}

################################################################################
# UI
################################################################################

ui <- fluidPage(
  theme = bslib::bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#144b6f",
    secondary = "#6c757d",
    base_font = bslib::font_google("Manrope"),
    heading_font = bslib::font_google("Source Serif 4")
  ),
  
  tags$head(
    tags$style(HTML("
      body {
        background: linear-gradient(180deg, #edf4f7 0%, #f7f3ec 100%);
      }
      .container-fluid {
        padding-top: 18px;
        padding-bottom: 18px;
      }
      .well, .sidebar-panel, .panel, .tab-content {
        border-radius: 12px;
      }
      .sidebar {
        background: rgba(255,255,255,0.96);
        border: 1px solid #e3e8ef;
        border-radius: 14px;
        padding: 18px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.04);
      }
      .main-panel-custom {
        background: rgba(255,255,255,0.96);
        border: 1px solid #e3e8ef;
        border-radius: 14px;
        padding: 18px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.04);
      }
      .hero-panel {
        margin-bottom: 14px;
        padding: 18px 20px;
        border-radius: 16px;
        background: linear-gradient(135deg, #123a56 0%, #2f6b7f 55%, #d6a35d 100%);
        color: #ffffff;
        box-shadow: 0 12px 28px rgba(18,58,86,0.18);
      }
      .hero-title {
        margin: 0;
        font-size: 2rem;
        line-height: 1.1;
      }
      .hero-subtitle {
        margin-top: 8px;
        margin-bottom: 0;
        max-width: 900px;
        color: rgba(255,255,255,0.9);
        font-size: 1rem;
      }
      .section-heading {
        margin-top: 8px;
        margin-bottom: 10px;
        color: #123a56;
        font-size: 0.82rem;
        font-weight: 800;
        letter-spacing: 0.08em;
        text-transform: uppercase;
      }
      .soft-note {
        margin-bottom: 12px;
        padding: 12px 14px;
        border-radius: 12px;
        background: #f7fafc;
        border: 1px solid #d8e2eb;
        color: #26435a;
        font-size: 0.94rem;
      }
      .soft-note strong {
        display: block;
        margin-bottom: 4px;
        color: #123a56;
      }
      .scenario-note {
        margin-top: 8px;
        margin-bottom: 12px;
        padding: 12px 14px;
        border-radius: 12px;
        background: #f7f1e7;
        border: 1px solid #ead5b5;
        color: #6c4f25;
      }
      .nav-tabs {
        margin-bottom: 16px;
      }
      .nav-tabs > li > a {
        border-radius: 10px 10px 0 0;
        font-weight: 600;
      }
      .btn-primary {
        border-radius: 10px;
        font-weight: 600;
      }
      .btn-default {
        border-radius: 10px;
      }
      .form-group {
        margin-bottom: 14px;
      }
      h1, h2, h3, h4 {
        font-weight: 700;
      }
      .irs, .js-irs-0 {
        margin-bottom: 8px;
      }
      pre {
        background: #f8fafc;
        border: 1px solid #e3e8ef;
        border-radius: 10px;
        padding: 14px;
      }
      .info-icon-btn {
        color: #1f4e79 !important;
        text-decoration: none !important;
        font-size: 16px;
        line-height: 1;
        padding: 2px 4px;
      }
      .info-icon-btn:hover {
        color: #163a59 !important;
        text-decoration: none !important;
      }
      .info-panel {
        margin-top: 6px;
        margin-bottom: 12px;
        padding: 10px 12px;
        background: #f8fafc;
        border: 1px solid #e3e8ef;
        border-radius: 10px;
        font-size: 0.95rem;
      }
      .validation-ok-box {
        margin-top: 10px;
        padding: 10px 12px;
        background: #eef8f0;
        border: 1px solid #b7dfbf;
        border-radius: 10px;
        color: #1e5e2b;
        font-weight: 600;
      }
      .validation-warn-box {
        margin-top: 10px;
        padding: 10px 12px;
        background: #fff4e5;
        border: 1px solid #f2cf8a;
        border-radius: 10px;
        color: #8a5a00;
        font-weight: 600;
      }
      .validation-bad-box {
        margin-top: 10px;
        padding: 10px 12px;
        background: #fdeeee;
        border: 1px solid #efb3b3;
        border-radius: 10px;
        color: #8b1e1e;
        font-weight: 600;
      }
      .tab-caption {
        margin-bottom: 14px;
        color: #516273;
      }
    "))
  ),
  
  titlePanel(div(
    class = "hero-panel",
    tags$h1(class = "hero-title", "Climate-Macro BVAR Explorer"),
    tags$p(
      class = "hero-subtitle",
      "Build baseline and scenario-aware BVAR forecasts, compare impulse responses, and inspect the final prior and hyperparameter choices with clearer guidance."
    )
  )),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      class = "sidebar",
      
      div(
        class = "soft-note",
        tags$strong("Workflow"),
        "Choose variables, use Exploratory Data Analysis to check definitions and quick charts, review the validation warnings, run the baseline model, then move to the Scenario Forecast tab to build what-if paths."
      ),
      
      div(class = "section-heading", "Data setup"),
      
      info_select_input(
        id = "dataset_name",
        label = "Dataset",
        choices = named_choices(names(all_datasets)),
        selected = names(all_datasets)[1],
        what_it_is = "Selects which dataset from the loaded .RData file will be used for validation, transformation, estimation, IRFs, and forecasts."
      ),
      
      uiOutput("date_col_ui"),
      
      tags$hr(),
      div(class = "section-heading", "Model variables"),
      
      info_selectize_input(
        "climate_vars", "Climate variables", choices = NULL, multiple = TRUE,
        options = list(placeholder = "Select one or more climate variables"),
        what_it_is = "Variables treated as climate-side variables in the model ordering. These are typically the variables used as climate shocks or climate indicators. Use the Exploratory Data Analysis tab to look up definitions, dictionary entries, and quick plots before deciding."
      ),
      
      info_selectize_input(
        "macro_vars", "Macro variables", choices = NULL, multiple = TRUE,
        options = list(placeholder = "Select one or more macro variables"),
        what_it_is = "Variables treated as macroeconomic variables in the model ordering. These are usually the main responses of interest. Use the Exploratory Data Analysis tab to look up definitions, dictionary entries, and quick plots before deciding."
      ),
      
      info_selectize_input(
        "response_vars", "Response variables", choices = NULL, multiple = TRUE,
        options = list(placeholder = "Defaults to selected macro variables"),
        what_it_is = "Variables whose impulse responses you want to inspect. If left empty, the app defaults to the selected macro variables. If you need a refresher on what a series means, use the Exploratory Data Analysis tab."
      ),
      
      info_selectize_input(
        "impulse_vars", "Impulse variables", choices = NULL, multiple = TRUE,
        options = list(placeholder = "Choose one or more impulse variables"),
        what_it_is = "Variables used as the source of shocks in the IRFs. If left empty, the app defaults to the selected climate variables. If you need a refresher on what a series means, use the Exploratory Data Analysis tab."
      ),
      
      info_selectize_input(
        "irf_plot_impulses", "IRF plots to display", choices = NULL, multiple = TRUE,
        options = list(placeholder = "Select one or more impulse variables to plot"),
        what_it_is = "Controls which selected impulse variables are actually shown in the IRF tab."
      ),
      
      tags$hr(),
      div(class = "section-heading", "Transformations"),
      div(
        class = "soft-note",
        tags$strong("Transformation defaults"),
        "Effective defaults: if selected, Drought Code Mean Top 10, Days of Flooding, Current Account Exports, and Runoff 6h Sum are preselected for log-differencing because they are persistent positive series that are often easier to model in change form. If the Log-transform box is left empty, the app log-transforms the selected macro variables that are not already log-differenced. If the Scale variables box is left empty, the app standardizes the selected climate variables. The Validation and Exploratory Data Analysis tabs show the effective transformation plan actually used."
      ),
      
      info_selectize_input(
        "log_vars", "Log-transform variables", choices = NULL, multiple = TRUE,
        what_it_is = "Variables to be log-transformed before estimation. Best for positive level variables where proportional changes are more meaningful. If this box is left empty, the app defaults to the selected macro variables that are not already log-differenced."
      ),
      
      info_selectize_input(
        "logdiff_vars", "Log-difference variables", choices = NULL, multiple = TRUE,
        what_it_is = "Variables transformed as diff(log1p(x)) before estimation. Useful for persistent positive level variables such as runoff sums. A short list of recommended series is preselected automatically when they are part of the model."
      ),
      
      info_selectize_input(
        "scale_vars", "Scale variables", choices = NULL, multiple = TRUE,
        what_it_is = "Variables to standardize before estimation. This can improve numerical stability and comparability across variables with different units. If this box is left empty, the app defaults to the selected climate variables."
      ),
      
      tags$hr(),
      div(class = "section-heading", "Model size"),
      
      info_numeric_input(
        "lags", "Lags", value = 5, min = 1, step = 1,
        what_it_is = "Number of lagged periods included in the BVAR.",
        increase_text = "Captures longer memory and richer dynamics, but increases the number of parameters and needs more usable observations.",
        decrease_text = "Makes the model smaller and easier to estimate, but may miss persistence and delayed effects."
      ),
      
      uiOutput("model_size_warning"),
      
      info_checkbox_input(
        "do_irf", "Run IRF", value = TRUE,
        what_it_is = "Whether to compute impulse response functions after estimation.",
        increase_text = "Turning this on computes IRFs and enables the IRF tab.",
        decrease_text = "Turning this off skips IRF computation and can slightly reduce runtime."
      ),
      
      info_numeric_input(
        "irf_horizon", "IRF horizon", value = 12, min = 1, step = 1,
        what_it_is = "Number of periods ahead shown in each impulse response function.",
        increase_text = "Shows longer-run shock effects, but later horizons are usually more uncertain.",
        decrease_text = "Focuses on short-run effects."
      ),
      
      info_checkbox_input(
        "identification", "Identification", value = TRUE,
        what_it_is = "Whether to impose structural identification for the IRFs.",
        increase_text = "Turning this on gives structurally identified responses under the chosen BVAR setup.",
        decrease_text = "Turning this off gives reduced-form style responses without that structural interpretation."
      ),
      
      info_checkbox_input(
        "do_forecast", "Run forecast", value = TRUE,
        what_it_is = "Whether to compute forecasts after estimation.",
        increase_text = "Turning this on produces forecast output and enables the Forecast tab.",
        decrease_text = "Turning this off skips forecasting and can reduce runtime."
      ),
      
      info_numeric_input(
        "forecast_horizon", "Forecast horizon", value = 8, min = 1, step = 1,
        what_it_is = "Number of periods ahead to forecast.",
        increase_text = "Produces longer forecasts, but forecast uncertainty grows with horizon.",
        decrease_text = "Produces shorter, more local forecasts."
      ),
      
      tags$details(
        tags$summary(style = "cursor:pointer; font-weight:600;", "Advanced settings"),
        div(
          class = "soft-note",
          tags$strong("Tuning recommendation"),
          "Keep hierarchical nonlinear tuning on as the default. For this BVAR, that is usually more informative than a coarse grid search because the prior hyperparameters are continuous and interact with one another."
        ),
        
        info_numeric_input(
          "n_draw", "MCMC draws", value = 15000, min = 500, step = 500,
          what_it_is = "Total number of posterior draws retained for inference.",
          increase_text = "Usually improves posterior stability and smoothness, but increases runtime.",
          decrease_text = "Runs faster, but posterior summaries may be noisier."
        ),
        
        info_numeric_input(
          "n_burn", "Burn-in", value = 5000, min = 0, step = 500,
          what_it_is = "Number of early MCMC draws discarded before posterior inference.",
          increase_text = "Can reduce the influence of unstable early draws, but increases total runtime.",
          decrease_text = "Keeps more draws, but risks including pre-convergence samples."
        ),
        
        info_numeric_input(
          "n_thin", "Thin", value = 1, min = 1, step = 1,
          what_it_is = "Keeps every nth MCMC draw.",
          increase_text = "Reduces autocorrelation and storage, but leaves fewer retained draws.",
          decrease_text = "Keeps more draws, but may preserve more autocorrelation."
        ),
        
        div(
          class = "soft-note",
          tags$strong("Convergence checks"),
          "The optimizer output later in the app refers only to hyperparameter initialization. The dedicated Convergence tab uses coda to assess the posterior sampler itself."
        ),
        
        info_checkbox_input(
          "run_multichain_diag", "Run multi-chain convergence diagnostics", value = FALSE,
          what_it_is = "When enabled, the app fits extra diagnostic-only BVAR chains after the main model succeeds so you can inspect Gelman-Rubin diagnostics.",
          increase_text = "Adds optional multi-chain convergence checks, but increases runtime.",
          decrease_text = "Keeps runtime lower and reports only single-chain diagnostics on the main fitted model."
        ),
        
        conditionalPanel(
          condition = "input.run_multichain_diag",
          div(
            class = "soft-note",
            tags$strong("Diagnostic-only extra chains"),
            "The extra chains reuse the same transformed data, lag length, priors, and sampler settings as the main model, but skip IRFs and forecasts."
          ),
          
          info_numeric_input(
            "n_chains", "Number of chains", value = 4, min = 2, step = 1,
            what_it_is = "Total number of posterior chains used for Gelman-Rubin diagnostics."
          ),
          
          info_checkbox_input(
            "parallel_chains", "Run chains in parallel when possible", value = TRUE,
            what_it_is = "The app first tries BVAR::par_bvar() for the extra diagnostic chains. If parallel execution fails, it falls back to sequential reruns."
          ),
          
          info_numeric_input(
            "seed_base", "Base random seed", value = 1234, min = 1, step = 1,
            what_it_is = "Base seed used for the extra diagnostic chains so repeated runs are easier to compare."
          )
        ),
        
        info_select_input(
          id = "prior_spec",
          label = "Prior family",
          choices = stats::setNames(names(prior_choice_labels), unname(prior_choice_labels)),
          selected = "minnesota",
          what_it_is = "Choose the prior structure. Minnesota is the default. SOC helps with persistent levels. SUR is useful when cointegration or near-unit-root behavior is plausible."
        ),
        
        uiOutput("prior_guidance_ui"),
        
        info_select_input(
          id = "hyper_mode",
          label = "Hierarchical hyperparameters",
          choices = c(
            "Automatic subset" = "auto",
            "Full set" = "full",
            "Lambda only" = "lambda",
            "Lambda + alpha" = "lambda_alpha"
          ),
          selected = "auto",
          what_it_is = "Controls which prior parameters are treated hierarchically."
        ),
        
        info_numeric_input(
          "alpha_sd", "Alpha prior SD", value = 0.25, step = 0.05,
          what_it_is = "Prior uncertainty around alpha."
        ),
        
        info_numeric_input(
          "alpha_min", "Alpha minimum", value = 1, step = 0.1,
          what_it_is = "Lower bound allowed for alpha."
        ),
        
        info_numeric_input(
          "alpha_max", "Alpha maximum", value = 3, step = 0.1,
          what_it_is = "Upper bound allowed for alpha."
        ),
        
        info_numeric_input(
          "soc_mode", "SOC mode", value = 1, step = 0.1,
          what_it_is = "Tightness parameter for the sum-of-coefficients prior."
        ),
        
        info_numeric_input(
          "soc_sd", "SOC prior SD", value = 1, step = 0.1,
          what_it_is = "Prior uncertainty around SOC."
        ),
        
        info_numeric_input(
          "sur_mode", "SUR mode", value = 1, step = 0.1,
          what_it_is = "Tightness parameter for the single-unit-root prior."
        ),
        
        info_numeric_input(
          "sur_sd", "SUR prior SD", value = 1, step = 0.1,
          what_it_is = "Prior uncertainty around SUR."
        ),
        
        info_numeric_input(
          "lambda_mode", "Lambda mode", value = 0.2, step = 0.05,
          what_it_is = "Main Minnesota prior tightness parameter.",
          increase_text = "Typically relaxes shrinkage and lets coefficients move more freely; can fit better but may overfit.",
          decrease_text = "Typically imposes stronger shrinkage; can stabilize estimation but may underfit."
        ),
        
        info_numeric_input(
          "lambda_sd", "Lambda prior SD", value = 0.4, step = 0.05,
          what_it_is = "Spread of the prior uncertainty around lambda.",
          increase_text = "Allows more variation in prior tightness.",
          decrease_text = "Makes the prior on tightness more concentrated."
        ),
        
        info_numeric_input(
          "lambda_min", "Lambda minimum", value = 1e-4, step = 1e-4,
          what_it_is = "Lower bound allowed for lambda.",
          increase_text = "Prevents lambda from becoming extremely small.",
          decrease_text = "Allows smaller values and potentially stronger shrinkage regimes."
        ),
        
        info_numeric_input(
          "lambda_max", "Lambda maximum", value = 5, step = 0.1,
          what_it_is = "Upper bound allowed for lambda.",
          increase_text = "Allows weaker shrinkage and more flexibility.",
          decrease_text = "Restricts the prior from becoming too loose."
        ),
        
        info_numeric_input(
          "alpha_mode", "Alpha mode", value = 2, step = 0.1,
          what_it_is = "Controls lag decay in the Minnesota prior.",
          increase_text = "Makes higher-order lags relatively less penalized.",
          decrease_text = "Shrinks higher-order lags more aggressively."
        ),
        
        info_numeric_input(
          "intercept_var", "Intercept prior variance", value = 1e7, step = 1e5,
          what_it_is = "Prior variance for the intercept term.",
          increase_text = "Lets the intercept vary more freely.",
          decrease_text = "Shrinks the intercept more strongly toward its prior mean."
        ),
        
        info_numeric_input(
          "scale_hess", "Proposal scale", value = 0.05, step = 0.01,
          what_it_is = "Scaling used in the Metropolis-Hastings proposal.",
          increase_text = "Larger proposal jumps; can explore faster, but acceptance may fall.",
          decrease_text = "Smaller proposal jumps; acceptance may improve, but mixing can slow."
        ),
        
        info_checkbox_input(
          "adjust_acc", "Auto-adjust acceptance", value = TRUE,
          what_it_is = "Whether the proposal scale is automatically adjusted toward the target acceptance range.",
          increase_text = "Turning this on lets the sampler self-tune.",
          decrease_text = "Turning this off keeps the proposal scale fixed."
        ),
        
        info_numeric_input(
          "acc_lower", "Acceptance lower bound", value = 0.25, step = 0.01,
          what_it_is = "Lower bound of the target Metropolis-Hastings acceptance range.",
          increase_text = "Requires a higher minimum acceptance rate before the proposal is considered acceptable.",
          decrease_text = "Allows lower acceptance before retuning."
        ),
        
        info_numeric_input(
          "acc_upper", "Acceptance upper bound", value = 0.45, step = 0.01,
          what_it_is = "Upper bound of the target Metropolis-Hastings acceptance range.",
          increase_text = "Allows higher acceptance before retuning.",
          decrease_text = "Pushes the sampler toward larger jumps sooner."
        )
      ),
      
      div(
        style = "display:flex; gap:10px; margin-top:14px;",
        actionButton("run_model", "Run model", class = "btn-primary"),
        actionButton("reset_defaults", "Reset defaults", class = "btn-default")
      )
    ),
    
    mainPanel(
      width = 8,
      div(
        class = "main-panel-custom",
        tabsetPanel(
          tabPanel(
            "Validation",
            br(),
            div(class = "tab-caption", "Check sample size, known singularity risks, and transformation recommendations before estimating the model."),
            verbatimTextOutput("validation_text"),
            br(),
            h4("Current transformation plan"),
            DT::dataTableOutput("transform_plan_table"),
            br(),
            DT::dataTableOutput("dataset_info")
          ),
          tabPanel(
            "Exploratory Data Analysis",
            br(),
            div(class = "tab-caption", "Use this tab to inspect dictionary definitions, quick charts, summary statistics, and the current transformation plan before running the model."),
            tabsetPanel(
              tabPanel(
                "Any variable",
                div(
                  class = "soft-note",
                  tags$strong("Dictionary-linked explorer"),
                  "Choose a variable from the dropdown or click it in the data dictionary table below to see its definition, summary statistics, and quick charts."
                ),
                fluidRow(
                  column(
                    5,
                    selectizeInput(
                      "eda_any_var",
                      "Variable",
                      choices = NULL,
                      selected = NULL,
                      options = list(placeholder = "Choose any available variable")
                    )
                  ),
                  column(7, uiOutput("eda_any_var_info"))
                ),
                fluidRow(
                  column(6, plotly::plotlyOutput("eda_any_var_plot", height = "360px")),
                  column(6, plotly::plotlyOutput("eda_any_var_hist", height = "360px"))
                ),
                br(),
                DT::dataTableOutput("eda_any_var_summary"),
                br(),
                h4("Data dictionary"),
                div(class = "tab-caption", "Click a row to update the variable explorer above."),
                DT::dataTableOutput("eda_dictionary_table")
              ),
              tabPanel(
                "Selected model variables",
                div(
                  class = "soft-note",
                  tags$strong("Manual refresh"),
                  "This section only recalculates after you click Update selected variable plots, so you can adjust the list without re-running the charts each time."
                ),
                uiOutput("eda_selected_vars_ui"),
                div(
                  style = "display:flex; gap:10px; margin-bottom:10px;",
                  actionButton("run_selected_eda", "Update selected variable plots", class = "btn-primary")
                ),
                h4("Transformation plan"),
                DT::dataTableOutput("eda_transform_table"),
                br(),
                h4("Selected variable overview"),
                DT::dataTableOutput("eda_selected_overview"),
                br(),
                uiOutput("eda_selected_plot_ui"),
                br(),
                h4("Pairwise correlations"),
                div(class = "tab-caption", "A quick pairwise correlation table for the currently displayed selected variables."),
                DT::dataTableOutput("eda_selected_correlation")
              )
            )
          ),
          tabPanel(
            "Summary",
            br(),
            verbatimTextOutput("run_status"),
            verbatimTextOutput("model_summary"),
            br(),
            h4("Model size and complexity"),
            DT::dataTableOutput("model_complexity_table"),
            div(class = "tab-caption", textOutput("model_complexity_note")),
            div(
              class = "tab-caption",
              "The effective coefficient count uses posterior 95% credible intervals excluding zero, so it is a practical shrinkage summary rather than a formal degrees-of-freedom estimate."
            )
          ),
          tabPanel(
            "Hyperparameters",
            br(),
            div(class = "tab-caption", "This tab shows the final tuned hyperparameters, the optimiser details used for hyperparameter initialization, the internally computed psi scales, and a short interpretation of what those values imply. Full MCMC convergence diagnostics are reported separately in the Convergence tab."),
            verbatimTextOutput("hyperparameter_text"),
            br(),
            h4("Posterior summary"),
            DT::dataTableOutput("hyperparameter_table"),
            br(),
            h4("Cross-lag prior scales (psi)"),
            DT::dataTableOutput("psi_table"),
            br(),
            h4("Optimizer / initialization details"),
            DT::dataTableOutput("optimizer_table"),
            br(),
            plot_info_section(
              id = "hyper_trace",
              label = "Posterior trace plot (not a full convergence assessment)",
              what_to_look_for = plot_help_text$hyper_trace,
              plot_tag = plotOutput("hyper_trace_plot", height = "500px")
            ),
            br(),
            plot_info_section(
              id = "hyper_density",
              label = "Posterior density plot",
              what_to_look_for = plot_help_text$hyper_density,
              plot_tag = plotOutput("hyper_density_plot", height = "500px")
            )
          ),
          tabPanel(
            "Convergence",
            br(),
            div(
              class = "tab-caption",
              "Optimizer convergence is not the same as MCMC convergence. The optimiser details in the Hyperparameters tab refer only to the nonlinear initialization step for hierarchical hyperparameters. The coda diagnostics below assess the posterior sampling behavior of the retained hyperparameter draws."
            ),
            verbatimTextOutput("convergence_text"),
            br(),
            h4("Single-chain diagnostics"),
            DT::dataTableOutput("single_chain_diag_table"),
            br(),
            plot_info_section(
              id = "convergence_trace",
              label = "Single-chain trace plot",
              what_to_look_for = plot_help_text$convergence_trace,
              plot_tag = plotOutput("convergence_trace_plot", height = "500px")
            ),
            br(),
            plot_info_section(
              id = "convergence_density",
              label = "Single-chain density plot",
              what_to_look_for = plot_help_text$convergence_density,
              plot_tag = plotOutput("convergence_density_plot", height = "500px")
            ),
            br(),
            plot_info_section(
              id = "convergence_geweke",
              label = "Geweke plot",
              what_to_look_for = plot_help_text$geweke,
              plot_tag = plotOutput("convergence_geweke_plot", height = "500px")
            ),
            br(),
            h4("Multi-chain diagnostics"),
            DT::dataTableOutput("multi_chain_diag_table"),
            br(),
            plot_info_section(
              id = "convergence_gelman",
              label = "Gelman plot",
              what_to_look_for = plot_help_text$gelman,
              plot_tag = plotOutput("convergence_gelman_plot", height = "500px")
            )
          ),
          tabPanel(
            "IRF",
            br(),
            uiOutput("irf_plots_ui")
          ),
          tabPanel(
            "FEVD",
            br(),
            uiOutput("fevd_plots_ui")
          ),
          tabPanel(
            "Forecast diagnostics",
            br(),
            div(class = "tab-caption", "Forecast ranges are reported on the original data scale wherever possible, so you can sanity-check the implied paths more directly."),
            DT::dataTableOutput("forecast_diag")
          ),
          tabPanel(
            "Forecast",
            br(),
            div(class = "tab-caption", "Baseline forecasts are shown with recent history and a 68% uncertainty band. These charts are interactive, so you can zoom and pan directly."),
            plot_info_section(
              id = "forecast_main",
              label = "Baseline forecast plot",
              what_to_look_for = plot_help_text$forecast,
              plot_tag = uiOutput("forecast_plot_ui")
            ),
            br(),
            tags$details(
              tags$summary(style = "cursor:pointer; font-weight:600;", "Median-focused detail view"),
              div(class = "tab-caption", "This is a readability aid only; use the chart above for the uncertainty-aware view."),
              plot_info_section(
                id = "forecast_median",
                label = "Median-focused forecast plot",
                what_to_look_for = plot_help_text$forecast_median,
                plot_tag = uiOutput("forecast_median_plot_ui")
              )
            )
          ),
          tabPanel(
            "Scenario forecast",
            br(),
            div(
              class = "scenario-note",
              tags$strong("Scenario workflow"),
              "Choose one variable, generate a suggestion such as plus or minus one sigma or higher or lower volatility, then fine-tune the Scenario Path column manually before updating the scenario forecast. The comparison plot shows recent history plus 68% bands for both the baseline and scenario forecasts."
            ),
            checkboxInput("use_conditional_forecast", "Enable scenario-adjusted forecast", FALSE),
            uiOutput("scenario_var_ui"),
            conditionalPanel(
              condition = "input.use_conditional_forecast",
              selectInput(
                "scenario_template",
                "Scenario template",
                choices = stats::setNames(names(scenario_template_labels), unname(scenario_template_labels)),
                selected = "baseline"
              ),
              numericInput("scenario_sd_multiple", "Standard deviation multiple", value = 1, min = 0.1, step = 0.1),
              numericInput("scenario_vol_multiplier", "Volatility multiplier", value = 1.5, min = 1.05, step = 0.05),
              div(
                style = "display:flex; gap:10px; margin-bottom:10px;",
                actionButton("apply_scenario_template", "Apply suggestion", class = "btn-default"),
                actionButton("reset_scenario_path", "Reset to baseline", class = "btn-default"),
                actionButton("run_scenario_forecast", "Update scenario forecast", class = "btn-primary")
              )
            ),
            uiOutput("scenario_table_ui"),
            verbatimTextOutput("scenario_status"),
            br(),
            plot_info_section(
              id = "scenario_forecast_main",
              label = "Scenario comparison plot",
              what_to_look_for = plot_help_text$scenario_forecast,
              plot_tag = uiOutput("scenario_forecast_plot_ui")
            ),
            br(),
            tags$details(
              tags$summary(style = "cursor:pointer; font-weight:600;", "Median-focused scenario detail view"),
              div(class = "tab-caption", "This helps compare trend shifts when the uncertainty bands are very wide."),
              plot_info_section(
                id = "scenario_forecast_median",
                label = "Median-focused scenario comparison plot",
                what_to_look_for = plot_help_text$scenario_median,
                plot_tag = uiOutput("scenario_forecast_median_plot_ui")
              )
            )
          ),
          tabPanel(
            "Transformed data",
            br(),
            DT::dataTableOutput("transformed_data")
          )
        )
      )
    )
  )
)

################################################################################
# SERVER
################################################################################

server <- function(input, output, session) {
  
  `%||%` <- function(x, y) if (is.null(x)) y else x
  model_run_result <- reactiveVal(NULL)
  scenario_values_rv <- reactiveVal(NULL)
  scenario_forecast_result <- reactiveVal(NULL)
  selected_eda_snapshot <- reactiveVal(NULL)
  
  current_dataset <- reactive({
    req(input$dataset_name)
    all_datasets[[input$dataset_name]]
  })
  
  current_prep <- reactive({
    dat <- current_dataset()
    date_col <- input$date_col %||% if ("quarter" %in% names(dat)) "quarter" else NULL
    prepare_dataset(dat, date_col = date_col)
  })
  
  normalized_date_col <- reactive({
    dat <- current_prep()$data
    if ("quarter" %in% names(dat)) {
      return("quarter")
    }
    requested <- input$date_col %||% NULL
    if (!is.null(requested) && nzchar(requested) && requested %in% names(dat)) {
      return(requested)
    }
    NULL
  })
  
  selected_model_vars <- reactive({
    unique(c(input$climate_vars %||% character(0), input$macro_vars %||% character(0)))
  })
  
  effective_logdiff_vars <- reactive({
    selected_vars <- selected_model_vars()
    current_logdiff <- intersect(input$logdiff_vars %||% character(0), selected_vars)
    must_logdiff <- intersect(default_logdiff_vars, selected_vars)
    unique(c(current_logdiff, must_logdiff))
  })
  
  effective_log_vars <- reactive({
    selected_vars <- selected_model_vars()
    selected_macros <- intersect(input$macro_vars %||% character(0), selected_vars)
    current_log <- setdiff(
      intersect(input$log_vars %||% character(0), selected_vars),
      effective_logdiff_vars()
    )
    
    if (length(current_log)) {
      return(current_log)
    }
    
    setdiff(selected_macros, effective_logdiff_vars())
  })
  
  effective_scale_vars <- reactive({
    selected_vars <- selected_model_vars()
    current_scale <- intersect(input$scale_vars %||% character(0), selected_vars)
    
    if (length(current_scale)) {
      return(current_scale)
    }
    
    intersect(input$climate_vars %||% character(0), selected_vars)
  })
  
  using_default_log_vars <- reactive({
    length(input$log_vars %||% character(0)) == 0 && length(input$macro_vars %||% character(0)) > 0
  })
  
  using_default_scale_vars <- reactive({
    length(input$scale_vars %||% character(0)) == 0 && length(input$climate_vars %||% character(0)) > 0
  })
  
  available_eda_vars <- reactive({
    cols <- current_prep()$all_columns
    date_col <- normalized_date_col()
    setdiff(cols, date_col %||% character(0))
  })
  
  eda_dictionary_table_data <- reactive({
    vars <- available_eda_vars()
    dict_tbl <- lookup_variable_dictionary(vars)
    dict_tbl <- dict_tbl[, c("display_name", "category", "full_variable_name", "description", "variable_name"), drop = FALSE]
    names(dict_tbl) <- c("Variable", "Category", "Dictionary name", "Description", "Raw name")
    dict_tbl
  })
  
  output$date_col_ui <- renderUI({
    dat <- current_dataset()
    date_candidates <- names(dat)
    preferred <- if ("quarter" %in% date_candidates) {
      "quarter"
    } else if ("Date" %in% date_candidates) {
      "Date"
    } else {
      date_candidates[1]
    }
    
    info_select_input(
      id = "date_col",
      label = "Date column",
      choices = named_choices(date_candidates),
      selected = preferred,
      what_it_is = "Choose the column that represents time. This is used to identify the time index and to preserve dates in transformed data and plots."
    )
  })
  
  hyper_mode_use <- reactive({
    switch(
      input$hyper_mode,
      "lambda_alpha" = c("lambda", "alpha"),
      "lambda" = "lambda",
      "full" = "full",
      "auto"
    )
  })
  
  output$prior_guidance_ui <- renderUI({
    note <- prior_recommendation(
      climate_vars = input$climate_vars %||% character(0),
      macro_vars = input$macro_vars %||% character(0),
      logdiff_vars = effective_logdiff_vars()
    )
    
    div(
      class = "soft-note",
      tags$strong(note$headline),
      tags$div(note$body),
      tags$div(style = "margin-top:4px;", note$follow_up)
    )
  })
  
  observeEvent(available_eda_vars(), {
    vars <- available_eda_vars()
    current_var <- input$eda_any_var %||% character(0)
    selected_var <- if (length(current_var) && current_var %in% vars) {
      current_var
    } else if (length(vars)) {
      vars[1]
    } else {
      character(0)
    }
    
    freezeReactiveValue(input, "eda_any_var")
    updateSelectizeInput(
      session,
      "eda_any_var",
      choices = named_choices(vars),
      selected = selected_var,
      server = TRUE
    )
  }, ignoreInit = FALSE)
  
  observeEvent(list(input$dataset_name, input$date_col, input$climate_vars, input$macro_vars), {
    selected_eda_snapshot(NULL)
  }, ignoreInit = FALSE)
  
  output$eda_selected_vars_ui <- renderUI({
    vars <- selected_model_vars()
    selectizeInput(
      "eda_selected_vars",
      "Selected variables to plot",
      choices = named_choices(vars),
      selected = vars,
      multiple = TRUE,
      options = list(placeholder = "Choose from the current model selection")
    )
  })
  
  ##############################################################################
  # 1) Update climate/macro choices ONLY when dataset changes
  ##############################################################################
  observeEvent(input$dataset_name, {
    cols <- current_prep()$all_columns
    climate_choices <- intersect(allowed_climate_vars, cols)
    macro_choices   <- intersect(allowed_macro_vars, cols)
    
    freezeReactiveValue(input, "climate_vars")
    freezeReactiveValue(input, "macro_vars")
    freezeReactiveValue(input, "response_vars")
    freezeReactiveValue(input, "impulse_vars")
    freezeReactiveValue(input, "irf_plot_impulses")
    freezeReactiveValue(input, "log_vars")
    freezeReactiveValue(input, "scale_vars")
    freezeReactiveValue(input, "logdiff_vars")
    
    updateSelectizeInput(
      session, "logdiff_vars",
      choices = character(0),
      selected = character(0),
      server = TRUE
    )
    
    updateSelectizeInput(
      session, "climate_vars",
      choices = named_choices(climate_choices),
      selected = character(0),
      server = TRUE
    )
    
    updateSelectizeInput(
      session, "macro_vars",
      choices = named_choices(macro_choices),
      selected = character(0),
      server = TRUE
    )
    
    updateSelectizeInput(
      session, "response_vars",
      choices = character(0),
      selected = character(0),
      server = TRUE
    )
    
    updateSelectizeInput(
      session, "impulse_vars",
      choices = character(0),
      selected = character(0),
      server = TRUE
    )
    
    updateSelectizeInput(
      session, "irf_plot_impulses",
      choices = character(0),
      selected = character(0),
      server = TRUE
    )
    
    updateSelectizeInput(
      session, "log_vars",
      choices = character(0),
      selected = character(0),
      server = TRUE
    )
    
    updateSelectizeInput(
      session, "scale_vars",
      choices = character(0),
      selected = character(0),
      server = TRUE
    )
    
    scenario_values_rv(NULL)
    scenario_forecast_result(NULL)
  }, ignoreInit = FALSE)
  
  ##############################################################################
  # 2) Update dependent inputs when climate/macro selections change
  ##############################################################################
  observeEvent(
    list(input$climate_vars, input$macro_vars),
    {
      cols <- current_prep()$all_columns
      selected_model_vars <- unique(c(input$climate_vars, input$macro_vars))
      selected_model_vars <- selected_model_vars[selected_model_vars %in% cols]
      
      current_response <- input$response_vars %||% character(0)
      current_log <- input$log_vars %||% character(0)
      current_logdiff <- input$logdiff_vars %||% character(0)
      current_scale <- input$scale_vars %||% character(0)
      current_impulse <- input$impulse_vars %||% character(0)
      
      must_logdiff <- intersect(default_logdiff_vars, selected_model_vars)
      
      valid_response <- intersect(current_response, selected_model_vars)
      valid_log <- setdiff(
        intersect(current_log, selected_model_vars),
        must_logdiff
      )
      valid_logdiff <- unique(c(
        intersect(current_logdiff, selected_model_vars),
        must_logdiff
      ))
      valid_scale <- intersect(current_scale, selected_model_vars)
      
      if (length(input$macro_vars) > 0) {
        valid_response <- unique(input$macro_vars[input$macro_vars %in% selected_model_vars])
      }
      
      valid_impulse <- intersect(current_impulse, selected_model_vars)
      if (length(valid_impulse) == 0 && length(input$climate_vars) > 0) {
        valid_impulse <- intersect(input$climate_vars, selected_model_vars)
      }
      
      freezeReactiveValue(input, "response_vars")
      freezeReactiveValue(input, "impulse_vars")
      freezeReactiveValue(input, "log_vars")
      freezeReactiveValue(input, "scale_vars")
      freezeReactiveValue(input, "logdiff_vars")
      
      updateSelectizeInput(
        session, "response_vars",
        choices = named_choices(selected_model_vars),
        selected = valid_response,
        server = TRUE
      )
      
      updateSelectizeInput(
        session, "impulse_vars",
        choices = named_choices(selected_model_vars),
        selected = valid_impulse,
        server = TRUE
      )
      
      updateSelectizeInput(
        session, "log_vars",
        choices = named_choices(selected_model_vars),
        selected = valid_log,
        server = TRUE
      )
      
      updateSelectizeInput(
        session, "logdiff_vars",
        choices = named_choices(selected_model_vars),
        selected = valid_logdiff,
        server = TRUE
      )
      
      updateSelectizeInput(
        session, "scale_vars",
        choices = named_choices(selected_model_vars),
        selected = valid_scale,
        server = TRUE
      )
      
      scenario_values_rv(NULL)
      scenario_forecast_result(NULL)
    },
    ignoreInit = FALSE
  )
  
  ##############################################################################
  # 3) Update IRF plots-to-display choices when impulse selection changes
  ##############################################################################
  observeEvent(input$impulse_vars, {
    current_irf_plot_impulses <- input$irf_plot_impulses %||% character(0)
    valid_impulse_choices <- input$impulse_vars %||% character(0)
    
    valid_irf_plot_impulses <- intersect(current_irf_plot_impulses, valid_impulse_choices)
    if (length(valid_irf_plot_impulses) == 0) {
      valid_irf_plot_impulses <- valid_impulse_choices
    }
    
    freezeReactiveValue(input, "irf_plot_impulses")
    
    updateSelectizeInput(
      session, "irf_plot_impulses",
      choices = named_choices(valid_impulse_choices),
      selected = valid_irf_plot_impulses,
      server = TRUE
    )
  }, ignoreInit = FALSE)
  
  
  ##############################################################################
  # Reset button
  ##############################################################################
  observeEvent(input$reset_defaults, {
    freezeReactiveValue(input, "climate_vars")
    freezeReactiveValue(input, "macro_vars")
    freezeReactiveValue(input, "response_vars")
    freezeReactiveValue(input, "impulse_vars")
    freezeReactiveValue(input, "irf_plot_impulses")
    freezeReactiveValue(input, "log_vars")
    freezeReactiveValue(input, "logdiff_vars")
    freezeReactiveValue(input, "scale_vars")
    
    updateSelectizeInput(session, "climate_vars", selected = character(0))
    updateSelectizeInput(session, "macro_vars", selected = character(0))
    updateSelectizeInput(session, "response_vars", selected = character(0))
    updateSelectizeInput(session, "impulse_vars", selected = character(0))
    updateSelectizeInput(session, "irf_plot_impulses", selected = character(0), server = TRUE)
    updateSelectizeInput(session, "log_vars", selected = character(0))
    updateSelectizeInput(session, "logdiff_vars", selected = character(0))
    updateSelectizeInput(session, "scale_vars", selected = character(0))
    
    updateNumericInput(session, "lags", value = 5)
    updateCheckboxInput(session, "do_irf", value = TRUE)
    updateNumericInput(session, "irf_horizon", value = 12)
    updateCheckboxInput(session, "identification", value = TRUE)
    updateCheckboxInput(session, "do_forecast", value = TRUE)
    updateNumericInput(session, "forecast_horizon", value = 8)
    updateNumericInput(session, "n_draw", value = 15000)
    updateNumericInput(session, "n_burn", value = 5000)
    updateNumericInput(session, "n_thin", value = 1)
    updateCheckboxInput(session, "run_multichain_diag", value = FALSE)
    updateNumericInput(session, "n_chains", value = 4)
    updateCheckboxInput(session, "parallel_chains", value = TRUE)
    updateNumericInput(session, "seed_base", value = 1234)
    updateSelectInput(session, "prior_spec", selected = "minnesota")
    updateSelectInput(session, "hyper_mode", selected = "auto")
    updateNumericInput(session, "alpha_sd", value = 0.25)
    updateNumericInput(session, "alpha_min", value = 1)
    updateNumericInput(session, "alpha_max", value = 3)
    updateNumericInput(session, "soc_mode", value = 1)
    updateNumericInput(session, "soc_sd", value = 1)
    updateNumericInput(session, "sur_mode", value = 1)
    updateNumericInput(session, "sur_sd", value = 1)
    updateCheckboxInput(session, "use_conditional_forecast", value = FALSE)
    scenario_values_rv(NULL)
    scenario_forecast_result(NULL)
    updateNumericInput(session, "lambda_mode", value = 0.2)
    updateNumericInput(session, "lambda_sd", value = 0.4)
    updateNumericInput(session, "lambda_min", value = 1e-4)
    updateNumericInput(session, "lambda_max", value = 5)
    updateSelectInput(session, "scenario_template", selected = "baseline")
    updateNumericInput(session, "scenario_sd_multiple", value = 1)
    updateNumericInput(session, "scenario_vol_multiplier", value = 1.5)
    updateNumericInput(session, "alpha_mode", value = 2)
    updateNumericInput(session, "intercept_var", value = 1e7)
    updateNumericInput(session, "scale_hess", value = 0.05)
    updateCheckboxInput(session, "adjust_acc", value = TRUE)
    updateNumericInput(session, "acc_lower", value = 0.25)
    updateNumericInput(session, "acc_upper", value = 0.45)
  })
  
  ##############################################################################
  # Exploratory data analysis
  ##############################################################################
  observeEvent(input$eda_dictionary_table_rows_selected, {
    row_id <- input$eda_dictionary_table_rows_selected
    dict_tbl <- eda_dictionary_table_data()
    req(length(row_id) == 1, nrow(dict_tbl) >= row_id)
    
    updateSelectizeInput(
      session,
      "eda_any_var",
      selected = dict_tbl$`Raw name`[row_id]
    )
  }, ignoreInit = TRUE)
  
  observeEvent(input$run_selected_eda, {
    selected_eda_snapshot(list(
      vars = input$eda_selected_vars %||% character(0),
      date_col = normalized_date_col()
    ))
  }, ignoreInit = TRUE)
  
  ##############################################################################
  # Validation
  ##############################################################################
  current_validation <- reactive({
    dat <- current_prep()$data
    
    response_vars_use <- input$response_vars %||% character(0)
    if (!length(response_vars_use)) {
      response_vars_use <- input$macro_vars %||% character(0)
    }
    
    impulse_vars_use <- input$impulse_vars %||% character(0)
    if (!length(impulse_vars_use)) {
      impulse_vars_use <- input$climate_vars %||% character(0)
    }
    
    validate_bvar_inputs(
      dat = dat,
      climate_vars = input$climate_vars %||% character(0),
      macro_vars = input$macro_vars %||% character(0),
      impulse_vars = impulse_vars_use,
      response_vars = response_vars_use,
      log_vars = effective_log_vars(),
      logdiff_vars = effective_logdiff_vars(),
      scale_vars = effective_scale_vars(),
      lags = input$lags,
      date_col = normalized_date_col()
    )
  })
  output$model_size_warning <- renderUI({
    v <- current_validation()
    
    if (v$k == 0 || is.na(v$k) || v$n_complete == 0 || is.na(v$n_complete)) {
      return(NULL)
    }
    
    ratio <- v$n_complete / max(1, v$k * v$lags)
    
    msg <- paste0(
      "Usable observations after NA filtering: ", v$n_complete,
      " | Variables (K): ", v$k,
      " | Lags: ", v$lags,
      " | T / (K x lags) = ", round(ratio, 2)
    )
    
    if (v$n_complete < (v$lags + 5)) {
      div(
        class = "validation-bad-box",
        paste0(msg, " - Not enough usable observations for this lag length.")
      )
    } else if (ratio < 3) {
      div(
        class = "validation-warn-box",
        paste0(msg, " - This is quite tight. Consider fewer variables or fewer lags.")
      )
    } else if (ratio < 5) {
      div(
        class = "validation-warn-box",
        paste0(msg, " - This is usable, but somewhat aggressive. Interpret results with care.")
      )
    } else {
      div(
        class = "validation-ok-box",
        paste0(msg, " - This looks reasonably sized for estimation.")
      )
    }
  })
  
  output$validation_text <- renderText({
    v <- current_validation()
    active_default_logdiff <- intersect(default_logdiff_vars, effective_logdiff_vars())
    
    lines <- c(
      paste("Dataset:", display_label(input$dataset_name)),
      paste("Date column:", display_label(input$date_col)),
      paste("Climate variables:", pretty_var_text(input$climate_vars %||% character(0))),
      paste("Macro variables:", pretty_var_text(input$macro_vars %||% character(0))),
      paste("Log variables (effective):", pretty_var_text(effective_log_vars())),
      paste("Log-difference variables (effective):", pretty_var_text(effective_logdiff_vars())),
      paste("Scaled variables (effective):", pretty_var_text(effective_scale_vars())),
      paste("Log-transform source:", if (using_default_log_vars()) "Default: selected macro variables not already log-differenced" else "User selection"),
      paste("Scaling source:", if (using_default_scale_vars()) "Default: selected climate variables" else "User selection"),
      paste("Automatic default log-differences currently active:", pretty_var_text(active_default_logdiff)),
      paste("Rows in dataset:", v$n_total),
      paste("Selected model variables (K):", v$k),
      paste("Complete observations after NA filtering:", v$n_complete),
      paste("Lag length:", v$lags),
      ""
    )
    
    if (length(v$messages)) {
      lines <- c(lines, "Errors:", paste("-", v$messages), "")
    } else {
      lines <- c(lines, "Errors:", "- None", "")
    }
    
    if (length(v$warnings)) {
      lines <- c(lines, "Warnings:", paste("-", v$warnings))
    } else {
      lines <- c(lines, "Warnings:", "- None")
    }
    
    paste(lines, collapse = "\n")
  })
  
  output$transform_plan_table <- DT::renderDataTable({
    build_transformation_summary(
      selected_vars = selected_model_vars(),
      climate_vars = input$climate_vars %||% character(0),
      macro_vars = input$macro_vars %||% character(0),
      log_vars = effective_log_vars(),
      logdiff_vars = effective_logdiff_vars(),
      scale_vars = effective_scale_vars()
    )
  }, options = list(pageLength = 8, scrollX = TRUE, searching = FALSE))
  
  output$dataset_info <- DT::renderDataTable({
    info_tbl <- current_prep()$info
    info_tbl$display_name <- display_label(info_tbl$column)
    info_tbl <- info_tbl[, c("display_name", "column", "class", "n_missing")]
    names(info_tbl) <- c("Display name", "Raw name", "Class", "Missing values")
    info_tbl
  }, options = list(pageLength = 15, scrollX = TRUE))
  
  output$eda_dictionary_table <- DT::renderDataTable({
    eda_dictionary_table_data()
  }, options = list(pageLength = 12, scrollX = TRUE), selection = "single", rownames = FALSE)
  
  output$eda_any_var_info <- renderUI({
    var_name <- input$eda_any_var %||% ""
    if (!nzchar(var_name)) {
      return(div(class = "soft-note", "Choose a variable to see its definition and current model role."))
    }
    
    dict_row <- lookup_variable_dictionary(var_name)
    role_label <- variable_role_label(
      var_name,
      climate_vars = input$climate_vars %||% character(0),
      macro_vars = input$macro_vars %||% character(0)
    )
    transform_label <- variable_transformation_label(
      var_name,
      log_vars = effective_log_vars(),
      logdiff_vars = effective_logdiff_vars(),
      scale_vars = effective_scale_vars()
    )
    
    div(
      class = "soft-note",
      tags$strong(dict_row$display_name[1]),
      tags$div(style = "margin-top:6px;", tags$strong("Category: "), dict_row$category[1]),
      tags$div(tags$strong("Dictionary name: "), dict_row$full_variable_name[1]),
      tags$div(tags$strong("Current model role: "), role_label),
      tags$div(tags$strong("Current transformation: "), transform_label),
      tags$div(style = "margin-top:6px;", dict_row$description[1])
    )
  })
  
  output$eda_any_var_summary <- DT::renderDataTable({
    dat <- current_prep()$data
    single_variable_summary_table(
      dat = dat,
      var_name = input$eda_any_var %||% "",
      date_col = normalized_date_col()
    )
  }, options = list(dom = "t", paging = FALSE, ordering = FALSE, info = FALSE, searching = FALSE, scrollX = TRUE), rownames = FALSE)
  
  output$eda_any_var_plot <- plotly::renderPlotly({
    dat <- current_prep()$data
    plot_df <- single_variable_plot_frame(
      dat = dat,
      var_name = input$eda_any_var %||% "",
      date_col = normalized_date_col()
    )
    
    req(!is.null(plot_df), nrow(plot_df) > 0)
    
    p <- suppressWarnings(
      ggplot2::ggplot(
        plot_df,
        ggplot2::aes(
          x = index,
          y = value,
          group = 1,
          text = paste0(variable, "<br>Period: ", index_label, "<br>Value: ", format_numeric_value(value))
        )
      ) +
        ggplot2::geom_line(color = "#2f6fb0", linewidth = 0.9) +
        ggplot2::geom_point(color = "#2f6fb0", size = 1.4) +
        ggplot2::labs(
          title = paste(display_label(input$eda_any_var %||% ""), "over time"),
          x = if (inherits(plot_df$index, "Date")) "Quarter" else "Observation",
          y = display_label(input$eda_any_var %||% "")
        ) +
        theme_bvar_plot()
    )
    
    as_interactive_plot(p)
  })
  
  output$eda_any_var_hist <- plotly::renderPlotly({
    dat <- current_prep()$data
    plot_df <- single_variable_plot_frame(
      dat = dat,
      var_name = input$eda_any_var %||% "",
      date_col = normalized_date_col()
    )
    
    req(!is.null(plot_df), nrow(plot_df) > 0)
    
    p <- suppressWarnings(
      ggplot2::ggplot(
        plot_df,
        ggplot2::aes(
          x = value,
          text = paste0(display_label(input$eda_any_var %||% ""), "<br>Value: ", format_numeric_value(value))
        )
      ) +
        ggplot2::geom_histogram(fill = "#9ec5ea", color = "white", bins = 20) +
        ggplot2::labs(
          title = paste(display_label(input$eda_any_var %||% ""), "distribution"),
          x = display_label(input$eda_any_var %||% ""),
          y = "Count"
        ) +
        theme_bvar_plot()
    )
    
    as_interactive_plot(p)
  })
  
  output$eda_transform_table <- DT::renderDataTable({
    snap <- selected_eda_snapshot()
    vars <- if (is.null(snap)) character(0) else (snap$vars %||% character(0))
    build_transformation_summary(
      selected_vars = vars,
      climate_vars = input$climate_vars %||% character(0),
      macro_vars = input$macro_vars %||% character(0),
      log_vars = effective_log_vars(),
      logdiff_vars = effective_logdiff_vars(),
      scale_vars = effective_scale_vars()
    )
  }, options = list(pageLength = 8, scrollX = TRUE, searching = FALSE), rownames = FALSE)
  
  output$eda_selected_overview <- DT::renderDataTable({
    snap <- selected_eda_snapshot()
    if (is.null(snap)) {
      return(empty_message_table("Click Update selected variable plots to load the selected-variable overview."))
    }
    
    selected_variable_overview_table(
      dat = current_prep()$data,
      vars = snap$vars %||% character(0),
      climate_vars = input$climate_vars %||% character(0),
      macro_vars = input$macro_vars %||% character(0),
      log_vars = effective_log_vars(),
      logdiff_vars = effective_logdiff_vars(),
      scale_vars = effective_scale_vars(),
      date_col = snap$date_col %||% if ("quarter" %in% names(current_prep()$data)) "quarter" else NULL
    )
  }, options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
  
  output$eda_selected_plot_ui <- renderUI({
    snap <- selected_eda_snapshot()
    n_vars <- if (is.null(snap)) 1L else length(snap$vars %||% character(0))
    plotly::plotlyOutput(
      "eda_selected_plot",
      height = paste0(max(360, 230 * max(1, n_vars)), "px")
    )
  })
  
  output$eda_selected_plot <- plotly::renderPlotly({
    snap <- selected_eda_snapshot()
    if (is.null(snap)) {
      return(as_interactive_plot(
        ggplot2::ggplot() +
          ggplot2::annotate("text", x = 1, y = 1, label = "Click Update selected variable plots to draw this chart.") +
          ggplot2::theme_void()
      ))
    }
    
    plot_df <- selected_variable_plot_frame(
      dat = current_prep()$data,
      vars = snap$vars %||% character(0),
      date_col = snap$date_col %||% if ("quarter" %in% names(current_prep()$data)) "quarter" else NULL
    )
    req(!is.null(plot_df), nrow(plot_df) > 0)
    
    plot_df$text <- paste0(
      plot_df$variable,
      "<br>Period: ",
      plot_df$index_label,
      "<br>Value: ",
      vapply(plot_df$value, format_numeric_value, character(1))
    )
    
    p <- suppressWarnings(
      ggplot2::ggplot(
        plot_df,
        ggplot2::aes(x = index, y = value, group = variable, text = text)
      ) +
        ggplot2::geom_line(color = "#2f6fb0", linewidth = 0.9) +
        ggplot2::geom_point(color = "#2f6fb0", size = 1.35) +
        ggplot2::facet_wrap(~variable, ncol = 1, scales = "free_y") +
        ggplot2::labs(
          title = "Selected model variables over time",
          x = if (inherits(plot_df$index, "Date")) "Quarter" else "Observation",
          y = NULL
        ) +
        theme_bvar_plot()
    )
    
    as_interactive_plot(p)
  })
  
  output$eda_selected_correlation <- DT::renderDataTable({
    snap <- selected_eda_snapshot()
    if (is.null(snap)) {
      return(empty_message_table("Click Update selected variable plots to compute the selected-variable correlations."))
    }
    selected_variable_correlation_table(
      dat = current_prep()$data,
      vars = snap$vars %||% character(0)
    )
  }, options = list(pageLength = 8, scrollX = TRUE, ordering = FALSE), rownames = FALSE)
  
  ##############################################################################
  # Run model only on button click
  ##############################################################################
  observeEvent(input$run_model, {
    v <- current_validation()
    if (!v$ok) {
      scenario_values_rv(NULL)
      scenario_forecast_result(NULL)
      model_run_result(list(
        ok = FALSE,
        result = NULL,
        error = paste(v$messages, collapse = "\n"),
        elapsed_sec = NA
      ))
      return()
    }
    
    dat <- current_prep()$data
    t0 <- Sys.time()
    scenario_forecast_result(NULL)
    estimate_detail <- if (isTRUE(input$run_multichain_diag)) {
      "Estimating BVAR and convergence diagnostics (this may take a while)"
    } else {
      "Estimating BVAR (this may take a while)"
    }
    post_detail <- if (isTRUE(input$run_multichain_diag)) {
      "Building IRFs / forecasts and finalising diagnostics"
    } else {
      "Building IRFs / forecasts"
    }
    
    res <- withProgress(message = "Running BVAR model", value = 0, {
      incProgress(0.10, detail = "Validating inputs")
      Sys.sleep(0.05)
      
      incProgress(0.15, detail = "Preparing data")
      Sys.sleep(0.05)
      
      incProgress(0.15, detail = "Setting priors and sampler")
      Sys.sleep(0.05)
      
      incProgress(0.45, detail = estimate_detail)
      
      out <- safe_run_bvar(
        dat = dat,
        climate_vars = input$climate_vars,
        macro_vars = input$macro_vars,
        date_col = normalized_date_col(),
        log_vars = effective_log_vars(),
        logdiff_vars = effective_logdiff_vars(),
        scale_vars = effective_scale_vars(),
        lags = input$lags,
        n_draw = input$n_draw,
        n_burn = input$n_burn,
        n_thin = input$n_thin,
        prior_spec = input$prior_spec,
        hyper_mode = hyper_mode_use(),
        lambda_mode = input$lambda_mode,
        lambda_sd = input$lambda_sd,
        lambda_min = input$lambda_min,
        lambda_max = input$lambda_max,
        alpha_mode = input$alpha_mode,
        alpha_sd = input$alpha_sd,
        alpha_min = input$alpha_min,
        alpha_max = input$alpha_max,
        soc_mode = input$soc_mode,
        soc_sd = input$soc_sd,
        sur_mode = input$sur_mode,
        sur_sd = input$sur_sd,
        intercept_var = input$intercept_var,
        scale_hess = input$scale_hess,
        adjust_acc = input$adjust_acc,
        acc_lower = input$acc_lower,
        acc_upper = input$acc_upper,
        do_irf = input$do_irf,
        irf_horizon = input$irf_horizon,
        identification = input$identification,
        impulse_vars = if (length(input$impulse_vars)) input$impulse_vars else NULL,
        response_vars = if (length(input$response_vars)) input$response_vars else input$macro_vars,
        do_forecast = input$do_forecast,
        forecast_horizon = input$forecast_horizon,
        conditional_forecast = FALSE,
        scenario_var = NULL,
        scenario_values = NULL,
        run_multichain_diag = input$run_multichain_diag,
        n_chains = input$n_chains,
        parallel_chains = input$parallel_chains,
        seed_base = input$seed_base,
        verbose = FALSE
      )
      
      incProgress(0.10, detail = post_detail)
      Sys.sleep(0.05)
      
      incProgress(0.05, detail = "Finishing")
      Sys.sleep(0.05)
      
      out
    })
    
    res$elapsed_sec <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    model_run_result(res)
  }, ignoreInit = TRUE)
  
  output$run_status <- renderText({
    req(input$run_model > 0)
    res <- model_run_result()
    req(!is.null(res))
    
    elapsed_txt <- if (!is.null(res$elapsed_sec) && is.finite(res$elapsed_sec)) {
      paste0(round(res$elapsed_sec, 2), " seconds")
    } else {
      "N/A"
    }
    
    if (!res$ok) {
      paste(
        "Model run failed:\n",
        res$error, "\n\n",
        "Elapsed time: ", elapsed_txt
      )
    } else {
      paste(
        "Model run completed successfully.\n",
        "Dataset:", display_label(input$dataset_name), "\n",
        "Climate vars:", pretty_var_text(input$climate_vars %||% character(0)), "\n",
        "Macro vars:", pretty_var_text(input$macro_vars %||% character(0)), "\n",
        "Log vars (effective):", pretty_var_text(effective_log_vars()), "\n",
        "Log-difference vars (effective):", pretty_var_text(effective_logdiff_vars()), "\n",
        "Scaled vars (effective):", pretty_var_text(effective_scale_vars()), "\n",
        "Impulses:", pretty_var_text(res$result$meta$impulse_vars_fit), "\n",
        "Responses:", pretty_var_text(res$result$meta$response_vars_fit), "\n",
        "Elapsed time:", elapsed_txt
      )
    }
  })
  
  output$model_summary <- renderText({
    req(input$run_model > 0)
    res <- model_run_result()
    req(!is.null(res))
    if (!res$ok) return("No summary available because the model did not run successfully.")
    paste(res$result$summary_text, collapse = "\n")
  })
  
  output$model_complexity_table <- DT::renderDataTable({
    req(input$run_model > 0)
    res <- model_run_result()
    req(!is.null(res))
    
    complexity <- compute_model_complexity_summary(if (isTRUE(res$ok)) res$result else NULL)
    complexity$table
  }, options = list(dom = "t", paging = FALSE, ordering = FALSE, info = FALSE, searching = FALSE, scrollX = TRUE), rownames = FALSE)
  
  output$model_complexity_note <- renderText({
    req(input$run_model > 0)
    res <- model_run_result()
    req(!is.null(res), res$ok)
    
    complexity <- compute_model_complexity_summary(res$result)
    complexity$note %||% ""
  })
  
  output$hyperparameter_table <- DT::renderDataTable({
    req(input$run_model > 0)
    res <- model_run_result()
    req(!is.null(res), res$ok)
    tbl <- res$result$hyper_summary
    names(tbl) <- c("Parameter", "Mean", "Median", "SD", "Q05", "Q95")
    tbl
  }, options = list(pageLength = 10, scrollX = TRUE))
  
  output$optimizer_table <- DT::renderDataTable({
    req(input$run_model > 0)
    res <- model_run_result()
    req(!is.null(res), res$ok)
    tbl <- res$result$optim_summary
    names(tbl) <- c("Item", "Value")
    tbl
  }, options = list(pageLength = 10, scrollX = TRUE))
  
  output$psi_table <- DT::renderDataTable({
    req(input$run_model > 0)
    res <- model_run_result()
    req(!is.null(res), res$ok)
    tbl <- res$result$psi_summary
    req(!is.null(tbl), nrow(tbl) > 0)
    tbl$variable <- display_label(tbl$variable)
    num_cols <- vapply(tbl, is.numeric, logical(1))
    tbl[num_cols] <- lapply(tbl[num_cols], function(x) signif(x, 4))
    names(tbl) <- c(
      "Variable", "Psi source", "Mode", "Min", "Max",
      "AR(p) scale", "ARIMA(p,1,0) scale", "AR OLS scale", "Diff SD", "Series SD"
    )
    tbl
  }, options = list(pageLength = 10, scrollX = TRUE))
  
  output$hyperparameter_text <- renderText({
    req(input$run_model > 0)
    res <- model_run_result()
    req(!is.null(res), res$ok)
    hyperparameter_insight_text(res$result)
  })
  
  output$hyper_trace_plot <- renderPlot({
    req(input$run_model > 0)
    res <- model_run_result()
    req(!is.null(res), res$ok)
    
    tryCatch(
      plot(res$result$fit, type = "trace"),
      error = function(e) {
        plot.new()
        text(0.5, 0.5, paste("Hyperparameter trace plot unavailable:", conditionMessage(e)), cex = 0.95)
      }
    )
  })
  
  output$hyper_density_plot <- renderPlot({
    req(input$run_model > 0)
    res <- model_run_result()
    req(!is.null(res), res$ok)
    
    tryCatch(
      plot(res$result$fit, type = "density"),
      error = function(e) {
        plot.new()
        text(0.5, 0.5, paste("Hyperparameter density plot unavailable:", conditionMessage(e)), cex = 0.95)
      }
    )
  })
  
  output$convergence_text <- renderText({
    req(input$run_model > 0)
    res <- model_run_result()
    req(!is.null(res), res$ok)
    
    conv <- res$result$convergence
    conv$summary_text %||% convergence_interpretation_text(conv, fit = res$result$fit)
  })
  
  output$single_chain_diag_table <- DT::renderDataTable({
    req(input$run_model > 0)
    res <- model_run_result()
    req(!is.null(res), res$ok)
    
    single <- res$result$convergence$single
    if (!isTRUE(single$ok) || is.null(single$table) || !nrow(single$table)) {
      tbl <- empty_message_table(single$error %||% "Single-chain diagnostics are unavailable.")
      names(tbl) <- "Message"
      return(tbl)
    }
    
    tbl <- single$table
    num_cols <- vapply(tbl, is.numeric, logical(1))
    tbl[num_cols] <- lapply(tbl[num_cols], function(x) signif(x, 4))
    names(tbl) <- c(
      "Parameter", "ESS", "Geweke Z", "Geweke abs(Z)", "Status",
      "Heidel Stationarity", "Heidel Halfwidth", "Heidel Status"
    )
    tbl
  }, options = list(pageLength = 10, scrollX = TRUE))
  
  output$convergence_trace_plot <- renderPlot({
    req(input$run_model > 0)
    res <- model_run_result()
    req(!is.null(res), res$ok)
    
    single <- res$result$convergence$single
    validate(
      need(isTRUE(single$ok), single$error %||% "Single-chain diagnostics are unavailable."),
      need(!is.null(single$mcmc), "No retained MCMC hyperparameter draws were available for the main chain.")
    )
    
    tryCatch(
      coda::traceplot(single$mcmc),
      error = function(e) {
        plot.new()
        text(0.5, 0.5, paste("Single-chain trace plot unavailable:", conditionMessage(e)), cex = 0.95)
      }
    )
  })
  
  output$convergence_density_plot <- renderPlot({
    req(input$run_model > 0)
    res <- model_run_result()
    req(!is.null(res), res$ok)
    
    single <- res$result$convergence$single
    validate(
      need(isTRUE(single$ok), single$error %||% "Single-chain diagnostics are unavailable."),
      need(!is.null(single$mcmc), "No retained MCMC hyperparameter draws were available for the main chain.")
    )
    
    tryCatch(
      coda::densplot(single$mcmc, show.obs = FALSE),
      error = function(e) {
        plot.new()
        text(0.5, 0.5, paste("Single-chain density plot unavailable:", conditionMessage(e)), cex = 0.95)
      }
    )
  })
  
  output$convergence_geweke_plot <- renderPlot({
    req(input$run_model > 0)
    res <- model_run_result()
    req(!is.null(res), res$ok)
    
    single <- res$result$convergence$single
    validate(
      need(isTRUE(single$ok), single$error %||% "Single-chain diagnostics are unavailable."),
      need(!is.null(single$mcmc), "No retained MCMC hyperparameter draws were available for the main chain.")
    )
    
    tryCatch(
      coda::geweke.plot(single$mcmc, ask = FALSE),
      error = function(e) {
        plot.new()
        text(0.5, 0.5, paste("Geweke plot unavailable:", conditionMessage(e)), cex = 0.95)
      }
    )
  })
  
  output$multi_chain_diag_table <- DT::renderDataTable({
    req(input$run_model > 0)
    res <- model_run_result()
    req(!is.null(res), res$ok)
    
    multi <- res$result$convergence$multi
    if (!isTRUE(multi$requested)) {
      tbl <- empty_message_table("Multi-chain diagnostics were not run. Enable the checkbox in Advanced settings to compute Gelman-Rubin diagnostics.")
      names(tbl) <- "Message"
      return(tbl)
    }
    
    if (!isTRUE(multi$ok) || is.null(multi$table) || !nrow(multi$table)) {
      msg <- multi$error %||% "Multi-chain diagnostics are unavailable."
      if (!is.null(multi$notes) && length(multi$notes)) {
        msg <- paste(c(msg, unique(multi$notes)), collapse = "\n")
      }
      tbl <- empty_message_table(msg)
      names(tbl) <- "Message"
      return(tbl)
    }
    
    tbl <- multi$table
    num_cols <- vapply(tbl, is.numeric, logical(1))
    tbl[num_cols] <- lapply(tbl[num_cols], function(x) signif(x, 4))
    names(tbl) <- c("Parameter", "PSRF point estimate", "PSRF upper CI", "Status")
    tbl
  }, options = list(pageLength = 10, scrollX = TRUE))
  
  output$convergence_gelman_plot <- renderPlot({
    req(input$run_model > 0)
    res <- model_run_result()
    req(!is.null(res), res$ok)
    
    multi <- res$result$convergence$multi
    validate(
      need(isTRUE(multi$requested), "Multi-chain diagnostics were not run."),
      need(isTRUE(multi$ok), multi$error %||% "Multi-chain diagnostics are unavailable."),
      need(!is.null(multi$mcmc_list), "No multi-chain MCMC object was available for Gelman-Rubin plotting.")
    )
    
    tryCatch(
      coda::gelman.plot(multi$mcmc_list, autoburnin = FALSE, ask = FALSE),
      error = function(e) {
        plot.new()
        text(0.5, 0.5, paste("Gelman plot unavailable:", conditionMessage(e)), cex = 0.95)
      }
    )
  })
  
  output$irf_plots_ui <- renderUI({
    req(input$run_model > 0)
    req(input$do_irf)
    
    res <- model_run_result()
    req(!is.null(res))
    req(res$ok)
    req(!is.null(res$result$irf))
    
    available_impulses <- res$result$meta$impulse_vars_fit
    
    impulses_to_show <- input$irf_plot_impulses %||% character(0)
    if (!length(impulses_to_show)) {
      impulses_to_show <- available_impulses
    }
    
    impulses_to_show <- intersect(impulses_to_show, available_impulses)
    req(length(impulses_to_show) > 0)
    
    plot_output_list <- lapply(seq_along(impulses_to_show), function(i) {
      impulse_name <- impulses_to_show[i]
      plotname <- paste0("irf_plot_", i)
      
      tagList(
        plot_info_header(
          id = paste0(plotname, "_header"),
          label = paste("Shock:", display_label(impulse_name)),
          what_to_look_for = plot_help_text$irf
        ),
        plotOutput(plotname, height = "500px"),
        tags$hr()
      )
    })
    
    do.call(tagList, plot_output_list)
  })
  
  output$fevd_plots_ui <- renderUI({
    req(input$run_model > 0)
    req(input$do_irf)
    
    res <- model_run_result()
    req(!is.null(res))
    req(res$ok)
    req(!is.null(res$result$fevd))
    
    fitted_vars <- res$result$meta$fitted_vars
    
    # Show one stacked bar chart per RESPONSE variable
    responses_to_show <- res$result$meta$response_vars_fit
    req(length(responses_to_show) > 0)
    
    plot_output_list <- lapply(seq_along(responses_to_show), function(i) {
      response_name <- responses_to_show[i]
      plotname <- paste0("fevd_plot_", i)
      
      tagList(
        plot_info_header(
          id = paste0(plotname, "_header"),
          label = paste("FEVD:", display_label(response_name)),
          what_to_look_for = plot_help_text$fevd
        ),
        tags$p(
          class = "tab-caption",
          "Stacked bars show the fraction of forecast error variance explained by each structural shock at each horizon. Bars sum to 1.0."
        ),
        plotOutput(plotname, height = "450px"),
        tags$hr()
      )
    })
    
    do.call(tagList, plot_output_list)
  })
  
  observe({
    req(input$run_model > 0)
    req(input$do_irf)
    
    res <- model_run_result()
    req(!is.null(res))
    req(res$ok)
    req(!is.null(res$result$irf))
    
    fitted_vars <- res$result$meta$fitted_vars
    idx_response <- res$result$meta$response_idx
    available_impulses <- res$result$meta$impulse_vars_fit
    
    impulses_to_show <- input$irf_plot_impulses %||% character(0)
    if (!length(impulses_to_show)) {
      impulses_to_show <- available_impulses
    }
    
    impulses_to_show <- intersect(impulses_to_show, available_impulses)
    req(length(impulses_to_show) > 0)
    req(length(idx_response) > 0, !any(is.na(idx_response)))
    
    for (i in seq_along(impulses_to_show)) {
      local({
        ii <- i
        impulse_name <- impulses_to_show[ii]
        impulse_idx <- match(impulse_name, fitted_vars)
        plot_id <- paste0("irf_plot_", ii)
        
        output[[plot_id]] <- renderPlot({
          req(!is.na(impulse_idx))
          
          plot(
            res$result$irf,
            area = TRUE,
            vars_impulse = impulse_idx,
            vars_response = idx_response
          )
        })
      })
    }
  })
  
  observe({
    req(input$run_model > 0)
    req(input$do_irf)
    
    res <- model_run_result()
    req(!is.null(res))
    req(res$ok)
    req(!is.null(res$result$fevd))
    
    fitted_vars <- res$result$meta$fitted_vars
    responses_to_show <- res$result$meta$response_vars_fit
    req(length(responses_to_show) > 0)
    
    for (i in seq_along(responses_to_show)) {
      local({
        ii <- i
        response_name <- responses_to_show[ii]
        response_idx <- match(response_name, fitted_vars)
        plot_id <- paste0("fevd_plot_", ii)
        
        output[[plot_id]] <- renderPlot({
          req(!is.na(response_idx))
          
          fevd_obj <- res$result$fevd
          arr <- fevd_obj$fevd
          
          validate(
            need(!is.null(arr), "FEVD array not found."),
            need(
              length(dim(arr)) == 4,
              paste("Unexpected FEVD dimensions:", paste(dim(arr), collapse = " x "))
            )
          )
          
          # arr dimensions: draws x response x horizon x impulse
          n_draws   <- dim(arr)[1]
          n_vars    <- dim(arr)[2]
          n_horizon <- dim(arr)[3]
          n_shocks  <- dim(arr)[4]
          
          # Compute posterior median FEVD share for this response from each shock
          # Result: matrix [horizon x shock]
          share_mat <- matrix(NA_real_, nrow = n_horizon, ncol = n_shocks)
          for (s in seq_len(n_shocks)) {
            draw_h <- arr[, response_idx, , s]  # draws x horizon
            share_mat[, s] <- apply(draw_h, 2, median, na.rm = TRUE)
          }
          
          # Normalise rows to sum to 1 (they should already, but ensure it)
          row_sums <- rowSums(share_mat, na.rm = TRUE)
          row_sums[row_sums == 0] <- 1
          share_mat <- share_mat / row_sums
          
          # Labels
          shock_labels <- display_label(fitted_vars)
          horizon_labels <- seq_len(n_horizon)
          colors <- fevd_color_palette(n_shocks)
          
          # ── Stacked bar chart ──
          oldpar <- par(no.readonly = TRUE)
          on.exit(par(oldpar))
          par(mar = c(5, 4.5, 3, 1))
          
          bp <- barplot(
            t(share_mat),
            beside = FALSE,
            col = colors,
            border = "white",
            names.arg = horizon_labels,
            xlab = "Forecast horizon (steps ahead)",
            ylab = "FEVD contribution",
            main = paste("FEVD:", display_label(response_name)),
            ylim = c(0, 1),
            las = 1,
            cex.names = 1,
            cex.lab = 1.1,
            cex.main = 1.2
          )
          
          # Legend
          legend(
            "topright",
            legend = shock_labels,
            fill = colors,
            border = "white",
            bty = "n",
            cex = 0.8,
            ncol = min(3, ceiling(n_shocks / 4))
          )
        })
      })
    }
  })
  
  
  output$forecast_diag <- DT::renderDataTable({
    req(input$run_model > 0)
    res <- model_run_result()
    req(!is.null(res), res$ok, !is.null(res$result$forecast))
    
    vars <- res$result$meta$fitted_vars
    out <- lapply(vars, function(v) {
      path_df <- forecast_path_on_original_scale(res$result, v, res$result$forecast)
      hist_vals <- as.numeric(res$result$data_used[[v]])
      
      if (is.null(path_df)) {
        return(NULL)
      }
      
      data.frame(
        Variable = display_label(v),
        `Historical Min` = round(min(hist_vals, na.rm = TRUE), 3),
        `Historical Max` = round(max(hist_vals, na.rm = TRUE), 3),
        `Forecast Median Min` = round(min(path_df$q50, na.rm = TRUE), 3),
        `Forecast Median Max` = round(max(path_df$q50, na.rm = TRUE), 3),
        `Lower 68% Min` = round(min(path_df$q16, na.rm = TRUE), 3),
        `Upper 68% Max` = round(max(path_df$q84, na.rm = TRUE), 3),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    })
    
    dplyr::bind_rows(out)
  }, options = list(pageLength = 10, scrollX = TRUE))
  
  forecast_plot_height <- function() {
    res <- model_run_result()
    n_vars <- if (!is.null(res) && isTRUE(res$ok)) {
      length(res$result$meta$fitted_vars %||% character(0))
    } else {
      1L
    }
    paste0(max(420, 230 * max(1, n_vars)), "px")
  }
  
  output$forecast_plot_ui <- renderUI({
    plotly::plotlyOutput("forecast_plot", height = forecast_plot_height())
  })
  
  output$forecast_median_plot_ui <- renderUI({
    plotly::plotlyOutput("forecast_median_plot", height = forecast_plot_height())
  })
  
  output$scenario_forecast_plot_ui <- renderUI({
    plotly::plotlyOutput("scenario_forecast_plot", height = forecast_plot_height())
  })
  
  output$scenario_forecast_median_plot_ui <- renderUI({
    plotly::plotlyOutput("scenario_forecast_median_plot", height = forecast_plot_height())
  })
  
  output$forecast_plot <- plotly::renderPlotly({
    req(input$run_model > 0)
    res <- model_run_result()
    req(!is.null(res))
    req(res$ok)
    req(input$do_forecast)
    req(!is.null(res$result$forecast))
    
    plot_forecast_panels(
      run_result = res$result,
      forecast_obj = res$result$forecast,
      title_prefix = "Baseline Forecast",
      t_back = min(16, nrow(res$result$data_used))
    )
  })
  
  output$forecast_median_plot <- plotly::renderPlotly({
    req(input$run_model > 0)
    res <- model_run_result()
    req(!is.null(res))
    req(res$ok)
    req(input$do_forecast)
    req(!is.null(res$result$forecast))
    
    plot_forecast_median_panels(
      run_result = res$result,
      forecast_obj = res$result$forecast,
      title_prefix = "Baseline Forecast Median Detail",
      t_back = min(16, nrow(res$result$data_used))
    )
  })
  
  refresh_scenario_table <- function(keep_manual = TRUE) {
    res <- model_run_result()
    if (is.null(res) || !isTRUE(res$ok) || is.null(res$result$forecast)) {
      return(invisible(NULL))
    }
    
    scenario_var <- input$scenario_var %||% ""
    if (!nzchar(scenario_var)) {
      return(invisible(NULL))
    }
    
    manual_values <- NULL
    if (isTRUE(keep_manual) && !is.null(scenario_values_rv())) {
      manual_values <- scenario_values_rv()[["Scenario Path"]]
    }
    
    scenario_values_rv(
      build_scenario_frame(
        run_result = res$result,
        scenario_var = scenario_var,
        template = input$scenario_template %||% "baseline",
        sd_multiple = input$scenario_sd_multiple %||% 1,
        vol_multiplier = input$scenario_vol_multiplier %||% 1.5,
        manual_values = manual_values
      )
    )
  }
  
  output$scenario_var_ui <- renderUI({
    selected_vars <- selected_model_vars()
    
    selectInput(
      "scenario_var",
      "Scenario variable",
      choices = c("Select a variable" = "", named_choices(selected_vars)),
      selected = if (!is.null(input$scenario_var) &&
                     nzchar(input$scenario_var) &&
                     input$scenario_var %in% selected_vars) {
        input$scenario_var
      } else {
        ""
      }
    )
  })
  
  observeEvent(list(input$run_model, input$scenario_var, input$use_conditional_forecast), {
    scenario_forecast_result(NULL)
    
    if (isTRUE(input$use_conditional_forecast) && nzchar(input$scenario_var %||% "")) {
      refresh_scenario_table(keep_manual = FALSE)
    } else {
      scenario_values_rv(NULL)
    }
  }, ignoreInit = FALSE)
  
  observeEvent(input$apply_scenario_template, {
    req(isTRUE(input$use_conditional_forecast))
    req(nzchar(input$scenario_var))
    refresh_scenario_table(keep_manual = FALSE)
    scenario_forecast_result(NULL)
  })
  
  observeEvent(input$reset_scenario_path, {
    req(isTRUE(input$use_conditional_forecast))
    req(nzchar(input$scenario_var))
    updateSelectInput(session, "scenario_template", selected = "baseline")
    refresh_scenario_table(keep_manual = FALSE)
    scenario_forecast_result(NULL)
  })
  
  output$scenario_table_ui <- renderUI({
    req(input$run_model > 0)
    req(isTRUE(input$use_conditional_forecast))
    req(nzchar(input$scenario_var))
    
    tagList(
      tags$h4(paste("Scenario path for", display_label(input$scenario_var))),
      div(
        class = "tab-caption",
        "The suggestion is generated on the original data scale. You can now edit the Scenario Path column directly before updating the scenario forecast."
      ),
      DT::dataTableOutput("scenario_table")
    )
  })
  
  output$scenario_table <- DT::renderDataTable({
    req(input$run_model > 0)
    req(isTRUE(input$use_conditional_forecast))
    req(nzchar(input$scenario_var))
    
    res <- model_run_result()
    req(!is.null(res), res$ok, !is.null(res$result$forecast))
    
    x <- scenario_values_rv()
    if (is.null(x)) {
      refresh_scenario_table(keep_manual = FALSE)
      x <- scenario_values_rv()
    }
    
    validate(
      need(!is.null(x), "Run the baseline forecast first so the app can generate a scenario path.")
    )
    
    DT::datatable(
      x,
      editable = list(target = "cell", disable = list(columns = c(0, 1, 2, 3, 5, 6))),
      rownames = FALSE,
      options = list(
        dom = "t",
        pageLength = nrow(x),
        columnDefs = list(list(targets = 6, visible = FALSE))
      )
    )
  })
  
  observeEvent(input$scenario_table_cell_edit, {
    req(isTRUE(input$use_conditional_forecast))
    req(nzchar(input$scenario_var))
    res <- model_run_result()
    req(!is.null(res), res$ok)
    info <- input$scenario_table_cell_edit
    x <- scenario_values_rv()
    req(!is.null(x))
    x[info$row, info$col] <- DT::coerceValue(info$value, x[info$row, info$col])
    rebuilt <- build_scenario_frame(
      run_result = res$result,
      scenario_var = input$scenario_var,
      template = input$scenario_template %||% "baseline",
      sd_multiple = input$scenario_sd_multiple %||% 1,
      vol_multiplier = input$scenario_vol_multiplier %||% 1.5,
      manual_values = x[["Scenario Path"]]
    )
    x$Baseline <- rebuilt$Baseline
    x$`1 Sigma` <- rebuilt$`1 Sigma`
    x$Suggested <- rebuilt$Suggested
    x$`Delta vs Baseline` <- rebuilt$`Delta vs Baseline`
    x$scenario_model <- rebuilt$scenario_model
    scenario_values_rv(x)
    scenario_forecast_result(NULL)
  })
  
  observeEvent(input$run_scenario_forecast, {
    req(input$run_model > 0)
    req(isTRUE(input$use_conditional_forecast))
    req(nzchar(input$scenario_var))
    
    res <- model_run_result()
    req(!is.null(res), res$ok, !is.null(res$result$forecast))
    
    scenario_tbl <- scenario_values_rv()
    if (is.null(scenario_tbl)) {
      refresh_scenario_table(keep_manual = FALSE)
      scenario_tbl <- scenario_values_rv()
    }
    
    out <- safe_run_conditional_forecast(
      run_result = res$result,
      scenario_var = input$scenario_var,
      scenario_model_values = scenario_tbl$scenario_model
    )
    
    scenario_forecast_result(out)
  })
  
  output$scenario_status <- renderText({
    req(input$run_model > 0)
    res <- model_run_result()
    
    if (!is.null(res) && !isTRUE(res$ok)) {
      return(paste("Baseline model run failed:", res$error))
    }
    
    if (is.null(res) || is.null(res$result$forecast)) {
      return("Run the baseline model with forecasting enabled before building a scenario.")
    }
    
    if (!isTRUE(input$use_conditional_forecast)) {
      return("Scenario mode is off. Enable it to generate a conditional path from the baseline forecast.")
    }
    
    if (!nzchar(input$scenario_var %||% "")) {
      return("Choose a scenario variable to generate a suggested path.")
    }
    
    scenario_out <- scenario_forecast_result()
    if (is.null(scenario_out)) {
      return("Suggestion ready. Edit the Scenario Path column if needed, then click Update Scenario Forecast.")
    }
    
    if (!scenario_out$ok) {
      return(paste("Scenario forecast failed:", scenario_out$error))
    }
    
    paste(
      "Scenario forecast updated successfully.\n",
      "Variable:", display_label(input$scenario_var), "\n",
      "Template:", scenario_template_labels[[input$scenario_template]] %||% input$scenario_template
    )
  })
  
  output$scenario_forecast_plot <- plotly::renderPlotly({
    req(input$run_model > 0)
    req(isTRUE(input$use_conditional_forecast))
    
    res <- model_run_result()
    req(!is.null(res), res$ok)
    req(!is.null(res$result$forecast))
    
    scenario_out <- scenario_forecast_result()
    validate(
      need(!is.null(scenario_out), "Click Update Scenario Forecast to compare the baseline and scenario paths."),
      need(isTRUE(scenario_out$ok), scenario_out$error)
    )
    
    plot_forecast_panels(
      run_result = res$result,
      forecast_obj = res$result$forecast,
      comparison_obj = scenario_out$result,
      title_prefix = "Scenario Comparison",
      t_back = min(16, nrow(res$result$data_used))
    )
  })
  
  output$scenario_forecast_median_plot <- plotly::renderPlotly({
    req(input$run_model > 0)
    req(isTRUE(input$use_conditional_forecast))
    
    res <- model_run_result()
    req(!is.null(res), res$ok)
    req(!is.null(res$result$forecast))
    
    scenario_out <- scenario_forecast_result()
    validate(
      need(!is.null(scenario_out), "Click Update Scenario Forecast to compare the baseline and scenario paths."),
      need(isTRUE(scenario_out$ok), scenario_out$error)
    )
    
    plot_forecast_median_panels(
      run_result = res$result,
      forecast_obj = res$result$forecast,
      comparison_obj = scenario_out$result,
      title_prefix = "Scenario Comparison Median Detail",
      t_back = min(16, nrow(res$result$data_used))
    )
  })
  
  output$transformed_data <- DT::renderDataTable({
    req(input$run_model > 0)
    res <- model_run_result()
    req(!is.null(res))
    req(res$ok)
    
    dat_out <- as.data.frame(res$result$Y)
    if ("quarter" %in% names(res$result$data_used)) {
      dat_out <- cbind(quarter = res$result$data_used$quarter, dat_out)
    }
    
    names(dat_out) <- display_label(names(dat_out))
    dat_out
  }, options = list(pageLength = 15, scrollX = TRUE))
}

shinyApp(ui, server)
