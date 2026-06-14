# Gold Reserve Diversification Forecasting
# ------------------------------------------------------------
# Public-facing analysis script for GitHub
#
# Project:
#   Predict gold reserve shares and year-to-year changes in gold reserve
#   shares for BRICS and selected emerging-market economies using World Bank
#   World Development Indicators panel data.
#
# Author:
#   Thomas Swide
#
# Notes:
#   - Place the raw WDI Excel export in data/raw/.
#   - Default expected file name:
#       P_Data_Extract_From_World_Development_Indicators.xlsx
#   - Outputs are written to data/processed/, outputs/tables/, and
#     outputs/figures/.
#   - Raw data files are intentionally excluded from the public repository.
# ------------------------------------------------------------


# -----------------------------
# 1. Packages and configuration
# -----------------------------

required_packages <- c(
  "readxl",
  "dplyr",
  "tidyr",
  "stringr",
  "glmnet",
  "ggplot2",
  "purrr",
  "readr",
  "tibble",
  "knitr"
)

missing_packages <- required_packages[
  !(required_packages %in% rownames(installed.packages()))
]

if (length(missing_packages) > 0) {
  stop(
    "Please install the following packages before running this script: ",
    paste(missing_packages, collapse = ", ")
  )
}

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(glmnet)
  library(ggplot2)
  library(purrr)
  library(readr)
  library(tibble)
  library(knitr)
})

set.seed(123)

data_dir <- "data/raw"
processed_dir <- "data/processed"
table_dir <- "outputs/tables"
figure_dir <- "outputs/figures"

dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

wdi_file <- file.path(data_dir, "P_Data_Extract_From_World_Development_Indicators.xlsx")

if (!file.exists(wdi_file)) {
  stop(
    "WDI file not found at: ", wdi_file,
    "\nPlace the raw World Bank WDI Excel export in data/raw/ or update `wdi_file`."
  )
}

start_year <- 1996
end_year <- 2022
train_end_year <- 2018

brics_codes <- c("BRA", "RUS", "IND", "CHN", "ZAF")

emerging_market_codes <- c(
  "BRA", "RUS", "IND", "CHN", "ZAF",
  "MEX", "ARG", "CHL", "COL", "PER",
  "TUR", "POL", "HUN", "ROU", "CZE", "UKR", "KAZ",
  "SAU", "ARE", "QAT", "EGY", "MAR",
  "NGA", "KEN", "GHA", "ETH",
  "IDN", "MYS", "THA", "VNM", "PHL", "PAK", "BGD", "LKA"
)

indicator_map <- c(
  total_reserves = "FI.RES.TOTL.CD",
  reserves_excluding_gold = "FI.RES.XGLD.CD",
  gdp_per_capita = "NY.GDP.PCAP.CD",
  gdp_per_capita_growth = "NY.GDP.PCAP.KD.ZG",
  inflation = "FP.CPI.TOTL.ZG",
  trade_share_gdp = "NE.TRD.GNFS.ZS",
  manufacturing_share_gdp = "NV.IND.MANF.ZS",
  fuel_exports_share = "TX.VAL.FUEL.ZS.UN",
  private_credit_share_gdp = "FS.AST.PRVT.GD.ZS",
  political_stability = "PV.EST"
)


# -----------------------------
# 2. Helper functions
# -----------------------------

clean_numeric <- function(x) {
  x <- as.character(x)
  x[x %in% c("..", "", "NA", "N/A", "na", "n/a")] <- NA_character_
  x <- gsub(",", "", x)
  suppressWarnings(as.numeric(x))
}


rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2, na.rm = TRUE))
}


mae <- function(actual, predicted) {
  mean(abs(actual - predicted), na.rm = TRUE)
}


save_table <- function(data, filename) {
  readr::write_csv(data, file.path(table_dir, filename))
}


detect_wdi_year_columns <- function(data) {
  year_cols <- names(data)[
    str_detect(names(data), "^(X)?\\d{4}(\\s*\\[YR\\d{4}\\])?$")
  ]

  if (length(year_cols) == 0) {
    stop("No WDI year columns detected. Check the raw file column names.")
  }

  year_cols
}


detect_indicator_column <- function(data) {
  if ("Indicator Code" %in% names(data)) {
    return("Indicator Code")
  }

  if ("Series Code" %in% names(data)) {
    return("Series Code")
  }

  stop("Could not find an indicator column named 'Indicator Code' or 'Series Code'.")
}


run_linear_models <- function(train_data, test_data, predictors) {
  train_complete <- train_data %>%
    select(outcome, all_of(predictors)) %>%
    drop_na()

  test_complete <- test_data %>%
    select(outcome, all_of(predictors)) %>%
    drop_na()

  if (nrow(train_complete) == 0 || nrow(test_complete) == 0) {
    stop("Complete-case train or test data has zero rows for the supplied predictors.")
  }

  x_train <- model.matrix(outcome ~ ., data = train_complete)[, -1, drop = FALSE]
  y_train <- train_complete$outcome

  x_test <- model.matrix(outcome ~ ., data = test_complete)[, -1, drop = FALSE]
  y_test <- test_complete$outcome

  # OLS
  ols_model <- lm(outcome ~ ., data = train_complete)
  ols_pred_train <- as.numeric(predict(ols_model, newdata = train_complete))
  ols_pred_test <- as.numeric(predict(ols_model, newdata = test_complete))

  # Ridge regression
  set.seed(123)
  ridge_cv <- cv.glmnet(
    x = x_train,
    y = y_train,
    alpha = 0,
    nfolds = 10,
    standardize = TRUE
  )

  ridge_pred_train <- as.numeric(
    predict(ridge_cv, s = "lambda.min", newx = x_train)
  )
  ridge_pred_test <- as.numeric(
    predict(ridge_cv, s = "lambda.min", newx = x_test)
  )

  # Lasso regression
  set.seed(123)
  lasso_cv <- cv.glmnet(
    x = x_train,
    y = y_train,
    alpha = 1,
    nfolds = 10,
    standardize = TRUE
  )

  lasso_pred_train <- as.numeric(
    predict(lasso_cv, s = "lambda.min", newx = x_train)
  )
  lasso_pred_test <- as.numeric(
    predict(lasso_cv, s = "lambda.min", newx = x_test)
  )

  performance <- tibble(
    n_train = nrow(train_complete),
    n_test = nrow(test_complete),

    train_rmse_ols = rmse(y_train, ols_pred_train),
    train_rmse_ridge = rmse(y_train, ridge_pred_train),
    train_rmse_lasso = rmse(y_train, lasso_pred_train),

    test_rmse_ols = rmse(y_test, ols_pred_test),
    test_rmse_ridge = rmse(y_test, ridge_pred_test),
    test_rmse_lasso = rmse(y_test, lasso_pred_test),

    train_mae_ols = mae(y_train, ols_pred_train),
    train_mae_ridge = mae(y_train, ridge_pred_train),
    train_mae_lasso = mae(y_train, lasso_pred_train),

    test_mae_ols = mae(y_test, ols_pred_test),
    test_mae_ridge = mae(y_test, ridge_pred_test),
    test_mae_lasso = mae(y_test, lasso_pred_test),

    cv_rmse_ridge = sqrt(min(ridge_cv$cvm)),
    cv_rmse_lasso = sqrt(min(lasso_cv$cvm)),

    ridge_lambda_min = ridge_cv$lambda.min,
    ridge_lambda_1se = ridge_cv$lambda.1se,
    lasso_lambda_min = lasso_cv$lambda.min,
    lasso_lambda_1se = lasso_cv$lambda.1se
  )

  ols_coefficients <- tibble(
    term = names(coef(ols_model)),
    coefficient = as.numeric(coef(ols_model))
  ) %>%
    mutate(abs_coefficient = abs(coefficient)) %>%
    arrange(desc(abs_coefficient))
  
  ridge_coef_matrix <- as.matrix(coef(ridge_cv, s = "lambda.min"))
  
  ridge_coefficients <- tibble(
    term = rownames(ridge_coef_matrix),
    coefficient = as.numeric(ridge_coef_matrix[, 1])
  ) %>%
    filter(term != "(Intercept)") %>%
    mutate(abs_coefficient = abs(coefficient)) %>%
    arrange(desc(abs_coefficient))
  
  
  lasso_coef_matrix <- as.matrix(coef(lasso_cv, s = "lambda.min"))
  
  lasso_coefficients <- tibble(
    term = rownames(lasso_coef_matrix),
    coefficient = as.numeric(lasso_coef_matrix[, 1])
  ) %>%
    filter(term != "(Intercept)") %>%
    filter(coefficient != 0) %>%
    mutate(abs_coefficient = abs(coefficient)) %>%
    arrange(desc(abs_coefficient))

  list(
    performance = performance,
    ols_coefficients = ols_coefficients,
    ridge_coefficients = ridge_coefficients,
    lasso_coefficients = lasso_coefficients
  )
}


# -----------------------------
# 3. Load and clean WDI data
# -----------------------------

message("Reading WDI data from: ", wdi_file)

wdi_raw <- readxl::read_excel(wdi_file)

year_cols <- detect_wdi_year_columns(wdi_raw)
indicator_col <- detect_indicator_column(wdi_raw)

wdi_long <- wdi_raw %>%
  pivot_longer(
    cols = all_of(year_cols),
    names_to = "year_raw",
    values_to = "value"
  ) %>%
  mutate(
    year = as.integer(str_extract(year_raw, "\\d{4}"))
  ) %>%
  select(-year_raw)

wdi_panel_raw <- wdi_long %>%
  select(
    `Country Name`,
    `Country Code`,
    indicator_code = all_of(indicator_col),
    year,
    value
  ) %>%
  pivot_wider(
    names_from = indicator_code,
    values_from = value,
    values_fn = dplyr::first
  )

required_columns <- c(
  "Country Name",
  "Country Code",
  "year",
  unname(indicator_map)
)

missing_columns <- setdiff(required_columns, names(wdi_panel_raw))

if (length(missing_columns) > 0) {
  stop(
    "The WDI file is missing required columns/series: ",
    paste(missing_columns, collapse = ", ")
  )
}

wdi_panel <- wdi_panel_raw %>%
  transmute(
    country_name = `Country Name`,
    country_code = `Country Code`,
    year = year,

    total_reserves = clean_numeric(.data[[indicator_map["total_reserves"]]]),
    reserves_excluding_gold = clean_numeric(.data[[indicator_map["reserves_excluding_gold"]]]),
    gdp_per_capita = clean_numeric(.data[[indicator_map["gdp_per_capita"]]]),
    gdp_per_capita_growth = clean_numeric(.data[[indicator_map["gdp_per_capita_growth"]]]),
    inflation = clean_numeric(.data[[indicator_map["inflation"]]]),
    trade_share_gdp = clean_numeric(.data[[indicator_map["trade_share_gdp"]]]),
    manufacturing_share_gdp = clean_numeric(.data[[indicator_map["manufacturing_share_gdp"]]]),
    fuel_exports_share = clean_numeric(.data[[indicator_map["fuel_exports_share"]]]),
    private_credit_share_gdp = clean_numeric(.data[[indicator_map["private_credit_share_gdp"]]]),
    political_stability = clean_numeric(.data[[indicator_map["political_stability"]]])
  ) %>%
  filter(
    country_code %in% emerging_market_codes,
    year >= start_year,
    year <= end_year
  ) %>%
  mutate(
    gold_reserves = total_reserves - reserves_excluding_gold,
    gold_share = if_else(
      total_reserves > 0,
      100 * gold_reserves / total_reserves,
      NA_real_
    ),
    brics_dummy = if_else(country_code %in% brics_codes, 1, 0),
    group = if_else(brics_dummy == 1, "BRICS", "Non-BRICS")
  ) %>%
  filter(
    !is.na(gold_share),
    gold_share >= 0,
    gold_share <= 100
  )


# -----------------------------
# 4. Feature engineering
# -----------------------------

model_df <- wdi_panel %>%
  arrange(country_code, year) %>%
  group_by(country_code) %>%
  mutate(
    gold_share_lag1 = lag(gold_share, 1),
    gdp_per_capita_lag1 = lag(gdp_per_capita, 1),
    gdp_per_capita_growth_lag1 = lag(gdp_per_capita_growth, 1),
    inflation_lag1 = lag(inflation, 1),
    trade_share_gdp_lag1 = lag(trade_share_gdp, 1),
    manufacturing_share_gdp_lag1 = lag(manufacturing_share_gdp, 1),
    fuel_exports_share_lag1 = lag(fuel_exports_share, 1),
    private_credit_share_gdp_lag1 = lag(private_credit_share_gdp, 1),
    political_stability_lag1 = lag(political_stability, 1),
    delta_gold_share = gold_share - gold_share_lag1
  ) %>%
  ungroup() %>%
  drop_na(
    gold_share,
    gold_share_lag1,
    gdp_per_capita_lag1,
    gdp_per_capita_growth_lag1,
    inflation_lag1,
    trade_share_gdp_lag1,
    manufacturing_share_gdp_lag1,
    fuel_exports_share_lag1,
    private_credit_share_gdp_lag1,
    political_stability_lag1,
    brics_dummy,
    delta_gold_share
  ) %>%
  arrange(country_code, year)

readr::write_csv(
  model_df,
  file.path(processed_dir, "gold_reserve_modeling_panel.csv")
)

message("Final modeling rows: ", nrow(model_df))
message("Countries in modeling data: ", n_distinct(model_df$country_code))


# -----------------------------
# 5. Descriptive tables and figures
# -----------------------------

group_summary <- model_df %>%
  group_by(group) %>%
  summarise(
    observations = n(),
    countries = n_distinct(country_code),
    mean_gold_share = mean(gold_share, na.rm = TRUE),
    median_gold_share = median(gold_share, na.rm = TRUE),
    sd_gold_share = sd(gold_share, na.rm = TRUE),
    min_gold_share = min(gold_share, na.rm = TRUE),
    max_gold_share = max(gold_share, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

save_table(group_summary, "group_summary.csv")

fig_distribution <- ggplot(
  model_df,
  aes(x = group, y = gold_share, fill = group)
) +
  geom_boxplot(alpha = 0.75, outlier.alpha = 0.45) +
  labs(
    title = "Distribution of Gold Reserve Share: BRICS vs Non-BRICS",
    x = "",
    y = "Gold share (% of total reserves)",
    fill = ""
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(
  filename = file.path(figure_dir, "gold_share_distribution_brics_vs_nonbrics.png"),
  plot = fig_distribution,
  width = 8,
  height = 5,
  dpi = 300
)

average_gold_share <- model_df %>%
  group_by(year, group) %>%
  summarise(
    average_gold_share = mean(gold_share, na.rm = TRUE),
    .groups = "drop"
  )

fig_time <- ggplot(
  average_gold_share,
  aes(x = year, y = average_gold_share, color = group)
) +
  geom_line(linewidth = 1) +
  labs(
    title = "Average Gold Reserve Share Over Time",
    subtitle = paste0(start_year, "–", end_year),
    x = "Year",
    y = "Average gold share (% of total reserves)",
    color = ""
  ) +
  theme_minimal()

ggsave(
  filename = file.path(figure_dir, "average_gold_share_over_time.png"),
  plot = fig_time,
  width = 8,
  height = 5,
  dpi = 300
)


# -----------------------------
# 6. Train/test split and model specifications
# -----------------------------

train_df <- model_df %>%
  filter(year <= train_end_year)

test_df <- model_df %>%
  filter(year > train_end_year)

message("Training years: ", min(train_df$year), "–", max(train_df$year))
message("Test years: ", min(test_df$year), "–", max(test_df$year))
message("Training rows: ", nrow(train_df))
message("Test rows: ", nrow(test_df))

level_predictors_with_lagged_y <- c(
  "gold_share_lag1",
  "gdp_per_capita_lag1",
  "gdp_per_capita_growth_lag1",
  "inflation_lag1",
  "trade_share_gdp_lag1",
  "manufacturing_share_gdp_lag1",
  "fuel_exports_share_lag1",
  "private_credit_share_gdp_lag1",
  "political_stability_lag1",
  "brics_dummy"
)

macro_predictors_only <- c(
  "gdp_per_capita_lag1",
  "gdp_per_capita_growth_lag1",
  "inflation_lag1",
  "trade_share_gdp_lag1",
  "manufacturing_share_gdp_lag1",
  "fuel_exports_share_lag1",
  "private_credit_share_gdp_lag1",
  "political_stability_lag1",
  "brics_dummy"
)

specifications <- list(
  list(
    name = "Spec 1: Level + Lagged Gold Share",
    train = train_df %>% mutate(outcome = gold_share),
    test = test_df %>% mutate(outcome = gold_share),
    predictors = level_predictors_with_lagged_y
  ),
  list(
    name = "Spec 2: Level Without Lagged Gold Share",
    train = train_df %>% mutate(outcome = gold_share),
    test = test_df %>% mutate(outcome = gold_share),
    predictors = macro_predictors_only
  ),
  list(
    name = "Spec 3: Change in Gold Share",
    train = train_df %>% mutate(outcome = delta_gold_share),
    test = test_df %>% mutate(outcome = delta_gold_share),
    predictors = macro_predictors_only
  )
)


# -----------------------------
# 7. Run models
# -----------------------------

model_results <- purrr::map(
  specifications,
  ~ run_linear_models(
    train_data = .x$train,
    test_data = .x$test,
    predictors = .x$predictors
  )
)

performance_comparison <- purrr::map2_dfr(
  specifications,
  model_results,
  ~ .y$performance %>%
    mutate(specification = .x$name, .before = 1)
)

performance_comparison <- performance_comparison %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

save_table(performance_comparison, "model_performance_comparison.csv")

coefficient_tables <- purrr::map2(
  specifications,
  model_results,
  function(spec, result) {
    list(
      ols = result$ols_coefficients %>%
        mutate(specification = spec$name, model = "OLS", .before = 1),
      ridge = result$ridge_coefficients %>%
        mutate(specification = spec$name, model = "Ridge", .before = 1),
      lasso = result$lasso_coefficients %>%
        mutate(specification = spec$name, model = "Lasso", .before = 1)
    )
  }
)

ols_coefficients <- purrr::map_dfr(coefficient_tables, "ols")
ridge_coefficients <- purrr::map_dfr(coefficient_tables, "ridge")
lasso_coefficients <- purrr::map_dfr(coefficient_tables, "lasso")

save_table(ols_coefficients, "ols_coefficients.csv")
save_table(ridge_coefficients, "ridge_coefficients.csv")
save_table(lasso_coefficients, "lasso_nonzero_coefficients.csv")


# -----------------------------
# 8. Baseline forecasts
# -----------------------------

test_level_complete <- test_df %>%
  select(gold_share, gold_share_lag1) %>%
  drop_na()

lag_only_baseline <- tibble(
  baseline = "Lag-only persistence baseline",
  test_rmse = rmse(test_level_complete$gold_share, test_level_complete$gold_share_lag1),
  test_mae = mae(test_level_complete$gold_share, test_level_complete$gold_share_lag1)
)

train_mean <- mean(train_df$gold_share, na.rm = TRUE)

mean_baseline <- tibble(
  baseline = "Training-set mean baseline",
  test_rmse = rmse(test_level_complete$gold_share, rep(train_mean, nrow(test_level_complete))),
  test_mae = mae(test_level_complete$gold_share, rep(train_mean, nrow(test_level_complete)))
)

baseline_metrics <- bind_rows(lag_only_baseline, mean_baseline) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

save_table(baseline_metrics, "baseline_metrics.csv")


# -----------------------------
# 9. Model comparison figures
# -----------------------------

rmse_plot_data <- performance_comparison %>%
  select(
    specification,
    starts_with("train_rmse_"),
    starts_with("test_rmse_"),
    cv_rmse_ridge,
    cv_rmse_lasso
  ) %>%
  pivot_longer(
    cols = -specification,
    names_to = "metric",
    values_to = "rmse"
  ) %>%
  mutate(
    set = case_when(
      str_detect(metric, "^train_") ~ "Train (in-sample)",
      str_detect(metric, "^test_") ~ "Test (held-out)",
      str_detect(metric, "^cv_") ~ "CV (training folds)",
      TRUE ~ "Other"
    ),
    model = case_when(
      str_detect(metric, "ols") ~ "OLS",
      str_detect(metric, "ridge") ~ "Ridge",
      str_detect(metric, "lasso") ~ "Lasso",
      TRUE ~ "Other"
    )
  )

fig_rmse <- ggplot(
  rmse_plot_data,
  aes(x = model, y = rmse, fill = set)
) +
  geom_col(position = "dodge") +
  facet_wrap(~ specification, scales = "free_y") +
  labs(
    title = "RMSE by Model Type",
    subtitle = "Training, held-out test, and cross-validation performance",
    x = "",
    y = "RMSE",
    fill = ""
  ) +
  theme_minimal()

ggsave(
  filename = file.path(figure_dir, "rmse_model_comparison.png"),
  plot = fig_rmse,
  width = 11,
  height = 6,
  dpi = 300
)

mae_plot_data <- performance_comparison %>%
  select(
    specification,
    starts_with("train_mae_"),
    starts_with("test_mae_")
  ) %>%
  pivot_longer(
    cols = -specification,
    names_to = "metric",
    values_to = "mae"
  ) %>%
  mutate(
    set = case_when(
      str_detect(metric, "^train_") ~ "Train (in-sample)",
      str_detect(metric, "^test_") ~ "Test (held-out)",
      TRUE ~ "Other"
    ),
    model = case_when(
      str_detect(metric, "ols") ~ "OLS",
      str_detect(metric, "ridge") ~ "Ridge",
      str_detect(metric, "lasso") ~ "Lasso",
      TRUE ~ "Other"
    )
  )

fig_mae <- ggplot(
  mae_plot_data,
  aes(x = model, y = mae, fill = set)
) +
  geom_col(position = "dodge") +
  facet_wrap(~ specification, scales = "free_y") +
  labs(
    title = "MAE by Model Type",
    subtitle = "Training and held-out test performance",
    x = "",
    y = "MAE",
    fill = ""
  ) +
  theme_minimal()

ggsave(
  filename = file.path(figure_dir, "mae_model_comparison.png"),
  plot = fig_mae,
  width = 11,
  height = 6,
  dpi = 300
)


# -----------------------------
# 10. Console summary
# -----------------------------

message("\nAnalysis complete.")
message("Processed data saved to: ", processed_dir)
message("Tables saved to: ", table_dir)
message("Figures saved to: ", figure_dir)

message("\nBaseline metrics:")
print(kable(baseline_metrics, caption = "Baseline Forecast Performance"))

message("\nModel performance comparison:")
print(kable(performance_comparison, caption = "OLS, Ridge, and Lasso Model Performance"))
