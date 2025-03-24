# libraries -------------------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(vip) # for variable importance
library(patchwork) # for patching plots together

# load data -------------------------------------------------------------------
nhanes_health_data <- read_csv("data/nhanes_health.csv")
nhanes_health_data |> print(width = Inf)


# --------------------------------------------------------------------------- #
# Data preprocessing -------------------------------------------------------- #
# --------------------------------------------------------------------------- #

# remove non-appropriate variables --------------------------------------------
# (n_healthcare_received, n_overnight_hospital_stays_year, 
# seen_mental_health_professional_year, bmi)
nhanes_health_data <- nhanes_health_data |>
  select(-n_healthcare_received,
         -n_overnight_hospital_stays_year,
         -seen_mental_health_professional_year,
         -bmi)


# split into training and test ------------------------------------------------
set.seed(21162)
nhanes_health_split <- initial_split(nhanes_health_data, prop = 0.7)
nhanes_health_train <- training(nhanes_health_split)
nhanes_health_test <- testing(nhanes_health_split)

# define recipe ---------------------------------------------------------------
# update_role, step_impute_mean, step_impute_mode, step_dummy, 
# step_ordinalscore, etc
nhanes_recipe <- recipe(healthcare_expenses_synth ~ .,
                        data = nhanes_health_train) |>
  update_role(SEQN, new_role = "ID") |>
  # imputation
  step_impute_mean(all_numeric_predictors()) |>
  step_impute_mode(all_factor_predictors()) |>
  step_impute_mode(all_string_predictors()) |>
  # create gender binary variable
  step_dummy(gender) |>
  # convert health condition self-reported to numeric
  step_mutate(health_condition_self_reported = 
                factor(health_condition_self_reported,
                       levels = c("poor", "fair", "good", "very good", "excellent"),
                       ordered = TRUE)) |>
  step_ordinalscore(health_condition_self_reported)



# --------------------------------------------------------------------------- #
# Fit all models ------------------------------------------------------------ #
# --------------------------------------------------------------------------- #


# Model specifications --------------------------------------------------------
# specify a linear regression (LS) model (& set engine and mode)
ls_spec <- linear_reg() |>
  set_engine("lm") |>
  set_mode("regression")

# specify a RF model (& set engine and mode)
# include `importance = "impurity"` when setting engine
rf_spec <- rand_forest() |>
  set_engine("ranger", importance = "impurity") |>
  set_mode("regression")

# specify an XGBoost model (& set engine and mode)
# include `importance = "impurity"` when setting engine
xgb_spec <- boost_tree() |>
  set_engine("xgboost", importance = "impurity") |>
  set_mode("regression")




# Workflows ------------------------------------------------------------------
# define a LS workflow (add recipe and model spec)
nhanes_ls_wf <- workflow() |>
  add_recipe(nhanes_recipe) |>
  add_model(ls_spec)

# define a RF workflow (add recipe and model spec)
nhanes_rf_wf <- workflow() |>
  add_recipe(nhanes_recipe) |>
  add_model(rf_spec)

# define an XGB workflow (add recipe and model spec)
nhanes_xgb_wf <- workflow() |>
  add_recipe(nhanes_recipe) |>
  add_model(xgb_spec)



# Fit models ------------------------------------------------------------------

# fit LS model
nhanes_ls_fit <- nhanes_ls_wf |>
  fit(data = nhanes_health_train)

# fit RF model
nhanes_rf_fit <- nhanes_rf_wf |>
  fit(data = nhanes_health_train)

# fit XGB model
nhanes_xgb_fit <- nhanes_xgb_wf |>
  fit(data = nhanes_health_train)



# --------------------------------------------------------------------------- #
# Evaluate models ----------------------------------------------------------- #
# --------------------------------------------------------------------------- #

# Generate test set predictions -----------------------------------------------

# generate LS predictions
nhanes_health_test_ls_pred <- augment(nhanes_ls_fit,
                                      new_data = nhanes_health_test)

# generate RF predictions
nhanes_health_test_rf_pred <- augment(nhanes_rf_fit,
                                      new_data = nhanes_health_test)

# generate XGB predictions
nhanes_health_test_xgb_pred <- augment(nhanes_xgb_fit,
                                      new_data = nhanes_health_test)



# Compute evaluations ---------------------------------------------------------

# write a function computeMetrics() that will compute the accuracy, sensitivity,
# specificity, and AUC for a given threshold
computeMetrics <- function(df_aug) {
  metrics <- df_aug |>
    summarize(rmse = rmse_vec(truth = healthcare_expenses_synth,
                              estimate = .pred),
              rsq = rsq_vec(truth = healthcare_expenses_synth,
                            estimate = .pred))
  return(metrics)
}

# apply computeMetrics to LS predictions
ls_metric <- computeMetrics(nhanes_health_test_ls_pred)
ls_metric

# apply computeMetrics to RF predictions
rf_metric <- computeMetrics(nhanes_health_test_rf_pred)
rf_metric

# apply computeMetrics to XGB predictions
xgb_metric <- computeMetrics(nhanes_health_test_xgb_pred)
xgb_metric









# --------------------------------------------------------------------------- #
# Variable importance ------------------------------------------------------- #
# --------------------------------------------------------------------------- #

# load the vip and patchwork libraries (at top of document)

# compute variable importance for LS model with extract_fit_parsnip() and vip()
vi_ls <- nhanes_ls_fit |>
  extract_fit_parsnip() |>
  vip(num_features = 100) +
  ggtitle("LS")
# compute variable importance for RF model with extract_fit_parsnip() and vip()
vi_rf <- nhanes_rf_fit |>
  extract_fit_parsnip() |>
  vip(num_features = 100) + 
  ggtitle("RF")
# compute variable importance for XGB model
vi_xgb <- nhanes_xgb_fit |>
  extract_fit_parsnip() |>
  vip(num_features = 100) +
  ggtitle("XGB")

# patch the plots together
vi_ls + vi_rf + vi_xgb
