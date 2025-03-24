# libraries -------------------------------------------------------------------
library(tidyverse)
library(tidymodels)

# load data -------------------------------------------------------------------
nhanes_health_data <- read_csv("data/nhanes_health.csv")
nhanes_health_data |> print(width = Inf)


# --------------------------------------------------------------------------- #
# Data preprocessing -------------------------------------------------------- #
# --------------------------------------------------------------------------- #

# remove non-appropriate variables --------------------------------------------
# (n_healthcare_received, n_overnight_hospital_stays_year, 
# seen_mental_health_professional_year, bmi AND healthcare_expenses_synth)
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

# Create a scaled version of the recipe for NN
nhanes_recipe_scaled <- nhanes_recipe |>
  step_scale(all_numeric_predictors()) |>
  step_center(all_numeric_predictors())


# --------------------------------------------------------------------------- #
# Fit all models ------------------------------------------------------------ #
# --------------------------------------------------------------------------- #


# Model specifications --------------------------------------------------------
# specify a linear regression (least squares) model (& set engine and mode)
ls_spec <- linear_reg() |>
  set_engine("lm") |>
  set_mode("regression")

# specify a RF model (& set engine and mode)
rf_spec <- rand_forest() |>
  set_engine("ranger") |>
  set_mode("regression")

# specify an XGBoost model (& set engine and mode)
xgb_spec <- boost_tree() |>
  set_engine("xgboost") |>
  set_mode("regression")

# specify a nn model 
nn_spec <- mlp(hidden_units = 50) |>
  set_engine("nnet") |>
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

# define a NN workflow 
nhanes_nn_wf <- workflow() |>
  add_recipe(nhanes_recipe_scaled) |>
  add_model(nn_spec) 



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

# fit NN model
nhanes_nn_fit <- nhanes_nn_wf |>
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

# generate NN predictions
nhanes_health_test_nn_pred <- augment(nhanes_nn_fit,
                                      new_data = nhanes_health_test)



# Compute evaluations ---------------------------------------------------------

# Compute metrics -------------------------------------------------------------
# compute metrics for LS model
ls_metric <- nhanes_health_test_ls_pred |>
  summarize(rmse = rmse_vec(truth = healthcare_expenses_synth,
                            estimate = .pred),
            rsq = rsq_vec(truth = healthcare_expenses_synth,
                          estimate = .pred))

# compute metrics for RF model
rf_metric <- nhanes_health_test_rf_pred |>
  summarize(rmse = rmse_vec(truth = healthcare_expenses_synth,
                            estimate = .pred),
            rsq = rsq_vec(truth = healthcare_expenses_synth,
                          estimate = .pred))

# compute metrics for XGB model
xgb_metric <- nhanes_health_test_xgb_pred |>
  summarize(rmse = rmse_vec(truth = healthcare_expenses_synth,
                            estimate = .pred),
            rsq = rsq_vec(truth = healthcare_expenses_synth,
                          estimate = .pred))

# compute metrics for NN model
nn_metric <- nhanes_health_test_nn_pred |>
  summarize(rmse = rmse_vec(truth = healthcare_expenses_synth,
                            estimate = .pred),
            rsq = rsq_vec(truth = healthcare_expenses_synth,
                          estimate = .pred))


# Visualize metrics -----------------------------------------------------------
# place metrics for each model in a tibble
metrics <- tribble(~model, ~rMSE, ~rsq,
                   "LS", ls_metric$rmse, ls_metric$rsq,
                   "RF", rf_metric$rmse, rf_metric$rsq,
                   "XGB", xgb_metric$rmse, xgb_metric$rsq,
                   "NN", nn_metric$rmse, nn_metric$rsq) 
metrics

# plot metrics
metrics |> ggplot() +
  geom_col(aes(x = model, y = rMSE, fill = model)) +
  theme_minimal() 
# plot metrics
metrics |> ggplot() +
  geom_col(aes(x = model, y = rsq, fill = model)) +
  theme_minimal() 

