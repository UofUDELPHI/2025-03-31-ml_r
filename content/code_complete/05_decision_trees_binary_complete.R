# libraries -------------------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(vip) # for variable importance
library(patchwork) # for patching plots together

# load data -------------------------------------------------------------------
nhanes_health_data <- read_csv("data/nhanes_health.csv")
nhanes_health_data |> print(width = Inf)

# create a binary response ----------------------------------------------------
nhanes_health_data <- nhanes_health_data |>
  mutate(healthcare_expenses_1k = factor(as.numeric(healthcare_expenses_synth > 1000),
                                         levels = c("1", "0")))

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
         -bmi,
         -healthcare_expenses_synth)


# split into training and test ------------------------------------------------
set.seed(21162)
nhanes_health_split <- initial_split(nhanes_health_data, prop = 0.7)
nhanes_health_train <- training(nhanes_health_split)
nhanes_health_test <- testing(nhanes_health_split)

# define recipe ---------------------------------------------------------------
# update_role, step_impute_mean, step_impute_mode, step_dummy, 
# step_ordinalscore, etc
nhanes_recipe <- recipe(healthcare_expenses_1k ~ .,
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
# specify a logistic regression model (& set engine and mode)
lr_spec <- logistic_reg() |>
  set_engine("glm") |>
  set_mode("classification")

# specify a RF model (& set engine and mode)
# include `importance = "impurity"` when setting engine
rf_spec <- rand_forest() |>
  set_engine("ranger", importance = "impurity") |>
  set_mode("classification")

# specify an XGBoost model (& set engine and mode)
# include `importance = "impurity"` when setting engine
xgb_spec <- boost_tree() |>
  set_engine("xgboost", importance = "impurity") |>
  set_mode("classification")




# Workflows ------------------------------------------------------------------
# define a LR workflow (add recipe and model spec)
nhanes_lr_wf <- workflow() |>
  add_recipe(nhanes_recipe) |>
  add_model(lr_spec)

# define a RF workflow (add recipe and model spec)
nhanes_rf_wf <- workflow() |>
  add_recipe(nhanes_recipe) |>
  add_model(rf_spec)

# define an XGB workflow (add recipe and model spec)
nhanes_xgb_wf <- workflow() |>
  add_recipe(nhanes_recipe) |>
  add_model(xgb_spec)



# Fit models ------------------------------------------------------------------

# fit LR model
nhanes_lr_fit <- nhanes_lr_wf |>
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

# generate LR predictions
nhanes_health_test_lr_pred <- augment(nhanes_lr_fit,
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
computeMetrics <- function(df_aug, threshold = 0.5) {
  metrics <- df_aug |>
    mutate(pred_threshold = factor(as.numeric(.pred_1 > threshold),
                                   levels = c("1", "0"))) |>
    summarize(acc = accuracy_vec(truth = healthcare_expenses_1k,
                                 estimate = pred_threshold),
              sens = sens_vec(truth = healthcare_expenses_1k,
                              estimate = pred_threshold),
              spec = spec_vec(truth = healthcare_expenses_1k,
                              estimate = pred_threshold),
              auc = roc_auc_vec(truth = healthcare_expenses_1k,
                                estimate = .pred_1))
  return(metrics)
}

# apply computeMetrics to LR predictions using 0.5 threshold
lr_metric <- computeMetrics(nhanes_health_test_lr_pred)
lr_metric

# apply computeMetrics to RF predictions using 0.5 threshold
rf_metric <- computeMetrics(nhanes_health_test_rf_pred)
rf_metric

# apply computeMetrics to XGB predictions using 0.5 threshold
xgb_metric <- computeMetrics(nhanes_health_test_xgb_pred)
xgb_metric




# compute the proportion of observations in the 1 class
prop_1 <- mean(nhanes_health_train$healthcare_expenses_1k == 1)

# apply computeMetrics to LR predictions using the proportional threshold
lr_metric_prop <- computeMetrics(nhanes_health_test_lr_pred,
                                 threshold = prop_1)
lr_metric_prop

# apply computeMetrics to RF predictions using the proportional threshold
rf_metric_prop <- computeMetrics(nhanes_health_test_rf_pred,
                                 threshold = prop_1)
rf_metric_prop

# apply computeMetrics to XGB predictions using the proportional threshold
xgb_metric_prop <- computeMetrics(nhanes_health_test_xgb_pred,
                                  threshold = prop_1)
xgb_metric_prop






# ROC curve ------------------------------------------------------------------

# compute ROC data frame for LR using roc_curve() and add model ID column
lr_roc <- nhanes_health_test_lr_pred |>
  roc_curve(truth = healthcare_expenses_1k,
            .pred_1) |>
  mutate(model = "LR")
lr_roc

# compute ROC data frame for RF using roc_curve() and add model ID column
rf_roc <- nhanes_health_test_rf_pred |>
  roc_curve(truth = healthcare_expenses_1k,
            .pred_1) |>
  mutate(model = "RF")
rf_roc

# compute ROC data frame for XGB using roc_curve() and add model ID column
xgb_roc <- nhanes_health_test_xgb_pred |>
  roc_curve(truth = healthcare_expenses_1k,
            .pred_1) |>
  mutate(model = "XGB")
xgb_roc

# plot all ROC curves on the same plot
lr_roc |>
  rbind(rf_roc) |>
  rbind(xgb_roc) |>
  ggplot() +
  geom_path(aes(x = 1 - specificity, y = sensitivity, color = model)) +
  coord_equal() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed")


# --------------------------------------------------------------------------- #
# Variable importance ------------------------------------------------------- #
# --------------------------------------------------------------------------- #

# load the vip and patchwork libraries (at top of document)

# compute variable importance for LR model with extract_fit_parsnip() and vip()
vi_lr <- nhanes_lr_fit |>
  extract_fit_parsnip() |>
  vip(num_features = 100) +
  ggtitle("LR")
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
vi_lr + vi_rf + vi_xgb
