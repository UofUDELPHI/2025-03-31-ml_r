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

# extract preprocessed training and testing data ------------------------------
# prep (recipe) then bake (recipe and training data)
nhanes_recipe_prepped <- prep(nhanes_recipe)
nhanes_health_train_preprocessed <- bake(nhanes_recipe_prepped,
                                         nhanes_health_train)

# --------------------------------------------------------------------------- #
# Tune RF and XGBoost ------------------------------------------------------- #
# --------------------------------------------------------------------------- #


# Create a 5-fold cross-validation object using vfold_cv-----------------------
set.seed(123)
cv_folds <- vfold_cv(nhanes_health_train, v = 5)


# Model specifications with tuning params -------------------------------------

# Create an RF spec with parameters to tune 
rf_tune_spec <- rand_forest(
  trees = tune(), # Number of trees
  min_n = tune(), # Minimum number of observations in terminal nodes
  mtry = tune()   # fraction of features per tree
) |>
  set_engine("ranger") |>
  set_mode("classification")

# Create an xgboost spec with parameters to tune 
# params to tune: learn_rate, loss_reduction, min_n, mtry
xgb_tune_spec <- boost_tree(
  learn_rate = tune(),    # Learning rate (eta)
  loss_reduction = tune(),# Gamma (min loss reduction for split)
  min_n = tune(),         # Minimum number of observations in terminal nodes
  mtry = tune()           # fraction of features per tree
) |>
  set_engine("xgboost") |>
  set_mode("classification")




# Define hyperparameter grids ------------------------------------------------

# Define a grid of RF hyperparameter values using grid_latin_hypercube()
rf_grid <- grid_latin_hypercube(
  trees(),
  min_n(),
  finalize(mtry(), select(nhanes_health_train_preprocessed, 
                          -SEQN, -healthcare_expenses_1k)),
  size = 16 # number of combinations
)

# Define a grid of XGB hyperparameter values using grid_latin_hypercube()
xgb_grid <- grid_latin_hypercube(
  min_n(),
  loss_reduction(),
  finalize(mtry(), select(nhanes_health_train_preprocessed, 
                          -SEQN, -healthcare_expenses_1k)),
  learn_rate(),
  size = 16
)



# Workflows for tuned specifications ------------------------------------------

# Define an rf workflow
rf_tune_wf <- workflow() |>
  add_recipe(nhanes_recipe) |>
  add_model(rf_tune_spec) 

# Define an xgb workflow
xgb_tune_wf <- workflow() |>
  add_recipe(nhanes_recipe) |>
  add_model(xgb_tune_spec) 


# Apply CV to tune models -----------------------------------------------------

# tune RF using tune_grid()
# args: workflow (rf_tune_wf), 
#       resamples (cv_folds), 
#       grid (rf_grid), 
#       metrics (metric_set(roc_auc))
set.seed(123)
rf_tuned <- tune_grid(
  rf_tune_wf,
  resamples = cv_folds,
  grid = rf_grid,
  metrics = metric_set(roc_auc) # Evaluate using AUC and accuracy
)

# tune XGB using tune_grid()
# args: workflow (rf_tune_wf), 
#       resamples (cv_folds), 
#       grid (rf_grid), 
#       metrics (metric_set(roc_auc))
set.seed(123)
xgb_tuned <- tune_grid(
  xgb_tune_wf,
  resamples = cv_folds,
  grid = xgb_grid,
  metrics = metric_set(roc_auc) # Evaluate using AUC and accuracy
)



# Identify the best hyperparameters -------------------------------------------

# identify RF hyperparams: select_best() on rf_tuned with metric = "roc_auc"
rf_best_params <- select_best(rf_tuned, metric = "roc_auc")
rf_best_params

# identify XGB hyperparams: select_best() on xgb_tuned with metric = "roc_auc"
xgb_best_params <- select_best(xgb_tuned, metric = "roc_auc")
xgb_best_params



# --------------------------------------------------------------------------- #
# Fit all models ------------------------------------------------------------ #
# --------------------------------------------------------------------------- #

# Default model specifications for comparison ---------------------------------
# specify a logistic regression model (& set engine and mode)
lr_spec <- logistic_reg() |>
  set_engine("glm") |>
  set_mode("classification")

# specify a RF model with default parameters
rf_spec <- rand_forest() |>
  set_engine("ranger") |>
  set_mode("classification")

# specify an XGBoost model with default parameters
xgb_spec <- boost_tree() |>
  set_engine("xgboost") |>
  set_mode("classification")




# Workflows ------------------------------------------------------------------
# define a LR workflow (add recipe and model spec)
nhanes_lr_wf <- workflow() |>
  add_recipe(nhanes_recipe) |>
  add_model(lr_spec)

# define a default RF workflow (add recipe and model spec)
nhanes_rf_wf <- workflow() |>
  add_recipe(nhanes_recipe) |>
  add_model(rf_spec)

# define a tuned RF workflow (use finalize_workflow)
nhanes_rf_tuned_wf <- finalize_workflow(rf_tune_wf, rf_best_params)

# define an XGB workflow (add recipe and model spec)
nhanes_xgb_wf <- workflow() |>
  add_recipe(nhanes_recipe) |>
  add_model(xgb_spec)

# define a tuned XGB workflow (use finalize_workflow)
nhanes_xgb_tuned_wf <- finalize_workflow(xgb_tune_wf, xgb_best_params)



# Fit models ------------------------------------------------------------------

# fit LR model
nhanes_lr_fit <- nhanes_lr_wf |>
  fit(data = nhanes_health_train)

# fit default RF model
nhanes_rf_fit <- nhanes_rf_wf |>
  fit(data = nhanes_health_train)

# fit tuned RF model
nhanes_rf_tuned_fit <- nhanes_rf_tuned_wf |>
  fit(data = nhanes_health_train)

# fit default XGB model
nhanes_xgb_fit <- nhanes_xgb_wf |>
  fit(data = nhanes_health_train)

# fit tuned XGB model
nhanes_xgb_tuned_fit <- nhanes_xgb_tuned_wf |>
  fit(data = nhanes_health_train)




# --------------------------------------------------------------------------- #
# Evaluate models ----------------------------------------------------------- #
# --------------------------------------------------------------------------- #

# Generate test set predictions -----------------------------------------------

# generate LR predictions
nhanes_health_test_lr_pred <- augment(nhanes_lr_fit,
                                      new_data = nhanes_health_test)

# generate default RF predictions
nhanes_health_test_rf_pred <- augment(nhanes_rf_fit,
                                      new_data = nhanes_health_test)

# generte tuned RF predictions
nhanes_health_test_rf_tuned_pred <- augment(nhanes_rf_tuned_fit,
                                            new_data = nhanes_health_test)

# generate default XGB predictions
nhanes_health_test_xgb_pred <- augment(nhanes_xgb_fit,
                                       new_data = nhanes_health_test)

# generate tuned XGB predictions
nhanes_health_test_xgb_tuned_pred <- augment(nhanes_xgb_tuned_fit,
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

# compute the proportion of observations in the 1 class
prop_1 <- mean(nhanes_health_train$healthcare_expenses_1k == 1)

# apply computeMetrics to LR predictions using the proportional threshold
lr_metric_prop <- computeMetrics(nhanes_health_test_lr_pred,
                                 threshold = prop_1)
lr_metric_prop

# apply computeMetrics to default RF predictions using the proportional threshold
rf_metric_prop <- computeMetrics(nhanes_health_test_rf_pred,
                                 threshold = prop_1)
rf_metric_prop

# apply computeMetrics to tuned RF predictions using the proportional threshold
rf_tuned_metric_prop <- computeMetrics(nhanes_health_test_rf_tuned_pred,
                                       threshold = prop_1)
rf_tuned_metric_prop

# apply computeMetrics to default XGB predictions using the proportional threshold
xgb_metric_prop <- computeMetrics(nhanes_health_test_xgb_pred,
                                  threshold = prop_1)
xgb_metric_prop

# apply computeMetrics to tuned XGB predictions using the proportional threshold
xgb_tuned_metric_prop <- computeMetrics(nhanes_health_test_xgb_tuned_pred,
                                        threshold = prop_1)
xgb_tuned_metric_prop




# ROC curve ------------------------------------------------------------------

# compute ROC data frame for LR using roc_curve() and add model ID column
lr_roc <- nhanes_health_test_lr_pred |>
  roc_curve(truth = healthcare_expenses_1k,
            .pred_1) |>
  mutate(model = "LR",
         hyperparams = "default")

# compute ROC data frame for default RF using roc_curve() and add model ID column
rf_roc <- nhanes_health_test_rf_pred |>
  roc_curve(truth = healthcare_expenses_1k,
            .pred_1) |>
  mutate(model = "RF",
         hyperparams = "default")

# compute ROC data frame for tuned RF using roc_curve() and add model ID column
rf_tuned_roc <- nhanes_health_test_rf_tuned_pred |>
  roc_curve(truth = healthcare_expenses_1k,
            .pred_1) |>
  mutate(model = "RF",
         hyperparams = "tuned")

# compute ROC data frame for default XGB using roc_curve() and add model ID column
xgb_roc <- nhanes_health_test_xgb_pred |>
  roc_curve(truth = healthcare_expenses_1k,
            .pred_1) |>
  mutate(model = "XGB",
         hyperparams = "default")

# compute ROC data frame for tuned XGB using roc_curve() and add model ID column
xgb_tuned_roc <- nhanes_health_test_xgb_tuned_pred |>
  roc_curve(truth = healthcare_expenses_1k,
            .pred_1) |>
  mutate(model = "XGB",
         hyperparams = "tuned")


# plot all ROC curves on the same plot ----------------------------------------
lr_roc |>
  rbind(rf_roc) |>
  rbind(rf_tuned_roc) |>
  rbind(xgb_roc) |>
  rbind(xgb_tuned_roc) |>
  ggplot() +
  geom_path(aes(x = 1 - specificity, y = sensitivity, 
                color = model, linetype = hyperparams)) +
  coord_equal() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") 

