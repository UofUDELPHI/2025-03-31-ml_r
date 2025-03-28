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



# Model specifications with tuning params -------------------------------------

# Create an RF spec with parameters to tune 



# Create an xgboost spec with parameters to tune 
# params to tune: learn_rate, loss_reduction, min_n, mtry




# Define hyperparameter grids ------------------------------------------------

# Define a grid of RF hyperparameter values using grid_latin_hypercube()



# Define a grid of XGB hyperparameter values using grid_latin_hypercube()




# Workflows for tuned specifications ------------------------------------------

# Define a rf workflow


# Define an xgb workflow




# Apply CV to tune models -----------------------------------------------------

# tune RF using tune_grid()
# args: workflow (rf_tune_wf), 
#       resamples (cv_folds), 
#       grid (rf_grid), 
#       metrics (metric_set(roc_auc))
set.seed(123)


# tune XGB using tune_grid()
# args: workflow (rf_tune_wf), 
#       resamples (cv_folds), 
#       grid (rf_grid), 
#       metrics (metric_set(roc_auc))
set.seed(123)



# Identify the best hyperparameters -------------------------------------------

# identify RF hyperparams: select_best() on rf_tuned with metric = "roc_auc"


# identify XGB hyperparams: select_best() on xgb_tuned with metric = "roc_auc"




# --------------------------------------------------------------------------- #
# Fit all models ------------------------------------------------------------ #
# --------------------------------------------------------------------------- #

# Define default model specifications for comparison --------------------------
# specify a logistic regression model (& set engine and mode)


# specify a RF model with default parameters


# specify an XGBoost model with default parameters




# Workflows ------------------------------------------------------------------
# define a LR workflow (add recipe and model spec)



# define a default RF workflow (add recipe and model spec)



# define a tuned RF workflow (use finalize_workflow)



# define an XGB workflow (add recipe and model spec)



# define a tuned XGB workflow (use finalize_workflow)




# Fit models ------------------------------------------------------------------

# fit LR model


# fit default RF model


# fit tuned RF model


# fit default XGB model


# fit tuned XGB model





# --------------------------------------------------------------------------- #
# Evaluate models ----------------------------------------------------------- #
# --------------------------------------------------------------------------- #

# Generate test set predictions -----------------------------------------------

# generate LR predictions


# generate default RF predictions


# generte tuned RF predictions


# generate default XGB predictions


# generate tuned XGB predictions




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


# apply computeMetrics to default RF predictions using the proportional threshold


# apply computeMetrics to tuned RF predictions using the proportional threshold


# apply computeMetrics to default XGB predictions using the proportional threshold


# apply computeMetrics to tuned XGB predictions using the proportional threshold





# ROC curve ------------------------------------------------------------------

# compute ROC data frame for LR using roc_curve() and add model ID column


# compute ROC data frame for default RF using roc_curve() and add model ID column


# compute ROC data frame for tuned RF using roc_curve() and add model ID column


# compute ROC data frame for default XGB using roc_curve() and add model ID column


# compute ROC data frame for tuned XGB using roc_curve() and add model ID column



# plot all ROC curves on the same plot ----------------------------------------

