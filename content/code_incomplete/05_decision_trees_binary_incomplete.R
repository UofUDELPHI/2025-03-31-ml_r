# libraries -------------------------------------------------------------------
library(tidyverse)
library(tidymodels)

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


# specify a RF model (& set engine and mode)


# specify an XGBoost model (& set engine and mode)





# Workflows ------------------------------------------------------------------
# define a LR workflow (add recipe and model spec)


# define a RF workflow (add recipe and model spec)


# define an XGB workflow (add recipe and model spec)




# Fit models ------------------------------------------------------------------

# fit LR model


# fit RF model


# fit XGB model




# --------------------------------------------------------------------------- #
# Evaluate models ----------------------------------------------------------- #
# --------------------------------------------------------------------------- #

# Generate test set predictions -----------------------------------------------

# generate LR predictions using augment()


# generate RF predictions using augment()


# generate XGB predictions using augment()




# Compute evaluations ---------------------------------------------------------

# write a function computeMetrics() that will compute the accuracy, sensitivity,
# specificity, and AUC for a given threshold




# apply computeMetrics to LR predictions using 0.5 threshold


# apply computeMetrics to RF predictions using 0.5 threshold


# apply computeMetrics to XGB predictions using 0.5 threshold





# compute the proportion of observations in the 1 class


# apply computeMetrics to LR predictions using the proportional threshold


# apply computeMetrics to RF predictions using the proportional threshold


# apply computeMetrics to XGB predictions using the proportional threshold







# ROC curve ------------------------------------------------------------------

# compute ROC data frame for LR using roc_curve() and add model ID column


# compute ROC data frame for RF using roc_curve() and add model ID column


# compute ROC data frame for XGB using roc_curve() and add model ID column


# plot all ROC curves on the same plot







# --------------------------------------------------------------------------- #
# Variable importance ------------------------------------------------------- #
# --------------------------------------------------------------------------- #

# load the vip and patchwork libraries (at top of document)



# compute variable importance for LR model with extract_fit_parsnip() and vip()


# compute variable importance for RF model with extract_fit_parsnip() and vip()


# compute variable importance for XGB model




# patch the plots together

