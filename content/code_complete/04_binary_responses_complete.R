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
nhanes_health_data |> print(width = Inf)




# --------------------------------------------------------------------------- #
# Exploration --------------------------------------------------------------- #
# --------------------------------------------------------------------------- #

# how many people in each class?
nhanes_health_data |> count(healthcare_expenses_1k)

# compute boxplots comparing the distribution of age, income_to_poverty_ratio,
# weight, and height for each class
nhanes_health_data |>
  select(age, income_to_poverty_ratio, weight, height, healthcare_expenses_1k) |>
  pivot_longer(-healthcare_expenses_1k) |>
  ggplot() +
  geom_boxplot(aes(x = healthcare_expenses_1k, y = value)) +
  facet_wrap(~name, scales = "free")




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
nhanes_health_train_prepped <- bake(nhanes_recipe_prepped,
                                    nhanes_health_train)
nhanes_health_test_prepped <- bake(nhanes_recipe_prepped,
                                    nhanes_health_test)



# --------------------------------------------------------------------------- #
# Fit Logistic Regression model --------------------------------------------- #
# --------------------------------------------------------------------------- #

# specify a logistic regression model (& set engine and mode) -----------------
lr_spec <- logistic_reg() |>
  set_engine("glm") |>
  set_mode("classification")

# define a workflow (add recipe and model spec) -------------------------------
nhanes_lr_wf <- workflow() |>
  add_recipe(nhanes_recipe) |>
  add_model(lr_spec)

# fit the logistic model using training data ----------------------------------
nhanes_lr_fit <- nhanes_lr_wf |>
  fit(data = nhanes_health_train)


# --------------------------------------------------------------------------- #
# Evaluate model ------------------------------------------------------------ #
# --------------------------------------------------------------------------- #


# generate predictions for the test set using augment() -----------------------
nhanes_health_test_lr_pred <- augment(nhanes_lr_fit,
                                      new_data = nhanes_health_test)
nhanes_health_test_lr_pred |> print(width = Inf)

# compute accuracy, sensitivity, specificity, and auc -------------------------
lr_metric <- nhanes_health_test_lr_pred |>
  summarize(acc = accuracy_vec(truth = healthcare_expenses_1k,
                               estimate = .pred_class),
            sens = sens_vec(truth = healthcare_expenses_1k,
                            estimate = .pred_class),
            spec = spec_vec(truth = healthcare_expenses_1k,
                            estimate = .pred_class),
            auc = roc_auc_vec(truth = healthcare_expenses_1k,
                              estimate = .pred_1))
lr_metric

# create ROC curve -----------------------------------------------------------
# ROC curve using roc_curve() and autoplot()
# add a point to the ROC curve for the model's performance
lr_roc <- nhanes_health_test_lr_pred |>
  roc_curve(truth = healthcare_expenses_1k,
            .pred_1) |>
  autoplot()
lr_roc +
  geom_point(aes(x = 1 - spec, y = sens),
             data = lr_metric,
             color = "firebrick",
             size = 3)




# --------------------------------------------------------------------------- #
# Proportional threshold evaluation ----------------------------------------- #
# --------------------------------------------------------------------------- #

# compute the proportion of observations in the 1 class -----------------------
prop_1 <- mean(nhanes_health_train$healthcare_expenses_1k == 1)
prop_1

# compute the binary predictions using proportional threshold -----------------
# add to the predictions data frame
nhanes_health_test_lr_pred <- nhanes_health_test_lr_pred |>
  mutate(pred_class_prop = factor(as.numeric(.pred_1 > prop_1),
                                  levels = c("1", "0")))
nhanes_health_test_lr_pred |> print(width = Inf)

# compute accuracy, sensitivity, specificity, and auc -------------------------
lr_metric_prop <- nhanes_health_test_lr_pred |>
  summarize(acc = accuracy_vec(truth = healthcare_expenses_1k,
                               estimate = pred_class_prop),
            sens = sens_vec(truth = healthcare_expenses_1k,
                            estimate = pred_class_prop),
            spec = spec_vec(truth = healthcare_expenses_1k,
                            estimate = pred_class_prop),
            auc = roc_auc_vec(truth = healthcare_expenses_1k,
                              estimate = .pred_1))
lr_metric_prop

# add a point to the ROC curve for the model's performance --------------------
lr_roc +
  geom_point(aes(x = 1 - spec, y = sens),
             data = lr_metric,
             color = "firebrick",
             size = 3) +
  geom_point(aes(x = 1 - spec, y = sens),
             data = lr_metric_prop,
             color = "darkblue",
             size = 3)
