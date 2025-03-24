# load tidyverse and tidymodels libraries -------------------------------------
library(tidyverse)
library(tidymodels)

# load data (data/nhanes_health.csv) ------------------------------------------
nhanes_health_data <- read_csv("data/nhanes_health.csv")
nhanes_health_data |> print(width = Inf)

# split data into training/testing using initial_split with prop = 0.7 --------
set.seed(21162)
nhanes_health_split <- initial_split(nhanes_health_data, prop = 0.7)
nhanes_health_train <- training(nhanes_health_split)
nhanes_health_test <- testing(nhanes_health_split)



# --------------------------------------------------------------------------- #
# Fit model ----------------------------------------------------------------- #
# --------------------------------------------------------------------------- #

# set up a recipe -------------------------------------------------------------
nhanes_recipe <- recipe(healthcare_expenses_synth ~ age, 
                        data = nhanes_health_train)

# specify a linear regression model (& set engine and mode) -------------------
ls_spec <- linear_reg() |>
  set_engine("lm") |>
  set_mode("regression")

# define a workflow (add recipe and model spec) -------------------------------
nhanes_wf <- workflow() |>
  add_recipe(nhanes_recipe) |>
  add_model(ls_spec)

# fit the linear model using training data ------------------------------------
ls_fit <- nhanes_wf |>
  fit(data = nhanes_health_train)

# extract the coefficients of the linear model using --------------------------
# extract_fit_parsnip() and tidy() --------------------------------------------
ls_coef <- ls_fit |>
  extract_fit_parsnip() |>
  tidy()



# --------------------------------------------------------------------------- #
# Evaluate model ------------------------------------------------------------ #
# --------------------------------------------------------------------------- #

# generate predictions for the test set using augment() -----------------------
nhanes_health_test_ls_pred <- augment(ls_fit, new_data = nhanes_health_test)
nhanes_health_test_ls_pred |> print(width = Inf)

# compute rmse and rsq using summarize and rmse_vec and rsq_vec ---------------
ls_metric <- nhanes_health_test_ls_pred |>
  summarize(rmse = rmse_vec(truth = healthcare_expenses_synth,
                            estimate = .pred),
            rsq = rsq_vec(truth = healthcare_expenses_synth,
                          estimate = .pred))
ls_metric

# compute scatterplot of observed vs predicted responses ----------------------
nhanes_health_test_ls_pred |>
  ggplot() +
  geom_point(aes(x = healthcare_expenses_synth,
                 y = .pred)) +
  geom_abline(intercept = 0,
              slope = 1)