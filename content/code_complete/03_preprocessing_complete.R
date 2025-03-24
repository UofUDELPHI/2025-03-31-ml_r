# load libraries --------------------------------------------------------------
library(tidyverse)
library(tidymodels)


# load data -------------------------------------------------------------------
nhanes_health_data <- read_csv("data/nhanes_health.csv")
nhanes_health_data |> print(width = Inf)


# --------------------------------------------------------------------------- #
# Explorations -------------------------------------------------------------- #
# --------------------------------------------------------------------------- #

# Explorations of numeric columns ---------------------------------------------
# make histograms of age, income_to_poverty_ratio, weight, height, bmi, 
# and healthcare_expenses_synth
nhanes_health_data |>
  select(age, income_to_poverty_ratio, weight, height, 
         bmi, healthcare_expenses_synth) |>
  pivot_longer(everything()) |>
  ggplot() +
  geom_histogram(aes(x = value)) +
  facet_wrap(~name, scales = "free")

# make histograms of the log-transformed versions of the same variables
nhanes_health_data |>
  select(age, income_to_poverty_ratio, weight, height, 
         bmi, healthcare_expenses_synth) |>
  pivot_longer(everything()) |>
  ggplot() +
  geom_histogram(aes(x = log(value))) +
  facet_wrap(~name, scales = "free")

# Explore the relationship of each variable to the response -------------------
# compute pairwise correlations of age, bmi, height, income_to_poverty_ratio, 
# and weight with response variable healthcare_expenses_synth
nhanes_health_data |>
  select(where(is.numeric), -SEQN) |>
  pivot_longer(-healthcare_expenses_synth) |>
  drop_na(value) |>
  group_by(name) |>
  summarize(cor = cor(healthcare_expenses_synth, value)) |>
  arrange(desc(abs(cor)))
  
# create scatterplots of of age, bmi, height, income_to_poverty_ratio, 
# and weight with response variable healthcare_expenses_synth
nhanes_health_data |>
  select(age, bmi, height, income_to_poverty_ratio, weight, 
         healthcare_expenses_synth) |>
  pivot_longer(-healthcare_expenses_synth) |>
  ggplot() +
  geom_point(aes(x = value, y = healthcare_expenses_synth),
             alpha = 0.2) +
  facet_wrap(~name, scales = "free")

# Create boxplots of pregnant, n_drinks_per_day, health_condition_self_reported,
# smoker, diabetes, all history_ variables against healthcare_expenses_synth (y)
nhanes_health_data |>
  select(pregnant, n_drinks_per_day, health_condition_self_reported, smoker,
         diabetes, starts_with("history_"), healthcare_expenses_synth) |>
  mutate(across(-healthcare_expenses_synth, as.character)) |>
  pivot_longer(-healthcare_expenses_synth) |>
  ggplot() +
  geom_boxplot(aes(x = value, y = healthcare_expenses_synth),
             alpha = 0.2) +
  facet_wrap(~name, scales = "free")


# Count the number of missing values in each column ---------------------------
nhanes_health_data |> map_dbl(~sum(is.na(.)))
nhanes_health_data |> map_dbl(function(x) sum(is.na(x)))



# --------------------------------------------------------------------------- #
# Apply preprocessing ------------------------------------------------------- #
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
# update_role, step_impute_mean, step_impute_mode, step_dummy, step_ordinalscore, etc
nhanes_recipe_full <- recipe(healthcare_expenses_synth ~ ., 
                             data = nhanes_health_train) |>
  update_role(SEQN, new_role = "ID") |>
  # imputation steps
  step_impute_mean(all_numeric_predictors()) |>
  step_impute_mode(all_factor_predictors()) |>
  step_impute_mode(all_string_predictors()) |>
  # create gender binary variable
  step_dummy(gender) |>
  # convert health_condition_self_reported to numeric
  step_mutate(health_condition_self_reported = 
                factor(health_condition_self_reported,
                       levels = c("poor", "fair", "good", "very good", "excellent"),
                       ordered = TRUE)) |>
  step_ordinalscore(health_condition_self_reported)



# extract preprocessed training and testing data ------------------------------
# prep (recipe) then bake (recipe and training data)
nhanes_recipe_full_prepped <- prep(nhanes_recipe_full)
nhanes_health_train_prepped <- bake(nhanes_recipe_full_prepped,
                                    nhanes_health_train)
nhanes_health_test_prepped <- bake(nhanes_recipe_full_prepped,
                                   nhanes_health_test)

#---------------------------------------------------------------------------- #
# Fit LS model -------------------------------------------------------------- #
#---------------------------------------------------------------------------- #

# specify lin reg model using all features (& set engine and mode) ------------
ls_spec <- linear_reg() |>
  set_engine("lm") |>
  set_mode("regression")

# define a workflow (add recipe and model spec) -------------------------------
nhanes_ls_wf <- workflow() |>
  add_recipe(nhanes_recipe_full) |>
  add_model(ls_spec)

# fit the linear model using training data ------------------------------------
nhanes_ls_fit <- nhanes_ls_wf |>
  fit(data = nhanes_health_train)

# What is the formula for our linear model? -----------------------------------



# --------------------------------------------------------------------------- #
# Evaluate predictions ------------------------------------------------------ #
# --------------------------------------------------------------------------- # 

# generate predictions for the test set using augment() -----------------------
nhanes_health_test_ls_pred <- augment(nhanes_ls_fit, 
                                      new_data = nhanes_health_test)
nhanes_health_test_ls_pred

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
