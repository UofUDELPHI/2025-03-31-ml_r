# load tidyverse and tidymodels libraries -------------------------------------



# load data (data/nhanes_health.csv) ------------------------------------------



# split data into training/testing using initial_split with prop = 0.7 --------
set.seed(21162)


# --------------------------------------------------------------------------- #
# Fit model ----------------------------------------------------------------- #
# --------------------------------------------------------------------------- #

# set up a recipe -------------------------------------------------------------



# specify a linear regression model (& set engine and mode) -------------------



# define a workflow (add recipe and model spec) -------------------------------



# fit the linear model using training data ------------------------------------



# extract the coefficients of the linear model using --------------------------
# extract_fit_parsnip() and tidy() --------------------------------------------



# --------------------------------------------------------------------------- #
# Evaluate model ------------------------------------------------------------ #
# --------------------------------------------------------------------------- #

# generate predictions for the test set using augment() -----------------------



# compute rmse and rsq using summarize and rmse_vec and rsq_vec ---------------



# compute scatterplot of observed vs predicted responses ----------------------



