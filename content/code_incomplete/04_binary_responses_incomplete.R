# libraries -------------------------------------------------------------------
library(tidyverse)
library(tidymodels)

# load data -------------------------------------------------------------------
nhanes_health_data <- read_csv("data/nhanes_health.csv")
nhanes_health_data |> print(width = Inf)




# create a binary response ----------------------------------------------------



# --------------------------------------------------------------------------- #
# Exploration --------------------------------------------------------------- #
# --------------------------------------------------------------------------- #

# how many people in each class?





# compute boxplots comparing the distribution of age, income_to_poverty_ratio,
# weight, and height for each class




# --------------------------------------------------------------------------- #
# Data preprocessing -------------------------------------------------------- #
# --------------------------------------------------------------------------- #

# remove non-appropriate variables --------------------------------------------
# (n_healthcare_received, n_overnight_hospital_stays_year, 
# seen_mental_health_professional_year, bmi AND healthcare_expenses_synth)




# split into training and test ------------------------------------------------
set.seed(21162)



# define recipe ---------------------------------------------------------------
# update_role, step_impute_mean, step_impute_mode, step_dummy, 
# step_ordinalscore, etc





# extract preprocessed training and testing data ------------------------------
# prep (recipe) then bake (recipe and training data)



# --------------------------------------------------------------------------- #
# Fit Logistic Regression model --------------------------------------------- #
# --------------------------------------------------------------------------- #

# specify a logistic regression model (& set engine and mode) -----------------





# define a workflow (add recipe and model spec) -------------------------------





# fit the logistic model using training data ----------------------------------





# --------------------------------------------------------------------------- #
# Evaluate model ------------------------------------------------------------ #
# --------------------------------------------------------------------------- #


# generate predictions for the test set using augment() -----------------------





# compute accuracy, sensitivity, specificity, and auc -------------------------






# create ROC curve -----------------------------------------------------------
# ROC curve using roc_curve() and autoplot()
# add a point to the ROC curve for the model's performance








# --------------------------------------------------------------------------- #
# Proportional threshold evaluation ----------------------------------------- #
# --------------------------------------------------------------------------- #

# compute the proportion of observations in the 1 class -----------------------




# compute the binary predictions using proportional threshold -----------------
# add to the predictions data frame




# compute accuracy, sensitivity, specificity, and auc -------------------------






# add a point to the ROC curve for the model's performance --------------------





