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



# Explore the relationship of each variable to the response -------------------
# compute pairwise correlations of age, bmi, height, income_to_poverty_ratio, 
# and weight with response variable healthcare_expenses_synth


  
# create scatterplots of of age, bmi, height, income_to_poverty_ratio, 
# and weight with response variable healthcare_expenses_synth



# Create boxplots of pregnant, n_drinks_per_day, health_condition_self_reported,
# smoker, diabetes, all history_ variables against healthcare_expenses_synth (y)



# Count the number of missing values in each column ---------------------------




# --------------------------------------------------------------------------- #
# Apply preprocessing ------------------------------------------------------- #
# --------------------------------------------------------------------------- #


# remove non-appropriate variables --------------------------------------------
# (n_healthcare_received, n_overnight_hospital_stays_year, 
# seen_mental_health_professional_year, bmi)



# split into training and test ------------------------------------------------



# define recipe ---------------------------------------------------------------
# update_role, step_impute_mean, step_impute_mode, step_dummy, 
# step_ordinalscore, etc




# extract preprocessed training and testing data ------------------------------
# prep (recipe) then bake (recipe and training data)



#-----------------------------------------------------------------------------#
# Fit LS model ---------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# specify lin reg model using all features (& set engine and mode) ------------



# define a workflow (add recipe and model spec) -------------------------------



# fit the linear model using training data ------------------------------------



# What is the formula for our linear model? -----------------------------------




# --------------------------------------------------------------------------- #
# Evaluate predictions ------------------------------------------------------ #
# --------------------------------------------------------------------------- # 

# generate predictions for the test set using augment() -----------------------




# compute rmse and rsq using summarize and rmse_vec and rsq_vec ---------------




# compute scatterplot of observed vs predicted responses ----------------------



