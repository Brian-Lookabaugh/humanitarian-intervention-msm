#======================================================================================# 
#================================= Empirical Analysis =================================#
#======================================================================================#

pacman::p_load(
  "marginaleffects", # Marginal Effects
  "ipw", # Estimating Marginal Structural Models
  "MASS", # Count Models
  install = FALSE 
)

# Filter NAs for the Outcome - Coverage Is the Same for Each Outcome
final <- final %>%
  filter(!is.na(gov_deaths)) %>%
  # Create a Dummy of Troops and Police Presence
  mutate(
    troop_dummy = ifelse(troop > 0, 1, 0),
    police_dummy = ifelse(police > 0, 1, 0)
  ) %>%
  # 

# Estimate Inverse Probability Weights (IPWs)
# Models Will Cover the Intersection of:
#   - Troops and Police
#   - Troops and Police as Dummies
#   - Troops and Police as a Count (Set as family = Gaussian)
#   - Troops and Police as Gaussian (Log-Transformed)
#   - Weights Truncated at 10%, 5%, 1% and No Truncation 

ipw_list <- list()

# Inspect IPWs

# Estimate Outcome Models
outcome_model_list <- list()

# Estimate Marginal Effects
marginal_effects_list <- list()

# Sensitivity Analysis

# Create Graphics for Marginal Effects