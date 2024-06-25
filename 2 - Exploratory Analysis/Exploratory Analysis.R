#======================================================================================# 
#================================ Exploratory Analysis ================================#
#======================================================================================#

pacman::p_load(
  "ggplot2", # Data Visualization
  "panelView", # Panel Data Visualization
  install = FALSE 
)

# Plot Treatment Assignment (Troops) - For Personal Inspection
treat_assign_troops_gov <- panelview(gov_deaths ~ troop + duration + lmilper + 
                                       lag_gov_deaths + lgdppc + log_actor_count, data = final,
                                     index = c("conflict_id", "year"))

# Plot Treatment Assignment (Police) - For Personal Inspection
treat_assign_police_gov <- panelview(gov_deaths ~ police + duration + lmilper + 
                                       lag_gov_deaths + lgdppc + log_actor_count, data = final,
                                     index = c("conflict_id", "year"))
# Map of UN Troops

# Map of UN Police
