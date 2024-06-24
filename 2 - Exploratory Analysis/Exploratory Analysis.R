#======================================================================================# 
#================================ Exploratory Analysis ================================#
#======================================================================================#

pacman::p_load(
  "ggplot2", # Data Visualization
  "panelView", # Panel Data Visualization
  install = FALSE
)

# Plot Treatment Assignment (Binary)
treat_assign_bin <- panelview(battle_deaths ~ un_pko + duration + lpop + lmilper + gov_conflict + 
                               lag_battle_deaths + lgdppc + log_actor_count, data = final,
                             index = c("statename", "year"))

# Plot Treatment Assignment (Continuous)

# Map of UN Troops

# Map of UN Police