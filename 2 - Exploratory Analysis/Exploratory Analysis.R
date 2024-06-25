#======================================================================================# 
#================================ Exploratory Analysis ================================#
#======================================================================================#

pacman::p_load(
  "ggplot2", # Data Visualization
  "panelView", # Panel Data Visualization
  "sf", # Maps
  "rnaturalearth", # Maps
  "rnaturalearthdata", # Maps
  "countrycode", # Converting Country Codes
  install = FALSE 
)

# Turn Off Scientific Notation for Graphics
options(scipen = 999)

# Plot Treatment Assignment (Troops) - For Personal Inspection
treat_assign_troops_gov <- panelview(gov_deaths ~ troop + duration + lmilper + 
                                       lag_gov_deaths + lgdppc + log_actor_count, data = final,
                                     index = c("conflict_id", "year"))

# Plot Treatment Assignment (Police) - For Personal Inspection
treat_assign_police_gov <- panelview(gov_deaths ~ police + duration + lmilper + 
                                       lag_gov_deaths + lgdppc + log_actor_count, data = final,
                                     index = c("conflict_id", "year"))

# Map Pre-Processing
world_sf <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
) %>%
  mutate(gwcode = countrycode(iso_a3, origin = "iso3c", destination = "gwn"))

map_data <- final %>%
  full_join(world_sf, by = "gwcode") %>%
  # Drop Antarctica
  filter(name_long != "Antarctica") %>%
  # Create Total PKO Counts for the Maps
  group_by(gwcode) %>%
  mutate(
    total_troops = sum(troop),
    total_police = sum(police)
  )

# Map of UN Troops -- Note: Will Not Use This for Publication
pko_troop_map <- map_data %>% 
  ggplot() + 
  geom_sf(
    aes(geometry = geometry, fill = total_troops), color = "black", size = .2, na.rm = T
  ) +
  # Adjust Color Scales
  scale_fill_viridis_c(
    option = "mako", na.value = "lightgrey", direction = -1, end = 0.8
  ) +
  # Legend and Margins Customization
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(), 
    axis.text.y=element_blank(),  
    axis.ticks.y=element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 8, family = "serif", vjust = 1),
    legend.key.height = unit(0.25, 'cm'),
    legend.text = element_text(size = 6, family = "serif"),
    legend.key.width = unit(1, 'cm'),
    plot.margin = unit(c(-1, -0.7, -1, -0.7), "cm")
  ) +
  # Add Labels
  labs(
    fill = "")

ggsave(
  "pko_troop_map.png",
  width = 6,
  height = 4,
  path = "2 - Exploratory Analysis"
)