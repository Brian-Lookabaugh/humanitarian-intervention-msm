#======================================================================================# 
#============================== Data Collection/Cleaning ==============================#
#======================================================================================#

pacman::p_load(
  "dplyr", # Data Manipulation
  "peacesciencer", # Conflict Data Sets
  "readxl", # Reading Excel Files
  install = FALSE
)

# Variables to Collect
#   - Deaths (UCDP Georeferenced Events Dataset) 
#   - UN PKOs (Kathman 2013)
#   - War Duration (UCDP Armed Conflict Dataset)
#   - Economic Development (Anders et al. 2020)
#   - Number of Military Interventions (UCDP External Support Dataset)
#   - Government Military Strength (Correlates of War) 

# Create a Country-Year Data Set (Gleditsch-Ward Codes)
states <- create_stateyears(system = "gw", subset_years = c(1946:2023)) %>%
  # Add the COW Code Identifier to Cases Where GW Codes Do Not Align with COW Codes
  add_ccode_to_gw() %>%
  # Add Government Military Capacity
  add_nmc() %>%
  # Add GDP Data
  add_sdp_gdp() %>%
  # Add Civil War Data
  add_ucdp_acd(type = "intrastate") %>%
  # Create Conflict Duration Variable (When Conflict Starts, the Counter Starts at "1")
  group_by(gwcode) %>%
  mutate(duration = ifelse(ucdpongoing == 1, 
                           cumsum(ucdpongoing) - cumsum(lag(ucdpongoing, default = 0) == 0 & ucdpongoing == 1) + 1, 0)) %>%
  ungroup() %>%
  # Create Variable Transformations
  mutate(lmilper = log(milper + 1)) %>%
  # Rename Variables
  rename(
    "lgdppc" = "wbgdppc2011est"
  ) %>%
  # Filter Only Conflict and Post-Conflict Cases
  group_by(gwcode) %>%
  mutate(ever_conflict = ifelse(cumsum(duration == 1) >= 1, 1, 0)) %>%
  ungroup() %>%
  filter(ever_conflict == 1) %>%
  # Keep Selected Variables
  select(gwcode, ccode, statename, year, lgdppc, lmilper, duration)

# Load and Clean UCDP GED Data
ged <- read.csv("Data/GEDEvent_v24_1.csv")

ged <- ged %>%
  # Filter for Civil Conflicts
  filter(is.na(gwnob)) %>%
  # Create Government, Rebel, and Civilian Deaths Counts
  mutate(gwcode = as.numeric(gwnoa)) %>%
  mutate(
    gov_deaths = ifelse(startsWith(side_a, "Government"), deaths_a, 0),
    reb_deaths = case_when(
      type_of_violence == 1 ~ deaths_b,
      type_of_violence == 2 ~ deaths_a + deaths_b,
      type_of_violence == 3 & !startsWith(side_a, "Government") ~ deaths_a,
      TRUE ~ 0
    )
  ) %>%
  # Collapse to the Country-Year Level
  group_by(gwcode, year) %>%
  summarise(
    gov_deaths = sum(gov_deaths),
    reb_deaths = sum(reb_deaths),
    civ_deaths = sum(deaths_civilians),
    overall_deaths = sum(best)
  ) %>%
  # Create Lagged Deaths Variable
  mutate(
    lag_gov_deaths = lag(gov_deaths, n = 1),
    lag_reb_deaths = lag(reb_deaths, n = 1),
    lag_civ_deaths = lag(civ_deaths, n = 1),
    lag_overall_deaths = lag(overall_deaths, n = 1)
  ) %>%
  ungroup()

# Load and Clean UCDP External Support Data Set
external <- read_excel("Data/ucdp-esd-ay-181.xlsx")

external <- external %>%
  # Only Look at Civil Wars
  filter(civil == 1) %>%
  # Only Look at External State Support
  filter(ext_sup_s == 1 & actor_nonstate == 0) %>%
  # Only Look at Military Troop Support
  filter(ext_x_s == 1 | ext_p_s == 1) %>% 
  # Select Variables of Interest
  select(location, gwno_a, year, actor_name, ext_x_s, ext_id, ext_x_count_s, ext_p_s, ext_p_count_s) %>%
  # Collapse to the Country-Year Level
  group_by(gwno_a, year) %>%
  summarise(
    external_troop_presence = max(ext_x_s, na.rm  = TRUE),
    external_troop_count = max(ext_x_count_s, na.rm  = TRUE),
    external_unclear_troop_presence = max(ext_p_s, na.rm  = TRUE),
    external_unclear_troop_count = max(ext_p_count_s, na.rm  = TRUE)
  ) %>%
  ungroup() %>%
  # Rename These Variables
  rename(
    "gwcode" = "gwno_a",
  ) %>%
  # Create New Variables
  mutate(
    any_external_troops = ifelse(external_troop_presence == 1 | external_unclear_troop_presence == 1, 1, 0),
    any_count_troops_external_actors = external_troop_count + external_unclear_troop_count
  ) %>%
  select(gwcode, year, any_external_troops, any_count_troops_external_actors) %>%
  # Create a Log-Transformation of the Count Variable
  mutate(log_actor_count = log(any_count_troops_external_actors))

# Load and Clean Geo-PKO Data
pkos <- read_excel("Data/mission-month_12-2019_kathman.xlsx")

pkos <- pkos %>%
  # Replace Negative Values with NA
  mutate(troop = ifelse(troop == -999, NA, troop),
         police = ifelse(police == -999, NA, police),
         ccode = ifelse(missionccode == -999, NA, missionccode)) %>%
  # Collapse to the Country-Year Level
  group_by(ccode, year) %>%
  summarise(
    troop = sum(troop),
    police = sum(police)
  ) %>%
  ungroup() 

# Merge Data Sets
final <- left_join(states, ged, by = c("gwcode", "year")) %>%
  left_join(external, by = c("gwcode", "year")) %>%
  left_join(pkos, by = c("ccode", "year"))

# Clean Merged Data Set 
final <- final %>%
  # Replace NA Values for Merged In Data with "0"
  mutate(
    any_external_troops = ifelse(is.na(any_external_troops), 0, any_external_troops),
    any_count_troops_external_actors = ifelse(is.na(any_count_troops_external_actors), 0, any_count_troops_external_actors),
    troop = ifelse(is.na(troop), 0, troop),
    police = ifelse(is.na(police), 0, police)
  ) %>%
  # Filter the United States
  filter(gwcode != 2) %>%
  # Create Conflict IDs for Each Conflict Spell
  group_by(gwcode) %>%
  mutate(
    new_conflict = ifelse(year - lag(year, default = first(year) - 1) > 1, 1, 0),
    conflict_id = cumsum(new_conflict) + gwcode) %>%
  ungroup() %>%
  # Filter Pre-1990 Observations
  filter(year >= 1990)

# Inspect Elements of Merged Data Set
summary(final)
