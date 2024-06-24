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
#   - UN PKOs (Geocoded Peacekeeping Operations)
#   - War Duration (UCDP Armed Conflict Dataset)
#   - Economic Development (Anders et al. 2020)
#   - Number of Military Interventions (UCDP External Support Dataset)
#   - Type of Conflict (UCDP Georeferenced Events Dataset)
#   - Government Military Strength (Correlates of War)
#   - Rebel Military Strength (UCDP Non-State Actor Dataset)

# Create a Country-Year Data Set (Gleditsch-Ward Codes)
states <- create_stateyears(system = "gw", subset_years = c(1946:2023)) %>%
  # Add the COW Code Identifier to Cases Where GW Codes Do Not Align with COW Codes
  add_ccode_to_gw() %>%
  # Add Government Military Capacity
  add_nmc() %>%
  # Add Peace Years to Determine Spells of Conflict and Peace
  add_spells() %>%
  # Add GDP Data
  add_sdp_gdp() %>%
  # Add Civil War Data
  add_ucdp_acd(type = "intrastate") %>%
  # Keep Selected Variables
  select(-c(milex, irst, pec, upop, cinc, wbgdp2011est, wbpopest, sdpest)) %>%
  # Create Conflict Duration Variable (When Conflict Starts, the Counter Starts at "1")
  group_by(gwcode) %>%
  mutate(duration = ifelse(ucdpongoing == 1, 
                           cumsum(ucdpongoing) - cumsum(lag(ucdpongoing, default = 0) == 0 & ucdpongoing == 1) + 1, 0)) %>%
  ungroup() %>%
  # Create Variable Transformations
  mutate(lpop = log(tpop),
         lmilper = ifelse(milper < 1, log(milper + 1), log(milper))) %>%
  # Rename Variables
  rename(
    "lgdppc" = "wbgdppc2011est"
  )

# Load and Clean UCDP GED Data
ged <- read.csv("Data/")

# Load and Clean UCDP Battle-Related Deaths Data
battle_deaths <- read.csv("Data/BattleDeaths_v24_1_conf.csv")

battle_deaths <- battle_deaths %>%
  # Only Look at Civil Wars
  filter(type_of_conflict >= 3) %>%
  # Select Variables of Interest
  select(gwno_a, year, incompatibility, bd_best) %>%
  # Collapse to the Country-Year Level
  group_by(gwno_a, year) %>%
  summarise(
    incompatibility = max(incompatibility),
    bd_best = max(bd_best)
  ) %>%
  ungroup() %>%
  # Create a Conflict Over the Government Dummy
  mutate(gov_conflict = ifelse(incompatibility >= 2, 1, 0)) %>%
  select(-incompatibility) %>%
  # Create a Lag Deaths Variable
  arrange(gwno_a, year) %>%
  group_by(gwno_a) %>%
  mutate(lag_battle_deaths = lag(bd_best, n=1),
         lag_year = lag(year, n = 1),
         # Replace Values with NA If the Prior Year Observation Is More Than 1 Year Apart
         lag_battle_deaths = ifelse(year - lag_year > 1, NA, lag_battle_deaths)) %>%
  select(-lag_year) %>%
  ungroup() %>%
  # Rename These Variables
  rename(
    "gwcode" = "gwno_a",
    "battle_deaths" = "bd_best"
  ) %>%
  mutate(gwcode = as.numeric(gwcode))

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
geo_pko <- read_excel("Data/geo_pko_v.2.1.xlsx")

geo_pko <- geo_pko %>%
  # Replace "Unknown" for UN Troops Count with NAs
  mutate(no.troops = ifelse(no.troops == "unknown", NA, no.troops)) %>%
  # Collapse to Country-Year Level
  mutate(gwcode = as.numeric(gnwo),
         no.tropps = as.numeric(no.troops)) %>%
  group_by(gwcode, year) %>%
  summarise(
    un_troops = as.numeric(max(no.troops, na.rm = TRUE))
  ) %>%
  ungroup() %>%
  mutate(un_pko = ifelse(un_troops > 0, 1, 0))

# Merge Data Sets
final <- left_join(states, battle_deaths, by = c("gwcode", "year")) %>%
  left_join(external, by = c("gwcode", "year")) %>%
  left_join(geo_pko, by = c("gwcode", "year"))

# Clean Merged Data Set
final <- final %>%
  # Remove All Observations with NA for the Outcome (Violence)
  filter(!is.na(battle_deaths)) %>%
  # Replace NA Values for Merged In Data with "0"
  mutate(
    any_external_troops = ifelse(is.na(any_external_troops), 0, any_external_troops),
    any_count_troops_external_actors = ifelse(is.na(any_count_troops_external_actors), 0, any_count_troops_external_actors),
    un_troops = ifelse(is.na(un_troops), 0, un_troops),
    un_pko = ifelse(is.na(un_pko), 0, un_pko)
  ) %>%
  # Filter the United States
  filter(gwcode != 2) %>%
  # Create Conflict IDs for Each Conflict Spell (Remove the Old ID)
  select(-conflict_ids) %>%
  group_by(gwcode) %>%
  mutate(
    new_conflict = ifelse(year - lag(year, default = first(year) - 1) > 1, 1, 0),
    conflict_id = cumsum(new_conflict) + gwcode) %>%
  ungroup()

# Inspect Elements of Merged Data Set
summary(final)