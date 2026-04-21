


# Goal --------------------------------------------------------------------


# Build the price database

# Source
# From way et al
# gas
# oil
# coal
# solar
# wind




# Load --------------------------------------------------------------------


library(here)
source(here("0_set_up.R"))
source(here("0.1_functions_forecast.R"))

energy_path<-file.path(data_path,"Energy_modelling")

# Read BEIS values --------------------------------------------------------


df_beis<-read_excel(path=file.path(energy_path,  "prices_energy_BEIS_import.xlsx"))



# Read way et al ----------------------------------------------------------


read_forecast <- function(energy,Scenario_p="Fast Transition") {
  # Scenario_p must be either "No Transition"   "Fast Transition" "Slow Transition"
  forecast_path <-
    file.path(data_path, "Way_2022/Way22_forecasts")
  filepath = file.path(forecast_path, paste0("tech_forecasts_", energy, ".xlsx"))
  
  # Get sheet names
  sheet_names <- excel_sheets(filepath)
  
  if(Scenario_p %in% sheet_names){
  df <- read_excel(path = filepath,sheet=Scenario_p)%>%
    mutate(Scenario_p=Scenario_p)}else{
  df <- read_excel(path = filepath)%>%
    mutate(Scenario_p=NA)
  }
  df <-
    df %>%
    rename("var" = "...1") %>%
    gather(key = Year, value = value, -c("var","Scenario_p")) %>%
    mutate(Year = as.numeric(Year)) %>%
    arrange(var) %>%
    mutate(energy = energy) %>%
    distinct()%>%
    filter(var=="point_forecast")
}

df_forecast <-
  rbind(
    read_forecast("gas",Scenario_p="Fast Transition"),
    # read_forecast("gas",Scenario_p="No Transition"),
    # read_forecast("gas",Scenario_p="Slow Transition"),
    read_forecast("coal",Scenario_p="Fast Transition"),
    # read_forecast("coal",Scenario_p="No Transition"),
    # read_forecast("coal",Scenario_p="Slow Transition"),
    read_forecast("oil",Scenario_p="Fast Transition"),
    # read_forecast("oil",Scenario_p="No Transition"),
    # read_forecast("oil",Scenario_p="Slow Transition"),
    read_forecast("solar_electricity",Scenario_p="Fast Transition"),
    read_forecast("solar_electricity",Scenario_p="No Transition"),
    read_forecast("solar_electricity",Scenario_p="Slow Transition"),
    read_forecast("wind_electricity",Scenario_p="Fast Transition"),
    read_forecast("wind_electricity",Scenario_p="No Transition"),
    read_forecast("wind_electricity",Scenario_p="Slow Transition"),
    read_forecast("elec_storage_batt_ST_all",Scenario_p="Fast Transition"),
    read_forecast("elec_storage_batt_ST_all",Scenario_p="No Transition"),
    read_forecast("elec_storage_batt_ST_all",Scenario_p="Slow Transition"),
    read_forecast("PEM_electrolysers",Scenario_p="Fast Transition"),
    read_forecast("PEM_electrolysers",Scenario_p="No Transition"),
    read_forecast("PEM_electrolysers",Scenario_p="Slow Transition")
  )%>%
  mutate(energy=ifelse(energy=="elec_storage_batt_ST_all","storage",energy),
         energy=ifelse(energy=="coal","solid",energy),
         energy=ifelse(energy=="solar_electricity","solar",energy),
         energy=ifelse(energy=="wind_electricity","wind",energy))



###
# Conversion
###
# Source: IEA https://www.iea.org/data-and-statistics/data-tools/unit-converter
# Coal : # Convert from USD/metric ton to USD/MWh # 1 ton of coal equivalent is 8.141 megawatt hours (MWh)
# Gas: Convert from USD/MMBtu ton to USD/MWh # 1 million Btu is 293.1 kilowatt hours (kWh), hence 0.2931 MWh
# Oil Convert from USD/bbl ton to USD/MWh # 1 bbl is 1.7 megawatt hours (MWh), 1.7 MWh

# Table S25 of Way et al, SI, p111
# bbl: 1bbl=1boe=6.12 GJ & 1 MWh = 3.6 GJ => 6.12 GJ = 1.7 MWh => $/bbl => 1/1.7 $/MWh
# coal: 30GJ=metric ton & 1 MWh = 3.6 GJ =>30 GJ = 8.57 MWh => $/metric => 1/8.57 $/MWh
# gas: 1 MMBtu = 1.05505585 GJ & 1 MWh = 3.6 GJ =>1.05505585 GJ = 0.2931 MWh => $/metric => 1/0.2931 $/MWh

x_conversion_forecasts <-
  data.frame(
    energy = unique(df_forecast$energy),
    efficiency_factor = c(1 / 0.2931, # gas
                          1 / 8.141, # coal
                          1 / 1.7, # oil
                          1, # solar
                          1, # wind
                          1, # batteries
                          1) # H2
  ) 

df_forecast <-
  df_forecast %>%
  filter(var=="point_forecast")%>%
  left_join(x_conversion_forecasts, by = "energy") %>%
  mutate(value = value * efficiency_factor)


###
# Storage
###


# “usable kWh of battery storage means that round trip efficiency and depth of discharge are accounted
# for in the price of the battery pack in dollars per kWh
  df_forecast%>%
  filter(energy=="storage")%>%
  filter(var=="point_forecast")%>%
  mutate(value=value*10^3/365*0.1)%>% #0.1 is the annualisation, from WAY et al, since it's only capital cost
  ggplot(aes(x=Year,y=value,colour=Scenario_p))+
  geom_line()+
  theme_minimal()
  
  # raw price is USD/kWh
  # I need price in USD/MWh
  df_forecast<-
    df_forecast%>%
    mutate(value=ifelse(energy=="storage" &
                        var=="point_forecast",
           value*10^3*0.13/365,value)) #0.1 is the annualisation, from WAY et al, since it's only capital cost
  # We transform it into capacity by dividing storage MWh by 365

  # p = price capacity kWh
  # To keep same cost
  # if C is the capacity and E is the storage quantity over the year (30% of generation)
  # pC=p*E/365=p/365*E
###
# hydrogen
###


df_hydrogen<-
  df_forecast%>%       
  filter(energy=="PEM_electrolysers" & 
           var=="point_forecast")%>%
  mutate(energy="hydrogen")%>%
  arrange(Year)%>%
  # division by 8760=24*365 : EJ/year => EJ/hour
  # 0.5 is the average utilisation rate
  # 0.7 is the efficiency of the electrolyser to transform 1 kWh of electricity into 0.7 kWh of Hydrogen
  # CRF (Capital recovery factor=11%)
  # 1000: transforming kw into MW to get USD/MWh
  mutate(value=value*10^3/(8760*0.5*0.7)*0.11)
  # mutate(log_growth_rate=log(value)-log(lag(value)))%>%
  # filter(!is.na(log_growth_rate))%>%
  # mutate(value=initial_hydrogen_cost$USD_2020_MWh)%>%
  # ungroup()%>%
  # mutate(value = value * exp(cumsum(log_growth_rate)))


df_forecast%>%
  filter(energy=="PEM_electrolysers" & 
           var=="point_forecast")%>%
  ggplot(aes(x=Year,y=value,colour=Scenario_p))+
  # facet_wrap(~Scenario_p)+
  geom_line()


df_hydrogen%>%
  ggplot(aes(x=Year,y=value,colour=Scenario_p))+
  geom_line()

# Hydrogen electricity
# Convert fuel cost into electricity cost using a 50% conversion efficiency
# and add a fixed plant-cost adder based on the BEIS CCGT benchmark.

thermal_power_efficiency <- 0.5
thermal_power_plant_cost_usd_mwh <- 14

df_elec_hydrogen <-
  df_hydrogen %>%
  mutate(energy = "elec_hydrogen") %>%
  mutate(value = value / thermal_power_efficiency + thermal_power_plant_cost_usd_mwh)


df_elec_hydrogen%>%
  ggplot(aes(x=Year,y=value,colour=Scenario_p))+
  # facet_wrap(~Scenario_p)+
  geom_line()


# # Way et al gives the cost of PEM electrolysis
# # We use the initial cost of MWH of H2 in UK (2025), see appendix of the paper
# # We apply the same decrease in the cost of a MWh as the cost of electroslysis 
# initial_hydrogen_cost<-
#   df_beis%>%
#   filter(technology_code=="hydrogen" & Horizon==2020)%>%
#   select(USD_2020_MWh)
# 
# ###
# # How Way et al wanted us to compute
# # price = Quantity * price electrolyser * CRF (Capital recovery factor=11%)
# # Price in MWh annualised
# df_forecast%>%       
#   filter(energy=="PEM_electrolysers" & 
#            var=="point_forecast", 
#          Scenario_p=="Fast Transition")%>%
#   mutate(value=value*0.11*10^3)%>%
#   ggplot(aes(x=Year,y=value))+
#   geom_line()
# ###
# non-bio waste energy
###
# linear decrease from 84.9 USD to 64.4 USD
nonbiowaste_cost<-
  df_beis%>%
  filter(technology_code=="non_bio_waste")%>%
  select(Horizon,USD_2020_MWh)%>%
  mutate(Horizon=paste0("Y",Horizon))%>%
  spread(key=Horizon,value=USD_2020_MWh)



df_non_bio_waste<-
  data.frame("Year"=seq(2020,2050),
        "value"=seq(nonbiowaste_cost$Y2020,nonbiowaste_cost$Y2050,length.out=31),
        "energy"="non_bio_waste")



# BEIS values -------------------------------------------------------------
# see table in the paper for the various sources
# in 2020 USD/MWh

# Constant cost
c_techno<-
    c("elec_unabated_gas",
    "elec_gas_CCS",
    "elec_biomass",
    "elec_biomass_CCS",
    "Nuclear",
    "thermal_low_carbon",
    "elec_other_gasCHP",
    "elec_other_hydro",
    "elec_coal",
    "bioenergy"
  )


df_forecast_beis<-
  crossing(data.frame("Year"=seq(2020,2050)),
           df_beis%>%
             filter(technology_code %in% c_techno)%>%
             select(technology_code,USD_2020_MWh))%>%
   rename("value"="USD_2020_MWh",
          "energy"="technology_code")%>%
  arrange(energy)
  


# Build price database ----------------------------------------------------

columns<-c("Year","energy","value")

df_prices_energy<-
  rbind(df_forecast%>%select(all_of(columns),Scenario_p),
        df_hydrogen%>%select(all_of(columns),Scenario_p),
        df_elec_hydrogen%>%select(all_of(columns),Scenario_p),
        rbind(df_non_bio_waste%>%select(all_of(columns)),
        df_forecast_beis)%>%
          mutate(Scenario_p=NA))


#  Get the unique values of Scenario_p (non-NA)
scenario_vals <- df_prices_energy %>% 
  filter(!is.na(Scenario_p)) %>% 
  distinct(Scenario_p)

# Separate rows with and without Scenario_p
df_na <- df_prices_energy %>% filter(is.na(Scenario_p))
df_non_na <- df_prices_energy %>% filter(!is.na(Scenario_p))

# Expand df_na with all possible Scenario_p values
df_expanded <- df_na %>% 
  select(-Scenario_p) %>% 
  crossing(scenario_vals)

#  Combine the original non-NA rows with the expanded ones
df_prices_energy <- bind_rows(df_non_na, df_expanded)

# replace spaces by _
df_prices_energy <- df_prices_energy %>%
  mutate(Scenario_p = str_replace_all(Scenario_p, " ", "_"))

save(df_prices_energy,file=file.path(dataset_path,"df_prices_energy.RData"))
  
# 
# df_duplicates <- df_prices_energy %>%
#   add_count(Year, energy,value,Scenario_p) %>%
#   filter(n > 1) %>%
#   select(-n)
# df_duplicates

# For comparison with Way et al
# Recompute to check
# BEIS gas CGGT plant 11£/MWh: 14.1 2020 USD/MWh
df_gas_elec_recomputed <-
  df_forecast %>%
  filter(energy == "gas") %>%
  mutate(energy = "gas_elec") %>%
  mutate(value = value / thermal_power_efficiency + thermal_power_plant_cost_usd_mwh) %>%
  filter(var == "point_forecast")

    


# Plot --------------------------------------------------------------------

df_prices_energy%>%
  filter(Year<=2050)%>%
  ggplot(aes(x=Year,y=value,colour=Scenario_p))+
  geom_line(lwd=1)+
  facet_wrap(~energy,scales="free")+
  theme_minimal()
  


