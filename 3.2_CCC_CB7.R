# 
# We can find the data for 
# 
# Cost: additional capital expenditure (adjusted) 
# 
# Cost: additional operating expenditure (adjusted) 
# 
# Cost: additional capital expenditure (unadjusted) 
# 
# Cost: additional operating expenditure (unadjusted) 
# 
# Cost: additional capital expenditure annualised (adjusted) 
# 
# in Raw data
# # For instance, row 1796
# # scenario |	country|	variable|	variable_unit|	year|	value
# # Balanced Pathway|	United Kingdom|	Cost: additional capital expenditure annualised (adjusted)	|£m|	2025	|2228.472513
# 



# Set-up ------------------------------------------------------------------


library(here)

# Load libraries and functions
source(here("0_set_up.R"))
source(here("0.1_functions_forecast.R"))

graph_path<-here("Graphs/CCC")
tables_path<-here("Tables/CCC")





### ******************************************* 
# Read CCC
### ******************************************* 

# file: The-Seventh-Carbon-Budget-full-dataset.xlsx
# link: https://www.theccc.org.uk/wp-content/uploads/2025/02/The-Seventh-Carbon-Budget-full-dataset.xlsx
# Tab: Economy-wide data
# Range: A2:F2549
# scenario: all
# Tab: Sector-level data
# Range: A2:G20385
# scenario: all

# read
raw_ccc_economy_wide<-
  read_excel(path=file.path(base_path,"Data/CCC_CB7/The-Seventh-Carbon-Budget-full-dataset.xlsx"),sheet="Economy-wide data")
raw_ccc_sector<-
  read_excel(path=file.path(base_path,"Data/CCC_CB7/The-Seventh-Carbon-Budget-full-dataset.xlsx"),sheet="Sector-level data")


### ******************************************* 
# Prepare data 
### ******************************************* 
# 
# Cost: additional capital expenditure (adjusted)
# Cost: additional capital expenditure (unadjusted)
# Cost: additional capital expenditure annualised (adjusted)
# Cost: additional operating expenditure (adjusted)
# Cost: additional operating expenditure (unadjusted)


df_cost_sector<-
  raw_ccc_sector%>%
  filter(year>=2020)%>%
  filter(country=="United Kingdom")%>%
  filter(variable %in% c("Cost: additional capital expenditure (adjusted)",
                      "Cost: additional capital expenditure (unadjusted)",
                      "Cost: additional capital expenditure annualised (adjusted)",
                      "Cost: additional operating expenditure (adjusted)",
                      "Cost: additional operating expenditure (unadjusted)"))%>%
  mutate(variable = dplyr::recode(variable,
                           "Cost: additional capital expenditure (adjusted)" = "capex_adj",
                           "Cost: additional capital expenditure (unadjusted)" = "capex_unadj",
                           "Cost: additional capital expenditure annualised (adjusted)" = "capex_ann_adj",
                           "Cost: additional operating expenditure (adjusted)" = "opex_adj",
                           "Cost: additional operating expenditure (unadjusted)" = "opex_unadj"
  )) %>%
  
  pivot_wider(
    names_from = variable,
    values_from = value
  ) %>%
  
  mutate(
    annual_resource  = opex_adj + capex_ann_adj
  )%>%
  pivot_longer(
    cols = c(capex_adj, capex_unadj,
             capex_ann_adj,
             opex_adj, opex_unadj,
             annual_resource),
    names_to = "variable",
    values_to = "value"
  )%>%
  mutate(year=as.numeric(year))


df_annual_resource <- 
  df_cost_sector%>%
  # Using the annualised resource cost
  filter(variable == "annual_resource") 



### for the economy_wide
df_cost_economy_wide<-
  raw_ccc_economy_wide%>%
  filter(year>=2020)%>%
  filter(country=="United Kingdom")%>%
  filter(variable %in% c("Cost: additional capital expenditure (adjusted)",
                         "Cost: additional capital expenditure annualised (adjusted)",
                         "Cost: additional operating expenditure (adjusted)"))%>%
  mutate(variable = dplyr::recode(variable,
                           "Cost: additional capital expenditure (adjusted)" = "capex_adj",
                           "Cost: additional capital expenditure annualised (adjusted)" = "capex_ann_adj",
                           "Cost: additional operating expenditure (adjusted)" = "opex_adj"
  )) %>%
  
  pivot_wider(
    names_from = variable,
    values_from = value
  ) %>%
  
  mutate(
    annual_resource  = opex_adj + capex_ann_adj,
    annual_resource_fin  = opex_adj + capex_ann_adj+capex_ann_adj*0.0035
  )%>%
  pivot_longer(
    cols = c(capex_adj,
             capex_ann_adj,
             opex_adj,
             annual_resource,
             annual_resource_fin),
    names_to = "variable",
    values_to = "value"
  )%>%
  mutate(year=as.numeric(year))



### *******************************************
# Compute productivity
### *******************************************

# From annualised cost of the transition by sector, we compute aggregate TFP changes
# by three approaches providing the same results
# Approach 1: computes directly the TFP changes from 2020 to 2050
# Approach 2: provides the year-in-year changes in TFP
# Approach 3: using the economy-wide data directly

# Units: results are in pp

###
# Approach 1: 2020-2050 TFP (aggregated)
###
sum_TFP_annual<-
  df_annual_resource%>%
  filter(year==2050)%>%
  arrange(scenario,sector,year)%>%
  group_by(scenario,sector)%>%
  # sectoral TFP changes
  summarise(contrib_Ahat=value*(zeta-1)/GDP_UK)%>%
  # Aggregate TFP changes (summed across sectors)
  group_by(scenario)%>%
  summarise(agg_Ahat=sum(contrib_Ahat))%>%
  mutate(agg_Ahat=transform_bp_into_pp(agg_Ahat))

###
# Approach 2: 2020-2050 yearly changes in TFP
###

# We add 2019 as a base year (cost = 0 for all sectors)
# Use distinct() across ALL sector-scenario combinations (not just those with year==2020),
# so sectors whose data starts after 2020 still get a zero base and telescope correctly
df_annual_resource_incl2019<-
  rbind(df_annual_resource,
        df_annual_resource%>%
          distinct(scenario, sector, country, variable, variable_unit)%>%
          mutate(value=0,
                 year=2019))%>%
  # order by year
  arrange(scenario,sector,year)

# Computing sectoral TFP changes year by year
cumsum_TFP_annual_sector<-
  df_annual_resource_incl2019 %>%
  group_by(sector,scenario) %>%
  # Computing the yearly changes in annualised resource cost
  mutate(delta_annual_resource=value-dplyr::lag(value))%>%
  # filter out the NA delta (2019-2020)
  filter(!is.na(delta_annual_resource))%>%
  # compute the cumulative log TFP change (raw, before exp transform)
  mutate(cumAhat_raw=cumsum(delta_annual_resource*(zeta-1))/GDP_UK)%>%
  # sector-level percentage-point TFP (for plots)
  mutate(cumAhat=transform_bp_into_pp(cumAhat_raw))

# Computing aggregate TFP changes year by year (cumulatively)
# sum the raw log values first, then apply transform once
# (avoids Jensen's inequality issue: exp(sum) != sum(exp))
cumsum_TFP_annual<-
  cumsum_TFP_annual_sector%>%
  group_by(year,scenario)%>%
  reframe(cumAhat_agg=transform_bp_into_pp(sum(cumAhat_raw)),
          cumAhat_agg_raw=sum(cumAhat_raw))

###
# Check that the two approaches give the same number
####
cumsum_TFP_annual%>%
  filter(year==2050)%>%
  left_join(sum_TFP_annual)%>%
  mutate(check=agg_Ahat/cumAhat_agg)
# it checks out, the col check is 1

###
# Approach 3: 2020-2050 yearly changes in TFP using economy-wide data
###

df_annual_resource_ew <-
  df_cost_economy_wide %>%
  filter(variable == "annual_resource")

# We add 2019 as a base year (annual resource cost = 0)
# Use distinct() across all scenario combinations (not just those with year==2020)
df_annual_resource_ew_incl2019 <-
  rbind(df_annual_resource_ew,
        df_annual_resource_ew %>%
          distinct(scenario, country, variable, variable_unit) %>%
          mutate(value = 0,
                 year = 2019)) %>%
  arrange(scenario, year)

# Computing aggregate TFP changes year by year (cumulatively) from economy-wide data
cumsum_TFP_annual_ew <-
  df_annual_resource_ew_incl2019 %>%
  group_by(scenario) %>%
  mutate(delta_annual_resource = value - dplyr::lag(value)) %>%
  filter(!is.na(delta_annual_resource)) %>%
  mutate(cumAhat_ew = cumsum(delta_annual_resource * (zeta - 1)) / GDP_UK) %>%
  mutate(cumAhat_ew = transform_bp_into_pp(cumAhat_ew))

###
# Check that approach 3 gives the same number as approaches 1 and 2
###
cumsum_TFP_annual_ew %>%
  filter(year == 2050) %>%
  left_join(cumsum_TFP_annual %>% mutate(year=as.numeric(year)) %>% filter(year == 2050)) %>%
  mutate(check = cumAhat_agg / cumAhat_ew) %>% t()
# Note: if check != 1, the economy-wide tab and sector-level tab are inconsistent in the source data

###
# Quick financing-cost approximation
###

# Quick shortcut: treat financing cost as an extra annual operating-cost flow
# equal to financing_rate * base annual cost. By default this is applied to the
# annual resource series; if you want a capital-only base, switch to
# financing_cost_base_var <- "capex_ann_adj".
df_annual_resource_ew_fin <-
  df_cost_economy_wide %>%
  filter(variable == "annual_resource_fin")

# We add 2019 as a base year (annual resource cost = 0)
# Use distinct() across all scenario combinations (not just those with year==2020)
df_annual_resource_ew_fin_incl2019 <-
  rbind(df_annual_resource_ew_fin,
        df_annual_resource_ew_fin %>%
          distinct(scenario, country, variable, variable_unit) %>%
          mutate(value = 0,
                 year = 2019)) %>%
  arrange(scenario, year)

# Computing aggregate TFP changes year by year (cumulatively) from economy-wide data
cumsum_TFP_annual_ew_fin <-
  df_annual_resource_ew_fin_incl2019 %>%
  group_by(scenario) %>%
  mutate(delta_annual_resource = value - dplyr::lag(value)) %>%
  filter(!is.na(delta_annual_resource)) %>%
  mutate(cumAhat_ew_fin = cumsum(delta_annual_resource * (zeta - 1)) / GDP_UK) %>%
  mutate(cumAhat_ew_fin = transform_bp_into_pp(cumAhat_ew_fin))

cumsum_TFP_annual_ew_fin%>%
  filter(year==2050)

###
#  confirm whether raw cost data are the same at the aggregate and sectoral level
###
df_annual_resource %>%
  filter(year == 2050) %>%
  group_by(scenario) %>%
  summarise(sector_sum_m = sum(value)) %>%
  left_join(
    df_annual_resource_ew %>%
      filter(year == 2050) %>%
      select(scenario, ew_value = value)
  ) %>%
  mutate(gap_m       = sector_sum_m - ew_value,
         ratio       = sector_sum_m / ew_value)
# 

# All three approaches give the same number: 0.119 for Balanced Net Zero. 



cumsum_TFP_annual_sector_CB7<-cumsum_TFP_annual_sector
save(cumsum_TFP_annual_sector_CB7,file=file.path(dataset_path,"cumsum_TFP_annual_sector_CB7.RData"))


cumsum_TFP_annual_CB7<-cumsum_TFP_annual
save(cumsum_TFP_annual_CB7,file=file.path(dataset_path,"cumsum_TFP_annual_CB7.RData"))

if (exists("cumsum_TFP_financing_quick_sector")) {
  cumsum_TFP_financing_quick_sector_CB7 <- cumsum_TFP_financing_quick_sector
  save(
    cumsum_TFP_financing_quick_sector_CB7,
    file = file.path(dataset_path, "cumsum_TFP_financing_quick_sector_CB7.RData")
  )
}


if (exists("cumsum_TFP_financing_quick")) {
  cumsum_TFP_financing_quick_CB7 <- cumsum_TFP_financing_quick
  save(
    cumsum_TFP_financing_quick_CB7,
    file = file.path(dataset_path, "cumsum_TFP_financing_quick_CB7.RData")
  )
}

compute_cb7_financing_effect <- function(financing_rate) {
  df_financing_rate <-
    df_cost_economy_wide %>%
    filter(variable %in% c("opex_adj", "capex_ann_adj")) %>%
    select(scenario, country, year, variable, value) %>%
    tidyr::pivot_wider(names_from = variable, values_from = value) %>%
    mutate(
      value = opex_adj + (1 + financing_rate) * capex_ann_adj,
      variable = paste0("annual_resource_fin_", financing_rate)
    ) %>%
    select(scenario, country, variable, year, value)

  df_financing_rate_incl2019 <-
    rbind(
      df_financing_rate,
      df_financing_rate %>%
        distinct(scenario, country, variable) %>%
        mutate(value = 0, year = 2019)
    ) %>%
    arrange(scenario, year)

  df_financing_rate_incl2019 %>%
    group_by(scenario) %>%
    mutate(delta_annual_resource = value - dplyr::lag(value)) %>%
    filter(!is.na(delta_annual_resource)) %>%
    mutate(cumAhat_agg = cumsum(delta_annual_resource * (zeta - 1)) / GDP_UK) %>%
    filter(year == 2050) %>%
    transmute(
      financing_rate = financing_rate,
      scenario,
      aggregate_effect_pp = 100 * cumAhat_agg
    ) %>%
    ungroup()
}

ccc_cb7_financing_summary <- bind_rows(
  lapply(c(0, 0.035, 0.15), compute_cb7_financing_effect)
)

save(
  ccc_cb7_financing_summary,
  file = file.path(dataset_path, "paper_text_ccc_cb7_financing_numbers.RData")
)



# Plot --------------------------------------------------------------------


### ******************************************* 
# Plot productivity Balanced Net Zero
### ******************************************* 


g_cumAhat_CCC_CB7_sector_BalancedPathway<-
  cumsum_TFP_annual_sector%>%
  left_join(cumsum_TFP_annual)%>%
  ungroup()%>%
  ggplot(aes(x=year))+
  facet_wrap(~scenario)+
  geom_line(aes(y=cumAhat,colour=sector),lwd=0.9)+
  geom_line(aes(y=cumAhat_agg),colour="black",lwd=1, linetype = "dashed")+
  scale_colour_manual(values = unname(sectors_palette)) + # Enhanced color palette
  labs(
    # title = "Cumulative Ahat over years by Sector and Aggregate",
    x = "",
    y = "Cumulative TFP growth (pp)",
    colour = "",
    caption="Source: CCC CB7 Balanced Pathway") + 
  # mytheme+
  theme_minimal() + # Cleaner theme
  theme(
    legend.position = c(0.05, 0.55), # Bottom-left corner
    legend.justification = c(0, 0), # Align legend to bottom-left
    # legend.box.background = element_rect(color = "black", fill = "white", size = 0.5, linetype = "solid"), # Box around legend
    legend.spacing.y = unit(0, "pt"), # Reduce space between legend items
    legend.spacing.x = unit(0, "pt"), # Reduce horizontal space
    legend.margin = margin(t = -5, r = 5, b = 5, l = 5), # Remove top margin
    legend.text = element_text(size = 10), # Small font size for compactness
    strip.text = element_text(size = 12, face = "bold"),
    axis.text =  element_text(size = 10)
  ) +
  # guides(colour = guide_legend(ncol = 2)) + 
  geom_text(data=cumsum_TFP_annual%>%filter(year==2040 & scenario=="Balanced Pathway"),
            aes(x = 2041, y = (cumAhat_agg)-0.0005, label = "Aggregate"),colour="black", size = 3, vjust = -0.5,fontface="plain") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))


g_cumAhat_CCC_CB7_sector_BalancedPathway+
  canvas(9,8,unit="in")

###
# Save
###
save_graph("g_cumAhat_CCC_CB7_sector_BalancedPathway",width=9,height=8)


# other version for slide
g_cumAhat_CCC_CB7_sector_BalancedPathway_beamer<-
  g_cumAhat_CCC_CB7_sector_BalancedPathway+
  theme_minimal() + # Cleaner theme
  theme(
    legend.position = "right", 
    strip.text = element_text(size = 0, face = "bold"),
    axis.text =  element_text(size = 10)
  )
g_cumAhat_CCC_CB7_sector_BalancedPathway_beamer+
  canvas(9,5,unit="in")
save_graph("g_cumAhat_CCC_CB7_sector_BalancedPathway",width=9,height=5)


### just electricity

g_cumAhat_CCC_CB7_elec<-
  cumsum_TFP_annual_sector%>%
  left_join(cumsum_TFP_annual)%>%
  # filter(scenario=="Balanced Pathway")%>%
  filter(sector=="Electricity supply")%>%
  ggplot(aes(x=year))+
  facet_wrap(~scenario,scales="free_x")+
  geom_line(aes(y=cumAhat),lwd=0.9,colour="#8da0cb")+
  geom_line(aes(y=cumAhat_agg),colour="black",lwd=1, linetype = "dashed")+
  labs(
    # title = "Cumulative Ahat over years by Sector and Aggregate",
    x = "",
    y = "Cumulative TFP growth (pp)",
    colour = "") + 
  # mytheme+
  theme_minimal() + # Cleaner theme
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text =  element_text(size = 10)
  ) +
  # guides(colour = guide_legend(ncol = 2)) + 
  geom_text(data=cumsum_TFP_annual%>%filter(year==2048 & scenario=="Balanced Pathway"),
            aes(x = 2048, y = (cumAhat_agg)-0.001, label = "Aggregate"),colour="black", size = 3, vjust = -0.5,fontface="plain") +
  geom_text(data=cumsum_TFP_annual_sector%>%filter(sector=="Electricity supply" & year==2048 & scenario=="Balanced Pathway"),
            aes(x = 2045, y = (cumAhat)-0.0015, label = "Electricity supply"),colour="#8da0cb", size = 3, vjust = -0.5,fontface="plain") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))

g_cumAhat_CCC_CB7_elec+
  canvas(9,5,unit="in")

save_graph("g_cumAhat_CCC_CB7_elec",width=9,height=5)

# table
tab_elec_ccc_UK<-cumsum_TFP_annual_sector%>%
  filter(sector=="Electricity supply"
         & year==2050)%>%
  select(scenario,cumAhat)
write_xlsx(tab_elec_ccc_UK,path=file.path(tables_path,"tab_elec_ccc_CB7_UK.xlsx"))



### ******************************************* 
# 2020-2050  waterfall chart
### ******************************************* 

df_waterfall_CCC_CB7_ordered <-
  cumsum_TFP_annual_sector %>%
  filter(year == 2050) %>%
  select(scenario, sector, cumAhat) %>%
  rename("value" = "cumAhat") %>%
  ungroup() %>%
  group_by(scenario) %>%
  # Reorder sectors within each scenario based on value
  arrange(desc(value)) %>%
  mutate(sector_reordered = factor(sector, levels = sector)) %>%
  # Create a sequential x-position variable (within each sector)
  mutate(x = row_number()) %>%
  # Calculate cumulative sums for start and end positions
  mutate(end = cumsum(value)) %>%
  mutate(start = lag(end, default = 0))


g_waterfall_CCC_CB7_ordered <-
  df_waterfall_CCC_CB7_ordered %>%
  mutate(scenario=paste0("CB7 - ",scenario))%>%
  ggplot(aes(x = x, fill = sector)) +
  facet_wrap(~scenario, scales = "free_x") +
  geom_rect(aes(
    xmin = x - 0.45,
    xmax = x + 0.45,
    ymin = start,
    ymax = end
  )) +
  mytheme +
  scale_fill_manual(values = unname(c(sectors_palette, "#404040"))) +
  labs(y = "Productivity growth (pp)", x = "", fill = "") +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank()
  ) +
  guides(fill = guide_legend(ncol = 2)) +
  theme(
    legend.position = "right",
    # legend.justification = c(0, 0),
    # legend.spacing.y = unit(0, "pt"),
    # legend.spacing.x = unit(0, "pt"),
    # legend.margin = margin(t = -5, r = 5, b = 5, l = 5),
    legend.text = element_text(size = 7)
  )+
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))

g_waterfall_CCC_CB7_ordered +
  canvas(7, 3, "in")
save_graph("g_waterfall_CCC_CB7_ordered", width=7, height=3)

df_wf_plot <-
  df_waterfall_CCC_CB7_ordered %>%
  mutate(
    scenario = paste0("CB7 - ", scenario)
  ) %>%
  group_by(scenario) %>%
  mutate(
    y_lab_below = pmin(start, end) - 0.0001,
    y_lab_inside = (start + end) / 2
  ) %>%
  ungroup()



g_waterfall_CCC_CB7_labelled <-
  ggplot(df_wf_plot, aes(x = x, fill = sector)) +
  facet_wrap(~scenario, scales = "free_x") +
  # geom_hline(yintercept = 0.001,colour="grey30",linetype="dashed")+
  geom_rect(aes(
    xmin = x - 0.45,
    xmax = x + 0.45,
    ymin = start,
    ymax = end
  )) +
  geom_text(
    data = \(d) d %>% filter(!sector %in%  c("Surface transport","Residential buildings")),
    aes(x = x, y = y_lab_below, label = sector),
    angle = 90,
    hjust = 1,
    vjust = 0.5,
    size = 3.5
  ) +
  geom_text(
    data = \(d) d %>% filter(sector == "Surface transport"),
    aes(x = x, y = y_lab_inside, label = sector),
    angle = 90,
    hjust = 0.5,
    vjust = 0.5,
    size = 3.5
  ) +
  geom_text(
    data = \(d) d %>% filter(sector == "Residential buildings"),
    aes(x = x, y = pmax(start, end), label = sector),
    angle = 90,
    hjust = 1.05,
    vjust = 0.5,
    size = 3.5
  )+ 
  mytheme +
  scale_fill_manual(values = unname(c(sectors_palette, "#404040"))) +
  labs(y = "Productivity growth (pp)", x = "", fill = "") +
  coord_cartesian(clip = "off") +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    plot.margin = margin(5.5, 5.5, 35, 5.5),
    legend.position = "None"
    # legend.text = element_text(size = 7)
  ) +
  guides(fill = guide_legend(ncol = 2)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1),breaks=c(0,0.00128,0.005,0.01))

g_waterfall_CCC_CB7_labelled +
  canvas(4, 3, "in")
save(g_waterfall_CCC_CB7_labelled,file=file.path(graph_path,"g_waterfall_CCC_CB7_labelled.RData"))

# Save data ---------------------------------------------------------------
