
### ******************************************* 
# Set up 
### ******************************************* 

library(here)

# Load libraries and functions
source(here("0_set_up.R"))
source(here("0.1_functions_forecast.R"))

# # set up folders
# base_path<-"C:/Users/smit0226/OneDrive - Nexus365/1_Research/PRINZ_productivity_agg/"
# scenario_path<-"C:/Users/smit0226/OneDrive - Nexus365/1_Research/PRINZ_productivity_agg/Scenarios/CCC_CB6_6th_budget"
graph_path<-here("Graphs/CCC")
tables_path<-here("Tables/CCC")



### ******************************************* 
# Read CCC
### ******************************************* 

# file: The-Sixth-Carbon-Budget-Dataset_v2.xlsx
# link: https://www.theccc.org.uk/wp-content/uploads/2020/12/The-Sixth-Carbon-Budget-Dataset_v2.xlsx 
# Tab: Raw Data
# Range: B2:AM34068
# Scenario: all

# Note: A more detailed version of this chart is presented in row 639 
# of the scenario explorer tab of the 6th Carbon Budget dataset. 

# read
raw_ccc<-
  read_excel(path=file.path(base_path,"Data/CCC_CB6/The-Sixth-Carbon-Budget-Dataset_v2.xlsx"),sheet="Raw data")


### ******************************************* 
# Prepare data 
### ******************************************* 

df_cost_raw<-
  raw_ccc%>%
  gather(key=Year,value=value,-c(1:7))%>%
  mutate(Year=as.numeric(Year))%>%
  filter(Year>=2020)%>%
  filter(Region=="UK")%>%
  filter(Metric %in% c("Additional operating costs",
                       "Annualised resource costs",
                       "Additional capital investment"))%>%
  mutate(metric=case_when(
    Metric=="Annualised resource costs"~"annual_resource",
    Metric=="Additional capital investment"~"capex",
    Metric=="Additional operating costs"~"opex"))%>%
  select(-`Metric index`, -Region, -Sector,-Metric)%>%
  rename("sector"="Amalgamation")%>%
  arrange(Year)

# df_cost_raw%>%
#   filter(Year==2050)%>%
#   group_by(Scenario)%>%
#   filter(!metric=="annual_resource")%>%
#   summarise(sum(value))

df_annual_resource <- 
  df_cost_raw%>%
  # Using the annualised resource cost
  filter(metric == "annual_resource") 



### ******************************************* 
# Compute productivity 
### ******************************************* 

# From annualised cost of the transition by sector, we compute aggregate TFP changes
# by two approaches providing the same results
# Approach 1: computes directly the TFP changes from 2020 to 2050
# Approach 2: provides the year-in-year changes in TFP

# Units: results are in pp

###
# Approach 1: 2020-2050 TFP (aggregated)
###
sum_TFP_annual<-
  df_annual_resource%>%
  filter(Year==2050)%>%
  arrange(Scenario,sector,Year)%>%
  group_by(Scenario,sector)%>%
  # sectoral TFP changes
  summarise(contrib_Ahat=value*(zeta-1)/GDP_UK)%>%
  # Aggregate TFP changes (summed across sectors)
  group_by(Scenario)%>%
  summarise(agg_Ahat=sum(contrib_Ahat))%>%
  mutate(agg_Ahat=transform_bp_into_pp(agg_Ahat))

###
# Approach 2: 2020-2050 yearly changes in TFP
###

# We add 2019 as a base year (capex and opex =0 for all sectors)
# if not done, slight difference with the aggregate approach
df_annual_resource_incl2019<-
  rbind(df_annual_resource,
        df_annual_resource%>%
          filter(Year==2020)%>%
          mutate(value=0,
                 Year=2019))%>%
  # order by Year
  arrange(Scenario,sector,Year)

# Computing sectoral TFP changes year by year
cumsum_TFP_annual_sector<-
  df_annual_resource_incl2019 %>%
  group_by(sector,Scenario) %>%
  # Computing the yearly changes in annualised resource cost
  mutate(delta_annual_resource=value-dplyr::lag(value))%>%
  # filter out the NA delta (2019-2020)
  filter(!is.na(delta_annual_resource))%>%
  # compute the changes in productivity as a cumulative sum
  mutate(cumAhat_raw=cumsum(delta_annual_resource*(zeta-1))/GDP_UK)%>%
  mutate(cumAhat=transform_bp_into_pp(cumAhat_raw))

# Computing aggregate TFP changes year by year (cumulatively)
# summing all sectors
cumsum_TFP_annual<-
  cumsum_TFP_annual_sector%>%
  group_by(Year,Scenario)%>%
  reframe(cumAhat_agg=transform_bp_into_pp(sum(cumAhat_raw)),
          cumAhat_agg_raw=sum(cumAhat_raw))

### 
# Check that the two approaches give the same number
####
cumsum_TFP_annual%>%
  filter(Year==2050)%>%
  left_join(sum_TFP_annual)%>%
  mutate(check=agg_Ahat/cumAhat_agg)
# it checks out, the col check is all 1


cumsum_TFP_annual_sector_CB6<-cumsum_TFP_annual_sector
save(cumsum_TFP_annual_sector_CB6,file=file.path(dataset_path,"cumsum_TFP_annual_sector_CB6.RData"))


cumsum_TFP_annual_CB6<-cumsum_TFP_annual
save(cumsum_TFP_annual_CB6,file=file.path(dataset_path,"cumsum_TFP_annual_CB6.RData"))




### ******************************************* 
# Plot productivity Balanced Net Zero
### ******************************************* 


g_cumAhat_CCC_CB6_sector_BNZ<-
  cumsum_TFP_annual_sector%>%
  left_join(cumsum_TFP_annual)%>%
  filter(Scenario=="Balanced Net Zero Pathway")%>%
  ggplot(aes(x=Year))+
  facet_wrap(~Scenario)+
  geom_line(aes(y=cumAhat,colour=sector),lwd=0.9)+
  geom_line(aes(y=cumAhat_agg),colour="black",lwd=1, linetype = "dashed")+
  scale_colour_manual(values = unname(sectors_palette)) + # Enhanced color palette
  labs(
    # title = "Cumulative Ahat over Years by Sector and Aggregate",
    x = "",
    y = "Cumulative TFP growth (pp)",
    colour = "",
    caption="Source: CCC CB6 Balanced Net Zero Pathway") + 
  # mytheme+
  theme_minimal() + # Cleaner theme
  theme(
    legend.position = c(0.05, 0.05), # Bottom-left corner
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
  geom_text(data=cumsum_TFP_annual%>%filter(Year==2048 & Scenario=="Balanced Net Zero Pathway"),
            aes(x = 2048, y = (cumAhat_agg)-0.0005, label = "Aggregate"),colour="black", size = 3, vjust = -0.5,fontface="plain") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))


g_cumAhat_CCC_CB6_sector_BNZ+
  canvas(9,8,unit="in")

###
# Save
###
save_graph("g_cumAhat_CCC_CB6_sector_BNZ",width=9,height=8)


# other version for slide
g_cumAhat_CCC_CB6_sector_BNZ_beamer<-
  g_cumAhat_CCC_CB6_sector_BNZ+
  theme_minimal() + # Cleaner theme
  theme(
    legend.position = "right", 
    strip.text = element_text(size = 0, face = "bold"),
    axis.text =  element_text(size = 10)
  )
g_cumAhat_CCC_CB6_sector_BNZ_beamer+
  canvas(9,5,unit="in")
save_graph("g_cumAhat_CCC_CB6_sector_BNZ_beamer",width=9,height=5)


### just electricity

g_cumAhat_CCC_CB6_elec<-
  cumsum_TFP_annual_sector%>%
  left_join(cumsum_TFP_annual)%>%
  # filter(Scenario=="Balanced Net Zero Pathway")%>%
  filter(sector=="Electricity supply")%>%
  ggplot(aes(x=Year))+
  facet_wrap(~Scenario,scales="free_x")+
  geom_line(aes(y=cumAhat),lwd=0.9,colour="#8da0cb")+
  geom_line(aes(y=cumAhat_agg),colour="black",lwd=1, linetype = "dashed")+
  labs(
    # title = "Cumulative Ahat over Years by Sector and Aggregate",
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
  geom_text(data=cumsum_TFP_annual%>%filter(Year==2048 & Scenario=="Balanced Net Zero Pathway"),
            aes(x = 2048, y = (cumAhat_agg)-0.002, label = "Aggregate"),colour="black", size = 3, vjust = -0.5,fontface="plain") +
  geom_text(data=cumsum_TFP_annual_sector%>%filter(sector=="Electricity supply" & Year==2048 & Scenario=="Balanced Net Zero Pathway"),
            aes(x = 2045, y = (cumAhat)-0.003, label = "Electricity supply"),colour="#8da0cb", size = 3, vjust = -0.5,fontface="plain") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))

g_cumAhat_CCC_CB6_elec+
  canvas(9,5,unit="in")

save_graph("g_cumAhat_CCC_CB6_elec",width=9,height=5)

# table
tab_elec_CCC_CB6_UK<-cumsum_TFP_annual_sector%>%
  filter(sector=="Electricity supply"
         & Year==2050)%>%
  select(Scenario,cumAhat)
write_xlsx(tab_elec_CCC_CB6_UK,path=file.path(tables_path,"tab_elec_CCC_CB6_UK.xlsx"))

### ******************************************* 
# Plot productivity all scenarios
### ******************************************* 


g_cumAhat_CCC_CB6_allscenarios<-
  cumsum_TFP_annual%>%
  ggplot(aes(x=Year))+
  geom_line(aes(y=cumAhat_agg,colour=Scenario),lwd=1)+
  scale_colour_manual(values = unname(scenarios_palette)) + # Enhanced color palette
  labs(
    # title = "Cumulative Ahat over Years by Sector and Aggregate",
    x = "",
    y = "Cumulative TFP growth (pp)",
    colour = "",
    caption="Source: CCC CB6") + 
  theme_minimal() + # Cleaner theme
  theme(legend.position = "None")+
  xlim(2022,2060)+
  geom_text_repel(data=cumsum_TFP_annual%>%filter(Year==2050),
                  aes(label =paste(Scenario,", ", round(cumAhat_agg*100,1),"%",sep=""),
                      y=cumAhat_agg,
                      colour=Scenario),
                  nudge_x = 3,
                  na.rm = TRUE,
                  size=3)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))

g_cumAhat_CCC_CB6_allscenarios+
  canvas(8,5,unit="in")

###
# Save
###
save_graph("g_cumAhat_CCC_CB6_allscenarios",width=8,height=5)


### ******************************************* 
# 2020-2050 Aggregate table 
### ******************************************* 

tab_scenario_2050TFP<-
  cumsum_TFP_annual%>%
  filter(Year==2050)%>%
  select(-Year)%>%
  rename("TFP (pp)"="cumAhat_agg")

print.xtable(xtable(x=tab_scenario_2050TFP,
                    digits=1,
                    # align = "cccP{3cm}P{3cm}P{3cm}P{3cm}",
                    align="crr",
                    caption = "2022-2050 TFP impact of CCC CB6 Net Zero transition pathways",
                    label = "tab_scenario_2050TFP"),
             include.rownames=FALSE,
             table.placement="H",
             type="latex",
             # longtable
             file=file.path(tables_path,"tab_scenario_2050TFP.tex"),
             floating =FALSE,
             tabular.environment="longtable",
             hline.after = c(-1,0,5),
             caption.placement = "top",
             print.results=T)


### ******************************************* 
# 2020-2050 Sectoral TFP table 
### ******************************************* 


tab_sector_Ahat_year<-
  cumsum_TFP_annual_sector%>%
  filter(Year==2050)%>%
  select(Scenario,sector,cumAhat)%>%
  mutate(cumAhat=round(cumAhat,1))%>%
  spread(key=Scenario,value=cumAhat)%>%
  mutate(sector=factor(sector,levels=c("Agriculture", "Aviation", "Electricity supply", 
                                       "F-gases", "Fuel supply", "LULUCF sinks", "LULUCF sources", 
                                       "M&C", "Non res buildings", "Removals", "Res buildings", "Shipping", 
                                       "Surface transport", "Waste", "Total")))%>%
  arrange(sector)%>%
  rename("Sector"="sector")


print.xtable(xtable(x=tab_sector_Ahat_year,
                    digits=1,
                    # align = "cccP{3cm}P{3cm}P{3cm}P{3cm}",
                    align="crrrrrr",
                    caption = "2022-2050 TFP impact of CCC CB6 Net Zero transition pathways",
                    label = "tab_CCC_CB6_TFP_opex_capex"),
             include.rownames=FALSE,
             table.placement="H",
             type="latex",
             # longtable
             file=file.path(tables_path,"tab_CCC_CB6_TFP_opex_capex.tex"),
             floating =FALSE,
             tabular.environment="longtable",
             hline.after = c(-1,0,14),
             caption.placement = "top",
             print.results=T)


### ******************************************* 
# Contribution per sector by scenario
### ******************************************* 


###
# Electricity supply - contribution (in pp)
###

cumsum_TFP_annual_sector%>%
  filter(Year==2050)%>%
  filter(sector=="Electricity supply")%>%
  select(Scenario,sector,cumAhat)

###
# Energy sector (elec + fuel) - contribution (in pp)
###

cumsum_TFP_annual_sector%>%
  filter(Year==2050)%>%
  filter(sector %in% c("Electricity supply","Fuel supply"))%>%
  select(Scenario,sector,cumAhat)%>%
  group_by(Scenario)%>%
  summarise(cumAhat=sum(cumAhat))

###
# Comparison of sectoral contribution (in pp)
###

g_sectoral_TFP_scenario<-cumsum_TFP_annual_sector%>%
  filter(Year==2050)%>%
  select(Scenario,sector,cumAhat)%>%
  ggplot(aes(x=cumAhat,y=sector,colour=Scenario))+
  geom_point(size=5)+
  guides(colour=guide_legend(nrow=2))+
  labs(colour="",y="",x="2020-2050 TFP")+
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.1))+
  mytheme
g_sectoral_TFP_scenario
save_graph("g_sectoral_TFP_scenario",8,4)



### ******************************************* 
# 2020-2050  waterfall chart
### ******************************************* 

df_waterfall_CCC_CB6_ordered <- 
  cumsum_TFP_annual_sector %>%
  filter(Year == 2050) %>%
  select(Scenario, sector, cumAhat) %>%
  rename("value" = "cumAhat") %>%
  ungroup() %>%
  group_by(Scenario) %>%
  # Reorder sectors within each Scenario based on value
  arrange(desc(value)) %>%
  mutate(sector_reordered = factor(sector, levels = sector)) %>%
  # Create a sequential x-position variable (within each sector)
  mutate(x = row_number()) %>%
  # Calculate cumulative sums for start and end positions
  mutate(end = cumsum(value)) %>%
  mutate(start = lag(end, default = 0))


g_waterfall_CCC_CB6_ordered <- 
  df_waterfall_CCC_CB6_ordered %>%
  ggplot(aes(x = x, fill = sector)) +
  facet_wrap(~Scenario, scales = "free_x") +
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
    legend.position = c(0.68, 0.15),
    legend.justification = c(0, 0),
    legend.spacing.y = unit(0, "pt"),
    legend.spacing.x = unit(0, "pt"),
    legend.margin = margin(t = -5, r = 5, b = 5, l = 5),
    legend.text = element_text(size = 7)
  )+
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))

g_waterfall_CCC_CB6_ordered +
  canvas(8, 6, "in")
save_graph("g_waterfall_CCC_CB6_ordered",width=8,height=6)


