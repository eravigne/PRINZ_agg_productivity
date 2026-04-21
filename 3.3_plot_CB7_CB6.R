# Set up ------------------------------------------------------------------

library(here)
source(here("0_set_up.R"))
source(here("0.1_functions_forecast.R"))

graph_path <- here("Graphs/CCC")
tables_path <- here("Tables/CCC")



scenario_cols <- c(
  "CB7 – Balanced Pathway" = "#1b9e77",
  "CB6 – Balanced Net Zero Pathway" = "#7f7f7f",
  "CB6 – Headwinds" = "#7f7f7f",
  "CB6 – Tailwinds" = "#7f7f7f",
  "CB6 – Widespread Engagement" = "#7f7f7f",
  "CB6 – Widespread Innovation" = "#7f7f7f"
)


scenario_linetypes <- c(
  "CB7 – Balanced Pathway" = "solid",
  "CB6 – Balanced Net Zero Pathway" = "dashed",
  "CB6 – Headwinds" = "dotted",
  "CB6 – Tailwinds" = "dotdash",
  "CB6 – Widespread Engagement" = "longdash",
  "CB6 – Widespread Innovation" = "twodash"
)

# Load cumsum_TFP_annual from CB6 and CB7 ---------------------------------
load(file=file.path(dataset_path,"cumsum_TFP_annual_CB7.RData"),verbose=T) #cumsum_TFP_annual_CB7
load(file=file.path(dataset_path,"cumsum_TFP_annual_CB6.RData"),verbose=T) #cumsum_TFP_annual_CB6
load(file=file.path(dataset_path,"cumsum_TFP_annual_sector_CB7.RData"),verbose=T) #cumsum_TFP_annual_sector_CB7
load(file=file.path(dataset_path,"cumsum_TFP_annual_sector_CB6.RData"),verbose=T) #cumsum_TFP_annual_sector_CB6


# Combine -----------------------------------------------------------------

cumsum_both <-
  bind_rows(
    cumsum_TFP_annual_CB6 %>%
      rename(year = Year, scenario = Scenario) %>%
      mutate(budget = "CB6"),
   cumsum_TFP_annual_CB7%>%
      mutate(budget = "CB7")
  ) %>%
  mutate(budget_scenario = paste(budget, scenario, sep = " \u2013 "))

load(file.path(dataset_path, "CCC_CB7_aggregate_error_bands.RData"), verbose = TRUE)        #ccc_error_bands_cb7
load(file.path(dataset_path, "CCC_CB7_aggregate_error_bands_sector.RData"), verbose = TRUE) #ccc_error_bands_cb7_sector


# Plot --------------------------------------------------------------------

g_cumAhat_CB6_CB7_allscenarios_common <-
  cumsum_both %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = cumAhat_agg,
                colour = budget_scenario,
                linetype = budget_scenario),
            lwd = 0.9) +
  scale_colour_manual(values = scenarios_palette) +
  scale_linetype_manual(values = c(rep("dashed", 5),"solid")) +
  labs(
    x = "",
    y = "Cumulative TFP growth (pp)",
    colour = "",
    linetype = "",
    caption = "Source: CCC CB6 and CB7"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
 
g_cumAhat_CB6_CB7_allscenarios_beamer<-
  g_cumAhat_CB6_CB7_allscenarios_common+
  xlim(2020,2065)+
  geom_text_repel(
    data = cumsum_both %>% filter(year == 2050),
    aes(label  = paste0(budget_scenario, ", ",
                        round(cumAhat_agg * 100, 1), "%"),
        y      = cumAhat_agg,
        colour = budget_scenario),
    nudge_x   = 3,
    na.rm     = TRUE,
    size      = 3,
    direction = "y",
    hjust     = 0
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))

g_cumAhat_CB6_CB7_allscenarios_beamer +
  canvas(9, 5, unit = "in")

save_graph("g_cumAhat_CB6_CB7_allscenarios_beamer", width = 9, height = 5)



# 1. Create abbreviated label mapping (adjust names to match your budget_scenario values)
abbrev_labels <- c(
  "CB6 – Balanced Net Zero Pathway" = "CB6 Balanced NZ",
  "CB6 – Headwinds"                 = "CB6 Headwinds",
  "CB6 – Tailwinds"                 = "CB6 Tailwinds",
  "CB6 – Widespread Engagement"     = "CB6 Widespread Eng.",
  "CB6 – Widespread Innovation"     = "CB6 Widespread Inn."
)

# 2. Add % values to legend labels from 2050 data
legend_vals <- cumsum_both %>%
  filter(year == 2050, !str_detect(budget_scenario, "CB7")) %>%
  mutate(legend_label = paste0(abbrev_labels[budget_scenario], ", ",
                               round(cumAhat_agg * 100, 1), "%")) %>%
  select(budget_scenario, legend_label)

# Named vector for scale_*_manual labels argument
legend_label_vec <- setNames(legend_vals$legend_label, legend_vals$budget_scenario)

manual_legend_df <- tibble(
  budget_scenario = names(abbrev_labels),
  legend_label    = unname(legend_label_vec[names(abbrev_labels)]),
  x0              = 2020.1,
  x1              = 2021.0,
  x_text          = 2021.8,
  y               = c(-0.0128, -0.0150, -0.0172, -0.0194, -0.0216)
)

cb7_colour <- "#1b9e77"

g_cumAhat_CB6_CB7_allscenarios_comp <-
  ggplot() +
  # CB7 ribbon behind all lines
  geom_ribbon(
    data = ccc_error_bands_cb7,
    aes(x = year, ymin = q10_cumAhat, ymax = q90_cumAhat),
    fill = cb7_colour, alpha = 0.12
  ) +
  geom_ribbon(
    data = ccc_error_bands_cb7,
    aes(x = year, ymin = q25_cumAhat, ymax = q75_cumAhat),
    fill = cb7_colour, alpha = 0.25
  ) +
  # All scenario lines (CB6 dashed, CB7 solid)
  geom_line(
    data = cumsum_both,
    aes(x = year, y = cumAhat_agg,
        colour   = budget_scenario,
        linetype = budget_scenario),
    lwd = 0.9
  ) +
  geom_segment(
    data = manual_legend_df,
    aes(x = x0, xend = x1, y = y, yend = y, linetype = budget_scenario),
    inherit.aes = FALSE,
    colour = "grey50",
    lwd = 0.9
  ) +
  geom_text(
    data = manual_legend_df,
    aes(x = x_text, y = y, label = legend_label),
    inherit.aes = FALSE,
    hjust = 0,
    vjust = 0.5,
    size = 3
  ) +
  geom_text_repel(
    data = cumsum_both %>% filter(year == 2050, str_detect(budget_scenario, "CB7")),
    aes(x = year, y = cumAhat_agg,
        label  = paste0(budget_scenario, ", ", round(cumAhat_agg * 100, 1), "%"),
        colour = budget_scenario),
    nudge_x = -15, na.rm = TRUE, size = 3, direction = "y", hjust = 0
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  scale_colour_manual(
    values = scenario_cols,
    breaks = names(abbrev_labels),
    labels = legend_label_vec
  ) +
  scale_linetype_manual(
    values = scenario_linetypes,
    breaks = names(abbrev_labels),
    labels = legend_label_vec
  ) +
  labs(x = "", y = "Cumulative TFP growth (pp)", colour = "", linetype = "", caption = "") +
  theme_minimal() +
  theme(
    legend.position = "none"
  )
  
g_cumAhat_CB6_CB7_allscenarios_comp+
  canvas(9, 5, unit = "in")

save_graph("g_cumAhat_CB6_CB7_allscenarios_comp",width=9,height=5)

# Sector-level: cumTFP by sector, all CB6+CB7 scenarios -------------------

# Harmonise CB7 sector names to CB6 equivalents
cb7_sector_recode <- c(
  "Engineered removals"      = "Removals",
  "Land use"                 = "LULUCF sinks",
  "Industry"                 = "M&C",
  "Residential buildings"    = "Res buildings",
  "Non-residential buildings"= "Non res buildings"
)

cumsum_sector_both <-
  bind_rows(
    cumsum_TFP_annual_sector_CB6 %>%
      rename(year = Year, scenario = Scenario) %>%
      mutate(budget = "CB6"),
    cumsum_TFP_annual_sector_CB7 %>%
      mutate(sector = dplyr::recode(sector, !!!cb7_sector_recode),
             budget = "CB7")%>%
      ungroup()%>%
      tidyr::complete(
        nesting(scenario, country, sector, variable_unit, variable, budget),
        year = 2024,
        fill = list(
          value = 0,
          delta_annual_resource = 0,
          cumAhat_raw = 0,
          cumAhat = 0
        )
      )
  ) %>%
  mutate(budget_scenario = paste(budget, scenario, sep = " \u2013 ")) %>%
  select(year, scenario, sector, cumAhat, budget, budget_scenario) %>%
  ungroup()


# Table: 2022-2050 sectoral TFP, CB6 and CB7 ------------------------------

sector_order <- c(
  "Agriculture", "Aviation", "Electricity supply", "F-gases",
  "Fuel supply", "LULUCF sinks", "LULUCF sources", "Manufacturing",
  "Non res buildings", "Removals", "Res buildings", "Shipping",
  "Surface transport", "Waste", "Total"
)

scenario_order <- c(
  paste("CB7", "Balanced Pathway", sep = " \u2013 "),
  paste("CB6", "Balanced Net Zero Pathway", sep = " \u2013 "),
  paste("CB6", "Headwinds", sep = " \u2013 "),
  paste("CB6", "Tailwinds", sep = " \u2013 "),
  paste("CB6", "Widespread Engagement", sep = " \u2013 "),
  paste("CB6", "Widespread Innovation", sep = " \u2013 ")
)

tab_CCC_CB6_CB7_TFP_opex_capex <-
  bind_rows(
    cumsum_sector_both %>%
      filter(year == 2050) %>%
      transmute(sector, budget_scenario, cumAhat),
    cumsum_both %>%
      filter(year == 2050) %>%
      transmute(
        sector = "Total",
        budget_scenario,
        cumAhat = cumAhat_agg
      )
  ) %>%
  mutate(
    sector = dplyr::recode(sector, "M&C" = "Manufacturing"),
    sector = factor(sector, levels = sector_order),
    budget_scenario = factor(budget_scenario, levels = scenario_order),
    cumAhat = round(100 * cumAhat, 2)
  ) %>%
  arrange(sector, budget_scenario) %>%
  pivot_wider(names_from = budget_scenario, values_from = cumAhat) %>%
  rename("Sector" = "sector")

print.xtable(xtable(x = tab_CCC_CB6_CB7_TFP_opex_capex,
                    digits = 2,
                    align = "crP{2cm}P{2cm}P{2cm}P{2cm}P{2cm}P{2cm}",
                    caption = "2022-2050 TFP impact of CCC CB6 and CB7 Net Zero transition pathways",
                    label = "tab_CCC_CB6_CB7_TFP_opex_capex"),
             include.rownames = FALSE,
             table.placement = "H",
             type = "latex",
             file = file.path(tables_path, "tab_CCC_CB6_CB7_TFP_opex_capex.tex"),
             floating = FALSE,
             tabular.environment = "longtable",
             hline.after = c(-1, 0, 14),
             caption.placement = "top",
             NA.string = "",
             print.results = TRUE)

# Linetype vector keyed on budget_scenario so colour+linetype legends auto-merge
budget_scenario_linetype <- setNames(
  ifelse(grepl("^CB6", unique(cumsum_sector_both$budget_scenario)), "dashed", "solid"),
  unique(cumsum_sector_both$budget_scenario)
)

smart_percent <- function(x) {
  # finds minimum needed precision per break vector
  magnitude <- max(abs(x), na.rm = TRUE)
  accuracy <- case_when(
    magnitude < 0.0001 ~ 0.0001,
    magnitude < 0.001  ~ 0.001,
    magnitude < 0.01   ~ 0.01,
    TRUE               ~ 0.1
  )
  scales::percent_format(accuracy = accuracy)(x)
}

# Then in your plot:
scale_y_continuous(labels = smart_percent)

g_cumAhat_CB6_CB7_bysector <-
  cumsum_sector_both %>%
  ggplot(aes(x = year,
             y = cumAhat,
             colour   = budget_scenario,
             linetype = budget_scenario)) +
  geom_hline(yintercept = 0, colour = "grey40", lwd = 0.2) +
  geom_line(lwd = 0.7,alpha=0.8) +
  facet_wrap(~sector, scales = "free_y") +
  scale_colour_manual(values = scenarios_palette) +
  scale_linetype_manual(values = budget_scenario_linetype) +
  labs(
    x        = "",
    y        = "Contribution to aggregate TFP growth (pp, cumulative)",
    colour   = "",
    linetype = ""
    # caption  = "Source: CCC CB6 and CB7"
  ) +
  theme_minimal() +
  theme(
    # legend.position  = "inside",
    legend.position = c(0.55, 0.0), # Bottom-right corner
    legend.justification = c(0, 0), # Align legend to bottom-left
    # 
    strip.text       = element_text( face = "bold")
    # size = 9,
    # axis.text        = element_text(size = 9),
    # legend.text      = element_text(size = 8)
  ) +
  guides(colour   = guide_legend(nrow = 3),
         linetype = guide_legend(nrow = 1)) +
  scale_y_continuous(labels = smart_percent)

g_cumAhat_CB6_CB7_bysector +
  canvas(12, 10, unit = "in")

save_graph("g_cumAhat_CB6_CB7_bysector", width = 12, height = 10)



# Main difference between CB6 and CB7 driving the 2040-2050 change in sign is Surface Transport : larger increase of +0.6pp
# and Industry (M&C): 0.1pp




g_cumAhat_CB6_CB7_bysector_comp <-
  ggplot() +
  # CB7 uncertainty ribbon (behind all lines), faceted by sector
  geom_ribbon(
    data = ccc_error_bands_cb7_sector,
    aes(x = year, ymin = q10, ymax = q90, group = scenario),
    fill = cb7_colour, alpha = 0.12
  ) +
  geom_ribbon(
    data = ccc_error_bands_cb7_sector,
    aes(x = year, ymin = q25, ymax = q75, group = scenario),
    fill = cb7_colour, alpha = 0.25
  ) +
  geom_hline(yintercept = 0, colour = "grey40", lwd = 0.2) +
  geom_line(
    data = cumsum_sector_both,
    aes(x = year, y = cumAhat, colour = budget_scenario, linetype = budget_scenario),
    lwd = 0.7, alpha = 0.8
  ) +
  facet_wrap(~sector, scales = "free_y") +
  scale_colour_manual(values = scenario_cols) +
  scale_linetype_manual(values = scenario_linetypes) +
  scale_y_continuous(labels = smart_percent) +
  labs(
    x        = "",
    y        = "Contribution to aggregate TFP growth (pp, cumulative)",
    colour   = "",
    linetype = ""
  ) +
  theme_minimal() +
  theme(
    legend.position      = c(0.55, 0.0),
    legend.justification = c(0, 0),
    strip.text           = element_text(face = "bold")
  ) +
  guides(colour   = guide_legend(nrow = 3),
         linetype = guide_legend(nrow = 1))

g_cumAhat_CB6_CB7_bysector_comp +
  canvas(12, 7, unit = "in")
save_graph("g_cumAhat_CB6_CB7_bysector_comp", width = 12, height = 7)



# Composite figure --------------------------------------------------------

load(file.path(graph_path,"g_waterfall_CCC_CB7_labelled.RData"),verbose=T) #g_waterfall_CCC_CB7_labelled
g_waterfall_CCC_CB7_labelled +
  canvas(6, 4, unit = "in")
save_graph("g_waterfall_CCC_CB7_labelled", width = 6, height = 4)



# g_cumAhat_CB6_CB7_allscenarios+scale_colour_manual(values=scenario_cols)

# Sector plot: move legend from absolute c(x,y) to bottom for patchwork compatibility

# Composite: (a)+(b) on top row, (c) spanning full width below
g_top_left <- g_cumAhat_CB6_CB7_allscenarios_comp + 
  theme(plot.margin = margin(2, 5, -20, 5))  # negative bottom

g_top_right <- g_waterfall_CCC_CB7_labelled + 
  theme(plot.margin = margin(2, 5, -20, 5))  # negative bottom

g_bottom <- g_cumAhat_CB6_CB7_bysector_comp + 
  theme(plot.margin = margin(-50, 5, 2, 5))

g_composite_CCC <-
  (g_top_left | g_top_right) /
  g_bottom +
  plot_layout(
    heights = c(0.8, 1.2),
    widths  = c(1.1, 0.9)
  ) +
  plot_annotation(
    tag_levels = "a",
    tag_prefix = "(",
    tag_suffix = ")"
  )&
  theme(
    # plot.margin = margin(2, 5, 2, 5),
    panel.spacing = unit(0.4, "lines"),
    plot.tag = element_text(size = 12, face = "bold")
  )

g_composite_CCC+canvas(12,9,units="in")
ggsave(file.path(graph_path, "g_composite_CCC.jpeg"),
       g_composite_CCC,
       width = 12, height = 9, dpi = 300, units = "in")
ggsave(file.path(graph_path, "g_composite_CCC.pdf"),
       g_composite_CCC,
       width = 12, height = 9, dpi = 300, units = "in")


# # Error bands around CB7 scenarios ----------------------------------------
# 
# g_cumAhat_CB7_errorbands <-
#   ccc_error_bands_cb7 %>%
#   ggplot(aes(x = year)) +
#   # CB6 scenarios as faint background reference
#   geom_line(
#     data = cumsum_both %>% filter(startsWith(budget_scenario, "CB6")),
#     aes(y = cumAhat_agg, group = budget_scenario),
#     colour = "grey70", lwd = 0.5, linetype = "dashed"
#   ) +
#   # Uncertainty bands (outer then inner so inner is on top)
#   geom_ribbon(aes(ymin = q10_cumAhat, ymax = q90_cumAhat, group = scenario),
#               fill = cb7_colour, alpha = 0.12) +
#   geom_ribbon(aes(ymin = q25_cumAhat, ymax = q75_cumAhat, group = scenario),
#               fill = cb7_colour, alpha = 0.25) +
#   # Central scenario line
#   geom_line(aes(y = base_cumAhat, group = scenario),
#             colour = cb7_colour, lwd = 0.9) +
#   # 2050 label
#   geom_text_repel(
#     data = ccc_error_bands_cb7 %>% filter(year == 2050),
#     aes(y     = base_cumAhat,
#         label = paste0(scenario, "\n",
#                        round(base_cumAhat * 100, 1), "% [",
#                        round(q25_cumAhat  * 100, 1), "–",
#                        round(q75_cumAhat  * 100, 1), "%]")),
#     colour  = cb7_colour,
#     nudge_x = 2, size = 3, hjust = 0, lineheight = 0.9
#   ) +
#   scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
#   xlim(2021, 2057) +
#   labs(
#     x       = "",
#     y       = "Cumulative TFP growth",
#     caption = "Shaded bands: [q25, q75] and [q10, q90] forecast uncertainty.\nGrey dashed: CB6 scenarios for reference."
#   ) +
#   theme_minimal() +
#   theme(legend.position = "none")
# 
# g_cumAhat_CB7_errorbands + canvas(9, 5, unit = "in")
# save_graph("g_cumAhat_CB7_errorbands", width = 9, height = 5)



# Computing GVA of sectors ------------------------------------------------

### Current price GVA 

# Data: outputperhoursworked
# Link: [here](https://www.ons.gov.uk/economy/economicoutputandproductivity/productivitymeasures/datasets/outputperhourworkeduk)
# retrieved 02/02/2023 16:50


Section_SIC2007<-read_excel(file.path(data_path,"/ONS_data/outputperhourworked.xlsx"), sheet="Section-level_industries",skip=2)
colnames(Section_SIC2007)<-c("Sector", "SIC2007_section","SIC2007_division")

### 
#GVA (current price)
###

# annual - Section
gva_raw_annual_sect<-read_excel(file.path(data_path,"/ONS_data/outputperhourworked.xlsx"), sheet="Table_15",skip=2)

df_gva<-gva_raw_annual_sect%>%
  filter(Year %in% seq(1997, 2021))%>%
  gather(key="Sector",value="GVA",-1)%>%
  left_join(Section_SIC2007,by=c("Sector"))%>%
  distinct()%>% #to remove Whole economy (common to the two nomenclature)
  mutate(Year=as.numeric(as.character(Year)))%>%
  mutate(GVA=as.numeric(as.character(GVA)))

gdp<-(df_gva%>%filter(Year==2021)%>% filter(SIC2007_section  %in% c("A to T")))$GVA

# %>%
#   filter(SIC2007_section  %in% c("A to T",LETTERS[1:7]))%>%
#   filter(Year==2021)

df_gva_ratio_AG <- df_gva %>%
  summarise(
    GVA_AG = sum(GVA[SIC2007_section %in% LETTERS[1:7]], na.rm = TRUE),
    GVA_A_to_T = GVA[SIC2007_section == "A to T"][1],
    ratio_AG_to_A_to_T = GVA_AG / GVA_A_to_T
  )


###
# Gross output (current price)
###

# Data: supplyanduseofproductsandindustrygva
# Table 2: industry supply-use table. P1 is domestic output / gross output.
go_raw_industry <- read_excel(file.path(data_path, "ONS_data/supplyanduseofproductsandindustrygva.xlsx"),
                              sheet = "Table 2", skip = 2, col_names = TRUE)

df_go <- go_raw_industry %>%
  filter(Price == "CP", Component == "P1") %>%
  gather(key = "Year", value = "GO", -c(1:4)) %>%
  rename("SIC2007_code" = "Industry SIC Code") %>%
  separate(col = SIC2007_code, into = c("SIC2007_code", "SIC2007_code_bis"), sep = "&") %>%
  select(-SIC2007_code_bis) %>%
  mutate(
    SIC2007_code = trimws(SIC2007_code),
    SIC2007_section = str_sub(SIC2007_code, 1, 1),
    SIC2007_section = ifelse(SIC2007_section == "_", "A to T", SIC2007_section),
    Year = as.numeric(as.character(Year)),
    GO = as.numeric(as.character(GO))
  ) %>%
  filter(SIC2007_section %in% c("A to T", LETTERS[1:7])) %>%
  group_by(Year, SIC2007_section) %>%
  summarise(GO = sum(GO, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(Year == max(Year, na.rm = TRUE))

df_go_ratio_AF <- df_go %>%
  summarise(
    GO_AF = sum(GO[SIC2007_section %in% LETTERS[1:6]], na.rm = TRUE),
    GO_A_to_T = GO[SIC2007_section == "A to T"][1],
    ratio_AF_to_A_to_T = GO_AF / GO_A_to_T
  )

df_go_ratio_AF/gdp
