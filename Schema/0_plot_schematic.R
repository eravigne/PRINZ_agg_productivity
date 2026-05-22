library(here)
library(ggplot2)
source(here("0_set_up.R"))


# q — area graph, increases over 5 periods, mint green

df_q <- data.frame(
  time = 1:5,
  q    = c(1.0, 1.6, 2.5, 3.6, 5.0)
)

plot_q <- ggplot(df_q, aes(x = time, y = q)) +
  geom_area(fill = "#3EB489", alpha = 0.8) +
  geom_line(color = "#2a7d5f", linewidth = 0.6) +
  scale_x_continuous(breaks = 1:5) +
  labs(x = "Time", y = "X") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid      = element_blank(),
    axis.line       = element_line(color = "grey40"),
    axis.ticks      = element_line(color = "grey40"),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave(here("Schema", "schematic_q.pdf"), plot_q, width = 3, height = 2.2)
# ggsave(here("Schema", "schematic_q.png"), plot_q, width = 3, height = 2.2, dpi = 300)


# p — line graph, decreases over 5 periods (not fully linear), orange

df_p <- data.frame(
  time = 1:5,
  # p    = c(5.0, 4.8, 4.6, 4.2, 4.0)
  p    = c(5.0, 3.8, 3.0, 2.4, 2.0)
)

plot_p <- ggplot(df_p, aes(x = time, y = p)) +
  geom_line(color = "#E87722", linewidth = 1.0) +
  geom_point(color = "#E87722", size = 2) +
  scale_x_continuous(breaks = 1:5) +
  labs(x = "Time", y = "p") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid      = element_blank(),
    axis.line       = element_line(color = "grey40"),
    axis.ticks      = element_line(color = "grey40"),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave(here("Schema", "schematic_p.pdf"), plot_p, width = 3, height = 2.2)
# ggsave(here("Schema", "schematic_p.png"), plot_p, width = 3, height = 2.2, dpi = 300)


# TFP — line graph with quantile ribbons, increases over 5 periods
# Ribbon colour scheme matching g_error_hulten_quantiles:
#   [q10, q90] = lightblue, [q25, q75] = navy, both alpha = 0.3
# TFP line: #003f5c (same as "Aggregate TFP" in hindcasting plots)

load(file.path(dataset_path, "CCC_CB7_aggregate_error_bands.RData"), verbose = TRUE)  
zeta=0.13

df_tfp <- data.frame(
  time = 1:5,
  tfp  = 0.02*c(0, cumsum(zeta * diff(log(df_q$q)) + (zeta-1) * diff(log(df_p$p)))))%>%
  left_join(ccc_error_bands_cb7,by=c("time"="horizon"))%>%
  mutate(
  q10  = tfp+q10_raw,
  q25  =  tfp+q25_raw,
  q75  =  tfp+q75_raw,
  q90  =  tfp+q90_raw
)%>%
  select(time,tfp,q10,q25,q75,q90)

plot_tfp <- ggplot(df_tfp, aes(x = time)) +
  geom_ribbon(aes(ymin = q10, ymax = q90), fill = "lightblue", alpha = 0.3) +
  geom_ribbon(aes(ymin = q25, ymax = q75), fill = "navy",      alpha = 0.3) +
  geom_line(aes(y = tfp), color = "#003f5c", linewidth = 1.0) +
  scale_x_continuous(breaks = 1:5) +
  labs(x = "Time", y = "TFP") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid      = element_blank(),
    axis.line       = element_line(color = "grey40"),
    axis.ticks      = element_line(color = "grey40"),
    plot.background = element_rect(fill = "white", color = NA)
  )+scale_y_continuous(labels = scales::number_format(scale=100,suffix='%'))
  

plot_tfp+canvas(3,2.2,unit="in")

ggsave(here("Schema", "schematic_tfp.pdf"), plot_tfp, width = 3, height = 2.2)
# ggsave(here("Schema", "schematic_tfp.png"), plot_tfp, width = 3, height = 2.2, dpi = 300)
