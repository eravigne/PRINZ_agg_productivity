
if (requireNamespace("here", quietly = TRUE)) {
  library(here)
  base_path <- here()
  here_path <- here
} else {
  base_path <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  here_path <- function(...) file.path(base_path, ...)
}

data_path <- here_path("Data")
dataset_path <- here_path("Datasets")

# scenario_path<-"C:/Users/smit0226/OneDrive - Nexus365/1_Research/PRINZ_productivity_agg/Scenarios/CCC_6th_budget"
# graph_path<-"C:/Users/smit0226/OneDrive - Nexus365/1_Research/PRINZ_productivity_agg/Graphs/CCC"


# Install libraries -------------------------------------------------------

libraries <- c("tidyverse", 
               "cowplot", 
               "ggpubr", 
               "extrafont", 
               "readxl", 
               "writexl", 
               "ggfittext",
               "ggridges", 
               "ggrepel",
               "knitr",
               # "reshape2",
               "kableExtra",
               "waterfalls",
               "ggforce",
               "ggdist",
               "directlabels",
               "gganimate",
               "stargazer",
               "gifski",
               "png")

try(
  detach("package:plyr"), 
  silent=T
)

# Function to check and install missing packages
install_if_missing <- function(packages) {
  for (package in packages) {
    if (!requireNamespace(package, quietly = TRUE)) {
      install.packages(package)
      library(package, character.only = TRUE)
    }
  }
}

# Install the required libraries if missing
install_if_missing(libraries)


# Load Libraries ----------------------------------------------------------

library(tidyverse)
library(cowplot)
library(ggpubr)
library(extrafont)
library(readxl)
library(writexl)
library(ggfittext)
library(ggridges)
library(ggrepel)
library(knitr)
# library(reshape2)
library(waterfalls)
library(ggforce)
library(ggdist)
library(directlabels)
library(gganimate)
library(gifski)
library(png) 
library(kableExtra)
library(stargazer)
library(zoo)
library(data.table)
library(dplyr)
library(purrr)
library(xtable)
library(readODS)
library(viridis)
library(scales)
library(broom)
library(Polychrome)
library(RColorBrewer)
library(ggpattern)
library(xtable)
library(readODS)
library(viridis)
library(scales)
library(xtable)
library(ggview)
# ggview previews need a live RStudio session; elsewhere (knit, Rscript) make canvas() a no-op
if (!(requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable())) {
  canvas <- function(...) NULL
}
library(tidytext)
library(patchwork)
library(car)
library(lmtest)
library(sandwich)
library(nlme)
library(stargazer)
library(xtable)
library(tidyverse)




# Databases ---------------------------------------------------------------


# # All Data KLEMS
# load("Datasets/df_KLEMS.RData") #df_KLEMS_bis
# 
# # Emissions (ONS)
# load("Datasets/ems_SIC.RData") #ems_SIC
# 
# # Domar weights
# load("Datasets/Domar_SIC_klems.RData") #Domar_SIC
# 
# load("Datasets/TFP_agg_exact.RData")
# 
# # Past TFP growth per sector
# load("Datasets/TFP_GO_SIC.RData") #TFP_GO_SIC
# 
# load("Datasets/TFP_GO_SIC_klems.RData") #TFP_GO_SIC


# Country code ------------------------------------------------------------

#First session 
# url <- "https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv"
# country_codes_df <- read_csv(url)
# colnames(country_codes_df) <- make.names(colnames(country_codes_df))
# save(country_codes_df,file="Datasets/country_codes_df.RData")

# Else
# load(file="Datasets/country_codes_df.RData") #country_codes_df

# Nace code ---------------------------------------------------------------

# nace_name<-read_excel("Data/nace_r2_classification.xlsx")

# If not replaced by a more precise version in each file
graph_path <- here_path("Graphs")
tables_path <- here_path("Tables")



save_graph<-function(name_graph,width,height,where_to_save=get("graph_path", envir = parent.frame())){
  force(where_to_save)
  filePath <- file.path(where_to_save,paste0(name_graph,".jpeg"))
  plot_obj <- get(name_graph, envir = parent.frame())
  ggsave(plot_obj,file=filePath,width=width,height=height,dpi=300)
  ggsave(plot_obj,file=sub("\\.jpeg$",".pdf",filePath),width=width,height=height,device=cairo_pdf)
  cat("Saved at ",filePath )
}



transform_bp_into_pp<-function(x){
  return(exp(x)-1)
}



transform_pp_into_bp<-function(x){
  return(log(x+1))
}

# Forecast CCC ------------------------------------------------------------


### ******************************************* 
# Parameters 
### ******************************************* 


# GDP 2019 (m£)
GDP_UK=2234*1000
# zeta
zeta=0.129


### ******************************************* 
# colours 
### ******************************************* 

# Sectors color scheme 
color_palette <- createPalette(14,seedcolors=c("#66c2a5","#fc8d62","#8da0cb","#e78ac3","#a6d854","#ffd92f","#e5c494","#b3b3b3"))
barplot(rep(1, 14), col = color_palette, space = 0, axes = FALSE, ylab = "", xlab = "Generated Colors")

sectors_palette <- c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#b3b3b3", 
                     "#d95f0e", "#7570b3", "#1b9e77", "#d73027", "#542788", "#2ca25f", 
                     "#e6ab02", "#666666")

barplot(rep(1, 14), col = sectors_palette, space = 0, axes = FALSE, ylab = "",xlab="")


# Scenarios color scheme
scenarios_palette <- c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00" ,"#1b9e77"  # pink
)
barplot(rep(1, 6), col = scenarios_palette, space = 0, axes = FALSE, ylab = "", xlab = "")




# Theme -------------------------------------------------------------------


mytheme<-
  theme_minimal_hgrid()+ 
  theme(axis.text = element_text(size=10),
        legend.position="top",
        axis.title = element_text(size=10),
        legend.text = element_text(size=10),
        legend.title=element_text(size=10),
        axis.line.y = element_line(linewidth=0.5,colour="grey85"),
        axis.line.x = element_line(linewidth=0.5,colour="grey85"),
        axis.text.x=element_text(size=10),
        plot.caption = element_text(size=10),
        strip.text = element_text(size=10,face="bold"),
        # panel.grid=element_blank(),
        plot.title = element_text(size=12,face="bold"),
        plot.subtitle = element_text(size=10,face="italic"),
        legend.box = "horizontal",
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

mytheme_grid<-
  theme_minimal_grid()+ 
  theme(axis.text = element_text(size=10),
        legend.position="top",
        axis.title = element_text(size=10),
        legend.text = element_text(size=10),
        legend.title=element_text(size=10),
        axis.line.y = element_line(linewidth=0.5,colour="grey85"),
        axis.line.x = element_line(linewidth=0.5,colour="grey85"),
        axis.text.x=element_text(size=10),
        strip.text = element_text(size=10,face="bold"),
        # panel.grid=element_blank(),
        plot.title = element_text(size=12,face="bold"),
        plot.subtitle = element_text(size=10,face="italic"),
        legend.box = "horizontal",
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

mytheme_small<-
  theme_minimal_hgrid()+ 
  theme(axis.text = element_text(size=8),
        legend.position="top",
        axis.title = element_text(size=8),
        legend.text = element_text(size=8),
        legend.title=element_text(size=8),
        axis.line.y = element_line(linewidth=0.5,colour="grey85"),
        axis.line.x = element_line(linewidth=0.5,colour="grey85"),
        axis.text.x=element_text(size=8),
        strip.text = element_text(size=8,face="bold"),
        # panel.grid=element_blank(),
        plot.title = element_text(size=12,face="bold"),
        plot.subtitle = element_text(size=8,face="italic"),
        legend.box = "horizontal",
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())
