# date created: 02/10/2023
# last modification : 06/05/2025

# Goals
# 1/ Read STAN OECD data
# STAN OECD data
# https://www.oecd.org/industry/ind/stanstructuralanalysisdatabase.htm

# 2/ Compute TFP VA and TFP GO for STAN Data

# 3/ Compare TFP results to KLEMS for the UK
# (both recomputed from input and output values and VAConTFP)
# Plot the results for UK Lev1 sectors

# Conclusion: all good. Small differences for A and B, to be investigated, 
# but might be due to self employment in agriculture


# STAN is not perfect for our use: missing GO_Q for all sectors in UK, 
# so cannot compute Xi. 
# We compute GO_Q as GO_CP/VA_PI, but that is just a proxy.
# Still missing VA_PI for 405 observations out of 1515 (26%)

# TO DO  ------------------------------------------------------------------

# Finish replacing K_P in the missing sectors



# Set up ------------------------------------------------------------------

library(here)
source(here("0_set_up.R"))

graph_path<-file.path(base_path,"Graphs/Zeta")




# Load --------------------------------------------------------------------

###
# STAN
###
stan_raw<-
  read_csv(file=file.path(data_path,"STAN/STANI4_2020_02102023162204854.csv"))
str(stan_raw)
table(stan_raw$Variable)
table(stan_raw$VAR)
# View(stan_raw%>%select(VAR,Variable)%>%distinct())

country_codes_df<-read_csv(file.path(data_path,"country_code.txt"))
colnames(country_codes_df)<-c("name","alpha2","alpha3")
country_codes_df<-
  country_codes_df%>%select(name,alpha2,alpha3)


# Functions ---------------------------------------------------------------

compute_tfp<-function(df,compute_K_Q=F){
  
  if(compute_K_Q){
    df<-
      df%>%
      # K_CP
      mutate(K_CP=VA_CP-L_CP)%>%
      # K_Q
      mutate(K_Q=K_CP)
  }
  
  df%>%
    # Labour share of GO
    mutate(share_L_GO=L_CP/GO_CP,
           # II share of GO
           share_II_GO=II_CP/GO_CP)%>%
    # Capital share of GO
    mutate(share_K_GO=1-share_L_GO-share_II_GO)%>%
    # Labour share of VA
    mutate(share_L_VA=L_CP/VA_CP)%>%
    # Capital share of VA
    mutate(share_K_VA=1-share_L_VA)%>%
    # ln(X(t+1))-ln(X(t))%>%
    group_by(geo_code,sector)%>%
    arrange(geo_code,sector,Year)%>%
    mutate(mean_share_L_GO=1/2*(share_L_GO+lag(share_L_GO)),
           mean_share_II_GO=1/2*(share_II_GO+lag(share_II_GO)),
           mean_share_K_GO=1/2*(share_K_GO+lag(share_K_GO)))%>%
    mutate(mean_share_L_VA=1/2*(share_L_VA+lag(share_L_VA)),
           mean_share_K_VA=1/2*(share_K_VA+lag(share_K_VA)))%>%
    # Log variations
    mutate(log_change_VA=log(VA_Q)-log(lag(VA_Q)),
           log_change_GO=log(GO_Q)-log(lag(GO_Q)),
           log_change_L=log(L_Q)-log(lag(L_Q)),
           log_change_II=log(II_Q)-log(lag(II_Q)),
           log_change_K=log(K_Q)-log(lag(K_Q)))%>%
    # Using Tornqvist
    # log change of TFP on Gross output (TFPQ)
    mutate(log_change_TFP_GO=
             log_change_GO -
             log_change_L*mean_share_L_GO-
             log_change_II*mean_share_II_GO-
             log_change_K*mean_share_K_GO)%>%
    # log change of TFP on Value added (TFPQ)
    mutate(log_change_TFP_VA=
             log_change_VA -
             log_change_L*mean_share_L_VA-
             log_change_K*mean_share_K_VA)%>%
    # Using Lag of factor share
    mutate(
      log_change_TFP_GO_lag =
        log_change_GO -
        log_change_L * lag(share_L_GO) -
        log_change_II * lag(share_II_GO) -
        log_change_K * lag(share_K_GO)
    ) %>%
    mutate(log_change_TFP_VA_lag =
             log_change_VA -
             log_change_L * lag(share_L_VA) -
             log_change_K * lag(share_K_VA))
}


# Functions to convert log change of TFP to level
TFP_level<-function(df){
  
  ref_year<-
    df%>%
    filter(sector %in% c("TOT","D01T99"))%>%
    filter(!is.na(log_change_TFP_GO))%>%
    group_by(geo_code)%>%
    filter(Year==min(Year))%>%
    rename("ref_Year"="Year")%>%
    select(geo_code,ref_Year)
  
  df%>%
    left_join(ref_year,by="geo_code")%>%
    filter(!is.na(log_change_TFP_GO))%>%
      group_by(geo_code,sector)%>%
      mutate(log_change_TFP_GO=ifelse(Year==ref_Year,0,log_change_TFP_GO))%>%
      mutate(log_change_TFP_VA=ifelse(Year==ref_Year,0,log_change_TFP_VA))%>%
      mutate(TFP_GO=exp(log(100)+cumsum(log_change_TFP_GO)))%>%
      mutate(TFP_VA=exp(log(100)+cumsum(log_change_TFP_VA)))%>%
      mutate(log_change_TFP_GO=ifelse(Year==ref_Year,NA,log_change_TFP_GO))%>%
      mutate(log_change_TFP_VA=ifelse(Year==ref_Year,NA,log_change_TFP_VA))%>%
      ungroup()

} 


# STAN -----------------------------------------------------------------

###
# Variables
###
stan_raw%>%select(VAR,Variable)%>%distinct()

# Variables we are going to use
stan_raw%>%
  filter(VAR%in% c("PROD","PRDK","PRDP","INTI","INTK","INTP","VALU","VALK","VALP","LABR","HRSN","GFCP","EMPN"))%>%
  select(VAR,Variable)%>%distinct()

# VAR   Variable                                 
# <chr> <chr>                                    
# 1 PROD  Production (gross output), current prices
# 2 PRDK  Production (gross output), volumes       
# 3 PRDP  Production (Gross Output), deflators     
# 4 INTI  Intermediate inputs, current prices      
# 5 INTK  Intermediate inputs, volumes             
# 6 INTP  Intermediate inputs, deflators           
# 7 VALU  Value added, current prices              
# 8 VALK  Value added, volumes                     
# 9 VALP  Value added, deflators                   
# 10 LABR  Labour costs (compensation of employees) 
# 11 HRSN  Hours worked - total engaged             
# 12 GFCP  Gross Fixed Capital Formation, deflators 
# CPNK	Net capital stock, volumes
# 


###
# Correspondance between Level 1 NACE and the sector codes in STAN
###

# Build corresponding table with NACE lev1 [A:T]
df_nace_tab<-
  stan_raw%>%
  select(IND,Industry)%>%
  distinct()%>%
  separate(col="Industry",into=c("name_1","nace_r2_code"),sep="\\[")%>%
  mutate(nace_r2_code=str_remove(nace_r2_code,"]"))%>%
  filter(!is.na(nace_r2_code))

df_nace_tab_lev1<-
  df_nace_tab%>%
  filter(nace_r2_code%in% LETTERS[1:20])
  

###
# Sectors
###
length(unique(stan_raw$LOCATION))
length(unique(stan_raw$IND))
# 154 different sectors, 109 for UK
min((stan_raw$Time))
max((stan_raw$Time))
# 2010 to 2019


###
# RENAME Variables
###
df_stan<-
  stan_raw%>%
  select(LOCATION,VAR,IND,TIME,Value)%>%
  rename("geo_code"="LOCATION","var"="VAR","sector"="IND","Year"="TIME")%>%
  filter(var %in% c("PROD","PRDK","PRDP","INTI","INTK","INTP","VALU","VALK","VALP","LABR","HRSN","CPNK","EMPN"
))%>%
  filter(!is.na(Value))%>%
  spread(key=var,value=Value)%>%
  rename(
    "GO_CP"="PROD",
    "GO_Q"="PRDK",
    "GO_PI"="PRDP",
    "II_CP"="INTI",
    "II_Q"="INTK",
    "II_PI"="INTP",
    "VA_CP"="VALU",
    "VA_Q"="VALK",
    "VA_PI"="VALP",
    "L_CP"="LABR",
    "L_Q"="HRSN",
    "K_Q"="CPNK",
    "L_Q_2"="EMPN")

###
# MISSING HOURS
###

# NB: some sectors/countries do not have the total number of hours: HRSN. Then we use EMPN, Total employment. 
# 670 observations missing out of 1515 for the UK

# This chunk of code test the share of missing L_Q per sector. If it's 100% then 
# we can just replace L_Q by L_Q. But we should be careful not to replace just the 2019 
# value with L_Q_2 and keep L_Q, because the log would mean anything then
df_replace_L_Q<-
  df_stan%>%
  group_by(geo_code,sector)%>%
  mutate(nb_na_LQ=sum(is.na(L_Q)/n()))
table((df_replace_L_Q%>%
        filter(geo_code=="GBR"))$nb_na_LQ)
# All good for the UK. Not for the other countries !!
table(df_replace_L_Q%>%ungroup()%>%select(geo_code,nb_na_LQ))

###
# We only replace L_Q by L_Q_2 when all the data for one sector are missing (for all the years)
###
df_stan<-
  df_stan%>%
  group_by(geo_code,sector)%>%
  mutate(nb_na_LQ=sum(is.na(L_Q)/n()))%>%
  ungroup()%>%
  mutate(L_Q=ifelse(is.na(L_Q) & nb_na_LQ==1,L_Q_2,L_Q))%>%
  select(-nb_na_LQ)


###
# MISSING CAPITAL PRICES
###
# For 34 sectors we have no capital price.
# No K_P for any sector in USA

# For some of the sectors where we are missing the values for K_P
# we can replace by adjacent sectors

# Not implemented. 


###
# Compute TFP 
###

df_stan_TFP<-compute_tfp(df_stan)
  
# Add levels to the STAN DATABASE
df_stan_TFP_level<-TFP_level(df_stan_TFP)


# Add the NACE LEVEL 1 code
df_stan_TFP_level<-
  df_stan_TFP_level%>%
  left_join(df_nace_tab_lev1,by=c("sector"="IND"))%>%
  left_join(stan_raw%>%select(IND,Industry,LOCATION,Country)%>%distinct(),by=c("sector"="IND","geo_code"="LOCATION"))%>%
  rename("sector_name"="Industry")

# SAVE
save(df_stan_TFP_level,file=file.path(dataset_path,"df_stan_TFP_level.RData"))



#  KLEMS -----------------------------------------------------------

###
# Read
###
# read prices
KLEMS_growth_raw<-fread(file=file.path(data_path,"KLEMS_1995_2020/growth accounts.csv"))
KLEMS_NA_raw<-fread(file=file.path(data_path,"KLEMS_1995_2020/national accounts.csv"))


KLEMS_names<-
  KLEMS_NA_raw%>%
  select(starts_with("nace_r2"))%>%
  distinct()




###
# Treat and bind all variables 
###
df_KLEMS_raw<-
  KLEMS_growth_raw%>%
  # delete first column
  select(-V1)%>%
  # spread var to manipulate the columns
  spread(key=var,value=value)%>%
  # delete variables that are also in national accounts
  select(-VA_CP,-VA_PYP,-VA_Q)%>%
  left_join(KLEMS_NA_raw,by=c("year","geo_code","geo_name","nace_r2_code","nace_r2_name"))%>%
  rename("sector"="nace_r2_code",
         "sector_name"="nace_r2_name",
         "L_CP"="LAB",
         "L_Q"="LAB_QI",
         "K_Q"="CAP_QI",
         "Year"="year")

GDP_deflator<-
  df_KLEMS_raw%>%
  filter(sector=="TOT")%>%
  select(Year,geo_code,VA_PI)%>%
  rename("gdp_deflator"="VA_PI")

# COMPUTE TFP in KLEMS
df_KLEMS<-compute_tfp(df_KLEMS_raw,
                      compute_K_Q = F)




df_KLEMS<-
  df_KLEMS%>%
  # filter(!geo_code %in% c("SK","IE","EL","EU11","EU12"))%>%
  filter(!sector %in% c("MARKT"    ,"MARKTxAG","T","TOT_IND"))%>%
  left_join(GDP_deflator,by=c("geo_code","Year"))

# Add levels
df_KLEMS_TFP_level<-TFP_level(df_KLEMS)

save(df_KLEMS_TFP_level,file=file.path(base_path,"Datasets/df_KLEMS_TFP_level.RData"))

# 
# # UK
# df_KLEMS_TFP_level%>%
#   filter(geo_code=="UK" & sector=="TOT")%>%
#   select(Year,contains("TFP"))



### 
# Compare recomputed TFP growth and VAConTFP
###
n=1000


df_KLEMS_GDP<-
  df_KLEMS%>%
  ungroup()%>%
  filter(sector=="TOT")%>%
  select(Year,geo_code,VA_CP)%>%
  rename("gdp_CP"="VA_CP")




# NBER_CES ----------------------------------------------------------------


# Load data
filePath <- file.path(base_path,"Data/NBER_CES/", "BEA_GDP_data.xlsx")

gdp_deflator_US<-
  read_excel(path=filePath,sheet="T10104-A",skip=7)%>%
  filter(code=="A191RG")%>%
  gather(key=year,value=GDP_price_index,-c(1:3))%>%
  select(year,GDP_price_index)%>%
  mutate(year=as.numeric(year),
         GDP_price_index=as.numeric(GDP_price_index))

raw_data<-read_csv(file=file.path(base_path,"Data/NBER_CES/", "nberces5818v1_n2012.csv"))
df_nber_raw<-
  raw_data%>%
  mutate(real_output=vship/piship)

naics_name<-read_xlsx(path=file.path(base_path,"Data/NBER_CES/", "2012-NAICS-2-3-4-5-6-Digit-Codes-Listed-Numerically-1.xlsx"),
                      sheet="6 Digit NAICS")
colnames(naics_name)<-c("naics","naics_name")


df_nber<-
  df_nber_raw%>%
  left_join(naics_name,by="naics")%>%
  left_join(gdp_deflator_US,by=c("year"))

###
# Description
###

# 1958 to 2018 : 61Y of data
table(df_nber_raw$year)
# 364 sectors : in manufacturing (from 311 to 339) 6-NAICS
length(unique(df_nber_raw$naics))


save(df_nber,file=file.path(base_path,"Datasets/df_nber.RData"))




# Compare STAN to KLEMS --------------------------------------------------------

# Works quite well

###
# Compare cumulative changes
###
# variable_name="log_change_L"
plot_log_change<-function(variable_name,sectors=data.frame(
  KLEMS = "C",
  STAN = "D10T33",
  stringsAsFactors = FALSE
)){

  # C 
  # D10T33
  # Manufacturing
  # TOT
df_level<-
  rbind(df_KLEMS_TFP_level%>%mutate(db="KLEMS")%>%
          filter(sector==sectors$KLEMS)%>%
          left_join(country_codes_df,by=c("geo_code"="alpha2"))%>%
          mutate(geo_code=alpha3)%>%
          select(-alpha3)%>%
          select(Year,geo_code,db,variable_name)%>%
          filter(!is.na(get(variable_name))),
        df_stan_TFP_level%>%mutate(db="STAN")%>%
          filter(sector==sectors$STAN)%>%
          select(Year,geo_code,db,variable_name)%>%
          filter(!is.na(get(variable_name))))%>%
  gather(key=var,value=value,-c("Year","geo_code","db"))%>%
  filter(!is.na(geo_code))
# variable<-unique(df_level$var)

print(
df_level%>%
  # distinct()%>%
  spread(key=db,value=value)%>%
  filter(!is.na(KLEMS) & !is.na(STAN))%>%
  arrange(geo_code,Year)%>%
  gather(key=db,value=gvalue,-c("Year","geo_code","var"))%>%
  group_by(geo_code,db)%>%
  mutate(cumgvalue=cumsum(gvalue))%>%
  ggplot(aes(x=Year,y=cumgvalue,colour=db))+
  geom_line()+
  facet_wrap(~geo_code,scales="free")+
  theme_minimal()+
  labs(title=paste(variable_name,sectors,sep=" - "))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))
)
}
# Manufacturing
plot_log_change("log_change_L")
plot_log_change("log_change_K")
plot_log_change("log_change_GO")
plot_log_change("log_change_TFP_GO")


# TOT
sectors_tot<-data.frame(
  KLEMS = "TOT",
  STAN = "D01T99",
  stringsAsFactors = FALSE
)
plot_log_change("log_change_L",sectors_tot)
plot_log_change("log_change_K",sectors_tot)
plot_log_change("log_change_GO",sectors_tot)
plot_log_change("log_change_TFP_GO",sectors_tot)

# For France: big difference on labour, explaining most of the gap in TFP. 