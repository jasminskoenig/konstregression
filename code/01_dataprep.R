
# merging VDem, PopuList, und ParlGov data
# TS: 11.01.2022

rm(list=ls())

library(tidyverse)
library(rio)
library(lubridate)
library(hrbrthemes)

theme_set(theme_ipsum(base_size = 14, axis_title_size = 14, strip_text_size = 14, axis_text_size = 14, base_family = "Noto Sans"))

# data import ----
library(vdemdata)

# vparty ----
vparty %>% 
  filter(year>1989) -> 
  vparty2

vparty2 %>% 
  select(v2paenname,v2paid,country_name,year, country_id,e_regiongeo,v2xpa_illiberal,v2xpa_popul,v2paseatshare,v2patotalseat,
         v2pavote,v2pagovsup,ep_type_populism,ep_type_populist_values, ep_v8_popul_rhetoric,ep_v9_popul_saliency) -> 
  vparty2

# calculate populism score of government
vparty2 -> df

# define who is in gov
df$gov_party <- ifelse(df$v2pagovsup %in% c(0,1,2),1,0)
df$primemin_party <- ifelse(df$v2pagovsup==0,1,0)
df %>% 
  filter(gov_party==1) -> 
  df

# calculate populism scores
df %>% 
  group_by(country_name,year) %>% 
  mutate(gov_seatshare = sum(v2paseatshare), 
         weight=v2paseatshare/gov_seatshare) %>% 
  summarise(gov_popul_mean=mean(v2xpa_popul), 
            gov_popul_weighted = sum(v2xpa_popul*weight), 
            gov_seatshare = gov_seatshare, 
            e_regiongeo=e_regiongeo, 
            no_govparties = n()) %>% 
  unique() ->
  df2

#variable on populism score of prime minister's party 
df %>% 
  group_by(country_name,year,primemin_party) %>% 
  summarise(gov_popul_prime=v2xpa_popul) %>% 
  filter(primemin_party==1) -> 
  df3

partydat <- left_join(df2,df3,by=c("country_name","year"))
head(df2)

# vdem data ----
vdem %>% 
  filter(year>1989) -> 
  vdem2

# only relevant columns
vdem2 %>% 
  select(country_name,year,country_id,e_regiongeo,v2x_polyarchy,v2x_regime, v2x_libdem,v2x_delibdem,v2x_egaldem, 
         v2x_liberal,v2xcl_rol,v2x_jucon,v2jureform,v2jupurge,v2jupoatck,v2jupack,v2juaccnt,v2jucorrdc,
         v2juhcind,v2juncind,v2juhccomp,v2jucomp,v2jureview) -> 
  vdem2

#data <- right_join(partydat,vdem2,by=c("country_name","year"))

# join vparty and vdem
data <- merge(vdem2,partydat,by=c("country_name","year","e_regiongeo"),all.x=TRUE)
head(data)

# fill non election years by country
data %>% 
  group_by(country_name) %>% 
  fill(gov_popul_mean,gov_popul_weighted,gov_seatshare,e_regiongeo,no_govparties,gov_popul_prime) %>% 
  ungroup() -> 
  data

# single party in gov?
data$singleparty_gov <- ifelse(data$no_govparties==1,1,0)

data %>% 
  rename(country=country_name) -> 
  data

# ccp data ----
ccpc <- read.csv("data/ccpcnc_v3_small.csv")

ccpc %>% 
  filter(year>1989) -> 
  ccpc

# only relevant columns 
ccpc %>% 
  select(country,year,syst,evnt,evnttype,overthrw,amend,execindp,intexec,invexe,levjud,judind,judprec,judfin, hosterm) -> 
  ccpc

head(ccpc)

# join vparty, vdem and ccpc
ccpc_vdem <- merge(data,ccpc,by=c("country","year"),all.x=TRUE) %>%
  filter(v2x_regime %in% c(3:4)) # only include liberal and electoral democracies

head(ccpc_vdem)

# save dataframes ----

# save df with all three datasets and all countries
saveRDS(ccpc_vdem,file="data/ccpc_vdem.rds")

# only European countries
ccpc_vdem %>% 
  filter(e_regiongeo %in% c(1:4)) -> 
  ccpc_vdem_europe

# save df with only European countries
saveRDS(ccpc_vdem_europe,file="data/ccpc_vdem_europe.rds")

# only European and Latin American countries
ccpc_vdem %>% 
  filter(e_regiongeo %in% c(1:4,17:29)) -> 
  ccpc_vdem_ela

# save df with only European and Latin American countries
saveRDS(ccpc_vdem_europe,file="data/ccpc_vdem_europe.rds")

