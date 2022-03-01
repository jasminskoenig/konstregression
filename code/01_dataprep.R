
# merging VDem, PopuList, und ParlGov data

rm(list=ls())

library(tidyverse)
library(rio)
library(lubridate)
library(readtext)
library(stringi)

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

# number of rights in constitution
# import names of rights in dataset

rightsindex <- readtext("codebooks/rightsindex_ccp.pdf") %>%
  select(-doc_id) %>%
  mutate(text = str_remove_all(text, "ID\\b")) %>%
  mutate(text = str_extract_all(text, "[:upper:]{2,}\\d{0,}")) %>%
  unlist(text) %>%
  str_to_lower() %>%
  stringi::stri_replace_all_regex(c("equal\\B","artspec", "debtrght", "health", "indrght", "intprop\\B", "socsec1", "socsec(?=\\d)"), c("equalgr_", "artspec_1", "debtors", "healthr", "indpolgr_", "intprop_", "socsec", "finsup_"), vectorize_all = FALSE)%>%
  str_remove("appeal|envref|intprop$|solissuf") %>%
  stri_remove_empty()

ccpc %>%
  select(one_of(rightsindex), rghtapp) %>%
  select(-arms) %>%
  mutate(across(, ~ifelse(.==1, 1, 0))) ->
  ccpc_rights

ccpc_rights %>%
  mutate(rights_sum = rowSums(across())) %>%
  pull(rights_sum) ->
  ccpc$rights_sum

ccpc_rights %>%
  mutate(rights_crim = jury, vicright, excrim, prerel, habcorp, wolaw, rghtapp, corppun, dueproc, falseimp, fairtri, speedtri, presinoc, trilang, juvenile, doubjep, miranda, couns, debtors) %>%
  mutate(rights_ruleolaw = rowSums(select(., "citren", 
           contains("equal"), 
           "infoacc", 
           "libel", 
           "freerel", "seprel", # religion
           "exprop", # enteignung möglich
           "remuner", "socsec", "standliv", contains("finsup"), "shelter", "healthr", # faire entlohnung, soziale sicherheit und lebensstandard, finanzielle unterstützung bestimmtr gruppen, recht auf wohnen, gesundheitsversorgung
           "jointrde", "strike", "occupate", "safework", "childwrk", #gewerkschaft und streik, eigene berufswahl, sichere arbeitsumgebung, verbot v kinderarbeit
           "testate", "transfer", "inherit", contains("intprop"), "proprght", # property and inheritance
           "busines", "freecomp", # establish busines, free market
           "conright", # consumer rights
           "scifree", "acfree",  # wissenschaftsfreiheit
           "achighed", # bildung
           "marriage", "fndfam", "matequal", "childpro", 2, # family
           "selfdet", # selfdetermination
           "life", 
           "slave", "torture", "cruelty",  
           "privacy",
           "freemove", 
           "opinion", "express", "petition", "censor", "press", "assem", "assoc",  # politische freiheit
           contains("intrght"), # international human rights declarations
           "devlpers", # persönliche freiheit
           "nomil", # verweigerung militär
           "asylum",
           "artspec_1"))) %>% 
  mutate(rights_political = rowSums(select(., "infoacc", "scifree", "acfree", "opinion", "express", "petition", "censor", "press", "assem", "assoc", contains("intrght"), "asylum","jointrde", "strike"))) %>% 
  mutate(rights_econ = rowSums(select(., "busines", "freecomp", "conright", "testate", "transfer", "inherit", contains("intprop"), "proprght"))) %>%  # also includes property 
  mutate(rights_ind = freerel, citren, selfdet, life, privacy, freemove, devlpers) %>%
  mutate(rights_social = rowSums(select(., "remuner", "socsec", "standliv", contains("finsup"), "shelter", "healthr", "safework", "childwrk", "achighed"))) %>% 
  select(contains("rights_")) ->
  ccpc_rights_sums

ccpc %>%
  cbind(ccpc_rights_sums) ->
  ccpc
  

# only relevant columns 
ccpc %>% 
  as_tibble() %>%
  select(country,year,syst,evnt,evnttype,overthrw,amend,execindp,intexec,invexe,levjud,judind,judprec,judfin, hosterm, contains("rights_")) -> 
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
saveRDS(ccpc_vdem_ela,file="data/ccpc_vdem_ela.rds")

