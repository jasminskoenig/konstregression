
# merging VDem, PopuList, und ParlGov data

rm(list=ls())

library(tidyverse)
library(rio)
library(lubridate)
library(readtext)
library(stringi)


# data import ----
library(vdemdata)

# vparty ----
vparty %>% 
  filter(year>1989) -> 
  vparty2

vparty2 %>% 
  select(v2paenname, v2paorname, v2paid,country_name,year, country_id,e_regiongeo,v2xpa_antiplural,v2xpa_popul,v2paseatshare,v2patotalseat,
         v2pavote,v2pagovsup,ep_type_populism,ep_type_populist_values, ep_v8_popul_rhetoric,ep_v9_popul_saliency) -> 
  vparty2

# check parties in latin america 
vparty2 %>%
  filter(e_regiongeo %in% c(17:29)) %>% 
  filter(v2pagovsup %in% c(0,1,2)) %>% 
  distinct(v2paenname, .keep_all = TRUE) %>% 
  select(v2paenname, country_name, v2paorname) ->
  parties_la

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
         v2x_liberal,v2xcl_rol,v2x_jucon, v2x_cspart, v2jureform,v2jupurge,v2jupoatck,v2jupack,v2juaccnt,v2jucorrdc,
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
  mutate(across(.cols = everything(), ~ifelse(.==1, 1, 0))) ->
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
  select(country,year,syst,evnt,evnttype,overthrw,amend,execindp,intexec,invexe,levjud,judind,judprec,judfin, conterm, conlim, hosterm, 
         contains("rights_"), hosdec, hosterml, emdecl, hogdec, legdiss, legapp, contains("challeg"), amndprop_1, amndprop_2,amndprop_3,
         contains("connom"), contains("votelim"), contains("conap"), conrem, amndapct, contains("amndappr"), contains("jrem"), judsal) %>%
  select(-challeg_90, challeg_96, challeg_98, -contains("supap_9"), -contains("supnom_9"), -contains("connom_9"), -contains("amndappr_9"), -votelim_90, -votelim_96, -votelim_98) -> 
  ccpc

head(ccpc)

# join vparty, vdem and ccpc
ccpc_vdem <- merge(data,ccpc,by=c("country","year"),all.x=TRUE) %>%
  filter(v2x_regime %in% c(1:3)) %>% # only include liberal and electoral democracies and electoral autocracies
  mutate(pop_in_gov = as.factor(ifelse(gov_popul_weighted > 0.5, 1, 0))) %>% # binary variable on populists in government
  mutate(pop_as_pm = as.factor(ifelse(gov_popul_prime > 0.5, 1, 0))) %>% # binary variable on populist as head of government
  mutate(amendment = as.factor(ifelse(evnttype == 1, 1, 0))) %>%
  mutate(newconst = as.factor(ifelse(evnttype == 3, 1, 0))) %>%
  mutate(constchange = as.factor(ifelse(evnttype == 1 | evnttype == 3, 1, 0))) %>%
  mutate(amendment_exe = as.factor(ifelse(amndprop_1 == 1|amndprop_2 == 1| amndprop_3 == 1, 1, 0)))

head(ccpc_vdem)

# choose only populist const change & see which variables change
# XXX HERE

data_for_pop <- data |> 
  select(country, year, contains("gov_popul"), v2x_regime, e_regiongeo) 

ccpc_pop <- merge(data_for_pop, ccpc, by=c("country","year"), all.x=TRUE) %>%
  filter(v2x_regime %in% c(1:3)) %>% # only include liberal and electoral democracies and electoral autocracies
  mutate(pop_in_gov = as.factor(ifelse(gov_popul_weighted > 0.5, 1, 0))) %>% # binary variable on populists in government
  mutate(pop_as_pm = as.factor(ifelse(gov_popul_prime > 0.5, 1, 0))) %>% # binary variable on populist as head of government
  mutate(amendment = as.factor(ifelse(evnttype == 1, 1, 0))) %>%
  mutate(newconst = as.factor(ifelse(evnttype == 3, 1, 0))) %>%
  mutate(constchange = as.factor(ifelse(evnttype == 1 | evnttype == 3, 1, 0))) %>%
  mutate(amendment_exe = as.factor(ifelse(amndprop_1 == 1|amndprop_2 == 1| amndprop_3 == 1, 1, 0)))

ccpc_pop |> 
  group_by(country) |> 
  mutate(constchange_lead = lead(constchange),
         gov_popul_mean_lead = lead(gov_popul_mean),
         gov_popul_prime_lead = lead(gov_popul_prime),
         gov_popul_weighted_lead = lead(gov_popul_weighted),
         filter_yes = case_when(
           constchange == 1 & gov_popul_mean > 0.5 ~ 1,
           constchange == 1 & gov_popul_weighted > 0.5 ~ 1,
           constchange == 1 & gov_popul_prime == 1  ~ 1,
           constchange_lead == 1 & gov_popul_mean_lead > 0.5 ~ 1,
           constchange_lead == 1 & gov_popul_weighted_lead > 0.5 ~ 1,
           constchange_lead == 1 & gov_popul_prime_lead == 1  ~ 1
         )) |> 
  ungroup() |> 
  filter(filter_yes == 1) |>
  filter(e_regiongeo %in% c(1:4,17:29)) |> 
  select(where(~n_distinct(., na.rm = TRUE) > 1)) ->
  pop_constchange

pop_constchange |> 
  filter(country == "Bolivia") |>  
  select(where(~n_distinct(., na.rm = TRUE) > 1)) |>  View()

# variable on liberal democratic regression
ccpc_vdem %>%
  group_by(country) %>%
  mutate(lag_libdem = lag(v2x_libdem)) %>%
  mutate(regression_lag_libdem = v2x_libdem - lag_libdem) %>%
  mutate(constchange_2y = ifelse(evnttype == 1 | evnttype == 3 | lag(evnttype) == 1 | lag(evnttype) == 3, 1, 0 )) %>%
  mutate(newconst_2y = ifelse(evnttype == 3 | lag(evnttype) == 3, 1, 0 )) %>%
  mutate(amendment_2y = ifelse(evnttype == 1 | lag(evnttype) == 1, 1, 0 )) ->
  ccpc_vdem

# laggig ccpc variables and calculating differences for some of them
ccpc_vdem %>%
  mutate_at(vars(starts_with("con")), ~as.numeric(.)) %>%
  mutate_at(vars(starts_with("jud")), ~as.numeric(.)) %>%
  mutate_at(vars(starts_with("hos")), ~as.numeric(.)) %>%
  mutate(hosterm = na_if(hosterm, 99)) %>%
  mutate(hosterm = na_if(hosterm, 0)) %>%
  group_by(country) %>%
  mutate(lag_rights_rol = lag(rights_ruleolaw)) %>%  # lagging the number of rights
  mutate(lag_rights_sum = lag(rights_sum)) %>% 
  mutate(lag_rights_ind = lag(rights_ind)) %>% 
  mutate(lag_rights_pol = lag(rights_political)) %>% 
  mutate(lag_rights_soc = lag(rights_social)) %>%
  mutate(diff_rights_rol = rights_ruleolaw - lag_rights_rol) %>%  # lagging the number of rights
  mutate(diff_rights_sum = rights_sum - lag_rights_sum) %>% 
  mutate(diff_rights_ind = rights_ind - lag_rights_ind) %>% 
  mutate(diff_rights_pol = rights_political - lag_rights_pol) %>% 
  mutate(diff_rights_soc = rights_social - lag_rights_soc) %>%
  mutate(decree = ifelse(hosdec == 1 | hogdec == 1, 1, 0)) %>% # from here on executive
  mutate(emergency = ifelse(emdecl < 5, 1, 0)) %>%
  mutate(removal_leg = ifelse(legdiss < 5, 1, 0)) %>%
  mutate(veto = ifelse(legapp < 5, 1, 0)) %>%
  mutate(review = ifelse(challeg_1 == 1 | challeg_2 == 1| challeg_3 == 1, 1, 0)) %>%
  mutate(amendement = ifelse(amndprop_1 == 1 | amndprop_2 == 1| amndprop_3 == 1, 1, 0)) %>%
  mutate(executive = decree + emergency + removal_leg + veto + review + amendement) %>% # additive indice for executive power
  mutate(last_executive = lag(executive)) %>%
  mutate(diff_executive = executive - last_executive) %>%
  mutate(last_hosterm = lag(hosterm)) %>% 
  mutate(diff_hosterm = hosterm - last_hosterm) %>% # difference term length head of state
  mutate(hosterml = na_if(hosterml, 99)) %>%
  mutate(hosterml = na_if(hosterml, 96)) %>%
  mutate(lag_hosterml = lag(hosterml)) %>%
  mutate(term_change = case_when(
    hosterml != 1 & lag_hosterml == 1 ~ 1,
    lag_hosterml == 2 & hosterml != 3 & hosterml != 1 & hosterml != 2 ~ 1,
    lag_hosterml == 3 & hosterml != 1 & hosterml != 3  ~ 1,
    lag_hosterml == 4 & hosterml == 5 & hosterml != 4 ~ 1,
    is.na(hosterml) | is.na(lag_hosterml) ~ NA_real_,
    TRUE ~ 0)) %>% # more terms allowed for head of state
  mutate(conterm = na_if(conterm, 99)) %>% # from here on judiciary
  mutate(conterm = na_if(conterm, 0)) %>%
  mutate(conterm = na_if(conterm, 99)) %>%
  mutate(conterm = na_if(conterm, 0)) %>%
  mutate(selection = ifelse(connom_1 + connom_2 + connom_3 + connom_4 + connom_5 + connom_7 + conap_1 + conap_2 + conap_3 + conap_4 + conap_5 + conap_7 > 1 | connom_6 + conap_6 > 0, 1, 0)) %>% # 2 actors involved or judicial council
  mutate(appointment = ifelse(conlim == 1, 1, 0)) %>% # no re-election
  mutate(removal = ifelse(jrempro_1 + jrempro_2 + jrempro_3 + jrempro_4 + jrempro_5 + jrempro_7 + jrempro_9 + jremap_1 + jremap_2 + jremap_3 + jremap_4 + jremap_5 + jremap_7 > 1 | jrempro_8 > 0 | jremap_6 + jrempro_6 > 0 | jrem == 2, 1, 0)) %>% # more than 2 actors or judiciary council involved in removal
  mutate(removal_reason = ifelse(jremcon_1 != 1 & jremcon_5 != 1 & jremcon_96 != 1, 1, 0)) %>%
  mutate(salary = ifelse(judsal == 1, 1, 0)) %>%
  mutate(judiciary = judind + selection + appointment + appointment + removal + removal_reason + salary) %>% # additive indice for judicial independence (ginsburg/melton 2014)
  mutate(lag_judiciary = lag(judiciary)) %>%
  mutate(regression_judiciary = judiciary - lag_judiciary) %>%
  mutate(last_conterm = lag(conterm)) %>%
  mutate(last_conlim = lag(conlim)) %>% 
  mutate(diff_conterm = conterm - last_conterm) %>%
  mutate(last_judind = lag(judind)) %>%
  mutate(diff_judind = judind - last_judind) ->
  ccpc_vdem

ccpc_vdem %>%
  select(country, contains("votelim")) %>%
  group_by(country) %>%
  mutate_if(is.numeric, ~.-lag(.)) %>% 
  rowwise() %>% 
  mutate(freq = -1*(sum(c_across(votelim_1:votelim_14)==1))) %>%
  pull(freq) ->
  ccpc_vdem$voting

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

