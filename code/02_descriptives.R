### DESCRIPTIVES ###

rm(list=ls())

# libraries ----
library(ggplot2) 
library(hrbrthemes)
library(tidyverse)

theme_set(theme_ipsum(base_size = 14, axis_title_size = 14, strip_text_size = 14, axis_text_size = 14, base_family = "Noto Sans"))

# import data ----

ccpc_vdem <- readRDS("data/ccpc_vdem.rds")
ccpc_vdem_ela <- readRDS("data/ccpc_vdem_ela.rds")

# number of changes ----


# absolute
ccpc_vdem %>%
  mutate(pop_in_gov = as.factor(ifelse(gov_popul_weighted > 0.5, 1, 0))) %>%
  filter(!is.na(pop_in_gov)) %>%
  filter(!is.na(evnttype)) %>%
  group_by(pop_in_gov, evnttype) %>%
  summarise(n_event = n()) %>%
  ggplot(aes(x= evnttype, y = n_event, fill = pop_in_gov)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_minimal()

# share of years in power for each eventtype 

ccpc_vdem %>%
  mutate(pop_in_gov = as.factor(ifelse(gov_popul_weighted > 0.5, 1, 0))) %>%
  filter(!is.na(pop_in_gov)) %>%
  filter(!is.na(evnttype)) %>%
  group_by(pop_in_gov) %>%
  mutate(n_years = n()) %>%
  group_by(pop_in_gov, evnttype) %>%
  summarise(n_event = n(), n_years = n_years) %>%
  unique() %>% 
  mutate(share = n_event/n_years*100) %>%
  ggplot(aes(x= evnttype, y = share, fill = pop_in_gov)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_minimal()

# only europe and LA - share of years in power for each eventtype 

ccpc_vdem_ela %>%
  mutate(pop_in_gov = as.factor(ifelse(gov_popul_weighted > 0.5, 1, 0))) %>%
  filter(!is.na(pop_in_gov)) %>%
  filter(!is.na(evnttype)) %>%
  group_by(pop_in_gov) %>%
  mutate(n_years = n()) %>%
  group_by(pop_in_gov, evnttype) %>%
  summarise(n_event = n(), n_years = n_years) %>%
  unique() %>% 
  mutate(share = n_event/n_years*100) %>%
  ggplot(aes(x= evnttype, y = share, fill = pop_in_gov)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_minimal()

# share of years in power for each eventtype - prime minister's party

ccpc_vdem %>%
  mutate(pop_in_gov = as.factor(ifelse(gov_popul_prime > 0.5, 1, 0))) %>%
  filter(!is.na(pop_in_gov)) %>%
  filter(!is.na(evnttype)) %>%
  group_by(pop_in_gov) %>%
  mutate(n_years = n()) %>%
  group_by(pop_in_gov, evnttype) %>%
  summarise(n_event = n(), n_years = n_years) %>%
  unique() %>% 
  mutate(share = n_event/n_years*100) %>%
  ggplot(aes(x= evnttype, y = share, fill = pop_in_gov)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_minimal()

# same only europe and LA - share of years in power for each eventtype - prime minister's party
ccpc_vdem_ela %>%
  mutate(pop_in_gov = as.factor(ifelse(gov_popul_prime > 0.5, 1, 0))) %>%
  filter(!is.na(pop_in_gov)) %>%
  filter(!is.na(evnttype)) %>%
  group_by(pop_in_gov) %>%
  mutate(n_years = n()) %>%
  group_by(pop_in_gov, evnttype) %>%
  summarise(n_event = n(), n_years = n_years) %>%
  unique() %>% 
  mutate(share = n_event/n_years*100) %>%
  ggplot(aes(x= evnttype, y = share, fill = pop_in_gov)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_minimal()

# number of rights ----

ccpc_vdem_ela %>%
  mutate(pop_in_gov = as.factor(ifelse(gov_popul_prime > 0.5, 1, 0))) %>%
  ggplot(aes(x = pop_in_gov, y = rights_ruleolaw - lag(rights_ruleolaw), label = paste(country, year))) +
  geom_jitter()+
  geom_text()

ccpc_vdem_ela %>%
  mutate(pop_in_gov = as.factor(ifelse(gov_popul_prime > 0.5, 1, 0))) %>%
  ggplot(aes(x = pop_in_gov, y = rights_sum - lag(rights_sum), label = paste(country, year))) +
  geom_jitter()+
  geom_text()

ccpc_vdem_ela %>%
  mutate(pop_in_gov = as.factor(ifelse(gov_popul_prime > 0.5, 1, 0))) %>%
  ggplot(aes(x = pop_in_gov, y = rights_ind - lag(rights_ind), label = paste(country, year))) +
  geom_jitter()+
  geom_text()

ccpc_vdem_ela %>%
  mutate(pop_in_gov = as.factor(ifelse(gov_popul_prime > 0.5, 1, 0))) %>%
  ggplot(aes(x = pop_in_gov, y = rights_political - lag(rights_political), label = paste(country, year))) +
  geom_jitter()+
  geom_text()

ccpc_vdem_ela %>%
  mutate(pop_in_gov = as.factor(ifelse(gov_popul_prime > 0.5, 1, 0))) %>%
  ggplot(aes(x = pop_in_gov, y = rights_social - lag(rights_social), label = paste(country, year))) +
  geom_jitter()+
  geom_text()

ccpc_vdem_ela %>%
  mutate(pop_in_gov = as.factor(ifelse(gov_popul_prime > 0.5, 1, 0))) %>% filter(is.na(pop_in_gov)) %>% View()

# executive ----

ccpc_vdem_ela %>%
  mutate(pop_in_gov = as.factor(ifelse(gov_popul_weighted > 0.5, 1, 0))) %>%
  mutate(decree = ifelse(hosdec == 1 | hogdec == 1, 1, 0)) %>%
  mutate(emergency = ifelse(emdecl < 5, 1, 0)) %>%
  mutate(removal_leg = ifelse(legdiss < 5, 1, 0)) %>%
  mutate(veto = ifelse(legapp < 5, 1, 0)) %>%
  mutate(review = ifelse(challeg_1 == 1 | challeg_2 == 1| challeg_3 == 1, 1, 0)) %>%
  mutate(amendement = ifelse(amndprop_1 == 1 | amndprop_2 == 1| amndprop_3 == 1, 1, 0)) %>%
  mutate(executive = decree + emergency + removal_leg + veto + review + amendement) %>% 
  mutate(last_executive = lag(executive)) %>%
  mutate(diff_executive = executive - last_executive) %>% 
  group_by(country) %>%
  ggplot(aes(x= pop_in_gov, y = diff_executive, label = paste(country, year))) +
  geom_jitter(size = 2, width = 0.2) 

ccpc_vdem_ela %>%
  mutate(pop_in_gov = as.factor(ifelse(gov_popul_weighted > 0.5, 1, 0))) %>%
  mutate_at(vars(starts_with("hos")), ~as.numeric(.)) %>%
  mutate(hosterm = na_if(hosterm, 99)) %>%
  mutate(hosterm = na_if(hosterm, 0)) %>%
  group_by(country) %>%
  mutate(last_hosterm = lag(hosterm)) %>%
  filter(!is.na(last_hosterm)) %>% 
  mutate(diff_hosterm = hosterm - last_hosterm) %>% 
  ggplot(aes(x= pop_in_gov, y = diff_hosterm, label = paste(country, year))) +
  geom_jitter() 


# judiciary ----

ccpc_vdem_ela %>%
  mutate_at(vars(starts_with("con")), ~as.numeric(.)) %>%
  mutate(conterm = na_if(conterm, 99)) %>%
  mutate(conterm = na_if(conterm, 0)) %>%
  mutate(conterm = na_if(conterm, 99)) %>%
  mutate(conterm = na_if(conterm, 0)) %>%
  group_by(country) %>%
  mutate(last_conterm = lag(conterm)) %>%
  mutate(last_conlim = lag(conlim)) %>% 
  mutate(diff_conterm = conterm - last_conterm) %>% 
  ggplot(aes(x= pop_in_gov, y = diff_conterm, label = paste(country, year))) +
  geom_jitter() +
  geom_text()


ccpc_vdem_ela %>%
  mutate_at(vars(starts_with("jud")), ~as.numeric(.)) %>%
  group_by(country) %>%
  mutate(last_judind = lag(judind)) %>%
  filter(!is.na(last_judind)) %>% 
  mutate(diff_judind = judind - last_judind) %>% 
  ggplot() +
  geom_jitter(aes(x= pop_in_gov, y = diff_judind)) +
  ylim(-5,5)

# right to constitutional review - nothing really

ccpc_vdem_ela %>% 
  group_by(country) %>%
  select(country, contains("challeg")) %>%
  mutate_if(is.numeric, ~.-lag(.)) %>% 
  rowwise() %>% 
  mutate(freq = sum(c_across(challeg_1:challeg_8)==-1))

# nominations of constitutional court judges

ccpc_vdem_ela %>%
  mutate(nomination_c_exe = ifelse(connom_1 == 1 | connom_2 == 1 | connom_3 == 1, 1, 0)) %>%
  mutate(nomination_c_leg = ifelse(connom_4 == 1 | connom_5 == 1, 1, 0)) %>%
  mutate(nomination_c_jud = ifelse(connom_6 == 1 | connom_7 == 1, 1, 0)) %>%
  mutate(across(contains("nomination"), ~ .-lag(.))) %>% 
  arrange(nomination_c_exe) %>% View()

# voting limitations - very seldom, but might be interesting

ccpc_vdem_ela %>%
  group_by(country) %>%
  select(country, contains("votelim")) %>%
  mutate_if(is.numeric, ~.-lag(.)) %>% 
  rowwise() %>% 
  mutate(freq = sum(c_across(votelim_1:votelim_14)==1)) %>%
  arrange(desc(freq)) %>% View()

ccpc_vdem_ela %>%
  ggplot(aes(x = year, y = regression_lag_libdem)) +
  geom_point() +
  geom_text(data=subset(ccpc_vdem_ela, regression_lag_libdem < -0.1),
            aes(year,regression_lag_libdem,label=paste(country, year)))

ccpc_vdem_ela %>%
  ggplot(aes(x = year, y = regression_lead_libdem)) +
  geom_point() +
  geom_text(data=subset(ccpc_vdem_ela, regression_lead_libdem < -0.1),
            aes(year,regression_lead_libdem,label=paste(country, year)))

ggsave("results/regression_lead.pdf", device = "pdf")

# only populist changes of the constitution

ccpc_vdem_ela %>%
  mutate(populist_incoming = lead(pop_in_gov)) %>%
  mutate(change_incoming = lead(evnttype)) %>%
  filter(populist_incoming == 1 | pop_in_gov == 1) %>%
  filter(evnttype == 1 & pop_in_gov == 1 | evnttype == 3 & pop_in_gov == 1 | change_incoming == 1 & populist_incoming == 1 | change_incoming == 3 & populist_incoming == 1) %>% View()
