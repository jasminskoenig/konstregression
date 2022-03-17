### DESCRIPTIVES ###

rm(list=ls())

# libraries ----
library(ggplot2) 
library(hrbrthemes)
library(tidyverse)
library(Cairo)
library(ggrepel)

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

# rights rule of law - nothing interesting  
ccpc_vdem_ela %>%
  ggplot(aes(x = year, y = rights_ruleolaw - lag_rights_rol, color = pop_in_gov, label = paste(country, year))) +
  geom_point() +
  geom_text_repel(data=subset(ccpc_vdem_ela, rights_ruleolaw - lag_rights_rol < -6 | rights_ruleolaw - lag_rights_rol > 6 ),
                  aes(year, rights_ruleolaw - lag_rights_rol,label=paste(country, year)))

# sum of rights, no interesting pattern
ccpc_vdem_ela %>%
  ggplot(aes(x = year, y = rights_sum - lag_rights_sum, color = pop_in_gov, label = paste(country, year))) +
  geom_point() +
  geom_text_repel(data=subset(ccpc_vdem_ela, rights_sum - lag_rights_sum < -3 | rights_sum - lag_rights_sum > 11 ),
                  aes(year, rights_sum - lag_rights_sum,label=paste(country, year)))


# social rights - interesting pattern - onyl vrey few cases though
ccpc_vdem_ela %>%
  ggplot(aes(x = year, y = rights_social - lag_rights_soc, color = pop_in_gov, label = paste(country, year))) +
  geom_point() +
  geom_text_repel(data=subset(ccpc_vdem_ela, rights_social - lag_rights_soc < -1 | rights_social - lag_rights_soc > 5 ),
                  aes(year, rights_social - lag_rights_soc,label=paste(country, year)))

# individual rights - nothing at all
ccpc_vdem_ela %>%
  ggplot(aes(x = year, y = rights_ind - lag_rights_ind, color = pop_in_gov, label = paste(country, year))) +
  geom_point() +
  geom_text_repel(data=subset(ccpc_vdem_ela, rights_ind - lag_rights_ind < -0.5 | rights_ind - lag_rights_ind > 0.5 ),
                  aes(year, rights_ind - lag_rights_ind,label=paste(country, year)))

# poltitical rights, again pretty much nothing
ccpc_vdem_ela %>%
  ggplot(aes(x = year, y = rights_political - lag_rights_pol, color = pop_in_gov, label = paste(country, year))) +
  geom_point() +
  geom_text_repel(data=subset(ccpc_vdem_ela, rights_political - lag_rights_pol < 0 | rights_political - lag_rights_pol > 5 ),
                  aes(year, rights_political - lag_rights_pol,label=paste(country, year)))

ccpc_vdem_ela %>%
  mutate(pop_in_gov = as.factor(ifelse(gov_popul_prime > 0.5, 1, 0))) %>% filter(is.na(pop_in_gov)) %>% View()

# executive ----

ccpc_vdem_ela %>%
  ggplot(aes(x= year, y = diff_executive, color = pop_in_gov, label = paste(country, year))) +
  geom_point() +
  geom_text_repel(data=subset(ccpc_vdem_ela, diff_executive > 0 ),
                  aes(year, diff_executive,label=paste(country, year)))

ccpc_vdem_ela %>%
  mutate_at(vars(starts_with("hos")), ~as.numeric(.)) %>%
  mutate(hosterm = na_if(hosterm, 99)) %>%
  mutate(hosterm = na_if(hosterm, 0)) %>%
  group_by(country) %>%
  mutate(last_hosterm = lag(hosterm)) %>%
  filter(!is.na(last_hosterm)) %>% 
  mutate(diff_hosterm = hosterm - last_hosterm) %>% 
  ggplot(aes(x= year, y = diff_hosterm, color = pop_in_gov, label = paste(country, year))) +
  geom_point() 


# judiciary ----

# additive
ccpc_vdem_ela %>%
  ggplot(aes(x= year, y = regression_judiciary, color = pop_in_gov)) +
  geom_point() +
  geom_text_repel(data=subset(ccpc_vdem_ela, regression_judiciary > 1 | regression_judiciary < 0 ),
                  aes(year, regression_judiciary,label=paste(country, year)))

ggsave("results/regression_judiciary.pdf", device = cairo_pdf)

# length of term
ccpc_vdem_ela %>%
  ggplot(aes(x= year, y = diff_conterm, color = pop_in_gov, label = paste(country, year))) +
  geom_point() +
  geom_text_repel(data=subset(ccpc_vdem_ela, diff_conterm != 0 ),
                  aes(year, diff_conterm,label=paste(country, year)))

# is judicial independence mentioned?
ccpc_vdem_ela %>%
  ggplot() +
  geom_point(aes(x= year, y = diff_judind, color = pop_in_gov)) +
  geom_text_repel(data=subset(ccpc_vdem_ela, diff_judind != 0 ),
                  aes(year, diff_judind,label=paste(country, year)))

# right to constitutional review - nothing really

ccpc_vdem_ela %>% 
  group_by(country) %>%
  select(country, contains("challeg")) %>%
  mutate_if(is.numeric, ~.-lag(.)) %>% 
  rowwise() %>% 
  mutate(freq = sum(c_across(challeg_1:challeg_8)==-1)) %>% # count frequency how often review rights are taken away
  arrange(desc(freq))

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

# constitutional change and democratic regression
ccpc_vdem_ela %>%
  ggplot(aes(x = year, y = regression_lag_libdem, color = as.factor(constchange_2y))) +
  geom_point() +
  geom_text_repel(data=subset(ccpc_vdem_ela, regression_lag_libdem < -0.05 | regression_lag_libdem > 0.18 ),
            aes(year,regression_lag_libdem,label=paste(country, year))) +
  theme(legend.position = "bottom")

ggsave("results/regression_constchange.pdf", device = cairo_pdf)


# only populist changes of the constitution

ccpc_vdem_ela %>%
  mutate(populist_incoming = lead(pop_in_gov)) %>%
  mutate(change_incoming = lead(evnttype)) %>%
  filter(populist_incoming == 1 | pop_in_gov == 1) %>%
  filter(evnttype == 1 & pop_in_gov == 1 | evnttype == 3 & pop_in_gov == 1 | change_incoming == 1 & populist_incoming == 1 | change_incoming == 3 & populist_incoming == 1) %>% View()
