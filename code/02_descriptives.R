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


# absolute, all countries
ccpc_vdem %>%
  mutate(pop_in_gov = as.factor(ifelse(gov_popul_weighted > 0.5, 1, 0))) %>%
  filter(!is.na(pop_in_gov)) %>%
  filter(!is.na(evnttype)) %>%
  group_by(pop_in_gov, evnttype) %>%
  summarise(n_event = n()) %>%
  ggplot(aes(x= evnttype, y = n_event, fill = pop_in_gov)) +
  geom_bar(position = "dodge", stat = "identity") +
  ylab("n")+
  xlab("Eventtype CCPC")

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
  ggplot(aes(x= pop_in_gov, y = share, fill = as.factor(evnttype))) +
  geom_bar(position = "Stack", stat = "identity") +
  ylab("Share of Years") +
  xlab( "Eventtype CCPC")

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
  ggplot(aes(x= pop_in_gov, y = share, fill = as.factor(evnttype))) +
  geom_bar(position = "Stack", stat = "identity") +
  ylab("Share of Years") +
  xlab( "Eventtype CCPC")

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
  ggplot(aes(x= pop_in_gov, y = share, fill = as.factor(evnttype))) +
  geom_bar(position = "Stack", stat = "identity") +
  ylab("Share of Years") +
  xlab( "Eventtype CCPC")

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
  ggplot(aes(x= pop_in_gov, y = share, fill = as.factor(evnttype))) +
  geom_bar(position = "Stack", stat = "identity") +
  ylab("Share of Years") +
  xlab( "Eventtype CCPC")

# number of rights ----

# all in one

ccpc_vdem_ela %>%
  select(country, year, diff_rights_ind, diff_rights_soc, diff_rights_pol, diff_executive, regression_judiciary, regression_lag_libdem, pop_in_gov) %>% 
  filter(regression_lag_libdem < -0.01) %>% 
  filter(pop_in_gov == 1) %>%
  rename(diff_judiciary = regression_judiciary) %>%
  filter(year > 1990) %>% 
  select(-regression_lag_libdem) %>%
  pivot_longer(cols = contains("diff"), values_to = "n", names_to = "rights") %>% 
  filter(n != 0) %>%
  ggplot(aes(x = rights, y = n, label=paste(country, year))) +
  geom_jitter(position=position_jitter(0.1)) +
  geom_text_repel() +
  geom_hline(yintercept=0, color = "black") +
  scale_x_discrete(labels=c("diff_executive" = "Executive Power", "diff_judiciary" = "Judicial Independence",
                            "diff_rights_pol" = "Political Rights", "diff_rights_soc" = "Social Rights")) +
  ylim(-3, 8)
  View()

# rights rule of law - nothing interesting  
ccpc_vdem_ela %>%
  ggplot(aes(x = year, y = rights_ruleolaw - lag_rights_rol, color = pop_in_gov, label = paste(country, year))) +
  geom_point() +
  geom_text_repel(data=subset(ccpc_vdem_ela, rights_ruleolaw - lag_rights_rol < -6 | rights_ruleolaw - lag_rights_rol > 6 ),
                  aes(year, rights_ruleolaw - lag_rights_rol,label=paste(country, year))) +
  ylab("Change in the number of rights on rule of law") +
  xlab("Year")

# sum of rights, no interesting pattern
ccpc_vdem_ela %>%
  ggplot(aes(x = year, y = rights_sum - lag_rights_sum, color = pop_in_gov, label = paste(country, year))) +
  geom_point() +
  geom_text_repel(data=subset(ccpc_vdem_ela, rights_sum - lag_rights_sum < -3 | rights_sum - lag_rights_sum > 11 ),
                  aes(year, rights_sum - lag_rights_sum,label=paste(country, year))) +
  ylab("Change in the number of rights") +
  xlab("Year")


# social rights - interesting pattern - onyl vrey few cases though
ccpc_vdem_ela %>%
  ggplot(aes(x = year, y = rights_social - lag_rights_soc, color = pop_in_gov, label = paste(country, year))) +
  geom_point() +
  geom_text_repel(data=subset(ccpc_vdem_ela, rights_social - lag_rights_soc < -1 | rights_social - lag_rights_soc > 5 ),
                  aes(year, rights_social - lag_rights_soc,label=paste(country, year))) +
  ylab("Change in the number of social rights") +
  xlab("Year")

ggsave("results/regression_socialrights.pdf", device = cairo_pdf)

# individual rights - nothing at all
ccpc_vdem_ela %>%
  ggplot(aes(x = year, y = rights_ind - lag_rights_ind, color = pop_in_gov, label = paste(country, year))) +
  geom_point() +
  geom_text_repel(data=subset(ccpc_vdem_ela, rights_ind - lag_rights_ind < -0.5 | rights_ind - lag_rights_ind > 0.5 ),
                  aes(year, rights_ind - lag_rights_ind,label=paste(country, year))) +
  ylab("Change in the number of individual rights") +
  xlab("Year")

# poltitical rights, again pretty much nothing
ccpc_vdem_ela %>%
  ggplot(aes(x = year, y = rights_political - lag_rights_pol, color = pop_in_gov, label = paste(country, year))) +
  geom_point() +
  geom_text_repel(data=subset(ccpc_vdem_ela, rights_political - lag_rights_pol < 0 | rights_political - lag_rights_pol > 5 ),
                  aes(year, rights_political - lag_rights_pol,label=paste(country, year))) +
  ylab("Change in the number of political rights") +
  xlab("Year")



# executive ----

ccpc_vdem_ela %>%
  ggplot(aes(x= year, y = diff_executive, color = pop_in_gov, label = paste(country, year))) +
  geom_point() +
  geom_text_repel(data=subset(ccpc_vdem_ela, diff_executive > 0 ),
                  aes(year, diff_executive,label=paste(country, year))) +
  ylab("Change in the additive index on executive power") +
  xlab("Year")


ggsave("results/executive.pdf", device = cairo_pdf)

ccpc_vdem_ela %>%
  ggplot(aes(x= year, y = diff_hosterm, color = pop_in_gov, label = paste(country, year))) +
  geom_point() +   
  geom_text_repel(data=subset(ccpc_vdem_ela, diff_hosterm != 0),
                                   aes(year, diff_hosterm,label=paste(country, year))) +
  ylab("Change in the term length of head of state") +
  xlab("Year")


ccpc_vdem_ela %>%
  filter(!is.na(term_change)) %>%
  group_by(pop_in_gov) %>%
  mutate(n_years = n()) %>%
  group_by(term_change, pop_in_gov) %>%
  summarise(n_change = n(), n_years = n_years) %>%
  unique() %>% 
  mutate(share = n_change/n_years*100) %>%
  ggplot(aes(x= pop_in_gov, y = share, fill = as.factor(term_change))) +
  geom_bar(position = "stack", stat = "identity") +
  theme(legend.position = "bottom") +
  labs(y = "Share of Years", x = "Populists in Government", fill = "Was there an increase in terms allowed for the head of state?") 

ccpc_vdem_ela %>%
  ggplot(aes(x= year, y = term_change, color = pop_in_gov)) +
  geom_point() +   
  geom_text_repel(data=subset(ccpc_vdem_ela, term_change != 0),
                  aes(year, term_change,label=paste(country, year))) +
  ylab("Was there an increase in terms allowed for hos 1 = yes") +
  xlab("Year")

# judiciary ----

# additive
ccpc_vdem_ela %>%
  ggplot(aes(x= year, y = regression_judiciary, color = pop_in_gov)) +
  geom_point() +
  geom_text_repel(data=subset(ccpc_vdem_ela, regression_judiciary > 1 | regression_judiciary < 0 ),
                  aes(year, regression_judiciary,label=paste(country, year))) +
  ylab("Change in additive index on independent judiciary") +
  xlab("Year")

ggsave("results/regression_judiciary.pdf", device = cairo_pdf)

# length of term
ccpc_vdem_ela %>%
  ggplot(aes(x= year, y = diff_conterm, color = pop_in_gov, label = paste(country, year))) +
  geom_point() +
  geom_text_repel(data=subset(ccpc_vdem_ela, diff_conterm != 0 ),
                  aes(year, diff_conterm,label=paste(country, year))) +
  ylab("Change in the Length of Terms of CC Justices") +
  xlab("Year")

ggsave("results/termlength_judiciary.pdf", device = cairo_pdf)

# is judicial independence mentioned?
ccpc_vdem_ela %>%
  ggplot() +
  geom_point(aes(x= year, y = diff_judind, color = pop_in_gov)) +
  geom_text_repel(data=subset(ccpc_vdem_ela, diff_judind != 0 ),
                  aes(year, diff_judind,label=paste(country, year))) +
  ylab("Is judicial independence removed from Constitution?") +
  xlab("Year")
 
# right to constitutional review - nothing really (first seven countries)

ccpc_vdem_ela %>% 
  group_by(country) %>%
  select(country, contains("challeg")) %>%
  mutate_if(is.numeric, ~.-lag(.)) %>% 
  rowwise() %>% 
  mutate(freq = sum(c_across(challeg_1:challeg_8)==-1)) %>% # count frequency how often review rights are taken away
  arrange(desc(freq)) 

# nominations of constitutional court judges - only Peru has changes in executive power over nomination if justices

ccpc_vdem_ela %>%
  mutate(nomination_c_exe = ifelse(connom_1 == 1 | connom_2 == 1 | connom_3 == 1, 1, 0)) %>%
  mutate(nomination_c_leg = ifelse(connom_4 == 1 | connom_5 == 1, 1, 0)) %>%
  mutate(nomination_c_jud = ifelse(connom_6 == 1 | connom_7 == 1, 1, 0)) %>%
  mutate(across(contains("nomination"), ~ .-lag(.))) %>% 
  arrange(nomination_c_exe) 

# when are voting rights taken away

ccpc_vdem_ela %>%
  ggplot(aes(x = year, y = voting, color = pop_in_gov)) +
  geom_point() +
  geom_text_repel(data=subset(ccpc_vdem_ela, voting != 0),
                  aes(year,voting,label=paste(country, year))) +
  theme(legend.position = "bottom") +
  ylab("From how many groups are voting rights taken away") +
  xlab("Year")

ggsave("results/votingrights.pdf", device = cairo_pdf)


# constitutional change and democratic regression
ccpc_vdem_ela %>%
  ggplot(aes(x = year, y = regression_lag_libdem, color = as.factor(constchange_2y))) +
  geom_point() +
  geom_text_repel(data=subset(ccpc_vdem_ela, regression_lag_libdem < -0.05 | regression_lag_libdem > 0.18 | (regression_lag_libdem > 0.1 & year == 2018)),
            aes(year,regression_lag_libdem,label=paste(country, year))) +
  theme(legend.position = "bottom") +
  labs(y = "Regression Liberal Democracy V-Dem", x = "Year", color = "Was there a Constitutional Change in That Year or the Year Before?") 

ggsave("results/constitutionalchange.pdf", device = cairo_pdf)


# only populist changes of the constitution

ccpc_vdem_ela %>%
  mutate(populist_incoming = lead(pop_in_gov)) %>%
  mutate(change_incoming = lead(evnttype)) %>%
  filter(populist_incoming == 1 | pop_in_gov == 1) %>%
  filter(evnttype == 1 & pop_in_gov == 1 | evnttype == 3 & pop_in_gov == 1 | change_incoming == 1 & populist_incoming == 1 | change_incoming == 3 & populist_incoming == 1) %>% View()
