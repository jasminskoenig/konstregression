### DESCRIPTIVES ###

rm(list=ls())

# libraries ----
library(ggplot2) 
library(hrbthemes)

# import data ----

ccpc_vdem <- readRDS("data/ccpc_vdem.rds")

# number of changes ----

# this uses tilkos dataset!

# absolute
ccpc_vdem %>%
  mutate(pop_in_gov = as.factor(ifelse(gov_popul_weighted > 0.6, 1, 0))) %>%
  filter(!is.na(pop_in_gov)) %>%
  filter(!is.na(evnttype)) %>%
  group_by(pop_in_gov, evnttype) %>%
  summarise(n_event = n()) %>%
  ggplot(aes(x= evnttype, y = n_event, fill = pop_in_gov)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_minimal()

# share of years in power - in shares they do use more amendments and impose new constitutions slightly more often

ccpc_vdem %>%
  mutate(pop_in_gov = as.factor(ifelse(gov_popul_weighted > 0.6, 1, 0))) %>%
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

# share of years in power - in shares they do use more amendments and impose new constitutions slightly more often

ccpc_vdem %>%
  mutate(pop_in_gov = as.factor(ifelse(gov_popul_prime > 0.6, 1, 0))) %>%
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

################################

# this was based on my dataset so it doesn't work yet ----

ccp %>%
  filter(country == "Hungary") %>%
  filter(evnttype != 4) %>%
  select(levjud, judind, contains("connom"), contains("conap"), conterm, conlim, contains("conres"), unconper, contains("challeg"), chalstag, amparo, contains("jrem")) %>%
  View()

ccp %>%
  filter(evnttype != 4) %>%
  select(conterm, conlim) %>%
  View()

# judiciary ----

ccp_gov %>%
  select(populist_vparty, country, evnttype, year, conterm, conlim) %>% 
  mutate_at(vars(starts_with("con")), ~as.numeric(.)) %>%
  mutate(populist_vparty = ifelse(populist_vparty > 0, 1, 0)) %>%
  filter(conterm < 90 | conlim < 90) %>%
  group_by(country) %>%
  mutate(last_conterm = lag(conterm)) %>%
  mutate(last_conlim = lag(conlim)) %>% 
  filter(!is.na(last_conterm)) %>% 
  mutate(diff_conterm = conterm - last_conterm) %>% 
  ggplot() +
  geom_jitter(aes(x= populist_vparty, y = diff_conterm))


ccp_gov %>%
  select(populist_vparty, country, evnttype, year, judind) %>% 
  mutate_at(vars(starts_with("jud")), ~as.numeric(.)) %>%
  mutate(populist_vparty = ifelse(populist_vparty > 0, 1, 0)) %>%
  group_by(country) %>%
  mutate(last_judind = lag(judind)) %>%
  filter(!is.na(last_judind)) %>% 
  mutate(diff_conterm = judind - last_judind) %>% 
  ggplot() +
  geom_jitter(aes(x= populist_vparty, y = diff_conterm))

# executive ----

ccp_gov %>%
  select(populist_vparty, country, evnttype, year, hosdec, emdecl, hogdec, legdiss, legapp, challeg_1, challeg_2, challeg_3, amndprop_1, amndprop_2,amndprop_3) %>% 
  mutate(decree = ifelse(hosdec == 1 | hogdec == 1, 1, 0)) %>%
  mutate(emergency = ifelse(emdecl < 5, 1, 0)) %>%
  mutate(removal_leg = ifelse(legdiss < 5, 1, 0)) %>%
  mutate(veto = ifelse(legapp < 5, 1, 0)) %>%
  mutate(review = ifelse(challeg_1 == 1 | challeg_2 == 1| challeg_3 == 1, 1, 0)) %>%
  mutate(amendement = ifelse(amndprop_1 == 1 | amndprop_2 == 1| amndprop_3 == 1, 1, 0)) %>%
  mutate(executive = decree + emergency + removal_leg + veto + review + amendement) %>% 
  mutate(last_executive = lag(executive)) %>%
  mutate(diff_executive = executive - last_executive) %>% 
  mutate(populist_vparty = ifelse(populist_vparty > 0, 1, 0)) %>%
  group_by(country) %>%
  ggplot(aes(x= populist_vparty, y = diff_executive)) +
  geom_jitter(size = 2, alpha = 0.25, width = 0.2)

ccp_gov %>%
  select(populist_vparty, country, evnttype, year, hosterm) %>% 
  mutate_at(vars(starts_with("hos")), ~as.numeric(.)) %>%
  mutate(populist_vparty = as.factor(ifelse(populist_vparty > 0, 1, 0))) %>%
  filter(hosterm != 0|hosterm <= 99) %>% 
  group_by(country) %>%
  mutate(last_hosterm = lag(hosterm)) %>%
  filter(!is.na(last_hosterm)) %>% 
  mutate(diff_hosterm = hosterm - last_hosterm) %>% 
  ggplot() +
  geom_jitter(aes(x= populist_vparty, y = diff_hosterm))
  