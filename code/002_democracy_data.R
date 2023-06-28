library(tidyverse)
library(lubridate)
library(vdemdata)

# ADD VDEM AND CCPC DATA TO PARTYDATA

# import party data
partydat <- readRDS("data/party_populism.rds")

# VDEM ----
vdem %>% 
  filter(year > 1989) -> 
  vdem2

vdem2 %>% 
  dplyr::select(country_name, year, country_id, e_regiongeo, v2x_polyarchy,
         v2x_libdem, v2x_delibdem, v2x_egaldem, v2x_liberal, v2xcl_rol, v2x_cspart,
         v2x_jucon, v2jureform, v2jupurge, v2jupoatck, v2jupack,
         v2juaccnt, v2jucorrdc, v2juhcind, v2juncind, v2juhccomp,
         v2jucomp, v2jureview, v2x_regime) |> 
  mutate(country_name = if_else(country_name == "Czechia", "Czech Republic", country_name)) -> 
  vdem2

joined_vdata <- left_join(vdem2,
                          partydat,
                          by=c("country_name", "year", "e_regiongeo")) 

glimpse(joined_vdata)

# add ruth data, as this isn't based on party but president

# Ruth 2022 ----

ruth <- read.csv("data/ruth2022.csv", header = FALSE)

ruth_smaller <- ruth |> 
  dplyr::select(V2, V4, V5, V7) |> 
  rename("country" = "V2",
         "year" = "V4",
         "president" = "V5",
         "ruth_populism" = "V7") |> 
  mutate(ruth_populism = case_when(
    ruth_populism == 1 ~ "Populist",
    ruth_populism == 0 ~ "Non-Populist",
    is.na(ruth_populism) ~ NA_character_
  ))

joined_vdata |> 
  left_join(ruth_smaller, by = c("country_name" = "country",
                                 "year" = "year")) ->
  joined_vdata

ruth_smaller |> 
  group_by(country) |> 
  distinct(country) |> 
  mutate(dataset = "Ruth-Lovell & Grahn") ->
  ruth_countries


# fill in vparty columns for the additional observations in vdem years
joined_vdata %>% 
  group_by(country_name) %>% 
  fill(gov_popul_mean,
       gov_popul_weighted,
       gov_seatshare,
       e_regiongeo,
       no_govparties,
       gov_popul_prime,
       rooduijn_government,
       rooduijn_government_senior) %>% 
  ungroup() -> 
  joined_vdata

# make table showing whether countries are included in datasets - needed for blog

joined_vdata %>%
  select(country_name, rooduijn_government, rooduijn_government_senior, ruth_populism, president, gov_popul_prime, gov_popul_mean, gov_popul_weighted) %>% 
  group_by(country_name) %>%
  summarize_all(~ sum(!is.na(.x))) ->
  country_datasets

write_csv(country_datasets, "C:/Users/Jasmin/OneDrive - Universität Hamburg/Desktop/Blog/data/country_datasets.csv")
  
# was there a single party government
joined_vdata$singleparty_gov <- ifelse(
  joined_vdata$no_govparties == 1, 1, 0
)

# clean names
joined_vdata %>%
  rename(country = country_name) -> 
  joined_vdata

joined_vdata |> 
  pull(country) |> 
  unique() ->
  country_list

# CCPC ----
ccpc <- read_csv("C:/Users/Jasmin/Dropbox/Jasmin - Tilko/Konstitutionelle Regression/data/ccpcnc_v3.csv")

ccpc %>% 
  filter(year > 1989) |> 
  mutate(country = case_when(
    country == "Slovak Republic" ~ "Slovakia",
    TRUE ~ country
  )) -> 
  ccpc

ccpc |> 
  filter(country %in% country_list)->
  ccpc

ccpc %>% 
  select(country, year, syst, evnt, evnttype, overthrw, amend,
         execindp, intexec, invexe, levjud, judind, judprec, judfin) |> 
  mutate(year = as.integer(year)) -> 
  ccpc

glimpse(ccpc)

ccpc_vdem <- left_join(joined_vdata,
                       ccpc,
                       by=c("country","year"))

glimpse(ccpc_vdem)

# Bolsonaro's party is coded as antipluralist but not populist
ccpc_vdem %>% 
  filter(country == "Brazil") 

save(ccpc_vdem,file="ccpc_vdem.Rdata")

# only la and europe

ccpc_vdem |> 
  filter(e_regiongeo %in% c(1:4, 17:18)) |> 
  arrange(country, year) |> 
  filter(country != "German Democratic Republic" & country != "Suriname") |> 
  mutate(date = ymd(year, truncated = 2L)) ->
  ccpc_vdem_eu_la

saveRDS(ccpc_vdem_eu_la, "data/ccpc_vdem_eu_la")

# v-party countries
# for loop blog ----

ccpc_vdem_eu_la |> 
  group_by(country) |> 
  distinct(country) |> 
  mutate(dataset = "V-Party") ->
  vparty_countries

# start and end date of year for plotting
ccpc_vdem_eu_la |>
  mutate(year_start = ymd(year, truncated = 2L)-184,
         year_end = ymd(year, truncated = 2L)+181,
         year_interval = interval(year_start, year_end),
         year_start = if_else(year_start < ymd("1990-01-01"), ymd("1990-01-01"), year_start),
         year_end = if_else(year_end > ymd("2020-12-31"), ymd("2020-12-31"), year_end)) |> 
  select(-starts_with("v2j"), -starts_with("jud"), -starts_with("int"),
         -levjud, -overthrw, -amend) |> 
  filter(year < 2022) ->
  ccpc_vdem_eu_la

saveRDS(ccpc_vdem_eu_la, "C:/Users/Jasmin/OneDrive - Universität Hamburg/Desktop/Blog/data/ccpc_vdem_eu_la.rds")
write_csv(country_overview, "C:/Users/Jasmin/OneDrive - Universität Hamburg/Desktop/Blog/data/overview.csv")

# vparty_countries |> 
#   left_join(popuList_countries, by = c("country" = "country_name")) |> 
#   left_join(ruth_countries, by = "country") ->
#   country_overview

# 
# ccpc_vdem_eu_la |> 
#   mutate(year_start = ymd(year, truncated = 2L),
#          year_before = ymd(year, truncated = 2L)-1) |> 
#   select(-starts_with("v2j"), -starts_with("jud"), -starts_with("int"), 
#          -levjud, -overthrw, -amend) |>
#   pivot_longer(c(year_start, year_before), names_to = "period", values_to = "date") |> 
#   group_by(country) |> 
#   arrange(country, date) |> 
#   mutate(across(starts_with("v2x_"), 
#                 .names = "final_{.col}", 
#                 .fns = ~ if_else(period == "year_before",
#                                lead(.x),
#                                .x))) |> 
#   mutate(across(c(gov_popul_weighted, gov_popul_mean, gov_popul_prime,
#                   ruth_populism,
#                   rooduijn_government, rooduijn_government_senior), 
#                 .names = "final_{.col}", 
#                 .fns = ~ if_else(period == "year_before",
#                                  lead(.x),
#                                  .x))) ->
#   ccpc_vdem_eu_la

