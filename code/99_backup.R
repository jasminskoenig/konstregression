library(tidyverse)
library(lubridate)
library(vdemdata)

# ADD VDEM AND CCPC DATA TO PARTYDATA

# VDEM ----
vdem %>% 
  filter(year > 1989) -> 
  vdem2

vdem2 %>% 
  select(country_name, year, country_id, e_regiongeo, v2x_polyarchy,
         v2x_libdem, v2x_delibdem, v2x_egaldem, v2x_liberal, v2xcl_rol,
         v2x_jucon, v2jureform, v2jupurge, v2jupoatck, v2jupack,
         v2juaccnt, v2jucorrdc, v2juhcind, v2juncind, v2juhccomp,
         v2jucomp, v2jureview) -> 
  vdem2

joined_vdata <- left_join(vdem2,
                          partydat,
                          by=c("country_name", "year", "e_regiongeo")) 

glimpse(joined_vdata)

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
       rooduijn_prime) %>% 
  ungroup() -> 
  joined_vdata

# was there a single party government
joined_vdata$singleparty_gov <- ifelse(
  joined_vdata$no_govparties == 1, 1, 0
)

# clean names
joined_vdata %>%
  rename(country = country_name) -> 
  joined_vdata

# CCPC ----
ccpc <- read.csv("repli/ccpcnc_v3.csv")

ccpc %>% 
  filter(year>1989) -> 
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