### Regression ###

rm(list=ls())

# libraries ----
library(tidyverse)
library(logistf)

# import data ----

ccpc_vdem <- readRDS("data/ccpc_vdem.rds")
ccpc_vdem_ela <- readRDS("data/ccpc_vdem_ela.rds")

# frequency of constitutional changes

model1 <- flic(newconst ~ pop_as_pm + amendment_exe + country, 
              data = ccpc_vdem_ela)
summary(model1)

model2 <- flic(amendment ~ pop_in_gov + amendment_exe + country, 
               data = ccpc_vdem_ela)
summary(model2)

model3 <- flic(constchange ~ pop_in_gov, 
               data = ccpc_vdem_ela)
summary(model3)


# regression and constitutional change

model_rc1 <- lm(regression_lag_libdem ~ newconst_2y, 
                data = ccpc_vdem_ela)
summary(model_rc1)

# support for our hypotheses 

model_rc2 <- lm(regression_lag_libdem ~ newconst_2y + pop_in_gov + newconst_2y*pop_in_gov, 
                data = ccpc_vdem_ela)
summary(model_rc2)

