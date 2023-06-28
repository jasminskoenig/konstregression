# analyzing ccpc_vdem, hierarchical models
# TS: 22.08.2022

library(tidyverse)
library(lubridate)
library(stargazer)
library(rms)
library(interplot)
library(hrbrthemes)
library(lme4)
library(margins)
library(broom.mixed)
library(gridExtra)
library(margins)
library(grid)
library(patchwork)
extrafont::loadfonts()


theme_gridY <- theme_ipsum_rc(grid = "Y") +
  theme(axis.text = element_text(size = 8), 
        axis.title = element_text(size = 10), 
        strip.text = element_text(size = 10),
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.key.size = unit(0.5, "cm"),
        plot.margin=grid::unit(c(1,0.5,0.5,0), "mm"),
        panel.border = element_rect(colour = "darkgrey", fill=NA, linewidth=1))

# set theme
theme_set(theme_gridY)


load("repli/ccpc_vdem.Rdata")
ccpc_vdem_new <- readRDS("data/ccpc_vdem_eu_la")

# year as factor for random intercepts
ccpc_vdem$year <- as.numeric(as.character(ccpc_vdem$year))

# filter european countries
ccpc_vdem %>% 
  filter(e_regiongeo %in% c(1:4),
         year>1990,
         v2x_regime>0) -> 
  df2

# year to factor for FE
df2$year <- as.factor(df2$year)
summary(df2)

# Constitutional Change Likelihood ----

# are constitutional events more likely under populist governments?
m_const <- lm(lead(evnt,1) ~ gov_popul_weighted, data=df2)
summary(m_const)

# Country FE-Model
m_const <- lmer(lead(evnt,1) ~ gov_popul_weighted + (1|country), data=df2)
summary(m_const)

# Year FE-Model
m_const <- lmer(lead(evnt,1) ~ gov_popul_weighted + (1|country), data=df2)
summary(m_const)

# XXX plot effect for la and europe 

# try to calculate 95% confidence interval

try <- quantile(df2$gov_popul_weighted,
                probs=c(0.05,0.95),
                na.rm=TRUE)

# copied from https://www.azandisresearch.com/2022/12/31/visualize-mixed-effect-regressions-in-r-with-ggplot2/
X <- df2 |> 
  filter((!is.na(year) & !is.na(gov_popul_weighted) & !is.na(country) & !is.na(lead(evnt))))

X <- X %>% 
  mutate(fit.m = predict(m_const, re.form = NA),
         fit.c = predict(m_const, re.form = NULL))

X <- X %>%
  mutate(resid = resid(m_const))

X %>%
  ggplot(aes(x = gov_popul_weighted, y = fit.m + resid, col = country)) +
  geom_point(pch = 16) +
  geom_line(aes(y = fit.m), col = 1, linewidth = 2)  +
  coord_cartesian()

# Models from Paper ----

# random-intercepts for country and year
m0 <- lmer(lead(v2x_libdem,1) ~ (1|country) + (1|year), data=df2)
m0cs <- lmer(lead(v2x_cspart,1) ~ (1|country) + (1|year), data=df2)

# assess clustering structure of the data
performance::icc(m0)
performance::icc(m0cs)

## Liberal Democracy ----

# weighted government -> lib dem
m1 <- lmer(lead(v2x_libdem,1) ~ gov_popul_weighted + (1|country) + (1|year), data=df2)
summary(m1)

# change in constitution in general -> lib dem 
m2 <- lmer(lead(v2x_libdem,1) ~ evnt + (1|country) + (1|year), data=df2)
summary(m2) 

# interaction -> libdem
m3 <- lmer(lead(v2x_libdem,1) ~ evnt*gov_popul_weighted + (1|country) + (1|year), data=df2)
summary(m3)

## Participation ----

# weighted government -> part
m1cs <- lmer(lead(v2x_cspart,1) ~ gov_popul_weighted + (1|country) + (1|year), data=df2)
summary(m1cs) 

# change in constitution in general -> part
m2cs <- lmer(lead(v2x_cspart,1) ~ evnt + (1|country) + (1|year), data=df2)
summary(m2cs) 

# interaction -> part
m3cs <- lmer(lead(v2x_cspart,1) ~ evnt*gov_popul_weighted + (1|country) + (1|year), data=df2)
summary(m3cs)

# quantile calculations for text ----
# democracy index
qs <- quantile(df2$gov_popul_weighted,probs=c(0.05,0.95),na.rm=TRUE)
qs[1]*-0.113 - qs[2]*-0.113

# civil society index
qs <- quantile(df2$gov_popul_weighted,probs=c(0.05,0.95),na.rm=TRUE)
qs[1]*-0.197 - qs[2]*-0.197

# make table
tidym1 <- broom::tidy(m1) %>% filter(effect=="fixed")
tidym2 <- broom::tidy(m2) %>% filter(effect=="fixed")
tidym3 <- broom::tidy(m3) %>% filter(effect=="fixed")
tidym1cs <- broom::tidy(m1cs) %>% filter(effect=="fixed")
tidym2cs <- broom::tidy(m2cs) %>% filter(effect=="fixed")
tidym3cs <- broom::tidy(m3cs) %>% filter(effect=="fixed")

# counts of random effects
num_year <- nrow(ranef(m1)$year)
num_country <- nrow(ranef(m1)$country)


# standard deviation of random effects
sd_year1 <- round(attributes(VarCorr(m1)$"year")$stddev, 3)
sd_country1 <- round(attributes(VarCorr(m1)$"country")$stddev, 3)

sd_year2 <- round(attributes(VarCorr(m2)$"year")$stddev, 3)
sd_country2 <- round(attributes(VarCorr(m2)$"country")$stddev, 3)

sd_year3 <- round(attributes(VarCorr(m3)$"year")$stddev, 3)
sd_country3 <- round(attributes(VarCorr(m3)$"country")$stddev, 3)

sd_year1cs <- round(attributes(VarCorr(m1cs)$"year")$stddev, 3)
sd_country1cs <- round(attributes(VarCorr(m1cs)$"country")$stddev, 3)

sd_year2cs <- round(attributes(VarCorr(m2cs)$"year")$stddev, 3)
sd_country2cs <- round(attributes(VarCorr(m2cs)$"country")$stddev, 3)

sd_year3cs <- round(attributes(VarCorr(m3cs)$"year")$stddev, 3)
sd_country3cs <- round(attributes(VarCorr(m3cs)$"country")$stddev, 3)

# calculate standard errors clustered by cabinet

tribble(~stat, ~m1, ~m2, ~m3, ~m1cs, ~m2cs, ~m3cs,
        "Number of Years", num_year, num_year, num_year, num_year, num_year, num_year,
        "Number of Governments", num_country, num_country, num_country, num_country, num_country, num_country,
        "sd(Year)", sd_year1, sd_year2, sd_year3, sd_year1cs, sd_year2cs, sd_year3cs,
        "sd(Country)", sd_country1, sd_country2, sd_country3, sd_country1cs, sd_country2cs, sd_country3cs,
        "", NA, NA, NA, NA, NA, NA,
        "N", nobs(m1), nobs(m2), nobs(m3),nobs(m1cs), nobs(m2cs), nobs(m3cs)) -> 
  mod_stats


# create table
stargazer(m1, m2, m3, m1cs, m2cs, m3cs, type="latex", 
          # Below: manually supply tidied coefficients and standard errors
          coef = list(tidym1$estimate, tidym2$estimate, tidym3$estimate, tidym1cs$estimate,tidym2cs$estimate,tidym3cs$estimate),
          se = list(tidym1$std.error, tidym2$std.error, tidym3$std.error, tidym1cs$std.error,tidym2cs$std.error,tidym3cs$std.error),
          omit=c("year","country"),
          # Omit model statistics by default...
          omit.table.layout = "s",
          # ...but supply your own that you created (with random effects)
          add.lines = lapply(1:nrow(mod_stats), function(i) unlist(mod_stats[i, ])),
          #covariate.labels = c("Populist","Gov Brief","2/3 Majority", "Total Laws", "Security", "Migration","Social","Plaintiff (Court)","Abstract Review"),
          #notes="<small>Data: ESS, Round 9 (United Kingdom)</small>",
          dep.var.labels="Decision",
          model.names = FALSE
          #column.labels = c("Pooled", "Fixed Effects", "Fixed Effects + Controls", "Multilevel + Controls")
)


interplot(m3,
          var1="evnt",
          var2="gov_popul_weighted",
          hist=TRUE) +
  xlab("Government Populism Score") +
  ylab("Effect of Constitutional Change") +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  ggtitle("Liberal Democracy") -> 
  p1

p1

ggsave("populismscoreregI.pdf",width=8,height=6,units="in",device=cairo_pdf)
#ggsave("populismscoreregI.png",width=8,height=6,units="in")


interplot(m3cs,
          var1="evnt",
          var2="gov_popul_weighted",
          hist=TRUE) +
  xlab("Government Populism Score") +
  ylab("Effect of Constitutional Change") +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  ggtitle("Civil Society Index") -> 
  p2

p2
ggsave("populismscoreregII.pdf",width=8,height=6,units="in",dev=cairo_pdf)
ggsave("populismscoreregII.png",width=8,height=6,units="in")

# combine libdem and civil plot for europe
grid.arrange(p1,p2,ncol=2)
arrangeGrob(p1,p2,ncol=2) -> g

ggsave("populismscoreregIIb.pdf",g,width=12,height=6,units="in",dev=cairo_pdf)
ggsave("populismscoreregIIb.png",width=12,height=6,units="in")

# Populist PM ----

# populist pm -> lib dem
pm1 <- lmer(lead(v2x_libdem,1) ~ gov_popul_prime + (1|country) + (1|year), data=df2)
summary(pm1)

# change in constitution in general -> lib dem 
pm2 <- lmer(lead(v2x_libdem,1) ~ evnt + (1|country) + (1|year), data=df2)
summary(pm2) 

# interaction -> libdem
pm3 <- lmer(lead(v2x_libdem,1) ~ evnt*gov_popul_prime + (1|country) + (1|year), data=df2)
summary(pm3)

# populist pm -> part
pm1cs <- lmer(lead(v2x_cspart,1) ~ gov_popul_prime + (1|country) + (1|year), data=df2)
summary(pm1cs) 

# change in constitution in general -> part
pm2cs <- lmer(lead(v2x_cspart,1) ~ evnt + (1|country) + (1|year), data=df2)
summary(pm2cs) 

# interaction -> part
pm3cs <- lmer(lead(v2x_cspart,1) ~ evnt*gov_popul_prime + (1|country) + (1|year), data=df2)
summary(pm3cs)


# LATINAMERICA ----

ccpc_vdem %>% 
  filter(e_regiongeo %in% c(1:4,17:18),
         year > 1990,
         v2x_regime > 0) -> 
  df4

# create dummy for latin  America
df4 |> 
  mutate(latin = if_else(e_regiongeo %in% c(17,18), 1, 0)) ->
  df4


m1lat <- lmer(lead(v2x_libdem,1) ~ gov_popul_weighted + (1|country) + (1|year), data=df4)
summary(m1lat)

m2lat <- lmer(lead(v2x_libdem,1) ~ evnt + (1|country) + (1|year), data=df4)
summary(m2lat)

m3lat <- lmer(lead(v2x_libdem,1) ~ evnt*gov_popul_weighted*latin + (1|country) + (1|year), data=df4)
summary(m3lat)


m1latcs <- lmer(lead(v2x_cspart,1) ~ gov_popul_weighted + (1|country) + (1|year), data=df4)
summary(m1latcs)

m2latcs <- lmer(lead(v2x_cspart,1) ~ evnt + (1|country) + (1|year), data=df4)
summary(m2latcs)

m3latcs <- lmer(lead(v2x_cspart,1) ~ evnt*gov_popul_weighted*latin + (1|country) + (1|year), data=df4)
summary(m3latcs)

# make table
tidym1lat <- broom::tidy(m1lat) %>% filter(effect=="fixed")
tidym2lat <- broom::tidy(m2lat) %>% filter(effect=="fixed")
tidym3lat <- broom::tidy(m3lat) %>% filter(effect=="fixed")
tidym1latcs <- broom::tidy(m1latcs) %>% filter(effect=="fixed")
tidym2latcs <- broom::tidy(m2latcs) %>% filter(effect=="fixed")
tidym3latcs <- broom::tidy(m3latcs) %>% filter(effect=="fixed")

# counts of random effects
num_year <- nrow(ranef(m1lat)$year)
num_country <- nrow(ranef(m1lat)$country)


# standard deviation of random effects
sd_year1 <- round(attributes(VarCorr(m1lat)$"year")$stddev, 3)
sd_country1 <- round(attributes(VarCorr(m1lat)$"country")$stddev, 3)

sd_year2 <- round(attributes(VarCorr(m2lat)$"year")$stddev, 3)
sd_country2 <- round(attributes(VarCorr(m2lat)$"country")$stddev, 3)

sd_year3 <- round(attributes(VarCorr(m3lat)$"year")$stddev, 3)
sd_country3 <- round(attributes(VarCorr(m3lat)$"country")$stddev, 3)

sd_year1cs <- round(attributes(VarCorr(m1latcs)$"year")$stddev, 3)
sd_country1cs <- round(attributes(VarCorr(m1latcs)$"country")$stddev, 3)

sd_year2cs <- round(attributes(VarCorr(m2latcs)$"year")$stddev, 3)
sd_country2cs <- round(attributes(VarCorr(m2latcs)$"country")$stddev, 3)

sd_year3cs <- round(attributes(VarCorr(m3latcs)$"year")$stddev, 3)
sd_country3cs <- round(attributes(VarCorr(m3latcs)$"country")$stddev, 3)


tribble(~stat, ~m1lat, ~m2lat, ~m3lat, ~m1latcs, ~m2latcs, ~m3latcs,
        "Number of Years", num_year, num_year, num_year, num_year, num_year, num_year,
        "Number of Governments", num_country, num_country, num_country, num_country, num_country, num_country,
        "sd(Year)", sd_year1, sd_year2, sd_year3, sd_year1cs, sd_year2cs, sd_year3cs,
        "sd(Country)", sd_country1, sd_country2, sd_country3, sd_country1cs, sd_country2cs, sd_country3cs,
        "", NA, NA, NA, NA, NA, NA,
        "N", nobs(m1lat), nobs(m2lat), nobs(m3lat),nobs(m1latcs), nobs(m2latcs), nobs(m3latcs)) -> mod_stats


# create table
stargazer(m1lat, m2lat, m3lat, m1latcs, m2latcs, m3latcs, type="latex", 
          # Below: manually supply tidied coefficients and standard errors
          coef = list(tidym1lat$estimate, tidym2lat$estimate, tidym3lat$estimate, tidym1latcs$estimate,tidym2latcs$estimate,tidym3latcs$estimate),
          se = list(tidym1lat$std.error, tidym2lat$std.error, tidym3lat$std.error, tidym1latcs$std.error,tidym2latcs$std.error,tidym3latcs$std.error),
          omit=c("year","country"),
          # Omit model statistics by default...
          omit.table.layout = "s",
          # ...but supply your own that you created (with random effects)
          add.lines = lapply(1:nrow(mod_stats), function(i) unlist(mod_stats[i, ])),
          #covariate.labels = c("Populist","Gov Brief","2/3 Majority", "Total Laws", "Security", "Migration","Social","Plaintiff (Court)","Abstract Review"),
          #notes="<small>Data: ESS, Round 9 (United Kingdom)</small>",
          dep.var.labels="Decision",
          model.names = FALSE
          #column.labels = c("Pooled", "Fixed Effects", "Fixed Effects + Controls", "Multilevel + Controls")
)

govpol_seq <- seq(min(df4$gov_popul_weighted,na.rm=TRUE),
                  max(df4$gov_popul_weighted,na.rm=TRUE),
                  0.05)

m3lat %>% 
  margins(variables="evnt",
          at=list(gov_popul_weighted = govpol_seq,latin=c(0,1))) %>% 
  summary() -> 
  meff

meff %>% 
  mutate(lower=AME -1.96*SE,upper=AME+1.96*SE) %>% 
  dplyr::select(gov_popul_weighted,latin,AME,upper,lower) -> 
  plotdf_ld

plotdf_ld |> 
  mutate(latin = if_else(latin == 0, "Europe", "Latin America")) ->
  plotdf_ld

## update theme of plots

theme_gridY <- theme_ipsum_rc(grid = "Y") +
  theme(axis.text = element_text(size = 8), 
        axis.title = element_text(size = 10), 
        strip.text = element_text(size = 10),
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.key.size = unit(0.5, "cm"),
        plot.title = element_text(size = 12, face = "plain", hjust = -0.2),
        plot.margin=grid::unit(c(1,0.5,0.5,0), "mm"),
        panel.border = element_rect(colour = "darkgrey", fill=NA, linewidth=1))

theme_set(theme_gridY)

ggplot(plotdf_ld,aes(x=gov_popul_weighted,y=AME)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "#6C6F7F") +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7) +
  geom_line() +
  facet_grid(~ latin) +
  labs(caption = "") +
  xlab("Government Populism Score") +
  ylab("") +
  ggtitle("Effect of Constitutional Change on Liberal Democracy") +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), 
                     limits = c(0, 1)) +
  ylim(-0.1, 0.1) +
  theme(panel.spacing = unit(1, "lines")) ->
  plot_ld

plot_ld

ggsave("populismscoreregIII.pdf",width=14,height=6,units="in",dev=cairo_pdf)
#ggsave("populismscoreregIII.png",width=14,height=6,units="in")


m3latcs %>% 
  margins(variables="evnt",
          at=list(gov_popul_weighted=govpol_seq,latin=c(0,1))) %>% 
  summary() -> 
  meff

meff %>% 
  mutate(lower=AME -1.96*SE,upper=AME+1.96*SE) %>% 
  dplyr::select(gov_popul_weighted,latin,AME,upper,lower) -> 
  plotdf_cs

plotdf_cs |> 
  mutate(latin = if_else(latin == 0, "Europe", "Latin America")) ->
  plotdf_cs

## update theme of plots

ggplot(plotdf_cs,
       aes(x = gov_popul_weighted, y = AME)) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "#6C6F7F") +
  geom_line() +
  facet_grid(~ latin) +
  labs(caption = "") +
  xlab("") +
  ylab("Marginal Effect") +
  ggtitle("  Effect of Constitutional Change on Civil Society") +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), 
                     limits = c(0, 1)) +
  ylim(-0.1, 0.1) +
  theme(panel.spacing = unit(1, "lines")) +
  theme(axis.text = element_text(size = 8), 
        axis.title = element_text(size = 10), 
        strip.text = element_text(size = 10),
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.key.size = unit(0.5, "cm"),
        plot.title = element_text(size = 12, face = "plain", hjust = -0.2),
        plot.margin=grid::unit(c(1,0.5,0.5,0), "mm"),
        panel.border = element_rect(colour = "darkgrey", fill=NA, linewidth=1),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())->
  plot_cs

ggsave("populismscoreregIV.pdf", width=14, height=6, units="in", dev=cairo_pdf)
ggsave("populismscoreregIV.png", width=14, height=6, units="in")

patchworked <- plot_cs + plot_ld + plot_layout(ncol = 1)

patchworked
