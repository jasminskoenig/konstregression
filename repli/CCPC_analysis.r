# analyzing ccpc_vdem
# TS: 22.03.2022

rm(list=ls())

library(tidyverse)
library(rio)
library(lubridate)
library(stargazer)
library(rms)
library(interplot)
library(hrbrthemes)
library(lme4)
library(margins)

theme_set(theme_ipsum(base_size = 16, axis_title_size = 16, strip_text_size = 16, axis_text_size = 16))


wd <- "/Users/tswalve/Dropbox/Research/Courts under pressure/VDem Data"
setwd(wd)


load("ccpc_vdem.Rdata")

glimpse(ccpc_vdem)


# cas study, Poland and Hungary
ccpc_vdem %>% filter(country %in% c("Hungary"),year>1990) -> df
df %>% filter(evnt==1) -> dfevnt


df %>% ggplot(aes(x=year,y=v2x_libdem)) + 
geom_line(size=1) + 
geom_vline(data=dfevnt,aes(xintercept=year),color="darkgrey") + 
ylab("Demokratieindex") + xlab("Jahr") + ggtitle("Ungarn") +
geom_vline(xintercept=2010,color='#F8766D',linetype='dashed',lwd=1.1) -> p1

p1

ggsave("hungary.pdf",width=8,height=6,units="in",device = cairo_pdf)


df %>% ggplot(aes(x=year,y=gov_popul_weighted)) + 
geom_line(size=1) + 
#geom_vline(data=dfevnt,aes(xintercept=year),color="darkgrey") + 
ylab("Populismusindex") + xlab("Jahr") + ggtitle("Ungarn") +
geom_vline(xintercept=2010,color='#F8766D',linetype='dashed',lwd=1.1) -> p1

p1
ggsave("hungary_popul.pdf",width=8,height=6,units="in",device = cairo_pdf)

df %>% select(year,gov_popul_weighted)

# case study, Poland and Hungary
ccpc_vdem %>% filter(country %in% c("Poland"),year>1990) -> df
df %>% filter(evnt==1) -> dfevnt


df %>% ggplot(aes(x=year,y=v2x_libdem)) + 
geom_line(size=1) + 
geom_vline(data=dfevnt,aes(xintercept=year),color="darkgrey") + 
ylab("Demokratieindex") + xlab("Jahr") + ggtitle("Polen") +
geom_vline(xintercept=2015,color='#F8766D',linetype='dashed',lwd=1.1)  -> p2

p2

ggsave("poland.pdf",width=8,height=6,units="in",device = cairo_pdf)

df %>% ggplot(aes(x=year,y=gov_popul_weighted)) + 
geom_line(size=1) + 
#geom_vline(data=dfevnt,aes(xintercept=year),color="darkgrey") + 
ylab("Populismusindex") + xlab("Jahr") + ggtitle("Polen") +
geom_vline(xintercept=2015,color='#F8766D',linetype='dashed',lwd=1.1) -> p2

p2
ggsave("poland_popul.pdf",width=8,height=6,units="in",device = cairo_pdf)


# in general, it is not easy to see a pattern here, it could be interesting to look at constitutional amendments in Hungary and Turkey? But is Turkey really a good country?


# time for some regressions
# do constitutional events have a different effect under populist governments compared to non-populist governments?
# interaction effect between evnt and populism indicator



#ccpc_vdem %>% mutate(populist=as.numeric(gov_popul_weighted>0.7)) -> ccpc_vdem
#table(ccpc_vdem$populist)

#ccpc_vdem$year <- as.factor(ccpc_vdem$year)

# filter for Europe
ccpc_vdem$year <- as.numeric(as.character(ccpc_vdem$year))
ccpc_vdem$rightwing <- 0
ccpc_vdem$rightwing[ccpc_vdem$gov_ideo_weighted>0.5 & ccpc_vdem$gov_popul_weighted>0.5] <- 1

ccpc_vdem$leftwing <- 0
ccpc_vdem$leftwing[ccpc_vdem$gov_ideo_weighted < -0.5 & ccpc_vdem$gov_popul_weighted>0.5] <- 1

ccpc_vdem$populist <- 0
ccpc_vdem$populist[ccpc_vdem$gov_popul_weighted>0.5] <- 1

table(ccpc_vdem$rightwing)
table(ccpc_vdem$leftwing)
table(ccpc_vdem$populist)

ccpc_vdem %>% filter(e_regiongeo %in% c(1:4),year>1990,v2x_regime>0) -> df2


dd <- datadist(df2)
options(datadist = "dd")

df2$year <- as.factor(as.character(df2$year))


# descriptives
length(unique(df2$country))
unique(df2$year)
table(df2$evnt)
table(df2$evnt,df2$country)
table(df2$country,df2$v2x_regime)


table(df2$evnt,df2$gov_popul_weighted>0.5)
# nur 29 verfassungsaenderungen durch govs mit populismus score von mehr als 0.5

df2 %>% filter(gov_popul_weighted>0.5) %>% group_by(country,evnt) %>% count()  %>% filter(evnt==1)
df2 %>% filter(gov_popul_weighted>0.5) %>% filter(evnt==1) %>% dplyr::select(country,year,gov_popul_weighted,rightwing,gov_ideo_weighted)

#df2 %>% filter(country!="Hungary") -> df2



m1 <- lm(lead(v2x_libdem,1) ~ gov_popul_weighted + year, data=df2)
summary.lm(m1) # decrease as we would expect, **

# m1 hierarchical model, country+year
m1hier <- lmer(lead(v2x_libdem,1) ~ gov_popul_weighted + (1|country) + (1|year), data=df2)
summary(m1hier)

qs <- quantile(df2$gov_popul_weighted,probs=c(0.05,0.95),na.rm=TRUE)
qs[1]*-0.112 - qs[2]*-0.112

m2 <- lm(lead(v2x_libdem) ~ evnt + year, data=df2)
summary.lm(m2) # this is positive, ***

# m2 hierarchical model, country+year
m2hier <- lmer(lead(v2x_libdem,1) ~ evnt+ (1|country) + (1|year), data=df2)
summary(m2hier) # nothing

m3 <- lm(lead(v2x_libdem,1) ~ evnt*gov_popul_weighted + year + country, data=df2)
summary.lm(m3) # evnt positive, interaction is negative **


m3hier <- lmer(lead(v2x_libdem,1) ~ evnt*gov_popul_weighted+ (1|country) + (1|year), data=df2)
summary(m3hier) # 10% level significances as above

m3dum <- lm(lead(v2x_libdem,1) ~ evnt*populist + year, data=df2)
summary.lm(m3dum) # evnt positive, interaction is negative **


m1cs <- lm(lead(v2x_cspart,1) ~ gov_popul_weighted + year, data=df2)
summary.lm(m1cs) # decrease as we would expect, **

m2cs <- lm(lead(v2x_cspart,1) ~ evnt + year, data=df2)
summary.lm(m2cs) # this is positive, ***

m3cs <- lm(lead(v2x_cspart,1) ~ evnt*gov_popul_weighted + year, data=df2)
summary.lm(m3cs) # evnt positive, interaction is negative **

stargazer(m1,m2,m3,m1cs,m2cs,m3cs,omit = "year",type="html"
,out="RegTab1.html",omit.stat=c("adj.rsq","ser","f"))



interplot(m3,var1="evnt",var2="gov_popul_weighted",hist=TRUE) +
    xlab("Populismusscore der Regierung") +
    ylab("Effekt von Verfassungsereignis") +
    geom_hline(yintercept = 0, linetype = "dashed") + ggtitle("Demokratieindex") -> p1

p1

ggsave("populismscoreregI.pdf",width=8,height=6,units="in",device=cairo_pdf)


m3_cs <- lm(lead(v2x_cspart,1) ~ evnt*gov_popul_weighted + year, data=df2)
summary(m3_cs)

interplot(m3_cs,var1="evnt",var2="gov_popul_weighted",hist=TRUE) +
    xlab("Populismusscore der Regierung") +
    ylab("Effekt von Verfassungsereignis") +
    geom_hline(yintercept = 0, linetype = "dashed") + ggtitle("Zivilgesellschaftsindex") -> p2

p2
ggsave("populismscoreregII.pdf",width=8,height=6,units="in",dev=cairo_pdf)

library(gridExtra)
grid.arrange(p1,p2,ncol=2)

ggsave("populismscoreregIIb.pdf",width=12,height=6,units="in",dev=cairo_pdf)



## Europe and Latin America
ccpc_vdem %>% filter(e_regiongeo %in% c(1:4,17:18),year>1990,v2x_regime>0) -> df4
length(unique(df4$country))

table(df4$evnt)
table(df4$evnt,df4$gov_popul_weighted>0.5)
table(df4$v2x_regime)
table(df4$country,df4$v2x_regime)


df4$latin <- rep(0,nrow(df4))
df4$latin[df4$e_regiongeo %in% c(17,18)] <- 1
table(df4$latin)
nrow(df4)
table(df4$evnt,df4$gov_popul_weighted>0.5,df4$country) # 76

df4 %>% filter(country=="Peru")

df4 %>% filter(gov_popul_weighted>0.5) %>% group_by(country,evnt) %>% count()  %>% filter(evnt==1)

df4 %>% filter(gov_popul_weighted>0.5) %>% group_by(country,evnt) %>% filter(latin==1) %>% filter(evnt==1) %>% dplyr::select(country,year) -> evntlatin
print(evntlatin,n=1000)

df4 %>% filter(gov_popul_weighted>0.5) %>% group_by(country,evnt) %>% filter(latin==0) %>% filter(evnt==1) %>% dplyr::select(country,year) -> evnteurope
print(evnteurope,n=1000)

df4 %>% filter(gov_popul_weighted>0.5) %>% filter(evnt==1) %>% dplyr::select(country,year,gov_popul_weighted,rightwing,gov_ideo_weighted)

# Histogram
df4$latin

df4 %>% mutate(latinind=ifelse(latin==0,"Europa","Lateinamerika")) %>% ggplot(aes(x=gov_popul_weighted)) + 
geom_histogram(color="darkgrey",fill="lightgrey",position='identity') + 
facet_wrap(~latinind) +
#scale_fill_manual(values=c("#69b3a2", "#404080")) +
xlab("Populismusscore der Regierung (gewichtet)") + ylab("Anzahl")

ggsave("populismscorehist.pdf",width=12,height=6,units="in",dev=cairo_pdf)

df4 %>% group_by(latin) %>% filter(gov_popul_weighted>0.5) %>% count()
df4 %>% group_by(latin) %>% filter(gov_popul_weighted<=0.5) %>% count()

190/(329+190) # 37%
114/(114+1025) # 10%
(114+1025) # 1139

dd <- datadist(df4)
options(datadist = "dd")
df4$year <- as.factor(as.character(df4$year))
## reanalyse
m1latcs <- lm(lead(v2x_cspart,1) ~ gov_popul_weighted + year, data=df4)
summary.lm(m1latcs) # decrease as we would expect, ***

m1hierlatcs <- lmer(lead(v2x_cspart,1) ~ gov_popul_weighted + (1|country) + (1|year), data=df4)
summary(m1hierlatcs)

m2latcs <- lm(lead(v2x_cspart) ~ evnt + year, data=df4)
summary.lm(m2latcs) # this is positive, ***
m2hierlatcs <- lmer(lead(v2x_cspart,1) ~ evnt + (1|country) + (1|year), data=df4)
summary(m2hierlatcs)

m3latcs <- lm(lead(v2x_cspart,1) ~ evnt*gov_popul_weighted*latin + year, data=df4)
summary.lm(m3latcs) # latin negative ***
m3hierlatcs <- lmer(lead(v2x_cspart,1) ~ evnt*gov_popul_weighted*latin + (1|country) + (1|year), data=df4)
summary(m3hierlatcs)
#m4latcs <- lm(lead(v2x_cspart,1) ~ evnt*rightwing + year, data=df4)
#summary.lm(m4latcs) # latin negative ***

#m4latcs <- lm(lead(v2x_cspart,1) ~ evnt*rightwing + latin + year, data=df4)
#summary.lm(m4latcs) # latin negative ***


## reanalyse
m1lat <- lm(lead(v2x_libdem,1) ~ gov_popul_weighted + year, data=df4)
summary.lm(m1lat) # decrease as we would expect, **

m1hierlat <- lmer(lead(v2x_libdem,1) ~ gov_popul_weighted + (1|country) + (1|year), data=df4)
summary(m1hierlat)

m2lat <- lm(lead(v2x_libdem) ~ evnt + year, data=df4)
summary.lm(m2lat) # this is positive, ***
m2hierlat <- lmer(lead(v2x_libdem,1) ~ evnt + (1|country) + (1|year), data=df4)
summary(m2hierlat)


m3lat <- lm(lead(v2x_libdem,1) ~ evnt*gov_popul_weighted*latin + year, data=df4)
summary.lm(m3lat) # latin negative ***
m3hierlat <- lmer(lead(v2x_libdem,1) ~ evnt*gov_popul_weighted*latin + (1|country) + (1|year), data=df4)
summary(m3hierlat)

#m4lat <- lm(lead(v2x_libdem,1) ~ evnt*gov_popul_weighted*rightwing + year, data=df4)
#summary.lm(m4lat) # latin negative ***

stargazer(m1lat,m2lat,m3lat,m1latcs,m2latcs,m3latcs,omit = "year",type="html"
,out="RegTab2.html",omit.stat=c("adj.rsq","ser","f"))



# marginal coefficient plot by hand, no canned solution for threeway interaction
library(margins)

govpol_seq <- seq(min(df4$gov_popul_weighted,na.rm=TRUE),max(df4$gov_popul_weighted,na.rm=TRUE),0.05)

m3lat %>% 
margins(variables="evnt",at=list(gov_popul_weighted=govpol_seq,latin=c(0,1))) %>% 
summary() -> meff

m3hierlat %>% 
margins(variables="evnt",at=list(gov_popul_weighted=govpol_seq,latin=c(0,1))) %>% 
summary() -> meff

meff %>% mutate(lower=AME -1.96*SE,upper=AME+1.96*SE) %>% dplyr::select(gov_popul_weighted,latin,AME,upper,lower) -> plotdf

plotdf$latin[plotdf$latin==0] <- "Europe"
plotdf$latin[plotdf$latin==1] <- "Latin America"

ggplot(plotdf,aes(x=gov_popul_weighted,y=AME)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  facet_grid(~latin) +
  labs(caption = "") +
  xlab("Regierung-Populismus Index") +
  ylab("Effekt von Verfassungsereignis auf Demokratieindex") +
  geom_hline(yintercept = 0, linetype = "dashed")

ggsave("populismscoreregIII.pdf",width=14,height=6,units="in",dev=cairo_pdf)

%%%%%
m4lat %>% 
margins(variables="evnt",at=list(gov_popul_weighted=govpol_seq,rightwing=c(0,1))) %>% 
summary() -> meff

meff %>% mutate(lower=AME -1.96*SE,upper=AME+1.96*SE) %>% select(gov_popul_weighted,rightwing,AME,upper,lower) -> plotdf

#plotdf$latin[plotdf$latin==0] <- "Europe"
#plotdf$latin[plotdf$latin==1] <- "Latin America"

ggplot(plotdf,aes(x=gov_popul_weighted,y=AME)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  facet_grid(~rightwing) +
  labs(caption = "") +
  xlab("Regierung-Populismus Index") +
  ylab("Effekt von Verfassungsereignis auf Demokratieindex") +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed")

#m4lat <- lm(lead(v2x_cspart,1) ~ evnt*gov_popul_weighted*latin + year, data=df4)
summary.lm(m4lat) # latin negative ***
####




m3latcs %>% 
margins(variables="evnt",at=list(gov_popul_weighted=govpol_seq,latin=c(0,1))) %>% 
summary() -> meff

meff %>% mutate(lower=AME -1.96*SE,upper=AME+1.96*SE) %>% dplyr::select(gov_popul_weighted,latin,AME,upper,lower) -> plotdf

plotdf$latin[plotdf$latin==0] <- "Europe"
plotdf$latin[plotdf$latin==1] <- "Latin America"

ggplot(plotdf,aes(x=gov_popul_weighted,y=AME)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  facet_grid(~latin) +
  labs(caption = "") +
  xlab("Regierung-Populismus Index") +
  ylab("Effekt von Verfassungsereignis auf Zivilgesellschaftsindex") +
  geom_hline(yintercept = 0, linetype = "dashed")

ggsave("populismscoreregIV.pdf",width=14,height=6,units="in",dev=cairo_pdf)






# Back to Europe

# Legislative constraints on the executive -- a lot here
m3legcon <- lm(lead(v2xlg_legcon,1) ~ evnt*gov_popul_weighted*latin + year, data=df4)
summary(m3legcon)

govpol_seq <- seq(min(df4$gov_popul_weighted,na.rm=TRUE),max(df4$gov_popul_weighted,na.rm=TRUE),0.05)

m3legcon %>% 
margins(variables="evnt",at=list(gov_popul_weighted=govpol_seq,latin=c(0,1))) %>% 
summary() -> meff

meff %>% mutate(lower=AME -1.96*SE,upper=AME+1.96*SE) %>% select(gov_popul_weighted,latin,AME,upper,lower) -> plotdf

plotdf$latin[plotdf$latin==0] <- "Europe"
plotdf$latin[plotdf$latin==1] <- "Latin America"

ggplot(plotdf,aes(x=gov_popul_weighted,y=AME)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  facet_grid(~latin) +
  labs(caption = "") +
  xlab("Regierung-Populismus Index") +
  ylab("Effekt von Verfassungsereignis auf Legislative Limitation der Exekutive") +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed") -> p1

ggsave("legcon.pdf",width=14,height=6,units="in")


#interplot(m3legcon,var1="evnt",var2="gov_popul_weighted",hist=TRUE) +
#    xlab("Populismusscore der Regierung") +
#    ylab("Effekt von Verfassungsereignis auf LEGCON") +
#    theme_minimal() +
#    geom_hline(yintercept = 0, linetype = "dashed") -> p1

# Judicial constraints on the executive -- much less here
m3jucon <- lm(lead(v2x_jucon,1) ~ evnt*gov_popul_weighted*latin + year, data=df4)
summary(m3jucon)

m3jucon %>% 
margins(variables="evnt",at=list(gov_popul_weighted=govpol_seq,latin=c(0,1))) %>% 
summary() -> meff

meff %>% mutate(lower=AME -1.96*SE,upper=AME+1.96*SE) %>% select(gov_popul_weighted,latin,AME,upper,lower) -> plotdf

plotdf$latin[plotdf$latin==0] <- "Europe"
plotdf$latin[plotdf$latin==1] <- "Latin America"

ggplot(plotdf,aes(x=gov_popul_weighted,y=AME)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  facet_grid(~latin) +
  labs(caption = "") +
  xlab("Regierung-Populismus Index") +
  ylab("Effekt von Verfassungsereignis auf Judikative Limitation der Exekutive") +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed") -> p2

ggsave("jucon.pdf",width=14,height=6,units="in")

#interplot(m3jucon,var1="evnt",var2="gov_popul_weighted",hist=TRUE) +
#    xlab("Populismusscore der Regierung") +
#    ylab("Effekt von Verfassungsereignis auf JUCON") +
#    theme_minimal() +
#    geom_hline(yintercept = 0, linetype = "dashed") -> p2


# Judicial Independence
m3judind <- lm(lead(judind,1) ~ evnt*gov_popul_weighted*latin + year, data=df4)
summary(m3judind)

m3judind %>% 
margins(variables="evnt",at=list(gov_popul_weighted=govpol_seq,latin=c(0,1))) %>% 
summary() -> meff

meff %>% mutate(lower=AME -1.96*SE,upper=AME+1.96*SE) %>% select(gov_popul_weighted,latin,AME,upper,lower) -> plotdf

plotdf$latin[plotdf$latin==0] <- "Europe"
plotdf$latin[plotdf$latin==1] <- "Latin America"

ggplot(plotdf,aes(x=gov_popul_weighted,y=AME)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  facet_grid(~latin) +
  labs(caption = "") +
  xlab("Regierung-Populismus Index") +
  ylab("Effekt von Verfassungsereignis auf Unabhängigkeit der Judikative") +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed") -> p3

ggsave("judin.pdf",width=14,height=6,units="in")



#interplot(m3judind,var1="evnt",var2="gov_popul_weighted",hist=TRUE) +
#    xlab("Populismusscore der Regierung") +
#    ylab("Effekt von Verfassungsereignis auf JUDIND") +
#    theme_minimal() +
#    geom_hline(yintercept = 0, linetype = "dashed") -> p3

# Judicial Independence
m3rol <- lm(lead(v2xcl_rol,1) ~ evnt*gov_popul_weighted*latin + year, data=df4)
summary(m3rol)

m3rol %>% 
margins(variables="evnt",at=list(gov_popul_weighted=govpol_seq,latin=c(0,1))) %>% 
summary() -> meff

meff %>% mutate(lower=AME -1.96*SE,upper=AME+1.96*SE) %>% select(gov_popul_weighted,latin,AME,upper,lower) -> plotdf

plotdf$latin[plotdf$latin==0] <- "Europe"
plotdf$latin[plotdf$latin==1] <- "Latin America"

ggplot(plotdf,aes(x=gov_popul_weighted,y=AME)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  facet_grid(~latin) +
  labs(caption = "") +
  xlab("Regierung-Populismus Index") +
  ylab("Effekt von Verfassungsereignis auf den Rechtsstaat") +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed") -> p4

ggsave("rol.pdf",width=14,height=6,units="in")



#interplot(m3rol,var1="evnt",var2="gov_popul_weighted",hist=TRUE) +
#    xlab("Populismusscore der Regierung") +
#    ylab("Effekt von Verfassungsereignis auf den Rechtsstaat") +
#    theme_minimal() +
#    geom_hline(yintercept = 0, linetype = "dashed") -> p4

library(ggpubr)

ggarrange(p1,p2,p3,p4,nrow=4,ncol=1)

ggsave("populismscoreregV.pdf",width=14,height=24,units="in")
getwd()
# delibdem
m3delibdem <- lm(lead(v2x_delibdem,1) ~ evnt*gov_popul_weighted + year, data=df2)
summary(m3delibdem)

interplot(m3delibdem,var1="evnt",var2="gov_popul_weighted",hist=TRUE) +
    xlab("Populismusscore der Regierung") +
    ylab("Effekt von Verfassungsereignis auf delibdem") +
    theme_minimal() +
    geom_hline(yintercept = 0, linetype = "dashed")


# egaldem
m3egaldem <- lm(lead(v2x_egaldem,1) ~ evnt*gov_popul_weighted + year, data=df2)
summary(m3egaldem)

interplot(m3egaldem,var1="evnt",var2="gov_popul_weighted",hist=TRUE) +
    xlab("Populismusscore der Regierung") +
    ylab("Effekt von Verfassungsereignis auf delibdem") +
    theme_ipsum() +
    geom_hline(yintercept = 0, linetype = "dashed")






glimpse(df2)






m4 <- lm(v2x_libdem ~ evnt + year, data=df4)
summary(m4) 

m5 <- lm(v2x_libdem ~ evnt*gov_popul_weighted + year, data=df4)
summary(m5) # gets slightly weaker



## Civil society participation index
m1_cs <- ols(lead(v2x_cspart,1) ~ gov_popul_weighted + year, data=df2)
summary.lm(m1_cs) # decrease as we would expect

m2_cs <- ols(lead(v2x_cspart,1) ~ evnt + year, data=df2)
summary.lm(m2_cs) # this is positive

m3_cs <- ols(lead(v2x_cspart,1) ~ evnt*gov_popul_weighted + year, data=df2)
summary.lm(m3_cs) # ok, here it gets interesting






## ok, settle for  m5, just Europe
## now check other vdem scores
dd <- datadist(df2)
options(datadist = "dd")

m5 <- ols(v2x_libdem ~ evnt*gov_popul_weighted + year, data=df2)
summary.lm(m5) # ok, here it gets interesting
anova(m5)
head(df2)

m5b <- ols(lead(v2x_delibdem) ~ evnt*gov_popul_weighted + year, data=df2)
summary.lm(m5b) # this is a bit stronger!
anova(m5b)
summary(lm(v2x_delibdem ~ evnt*gov_popul_weighted + year, data=df2))

m5c <- ols(v2x_polyarchy ~ evnt*gov_popul_weighted + year, data=df2)
summary(m5c) # a bit less significance than libdem
anova(m5c)

m5d <- ols(v2x_egaldem ~ evnt*gov_popul_weighted + year, data=df2)
summary(m5d) # also a bit stronger
anova(m5d)
summary(lm(v2x_egaldem ~ evnt*gov_popul_weighted + year, data=df2))

## CONCLUSION INDICES:
# evidence for an effect is the strongest for v2x_libdem, v2x_delibdem and v2x_egaldem, less strong for v2x_polyarchy
#

m5e <- ols(lead(v2xcl_rol,1) ~ evnt*gov_popul_weighted + year, data=df2)
summary.lm(m5e) # less good

m5f <- ols(lead(v2x_jucon,1) ~ evnt*gov_popul_weighted + year, data=df2)
summary.lm(m5f) # nothing

m5g <- ols(lead(v2jureform,1) ~ evnt*gov_popul_weighted + year, data=df2)
summary.lm(m5g) # nothing

m5h <- ols(lead(v2juncind,1) ~ evnt*gov_popul_weighted + year, data=df2)
summary.lm(m5h) # nothing


# try with leads!
df2 %>% group_by(country) %>% mutate(lag_v2x_libdem = lag(v2x_libdem),lag2_v2x_libdem=lag(lag(v2x_libdem)),lead_v2x_libdem=lead(v2x_libdem),
lead4_v2x_libdem=lead(v2x_libdem,n=4)) -> df2l

dd <- datadist(df2l)
options(datadist = "dd")

# 1 lag
m5lag <- ols(lag_v2x_libdem ~ evnt*gov_popul_weighted + year, data=df2l)
summary.lm(m5lag) # thats good!

m5lag <- ols(lag(v2x_libdem) ~ evnt*gov_popul_weighted + year, data=df2l)

# 2 lags
m5lag2 <- lm(lag2_v2x_libdem ~ evnt*gov_popul_weighted + year, data=df2l)
summary(m5lag2) # thats good!

# a lead
m5lead <- lm(lead_v2x_libdem ~ evnt*gov_popul_weighted + year, data=df2l)
summary(m5lead) # thats also there - is this weird??

# a lead
m5lead2 <- lm(lead(v2x_libdem,n=2) ~ evnt*gov_popul_weighted + year, data=df2l)
summary(m5lead2) # here it gets less

m5lead3 <- lm(lead(v2x_libdem,n=3) ~ evnt*gov_popul_weighted + year, data=df2l)
summary(m5lead3) # here its back

m5lead5 <- lm(lead(v2x_libdem,n=5) ~ evnt*gov_popul_weighted + year, data=df2l)
summary(m5lead5) # here its back

# let's graph the regression's predictions
min.popul <- min(df2$gov_popul_weighted,na.rm=TRUE)
max.popul <- max(df2$gov_popul_weighted,na.rm=TRUE)
ggplot(Predict(m5,evnt,gov_popul_weighted=c(min.popul,max.popul)))

ggplot(Predict(m5lag,evnt,gov_popul_weighted=c(min.popul,max.popul)))
# doesnt work


