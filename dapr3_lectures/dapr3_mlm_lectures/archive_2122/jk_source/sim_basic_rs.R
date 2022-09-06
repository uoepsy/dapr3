library(tidyverse)
set.seed(85321)
N = 200                                  # total sample size
n_groups = 20                          # number of groups
g = rep(1:n_groups, e = N/n_groups)      # the group identifier
x = rnorm(N)                             # an observation level continuous variable
b = rbinom(n_groups, size = 1, prob=.5)  # a cluster level categorical variable
b = b[g]
jx = rbinom(N, 1, .8)

sd_g = .4     # standard deviation for the random effect
sigma = .5     # standard deviation for the observation
sd_x = .4

re0 = rnorm(n_groups, sd = sd_g)  # random effects
re  = re0[g]
rex = rnorm(n_groups, sd = sd_x)  # random effects
re_x  = rex[g]
lp = (0 + re) + (-.5 + re_x)*x + .35*b + .2*jx + (-.2*b*x)

y = rnorm(N, mean = lp, sd = sigma)               # create a continuous target variable
y_bin = rbinom(N, size = 1, prob = plogis(lp))    #- create a binary target variable

d = tibble(x, b, y, y_bin, g = factor(g), jx)

ggplot(d, aes(x=x,y=y,group=g,col=factor(b)))+geom_smooth(method="lm",se=F, alpha=.2)+
  geom_point()+NULL+
  #facet_wrap(~b)+
  labs(y="emotional dysregulation",x="CRQ")

library(lme4)
lmer(y~x*b+(1+x|g),d) %>% summary

d %>% transmute(
  emot_dysreg = round(y,2),
  crq = round(x,2),
  int = fct_relevel(factor(b, levels=c(0,1),labels=c("Treatment","Control")), "Control"),
  schoolid = paste0("school",g),
  #sleep = rbinom(N, 1, .3)
  sleep = factor(jx, levels=c(0,1), labels=c("8hr+","<8hr"))
) -> df
lmer(emot_dysreg ~ crq*int + sleep + (1+crq|schoolid), data = df) %>% summary
# school performance ~ emotional dysregulation + sleeps8hrs
# emotional dysregulation ~ child routine questionnaire CRQ * int + sleeps8hrs



