library(tidyverse)
library(geepack)
library(plm)
dd <- read_csv("../../data/toyexample.csv")

clm <- plm(R_AGE ~ hrs_week, data=dd, model='pooling', index='toy_type')
summary(clm)
sqrt(diag(vcovHC(clm, method='arellano', cluster='group')))

dd <- dd %>% arrange(toy_type) %>%
  mutate(
    id = as.numeric(as.factor(toy_type))
  )
geemod  = geeglm(R_AGE ~ hrs_week, data=dd, corstr='independence', id=id)
summary(geemod)$coefficients %>% round(3)
