### More Aliens
library(tidyverse)

load('R/singers.Rdata')


stat <- 1
while (stat > 0) {
  singers <- singers |> mutate(song=as.factor(sample(c('Williams','BC Camplight'),1000,replace=T)))
  mod <- glm(splatted~quality*song,family=binomial,data=singers)
  pvals <- summary(mod)$coefficients[,4]
  cok <- (coef(mod)[4] > 0)
  if(pvals[2]<.05 & pvals[3]<.05 & pvals[4]<.05 & cok) {
    stat <- 0
  }
}

singers |> ggplot(aes(x=quality,y=splatted,colour=song)) +
  geom_smooth(method="glm",method.args=list(family=binomial))
