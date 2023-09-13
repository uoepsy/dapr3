
library(tidyverse)
library(lme4)
set.seed(128)
ppl <- tibble(
  cl = 1:20,
  ri = rnorm(20),
  rs = scale(ri*2.5+rnorm(20))[,1],
  x = map(cl, ~rnorm(20,0,1)),
) %>% unnest %>% 
  mutate(
    x = x-min(x),
    y = (1 +ri) + (2+rs)*x +  rnorm(n(),0,1)
  )


ggplot(ppl,aes(x=x,y=y,group=cl))+
  geom_point()+
  geom_smooth(method=lm,se=F)

m1 <- lmer(y~1+x+ (1+x|cl), data=ppl)
m1z <- lmer(y~1+x+ (1+x||cl), data=ppl)

library(patchwork)
augment(m1) %>% 
  ggplot(.,aes(x=x,y=.fitted))+
  geom_line(aes(group=cl)) +
  labs(title="y~1+x+ (1+x|cl)")+
  
  augment(m1z) %>% 
  ggplot(.,aes(x=x,y=.fitted))+
  geom_line(aes(group=cl))+
  labs(title="y~1+x+ (1+x||cl)")

summary(m1)$coefficients
summary(m1z)$coefficients

cor(ranef(m1)$cl)
cor(ranef(m1z)$cl)

ppl2 <- ppl %>% mutate(pr = simulate(m1)[,1],
                       pr2 = simulate(m1z)[,1])
ggplot(ppl2,aes(x=x,group=cl))+
  #geom_smooth(method=lm,se=F,fullrange=T,aes(y=pr),col="blue")+ 
  geom_point(aes(y=pr),col="blue")+
  #geom_smooth(method=lm,se=F, fullrange=T,aes(y=pr2),col="red")+
  geom_point(aes(y=pr2),col="red")+
  geom_point(aes(y=y))+
  facet_wrap(~cl)
