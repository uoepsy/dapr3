library(tidyverse)
library(lme4)
fulldat <- read_csv("../../../data/countrysleep.csv")
head(fulldat)

m = lmer(Reaction ~ Age + Caff + Days + 
           (1 + Days | Subject) + 
           (1 + Age + Days | Country),
         control=lmerControl(optimizer="bobyqa"), 
         REML=TRUE,
         data = fulldat)
# converging (finding a solution) but singular fit (not a solution we want)
# sometimes the warning is false positive, so we can check:
isSingular(m)
# fair enough
summary(m)
VarCorr(m)

# age | Country is explaining comparatively little variance in reaction times  
# also, focal predictor is Days  
# more important to include random slopes for these 
# (especially for |Subjects, because we'd expect differences there - people vary.)
# (do countries vary? probably lots of psychology results don't hold cross culturally)
# (do countries vary in how age influences RT? not sure. maybe Italians age a lot better than people in the UK or something?)  
m1 = lmer(Reaction ~ Age + Caff + Days + 
           (1 + Days | Subject) + 
           (1 + Days | Country),
         control=lmerControl(optimizer="bobyqa"), REML=TRUE,
         data = fulldat)
VarCorr(m1)


m2A = lmer(Reaction ~ Age + Caff + Days + 
           (1 + Days | Subject) + 
           (1 | Country),
         control=lmerControl(optimizer="bobyqa"), REML=TRUE,
         data = fulldat)

m2B = lmer(Reaction ~ Age + Caff + Days + 
           (1 + Days | Subject) + 
           (1 + Days || Country),
         control=lmerControl(optimizer="bobyqa"), REML=TRUE,
         data = fulldat)
# Countrys vary in: 
# - overall RT average
# - amount by which RT is influenced by days of sleep deprivation 
# these are NOT correlated with one another. 

plot(ranef(m1)$Country)
cor(ranef(m1)$Country)
# plot the ranef of the model _without_ the correlation parameter
plot(ranef(m2B)$Country)
cor(ranef(m2B)$Country)

# plot the cluster lm models 
ggplot(fulldat, aes(x=Days, y=Reaction,group=Country))+
  geom_smooth(method=lm, se=F)

cAIC4::cAIC(mA)
cAIC4::cAIC(mB)
BIC(mA,mB)


# which can you more easily swallow?  


