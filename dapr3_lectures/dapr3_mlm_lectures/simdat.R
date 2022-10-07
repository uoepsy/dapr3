library(tidyverse)
library(lme4)
set.seed(365)

m = lmer(Reaction ~ Days + (1|Subject) + (0+Days|Subject), sleepstudy)

fulldat = map_dfr(1:20, ~sleepstudy %>% mutate(Reaction = simulate(m)[,1],
                                     Country = paste(sample(letters,4),collapse=""),
                                     c_int = rnorm(1,0,20),
                                     Subject = paste0(Country,Subject)))
fulldat = fulldat %>% mutate(
  Reaction = Reaction + round(c_int)
) %>% select(-c_int)


m = lmer(Reaction ~ Days + (1+Days|Subject)+ (1+Days|Country), fulldat)

subran = as.data.frame(ranef(m)$Subject)
subran = subran %>% 
  mutate(Subject = row.names(subran),
         Age = round(40+scale(.1*`(Intercept)` + rnorm(n(),0,4))[,1]*10),
         Days = scale(Days+rnorm(n()))[,1],
         Drug = map_dbl(Days, ~rbinom(1,1,prob=plogis(.)))
         #CaffCon = map(Days, ~100 + scale(-.1*. + rnorm(10,0,3))[,1]*30),
         #Days = map(Subject, ~0:9),
  ) %>% select(Subject, Age, Drug)
cenran = as.data.frame(ranef(m)$Country)
cenran = cenran %>% 
  mutate(Country = row.names(cenran),
         Hemisphere = sample(c("S","N"),size=n(),replace=T)
         ) %>% 
  select(Country, Hemisphere)

fulldat  = left_join(fulldat, subran) %>%
  left_join(., cenran)

require(rworldmap)
data(countryExData)
countries <- countryExData[countryExData$GEO_subregion%in%c("Central Europe","Western Europe","Eastern Europe","Australia + New Zealand"), 2]

set.seed(22)
tibble(
  Country = unique(fulldat$Country),
  Country2 = sample(countries, 20)
) %>% left_join(fulldat, .) %>% 
  mutate(Country = Country2) %>% select(-Country2) -> fulldat

fulldat = left_join(fulldat, countryExData %>% select(Country, landlock)) %>% mutate(clockchange = landlock) %>% select(-Hemisphere,-landlock)

ggplot(fulldat,aes(x=Days,y=Reaction))+
  geom_smooth(aes(group=Subject,col=Drug),method=lm,se=F)+
  facet_wrap(~Country)

# write_csv(fulldat, "../../../data/countrysleep.csv")
