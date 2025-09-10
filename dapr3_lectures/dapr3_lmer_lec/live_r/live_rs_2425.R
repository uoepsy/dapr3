library(tidyverse)
library(lme4)
pets = read_csv("https://uoepsy.github.io/data/pets_seattle.csv") |>
  filter(species=="Dog")
  
# main data ----
eff_seed <- sample(1:2^15, 1)
print(sprintf("Seed for session: %s", eff_seed))
set.seed(eff_seed)
set.seed(9373)
n_groups = 20
N = n_groups*7
g = rep(1:n_groups, e = N/n_groups)
x = rep(0:6,n_groups)
ag = round(runif(n_groups,25,60))
a = rep(ag,e=7)
bw = rep(rbinom(n_groups,1,plogis(scale(a))),e=7)

re0 = rnorm(n_groups, sd = 10)
re = re0[g]
rex = rnorm(n_groups, sd = 3)
re_x = rex[g]
lp = (0 + re) + 0*a + (.9 *bw) -  (3 + re_x) * x +1.2*bw*x 
y = rnorm(N, mean = lp, sd = 7.6)
y_bin = rbinom(N, size = 1, prob = plogis(lp))
df = data.frame(x, g = factor(g), a,bw, y, y_bin)


#print(sprintf("Seed for session: %s", eff_seed))
# ggplot(df,aes(x=x,y=y,group=g,col=bw))+
#   geom_point()+geom_smooth(method=lm,se=F)
# library(lme4)
# sjPlot::tab_model(lmer(y~1+x+(1+x|g),df),
#                   lmer(y~1+bw*x+(1+x|g),df),
#                   lmer(y~1+a + bw*x+(1+x|g),df))

#read_csv("https://uoepsy.github.io/data/pets_seattle.csv")

pidnames = c("Shadow","Shell","Dan","Dudley","Nico",
             "George","Maia","Dougal","Rufus","Dozer",
             "Jura","Isla","Sika","Rosie","Sox",
             "Nellie","Pippin","Loki","Sonika","Summer",
             "Bobby","Newt","Norman","Henry","Molly"
             )


df = df |> transmute(
  attempt_nr = x,
  #caff = x*80,
  did = map_chr(g,~pidnames[.]),
  # age = a,
  age = factor(bw, levels=c("0","1"), labels=c("puppy","adult")),
  comptime = round(scale(y)[,1]*10.3 + 39.4)
  #stress = round(scale(y)[,1]*13.4 + 28.4),
  #anx = y_bin
) |>
  mutate(
    comptime = case_when(
      did=="Rufus" ~ comptime + -1.5*attempt_nr,
      TRUE~comptime
    )
  )


m = lmer(comptime ~ attempt_nr * age + (1 + attempt_nr | did), df)

summary(m)
dotplot.ranef.mer(ranef(m))
ggplot(df,aes(x=attempt_nr,y=comptime,col=age))+
  geom_point()+
  facet_wrap(~did)


set.seed(38)
quickdf <- df |> group_by(did) |>
  slice_sample(n = 1)

df = df |> select(did,age,attempt_nr,comptime)
quickdf = quickdf |> select(did,age,attempt_nr,comptime)


# nested data ---- 

simg2 = function(dd){
  n_groups = 20
  N = n_groups*7
  g = rep(1:n_groups, e = N/n_groups)
  x = rep(0:6,n_groups)
  ag = round(runif(n_groups,25,60))
  a = rep(ag,e=7)
  bw = rep(rbinom(n_groups,1,plogis(scale(a))),e=7)
  
  re0 = rnorm(n_groups, sd = 10)
  re = re0[g]
  rex = rnorm(n_groups, sd = 3)
  re_x = rex[g]
  
  bb0 = rnorm(1,0,2)
  bbx = rnorm(1,2.5,.5)
  bbxb = runif(1,0,.7)
  lp = (bb0 + re) + 0*a + (.9 *bw) - 
    (bbx + re_x) * x + bbxb*bw*x

  y = rnorm(N, mean = lp, sd = 7.6)
  y_bin = rbinom(N, size = 1, prob = plogis(lp))
  df = data.frame(x, g = factor(g), a,bw, y, y_bin)
  df |> transmute(
    breed = dd,
    attempt_nr = x,
    #caff = x*80,
    #did = map_chr(g,~pidnames[.]),
    g = g,
    # age = a,
    age = factor(bw, levels=c("0","1"), labels=c("puppy","adult")),
    y = y,
    #comptime = round(scale(y)[,1]*10.3 + 39.4)
    #stress = round(scale(y)[,1]*13.4 + 28.4),
    #anx = y_bin
  )
}

eseed = round(runif(1,1e3,1e5))
set.seed(eseed)
set.seed(65003)

unique(pets$primary_breed)
breeds = c("Whippet","Retreiver","Corgi","Lurcher",
  "Alsation","Border Collie","Spaniel","Dachshund","Visla","Terrier","Staffordshire Terrier")

df_nest <- map_dfr(breeds,~simg2(.)) |> 
  mutate(
    comptime = round(scale(y)[,1]*10.3 + 39.4)
  ) |> 
  group_by(breed, g) |>
  mutate(
    did = sample(unique(pets$animals_name),1)
  ) |>
  ungroup()


left_join(
  df, 
  tibble(
    did=sort(unique(df$did)),
    breed=c("Border Collie","Lurcher","Staffordshire Terrier",
            "Retreiver","Whippet","Retreiver","Retreiver","Alsation",
            "Retreiver","Border Collie","Visla","Terrier","Spaniel",
            "Visla","Alsation","Border Collie","Retreiver","Alsation","Lurcher","Retreiver")  
  )
) |>
  bind_rows(df_nest) |>
  arrange(breed,did,attempt_nr) |> 
  select(-g) -> df_nest

df_nest |> count(breed, did) |> filter(n>7)  

mnest = lmer(comptime ~ 1+attempt_nr * age + 
               (1+attempt_nr|breed:did)+
               (1+attempt_nr+age|breed),
             data=df_nest)

VarCorr(mnest)
dotplot.ranef.mer(ranef(mnest))
summary(mnest)$coefficients
# save(df_nest, file="dfnest.Rdata")
# load("dfnest.Rdata")
# crossed data ---- 

simcross = function(t,b,bb){
  df |>
    mutate(
      trick = t,
      comptime = round(comptime + rnorm(n(),b,5) + attempt_nr*bb)
    )
}
set.seed(235)
df_cross <- pmap_dfr(list(
  c("walk backwards","shake head","bow","crawl","jump",
    "ring bell","speak","wave","close door","tidy up"),
  rnorm(10,0,4),
  rnorm(10,-4,3)
),~simcross(..1,..2,..3)) |>
  arrange(did, attempt_nr,trick)


mcross = lmer(comptime ~ 1+attempt_nr * age + 
                 (1+attempt_nr|did)+
                 (1+attempt_nr|trick),
               data=df_cross)

VarCorr(mcross)
dotplot.ranef.mer(ranef(mcross))



#### tidy data -----
quick_new_trick = quickdf |> transmute(DOG=did, isOld=age,trainingdays=attempt_nr,tricktime=comptime)

new_trick = df |> transmute(DOG=did, isOld=age,trainingdays=attempt_nr,
                            tricktime=comptime)
new_trick_dogbreeds = df_nest |> transmute(BREED=breed,DOG=did, 
                                isOld=age,trainingdays=attempt_nr,
                                tricktime=comptime)
many_new_tricks = df_cross |> transmute(DOG=did, isOld=age,
                                        trainingdays=attempt_nr,
                                        trick,tricktime=comptime)


lmer(tricktime ~ trainingdays * isOld + (1+trainingdays|DOG), 
     data = new_trick) |> summary()

lmer(tricktime ~ trainingdays * isOld + 
       (1+trainingdays|BREED)+
       (1+trainingdays|BREED:DOG), 
     data = new_trick_dogbreeds) |> summary()

lmer(tricktime ~ trainingdays * isOld + 
       (1+trainingdays|DOG)+
       (1+trainingdays|trick), 
     data = many_new_tricks) |> summary()


save(new_trick, new_trick_dogbreeds, many_new_tricks,
     quick_new_trick, file="dapr3_liveR_newtricks.RData")










# week 1 ----

ggplot(new_trick, aes(x=trainingdays, y=tricktime))+
  geom_point() +
  geom_smooth(method=lm,se=F) +
  geom_label(data=filter(new_trick,DOG=="Dougal"),
             aes(label=DOG),hjust=0,
             position=position_nudge(x=.1))+
  geom_label(data=filter(new_trick,DOG=="Rufus"),
             aes(label=DOG),hjust=0,
             position=position_nudge(x=.1))

# counting sample sizes:
nrow(new_trick)
new_trick |> count(DOG) |>
  summary()

library(ICC)
ICCbare(DOG, tricktime, new_trick)

# week 2 ----
lmer(tricktime ~ trainingdays + (1+trainingdays|DOG), 
     data = new_trick) |> 
  summary()

lmer(tricktime ~ trainingdays * isOld + (1+trainingdays|DOG), 
     data = new_trick) |> 
  summary()

lmer(tricktime ~ trainingdays * isOld + (1+trainingdays + isOld|DOG), 
     data = new_trick) |> 
  summary()



library(lattice)
xyplot(tricktime~trainingdays|DOG, data= new_trick)
#compare with:
xyplot(tricktime~isOld|DOG, data= new_trick)

# warning. it needs thought.. 
# you shouldn't be able to fit this model, but you can.
lmer(tricktime ~ trainingdays * isOld + (1 + isOld|DOG), 
     data = new_trick) |> 
  summary()
# the estimates in the variance components are not what you think





# week 3 ----

# imagine, instead of our experiment, we just let people drink as much coffee as they want, and we just observe them.  
ggplot(obsdf,aes(x=ncoffees,y=RT,group=pid,col=milk))+
  geom_point()+
  geom_smooth(method=lm,se=F)

library(lattice)
xyplot(RT~ncoffees|pid, obsdf, type=c("p","r"))

# two questions:
# - do people who drink more coffee than other people, have faster/slower RTs?
# - if a person drinks more coffee than they usually do, do their RTs increase/decrease?  


## Scale can affect fit ----
# works
m1 = lmer(RT~ 1 + age + ncoffees * milk + (1 + ncoffees | pid), data = df)
# doesn't work:  
m2 = lmer(RT~ 1 + age + caff * milk + (1 + caff | pid), data = df)
# why? because slope of caff is tiny compared to slope of coffees. because it's "1 increase in mg of caffeine". 
# can cause issues because the estimate variance in slopes is v small.  
VarCorr(m1)
VarCorr(m2)

## Centering can affect fit ---- 
# additionally, remember that intercepts are "when predictor is zero"
# consider the lecture data:
d3 <- read_csv("https://uoepsy.github.io/data/dapr3_mindfuldecline.csv")
library(patchwork)
ggplot(d3, aes(x=visit, y=ACE, group=ppt)) + 
  geom_point()+
  geom_line() +
  ggplot(d3, aes(x=age, y=ACE, group=ppt)) + 
  geom_point()+
  geom_line()

m1 = lmer(ACE ~ 1 + visit + (1 + visit | ppt), data = d3)
m2 = lmer(ACE ~ 1 + age + (1 + age | ppt), data = d3)
VarCorr(m1)
VarCorr(m2)

plot_m2 <- 
  broom.mixed::augment(m2) |>
  ggplot(data=_,aes(x=age,y=.fitted,group=ppt))+
  geom_point()+
  geom_line()

plot_m2 +
  stat_smooth(geom="line",method=lm,se=F,fullrange=T,lty="longdash")+
  xlim(0,78)

broom.mixed::augment(m1) |>
  ggplot(data=_,aes(x=visit,y=.fitted,group=ppt))+
  geom_point()+
  geom_line()+
  stat_smooth(geom="line",method=lm,se=F,fullrange=T,lty="longdash")+
  xlim(0,10)


# week 4 ----

mnest = lmer(RT ~ 1+age+ncoffees*milk + (1+ncoffees|dept/pid),data=df_nest)

mnest = lmer(RT ~ 1+age+ncoffees*milk + 
               (1+ncoffees|dept:pid)+
               (1+ncoffees|dept),
             data=df_nest)
summary(allFit(mnest))
xyplot(RT~milk|dept,df_nest, type=c("p","r"))
VarCorr(mnest)
dotplot.ranef.mer(ranef(mnest))


mcross = lmer(stress ~ 1+age+ncoffees*sugar + (1+ncoffees|pid) + 
                (1+ncoffees|task),
              control=lmerControl(optimizer="bobyqa"),
              data=df_cross)

summary(mcross)
dotplot.ranef.mer(ranef(mcross))




