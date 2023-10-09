library(tidyverse)
library(lme4)
# obs data ----
eff_seed <- sample(1:2^15, 1)
print(sprintf("Seed for session: %s", eff_seed))
set.seed(eff_seed)
set.seed(22874)
n_groups = 20
N = n_groups*7
g = rep(1:n_groups, e = N/n_groups)
xg = rep(runif(n_groups, 3, 6), e = 7)
xd = rnorm(N,0,2)
ag = round(runif(n_groups,25,60))
a = rep(ag,e=7)
bw = rep(rbinom(n_groups,1,plogis(scale(a))),e=7)
re0 = rnorm(n_groups, sd = 3)
re = re0[g]
rex = rnorm(n_groups, sd = 1)
re_x = rex[g]
lp = (0 + re) + .35*a + (.9*bw) + 10*xg+  (-1.5 + re_x) * xd +1.2*bw*xd 
y = rnorm(N, mean = lp, sd = 2.6)
y_bin = rbinom(N, size = 1, prob = plogis(lp))
df = data.frame(x=round(xd+xg), g = factor(g), a,bw, y, y_bin)
print(sprintf("Seed for session: %s", eff_seed))
df = df |> transmute(
  ncoffees = pmax(0,x),
  caff = x*80,
  pid = map_chr(g,~paste0("person",LETTERS[.])),
  age = a,
  milk = factor(bw),
  RT = round((532+scale(y)[,1]*83)),
  anx = y_bin
) |> group_by(pid) |>
  mutate(
    ncoffees_g = mean(ncoffees),
    ncoffees_d = ncoffees - mean(ncoffees)
  )
ggplot(df,aes(x=ncoffees,y=RT,group=pid,col=milk))+
  geom_point()+geom_smooth(method=lm,se=F)
obsdf = df

lmer(RT~ 1 + age + (ncoffees_g + ncoffees_d)*milk + (1 + ncoffees_d | pid), data = obsdf) |> summary()

# main data ----
eff_seed <- sample(1:2^15, 1)
print(sprintf("Seed for session: %s", eff_seed))
set.seed(eff_seed)
set.seed(8453)
n_groups = 20
N = n_groups*7
g = rep(1:n_groups, e = N/n_groups)
x = rep(0:6,n_groups)
ag = round(runif(n_groups,25,60))
a = rep(ag,e=7)
bw = rep(rbinom(n_groups,1,plogis(scale(a))),e=7)

re0 = rnorm(n_groups, sd = 3)
re = re0[g]
rex = rnorm(n_groups, sd = 1)
re_x = rex[g]
lp = (0 + re) + .35*a + (.9 *bw) +  (-1.5 + re_x) * x +1.2*bw*x 
y = rnorm(N, mean = lp, sd = 3.6)
y_bin = rbinom(N, size = 1, prob = plogis(lp))
df = data.frame(x, g = factor(g), a,bw, y, y_bin)


#print(sprintf("Seed for session: %s", eff_seed))
# ggplot(df,aes(x=x,y=y,group=g,col=bw))+
#   geom_point()+geom_smooth(method=lm,se=F)
# library(lme4)
# sjPlot::tab_model(lmer(y~1+x+(1+x|g),df),
#                   lmer(y~1+bw*x+(1+x|g),df),
#                   lmer(y~1+a + bw*x+(1+x|g),df))
pidnames = c("Holly","Tom","Bérengère","Umberto","Hannah","Josiah","Zachary","Steve","Graham","Sarah","Cristina","Jasna","Robert","Daniel","Aja","Martin","Hugh","Monica","Emma","John")


df = df |> transmute(
  ncoffees = x,
  caff = x*80,
  pid = map_chr(g,~pidnames[.]),
  age = a,
  milk = factor(bw),
  RT = round((532+scale(y)[,1]*83)),
  anx = y_bin
)
set.seed(38)
quickdf <- df |> group_by(pid) |>
  slice_sample(n = 1)
  # summarise(
  #   RT = round(mean(RT)),
  #   age = mean(age),
  #   milk = first(milk)
  # ) |> mutate(ncoffees = round(RT*-.01 + rnorm(n(),0,2)), 
  #             ncoffees = ncoffees + abs(min(ncoffees)),
  #             caff = ncoffees*80
  # )

df = df |> select(pid,age,ncoffees,caff,milk,RT)
quickdf = quickdf |> select(pid,age,ncoffees,caff,milk,RT)
obsdf = obsdf |> select(pid,age,ncoffees,caff,milk,RT)

# nested data ---- 

simg2 = function(dd){
  n_groups = 20
  N = n_groups*7
  g = rep(1:n_groups, e = N/n_groups)
  x = rep(0:6,n_groups)
  ag = round(runif(n_groups,25,60))
  a = rep(ag,e=7)
  bw = rep(rbinom(n_groups,1,plogis(scale(a))),e=7)
  
  re0 = rnorm(n_groups, sd = 3)
  re = re0[g]
  rex = rnorm(n_groups, sd = 1)
  re_x = rex[g]
  lp = (0 + re) + .35*a + (.9 *bw) +  (-1.5 + re_x) * x +1.2*bw*x 
  y = rnorm(N, mean = lp, sd = 3.6)
  y_bin = rbinom(N, size = 1, prob = plogis(lp))
  df = data.frame(x, g = factor(g), a,bw, y, y_bin)
  df |> transmute(
    dept = dd,
    ncoffees = x,
    caff = x*80,
    g = map_chr(g,~paste0(dd,"_",.)),
    age = a,
    milk = factor(bw),
    RT = round((532+scale(y)[,1]*83)),
    anx = y_bin
  )
}
set.seed(042)
df_nest <- map_dfr(c("LEL","Phil","Math","Sociology","Health & Social Science"),~simg2(.))

df_nest <- df_nest |> group_by(g) |>
  mutate(pid = randomNames::randomNames(1, which.names="first",
                                        ethnicity=sample(1:6, 1, prob=c(.01,.1,.1,.1,.59,.1))))

df_nest <- bind_rows(
  df |> mutate(dept="Psych", g=pid),
  ungroup(df_nest) #|> select(-g,-anx)
)

tochange = df_nest |> group_by(dept, pid) |> summarise(n=n(),age=first(age)) |>
  filter(n>7) |> ungroup() |> 
  mutate(
    newnames = randomNames::randomNames(n(), which.names="first",
                                        ethnicity=sample(1:6, 1, prob=c(.01,.1,.1,.1,.59,.1)))
  )
df_nest <- left_join(
  df_nest,
  tochange |> select(-n)
) |>
  mutate(
    pid = case_when(
      !is.na(newnames) ~ newnames,
      TRUE ~ pid
    )
  ) |> select(-g,-anx,-newnames)

df_nest |> count(dept, pid) |> filter(n>7)  

mnest = lmer(RT ~ 1+age+ncoffees*milk + 
               (1+ncoffees|dept:pid)+
               (1+ncoffees+milk|dept),
             data=df_nest)

VarCorr(mnest)
# save(df_nest, file="dfnest.Rdata")
load("dfnest.Rdata")
# crossed data ---- 
simcross = function(t,b,bb){
df |>
  mutate(
    task = t,
    RT = round(RT + rnorm(n(),b,20) + ncoffees*bb)
  )
}
set.seed(235)
df_cross <- pmap_dfr(list(
  c("button","ruler catch","picture","speech","choice"),
  c(-20,-10,40,0,60),
  c(0,-5,-2,-5,20)
  ),~simcross(..1,..2,..3)) |>
  arrange(pid, ncoffees,task)






# week 1 ----

lm(RT ~ ncoffees, quickdf) |> summary()
ggplot(quickdf, aes(x=ncoffees, y=RT))+
  geom_point() +
  geom_smooth(method=lm,se=F)+
  geom_label(aes(label=pid),hjust=0,position=position_nudge(x=.1))

head(quickdf)
head(df, 15L)


ggplot(df, aes(x=ncoffees, y=RT))+
  geom_point() +
  geom_smooth(method=lm,se=F) +
  geom_label(data=filter(df,pid=="Josiah"),
             aes(label=pid),hjust=0,
             position=position_nudge(x=.1))

# counting sample sizes:
nrow(df)
df |> count(pid) |>
  nrow()

library(ICC)
ICCbare(pid,RT,df)

lm(RT ~ ncoffees, df) |> summary()



# week 2 ----

lmer(RT~ 1 + age + ncoffees + (1 + ncoffees | pid), data = df) |> summary()
lmer(RT~ 1 + age + ncoffees * milk + (1 + ncoffees | pid), data = df) |> summary()
lmer(RT~ 1 + age + ncoffees * milk + (1 + age| pid), data = df) |> summary()
lmer(RT~ 1 + age + ncoffees * milk + (1 + milk | pid), data = df) |> summary()

library(lattice)
xyplot(RT~age|pid,data=df)
xyplot(RT~milk|pid,data=df)
#compare with:
xyplot(RT~ncoffees|pid,data=df)
xyplot(RT~ncoffees|pid,data=df, type=c("p","r"))

# warning. it needs thought.. 
# you shouldn't be able to fit this model, but you can.
mm = lmer(RT~ 1 + milk + (0 + milk | pid), data = df)
summary(mm)
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


mcross = lmer(RT ~ 1+age+ncoffees*milk + (1+ncoffees|pid) + 
                (1+ncoffees|task),
              control=lmerControl(optimizer="bobyqa"),
              data=df_cross)

summary(mcross)
dotplot.ranef.mer(ranef(mcross))




#### tidy data -----

psychRT = df
psychRT_quick = quickdf
uoeRT = df_nest
psychRT_tasks = df_cross
psych_OBS = obsdf


save(psychRT, psychRT_quick, uoeRT, psych_OBS, psychRT_tasks, file="dapr3_liveRs.RData")
s
