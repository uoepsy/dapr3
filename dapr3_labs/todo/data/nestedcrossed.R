library(tidyverse)
library(lme4)

# init data ----
eff_seed <- sample(1:2^15, 1)
print(sprintf("Seed for session: %s", eff_seed))
set.seed(eff_seed)
set.seed(751)
n_groups = 20
N = n_groups*5
g = rep(1:n_groups, e = N/n_groups)
x = rep(1:5,n_groups)
ag = round(runif(n_groups,25,60))
a = rep(ag,e=5)
bw = rep(rbinom(n_groups,1,plogis(scale(a))),e=5)

re0 = rnorm(n_groups, sd = 3)
re = re0[g]
rex = rnorm(n_groups, sd = 1)
re_x = rex[g]
lp = (0 + re) -.19*a + (0.7 + re_x) * x -1.5*bw*x 
y = rnorm(N, mean = lp, sd = 3.1)
y_bin = rbinom(N, size = 1, prob = plogis(lp))
df = data.frame(x, g = factor(g), a,bw, y, y_bin)

pidnames = c("Holly","Tom","Dan","Umberto","Hannah","Josiah","Zachary","Steve","Graham","Sarah","Cristina","Jasna","Robert","Daniel","Aja","Martin","Hugh","Monica","Emma","John")

df = df |> transmute(
  dept="Psych",
  day = x,
  pid = map_chr(g,~pidnames[.]),
  age = a,
  CBD = factor(bw, levels=c("0","1"),labels=c("N","Y")),
  stress = y#round((scale(y)[,1]),2),
)

ggplot(df,aes(x=day,y=stress,col=CBD,group=pid))+
  geom_point()+geom_smooth(method=lm,se=F)

lmer(stress~age+I(day-1)*CBD + (1+I(day-1)|pid),df) |> summary()


# nested data ---- 

simg2 = function(dd){
  n_groups = 20
  N = n_groups*5
  g = rep(1:n_groups, e = N/n_groups)
  x = rep(1:5,n_groups)
  ag = round(runif(n_groups,25,60))
  a = rep(ag,e=5)
  bw = rep(rbinom(n_groups,1,plogis(scale(a))),e=5)
  
  re0 = rnorm(n_groups, sd = 3)
  re = re0[g]
  rex = rnorm(n_groups, sd = 1)
  re_x = rex[g]
  di = rnorm(1,0,.7)
  da = rnorm(1,-.19,.2)
  dx = rnorm(1,.7,.3)
  dbx = rnorm(1,-1.3,.2)
  lp = (di + re) + da*a + (dx + re_x) * (x-1) + dbx*bw*(x-1) 
  y = rnorm(N, mean = lp, sd = 3.5)
  y_bin = rbinom(N, size = 1, prob = plogis(lp))
  df = data.frame(x, g = factor(g), a,bw, y, y_bin)
  df |> transmute(
    dept= dd,
    g = map_chr(g,~paste0(dd,"_",.)),
    day = x,
    age = a,
    CBD = factor(bw, levels=c("0","1"),labels=c("N","Y")),
    stress = y#round((scale(y)[,1]),4),
  )
}
# while(TRUE){
  # eff_seed <- sample(1:2^15, 1)
  # print(sprintf("Seed for session: %s", eff_seed))
  # set.seed(eff_seed)
  set.seed(6901)
  df_nest <- map_dfr(c("LEL","Phil","Math","Sociology","Health & Social Science","CMVM","Business","PolSci","Art","History","Theology"),~simg2(.))
  
  df_nest <- df_nest |> group_by(g) |>
    mutate(pid = randomNames::randomNames(1, which.names="first",
                                          ethnicity=sample(1:6, 1, prob=c(.01,.1,.1,.1,.59,.1))))
  
  df_nest <- bind_rows(
    ungroup(df_nest), #|> select(-g,-anx)
    df |> mutate(dept="Psych", g=pid)
  )
  
  # mnest = lmer(stress~age+I(day-1)*CBD + (1+I(day-1)|dept:pid)+(1+I(day-1)+CBD|dept),df_nest,
  #              control=lmerControl(optimizer="bobyqa"))
#   if(is.null(mnest@optinfo$conv$lme4$messages)){
#     break
#   }
# }
# eff_seed
# VarCorr(mnest)
# dotplot.ranef.mer(ranef(mnest))
# 



dups = df_nest |> count(dept,pid) |> arrange(desc(n)) |> filter(n>5)

nd = length(df_nest$pid[df_nest$dept %in% dups$dept & df_nest$pid %in% dups$pid])

df_nest[df_nest$dept %in% dups$dept & df_nest$pid %in% dups$pid, "pid"] <- 
  paste0(df_nest$pid[df_nest$dept %in% dups$dept & df_nest$pid %in% dups$pid],
    " ",rep(sample(LETTERS),e=5)[1:nd])

df_nest <- df_nest |> mutate(
  stress = round(scale(stress)[,1],3),
  dept1 = case_when(
    dept=="CMVM"~"LEL",
    dept=="LEL"~"CMVM",
    TRUE~dept
  ),
  dept=dept1
)

mnest = lmer(stress~I(day-1)*CBD + (1+I(day-1)|dept:pid)+(1+I(day-1)+CBD|dept),df_nest)
summary(mnest)
dotplot.ranef.mer(ranef(mnest))

df <- df_nest |> filter(dept=="Psych")





# crossed data ---- 
simcross = function(t,b,bb,bc,bi){
  df |>
    mutate(
      measure = t,
      stress = stress + b + day*bb + bc*(CBD=="Y") + bi*day*(CBD=="Y")
    )
}

eff_seed <- sample(1:2^15, 1)
print(sprintf("Seed for session: %s", eff_seed))
set.seed(eff_seed)
set.seed(32182)
df_cross <- pmap_dfr(list(
  c("Self-report","LSQ","JSQ","EEQ","STAI","Informant","Cortisol",
    "Alpha-Amylase","Blood Pressure","HRV"),
  c(0,rnorm(9,0,.4)),
  c(0,rnorm(9,0,.2)),#sort(rnorm(10,0,.2),decreasing = TRUE),
  c(0,rnorm(9,0,.1)),
  c(0,sort(rnorm(9,-.1,.2),decreasing = TRUE))
),~simcross(..1,..2,..3,..4,..5)) |>
  arrange(pid, day, measure)

# df_cross <- df_cross |> group_by(measure) |>
#   mutate(score = round(scale(stress)[,1],3)) |>
#   ungroup()

ggplot(df_cross, aes(x=day,y=stress,col=CBD,group=pid))+
  geom_smooth(method=lm,se=F)+
  facet_wrap(~measure)

mcross = lmer(stress ~ age+I(day-1)*CBD + (1+I(day-1)|pid)+(1+I(day-1)*CBD|measure),df_cross,control=lmerControl(optimizer="bobyqa"))
VarCorr(mcross)
dotplot.ranef.mer(ranef(mcross))






df |> mutate(measure="Self-report") |> 
  select(dept,pid,CBD,measure,day,stress) |> 
  #arrange(dept,pid,day) |> 
  write_csv(file="../../data/stressweek1.csv")

df_nest |> mutate(measure="Self-report") |>
  select(dept,pid,CBD,measure,day,stress) |>
  #arrange(dept,pid,day) |>
  write_csv(file="../../data/stressweek_nested.csv")
 
df_cross |> 
  select(dept,pid,CBD,measure,day, stress) |>
  #arrange(dept,pid,day) |> 
  write_csv(file="../../data/stressweek_crossed.csv")

# write_csv(df,file="../../data/stressweek1.csv")
# write_csv(df_nest,file="../../data/stressweek_nested.csv")
# write_csv(df_cross,file="../../data/stressweek_crossed.csv")




