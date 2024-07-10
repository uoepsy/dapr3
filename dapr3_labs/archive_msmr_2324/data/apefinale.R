
# PCOG ~ age + hip + enc_complx + enclosure_size * SUBDOM + 
#   (1 + age + SUBDOM | zoo) + 
#   (1 + age | zoo:id)

simzoo <- function(seed=NULL,b1,b2,l2,z0,e){
  if(!is.null(seed)){
    set.seed(seed)
  }
  n_groups = round(runif(1,4,20))
  #npg = round(runif(n_groups,2,9))
  npg = rep(3, n_groups)
  
  g = unlist(sapply(1:n_groups, function(x) rep(x, npg[x])))
  x = unlist(sapply(1:n_groups, function(x) 1:npg[x]))
  
  c1 = rbinom(n_groups,1,prob=.4)
  #b = rbinom(n_groups,1,prob=.4)
  b = rnorm(n_groups,c1*1.4,.2)
  b = b[g]
  c1 = c1[g]
  re0 = rnorm(n_groups, sd = z0)
  re  = re0[g]
  lp = (b1 + re) + b2*b + .7*c1 + l2
  y = rnorm(length(g), mean = lp, sd = e) # create a continuous target variable
  # y_bin = rbinom(N, size = 1, prob = plogis(lp)) # create a binary target variable
  data.frame(b, c1, g=factor(g), y)
}

ss=round(runif(1,1e3,1e6))
set.seed(413422)
#set.seed(ss)
many = tibble(
  zoo = 1:13,
  zoo_int = rnorm(13,0,2),
  rc = rbinom(13,1,.4)*2,
  enc = rbinom(13,1,plogis(scale(rc)*1)),
  zoo_b2 = rnorm(13,-1.4,.7) + enc*1.11,
  zoo_l2 = (rc+enc)*.55,
  z0 = runif(13,.5,2),
  e = runif(13,.75,2.3)
) |> mutate(
    data = pmap(list(zoo_int,zoo_b2, zoo_l2,z0,e), 
                ~simzoo(b1=..1,b2=..2,l2=..3,z0=..4,e=..5))
  ) |> unnest(data)

library(lme4)

anames = randomNames::randomNames(1e3, which.names="first",
                                  sample.with.replacement = FALSE)
set.seed(65)
a_ages = round(runif(1e4,8,39))

many |> transmute(
  zoo,
  zoo_loc = factor(rc, labels=c("US","UK")),
  prev_ppt = fct_relevel(factor(c1, labels=c("Y","N")),"N"),
  enclosure_size = factor(enc, labels=c("small","big")),
  apID = anames[as.numeric(paste0(zoo,g))],
  age = a_ages[as.numeric(paste0(zoo,g))],
  DOM = round(scale(b)[,1],2),
  PCOG = round(scale(y)[,1]*4.5 + 16)
) -> apfin


apfin |> count(zoo,zoo_loc) |>
  arrange(zoo_loc) |>
  mutate(
    newid = c("Wolfgang Khler Primate Research Center","Copenhagen Zoo","Tallinn Zoo","Zoo de la Fleche","Zoo Berlin","Zoo Firenze","Edinburgh","Aspinall Zoos","Chester","Twycross","Monkey World","Wingham","Bristol")
  ) |> select(zoo, newid) |>
  left_join(
    apfin,
    y=_
  ) |>
  mutate(
    zoo = newid,
    zoo_loc = ifelse(zoo_loc=="US","EU","UK")
  ) |> select(-newid) -> apfin



ma = lmer(PCOG ~age+ prev_ppt + DOM*enclosure_size + 
            (1 + DOM  | zoo) +
            (1 | zoo:apID), data = apfin)

mu = lmer(PCOG ~ DOM*enclosure_size + 
            (1 + DOM | zoo) +
            (1 | zoo:apID), data = apfin)
sjPlot::tab_model(ma,mu)

# add in zoo location
# add in ape nationality

#summary(apfin)
write_csv(apfin,"../../data/chimpcogdiff.csv")
