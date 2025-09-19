qnames = c("i just can't stop watching videos of animals",
"i spend hours scrolling through tutorials but never actually attempt any projects.",
"cats are my main source of entertainment.",
"life without the internet would be boring, empty, and joyless",
"i try to hide how long i’ve been online",
"i avoid thinking about things by scrolling on the internet",
"everything i see online is either sad or terrifying",
"all the negative stuff online makes me feel better about my own life",
"i feel better the more 'likes' i receive",
"most of my time online is spent communicating with others",
"my work suffers because of the amount of time i spend online",
"i spend a lot of time online for work",
"i check my emails very regularly",
"others in my life complain about the amount of time i spend online",
"i neglect household chores to spend more time online")


l1items = 1:10
l2items = 10:15

set.seed(87)
msim = paste0(
  paste0("Lv1 =~ ",paste0(runif(10,.4,1),"*",paste0("y",l1items), collapse="+"),"\n"),
  paste0("Lv2 =~ ",paste0(runif(6,.4,1),"*",paste0("y",l2items), collapse="+"),"\n"),
  "Lv1 ~~ .5*Lv2 
  y1 ~~ .4*y3 # animals
  y7 ~~ .5*y8 # doom
  y11 ~~ -.3*y12 # work
  "
)
df = simulateData(msim, sample.nobs = 476)
df = as.data.frame(apply(df,2,function(x) as.numeric(cut(x,7))))

ait2 <- "
lv1 =~ y1 + y2 + y3 + y4 + y5 + y6 + y7 + y8 + y9
lv2 =~ y10 + y11 + y12 + y13 + y14 + y15 
"
m.est <- cfa(ait2, df)

fitmeasures(m.est)[c("rmsea","srmr","cfi","tli")]
modindices(m.est, sort=T) |> head()

names(df) <- paste0("item_", 1:15)

write_csv(df, "../../data/doom.csv")

