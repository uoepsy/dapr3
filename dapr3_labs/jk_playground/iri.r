iri <- tibble(
  q = c("I daydream and fantasize, with some regularity, about things that might happen to me. (FS)",
"I often have tender, concerned feelings for people less fortunate than me. (EC)",
"I sometimes find it difficult to see things from the other person's point of view. (PT) (-)",
"Sometimes I don't feel very sorry for other people when they are having problems. (EC) (-)",
"I really get involved with the feelings of the characters in a novel. (FS)",
"In emergency situations, I feel apprehensive and ill-at-ease. (PD)",
"I am usually objective when I watch a movie or play, and I don't often get completely caught up in it. (FS) (-)",
"I try to look at everybody's side of a disagreement before I make a decision. (PT)",
"When I see someone being taken advantage of, I feel kind of protective towards them. (EC)",
"I sometimes feel helpless when I am in the middle of a very emotional situation. (PD)",
"I sometimes try to understand my friends better by imagining how things look from their perspective. (PT)",
"Becoming extremely involved in a good book or movie is somewhat rare for me. (FS) (-)",
"When I see someone get hurt, I tend to remain calm. (PD) (-)",
"Other people's misfortunes do not usually disturb me a great deal. (EC) (-)",
"If I'm sure I'm right about something, I don't waste much time listening to other people's arguments. (PT) (-)",
"After seeing a play or movie, I have felt as though I were one of the characters. (FS)",
"Being in a tense emotional situation scares me. (PD)",
"When I see someone being treated unfairly, I sometimes don't feel very much pity for them. (EC) (-)",
"I am usually pretty effective in dealing with emergencies. (PD) (-)",
"I am often quite touched by things that I see happen. (EC)",
"I believe that there are two sides to every question and try to look at them both. (PT)",
"I would describe myself as a pretty soft-hearted person. (EC)",
"When I watch a good movie, I can very easily put myself in the place of a leading character. (FS)",
"During emergency situations, I completely fall apart and go to pieces. (PD)",
"When I'm upset at someone, I usually try to 'put myself in their shoes' for a while. (PT)",
"When I am reading an interesting story or novel, I imagine how I would feel if the events in the story were happening to me. (FS)",
"When I see someone who badly needs help in an emergency, I go to pieces. (PD)",
"Before criticizing somebody, I try to imagine how I would feel if I were in their place. (PT)"),
  wording = sub(" *\\(.*", "", q),
  subscale = sub("^[^(]*\\(([^)]*)\\).*", "\\1", q),
  neg = grepl("(-)",q)
) |> mutate(q = paste0("item",1:n()))

head(iri)

dgp <- '
  #measurement
  FS =~ 0.65*item1 + 0.72*item12 + 0.68*item16 + 0.70*item26
  EC =~ 0.70*item2 + 0.68*item14 + 0.74*item20 + 0.66*item22
  PT =~ 0.65*item8 + 0.72*item11 + 0.68*item21 + 0.63*item28
  PD =~ 0.65*item13 + 0.70*item17 + 0.64*item24 + 0.75*item27
  # factor cors
  PT ~~ 0.65*EC
  PT ~~ -0.20*PD
  PT ~~ 0.35*FS
  EC ~~ 0.15*PD
  EC ~~ 0.40*FS
  PD ~~ 0.25*FS
  item24 ~~ 0.65*item27
'
library(lavaan)

set.seed(8887)

df <- simulateData(dgp) |>
  apply(2,\(x) cut(x,5,labels=FALSE)) |> as.data.frame()
head(df)

short_iri <- iri |> filter(q %in% names(df))

df <- df |> 
  mutate(across(short_iri$q[short_iri$neg], ~6-.))

siridat <- df

mm <- "
FS =~ item1 + item12 + item16 + item26
EC =~ item2 + item14 + item20 + item22
PT =~ item8 + item11 + item21 + item28
PD =~ item13 + item17 + item24 + item27
"
mod1 <- cfa(mm,df,std.lv=T)

fitmeasures(mod1)[c("rmsea","srmr","cfi","tli")]
#modindices(mod1,sort=T) |> head(4)
summary(mod1,std=T)


mm1 <- "
EMP =~ item1 + item12 + item16 + item26 + item2 + item14 + item20 + item22 + item8 + item11 + item21 + item28 + item13 + item17 + item24 + item27
"
mod2 <- cfa(mm1,df,std.lv=T)

anova(mod2,mod1)
library(semTools)
compareFit(mod1,mod2) |> summary()

dgp_adlsc <- '
  PT =~ 0.65*item8 + 0.70*item11 + 0.35*item21 + 0.38*item28
  EC =~ 0.72*item2 + 0.65*item14 + 0.75*item20 + 0.68*item22
  PD =~ 0.62*item13 + 0.72*item17 + 0.32*item24 + 0.74*item27
  FS =~ 0.68*item1 + 0.42*item12 + 0.70*item16 + 0.72*item26
  PT ~~ 0.40*EC
  PT ~~ -0.15*PD
  PT ~~ 0.30*FS
  EC ~~ 0.20*PD
  EC ~~ 0.42*FS
  PD ~~ 0.22*FS
  item24 ~~ 0.45*item27
'

set.seed(7423)
df2 <- simulateData(dgp_adlsc) |>
  apply(2,\(x) cut(x,5,labels=FALSE)) |> as.data.frame()

df2 <- df2 |> 
  mutate(across(short_iri$q[short_iri$neg], ~6-.))

head(df2)


m2 <- cfa(mm,df2,std.lv=T)
fitmeasures(m2)[c("rmsea","srmr","cfi","tli")]

full <- bind_rows(
  "adult" = df,
  "adolescent" = df2,
  .id = "sample"
)
mm <- "
FS =~ item1 + item12 + item16 + item26
EC =~ item2 + item14 + item20 + item22
PT =~ item8 + item11 + item21 + item28
PD =~ item13 + item17 + item24 + item27
"
modfull <- cfa(mm,full,std.lv =TRUE, group="sample")
modfullm <- cfa(mm,full,std.lv =TRUE, group="sample",
            group.equal = "loadings")
anova(modfull,modfullm)
compareFit(modfull,modfullm) |> summary()
